{-# LANGUAGE OverloadedStrings #-}

module Network.Mircy
    -- datatypes
    ( MircyT (..)
    , Mircy
    , IRCMessage (..)
    , IRCCommand (..)

    -- class
    , MonadMircy (..)

    -- functions
    , runMircyT
    , runMircy
    , getIRCMessage
    , sendIRCCommand
    )where

import           Control.Applicative
import           Control.Exception
import           Control.Monad.IO.Class
import           Control.Monad.Trans
import qualified Data.ByteString.Char8 as B
import           Network.Mircy.Internal
import           Network.Socket
import           System.IO 
type Port = String

runMircy :: HostName -> Port -> Mircy a -> IO a
runMircy hostname port prog = do
    addrinfos <- getAddrInfo Nothing (Just hostname) (Just port)
    let serveraddr = head addrinfos
    sock <- socket (addrFamily serveraddr) Stream defaultProtocol
    setSocketOption sock KeepAlive 1
    bracket (doConnect sock serveraddr)
            hClose
            (runMircyT prog)
  where
    doConnect :: Socket -> AddrInfo -> IO Handle
    doConnect sock serveraddr = do
        connect sock (addrAddress serveraddr)
        h <- socketToHandle sock ReadWriteMode
        hSetBuffering h LineBuffering
        return h

getIRCMessage :: (MonadMircy m, MonadIO m) => m IRCMessage
getIRCMessage = do
    h <- getIRCHandle
    l <- liftIO $ B.init <$> B.hGetLine h
    if "PING" `B.isPrefixOf` l
        then doPong (B.drop 5 l) >> getIRCMessage
        else return $ handleIRCMessage l

doPong :: (MonadMircy m, MonadIO m) => B.ByteString -> m ()
doPong reply = do
    h <- getIRCHandle
    liftIO . B.hPutStrLn h $ B.append "PONG " reply

handleIRCMessage :: B.ByteString -> IRCMessage
handleIRCMessage msg
    | "NOTICE" `B.isPrefixOf` replyForm = readNotice $ dropWord replyForm
    | "PRIVMSG" `B.isPrefixOf` privFrom = readPrivMsg msg
    | code >= '0' && code < '4'         = readReply replyForm
    | code == '4'                       = readError replyForm
    | otherwise                         = IRCUnknown msg
  where
    replyForm = B.tail $ B.dropWhile (/= ' ') msg
    privFrom  = dropWord msg
    code      = B.head replyForm

dropWord :: B.ByteString -> B.ByteString
dropWord = B.dropWhile (== ' ') . B.dropWhile (/= ' ')

takeWord :: B.ByteString -> B.ByteString
takeWord = B.takeWhile (/= ' ')

readNotice :: B.ByteString -> IRCMessage
readNotice l = let noticetype = B.takeWhile (/= ' ') l
                   message    = B.tail $ dropWord l
               in IRCNotice noticetype message

readReply :: B.ByteString -> IRCMessage
readReply l = let (code, command, message) = getReplyParts l
              in  IRCReply code command message

readError :: B.ByteString -> IRCMessage
readError l = let (code, command, message) = getReplyParts l
              in  IRCError code command message

getReplyParts :: B.ByteString -> (Int, B.ByteString, B.ByteString)
getReplyParts l = let code    = read . B.unpack $ B.takeWhile (/= ' ') l
                      temp    = dropWord l
                      command = B.takeWhile (/= ' ') temp
                      temp2   = dropWord temp
                      message = if B.head temp2 == ':' then B.tail temp2
                                                       else temp2
                  in (code, command, message)

readPrivMsg :: B.ByteString -> IRCMessage
readPrivMsg msg = let nick    = B.takeWhile (/= '!') $ B.tail msg
                      temp    = B.tail $ B.dropWhile (/= '!') msg
                      user    = takeWord temp
                      temp2   = dropWord $ dropWord temp
                      chan    = takeWord temp2
                      message = B.tail $ dropWord temp2
                  in IRCMsg nick user chan message

sendIRCCommand :: (MonadMircy m, MonadIO m) => IRCCommand -> m ()
sendIRCCommand (IRCUser name mode host real) = sendIRCCommand'
    $ foldl1 B.append [ "USER ", name, " ", mode, " ", host, " : ", real ]
sendIRCCommand (IRCNick nick) = sendIRCCommand' $ B.append "NICK " nick
sendIRCCommand (IRCJoin chan) = sendIRCCommand' $ B.append "JOIN " chan
sendIRCCommand (IRCPrivMsg chan msg) = sendIRCCommand'
    $ foldl1 B.append [ "PRIVMSG ", chan, " ", msg ]

sendIRCCommand' :: (MonadMircy m, MonadIO m) => B.ByteString -> m ()
sendIRCCommand' m = getIRCHandle >>= liftIO . (`B.hPutStrLn` m)
