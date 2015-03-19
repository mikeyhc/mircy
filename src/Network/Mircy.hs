{-# LANGUAGE OverloadedStrings #-}

module Network.Mircy where

import           Control.Exception
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
            (\_ -> sClose sock)
            (runMircyT prog)
  where
    doConnect :: Socket -> AddrInfo -> IO Handle
    doConnect sock serveraddr = do
        connect sock (addrAddress serveraddr)
        h <- socketToHandle sock ReadWriteMode
        hSetBuffering h LineBuffering
        return h

getIRCMessage :: Mircy IRCMessage 
getIRCMessage = do
    h <- getIRCHandle
    l <- lift $ B.hGetLine h
    let noticeForm = B.tail $ B.dropWhile (/= ' ') l
    return $ if "NOTICE" `B.isPrefixOf` noticeForm
        then handleNotice . B.tail $ B.dropWhile (/= ' ') noticeForm
        else IRCUnknown l

handleNotice :: B.ByteString -> IRCMessage
handleNotice l = let noticetype = B.takeWhile (/= ' ') l
                     extract = B.init . B.tail . B.tail . B.dropWhile (/= ' ') 
                     message = extract l
                 in IRCNotice noticetype message
