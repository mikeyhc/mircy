module Network.Mircy where

import Control.Exception
import Network.Mircy.Internal
import Network.Socket
import System.IO

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
