{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances,
             GeneralizedNewtypeDeriving #-}

module Network.Mircy.Internal where

import           Control.Applicative
import           Control.Monad.State
import           Control.Monad.Reader
import qualified Data.ByteString as B
import           System.IO

newtype MircyT m a = MircyT (ReaderT Handle m a)
    deriving (Functor, Applicative, Monad, MonadTrans)

type Mircy a = MircyT IO a

runMircyT :: MircyT m a -> Handle -> m a
runMircyT (MircyT r) = runReaderT r

instance (Monad m) => MonadReader Handle (MircyT m) where
    ask = MircyT ask
    local f (MircyT m) = MircyT $ local f m

class MonadMircy m where
    getIRCHandle :: (Monad m) => m Handle

instance (Monad m) => MonadMircy (MircyT m) where
    getIRCHandle = ask

data IRCMessage = IRCReply Int B.ByteString
                | IRCError Int B.ByteString
                | IRCNotice B.ByteString B.ByteString
                | IRCUnknown B.ByteString
    deriving (Eq, Show)

data IRCCommand = IRCUser B.ByteString B.ByteString B.ByteString B.ByteString
                | IRCNick B.ByteString
                | IRCJoin B.ByteString
                | IRCPrivMsg B.ByteString B.ByteString
