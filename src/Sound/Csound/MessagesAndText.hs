{-# LANGUAGE ForeignFunctionInterface #-}
-------------------------------------------------------------------------------
-- |
-- Copyright    : (c) 2015 Michael Carpenter
-- License      : BSD3
-- Maintainer   : Michael Carpenter <oldmanmike.dev@gmail.com>
-- Stability    : experimental
-- Portability  : portable
--
-------------------------------------------------------------------------------
module Sound.Csound.MessagesAndText (
    csoundGetMessageLevel,
    csoundSetMessageLevel,
    csoundCreateMessageBuffer,
    csoundGetFirstMessageAttr,
    csoundPopFirstMessage,
    csoundGetMessageCnt,
    csoundDestroyMessageBuffer
) where

import Control.Monad.IO.Class
import Foreign
import Foreign.Ptr
import Foreign.C.Types

foreign import ccall "csound.h csoundGetMessageLevel" csoundGetMessageLevel' :: Ptr () -> IO CInt
foreign import ccall "csound.h csoundSetMessageLevel" csoundSetMessageLevel' :: Ptr () -> CInt -> IO ()
foreign import ccall "csound.h csoundCreateMessageBuffer" csoundCreateMessageBuffer' :: Ptr () -> CInt -> IO ()
foreign import ccall "csound.h csoundGetFirstMessageAttr" csoundGetFirstMessageAttr' :: Ptr () -> IO CInt
foreign import ccall "csound.h csoundPopFirstMessage" csoundPopFirstMessage' :: Ptr () -> IO ()
foreign import ccall "csound.h csoundGetMessageCnt" csoundGetMessageCnt' :: Ptr () -> IO CInt
foreign import ccall "csound.h csoundDestroyMessageBuffer" csoundDestroyMessageBuffer' :: Ptr () -> IO ()

csoundGetMessageLevel :: MonadIO m => Ptr () -> m CInt
csoundGetMessageLevel csoundptr = liftIO (csoundGetMessageLevel' csoundptr)

csoundSetMessageLevel :: MonadIO m => Ptr () -> CInt -> m ()
csoundSetMessageLevel csoundptr messagelvl = liftIO (csoundSetMessageLevel' csoundptr messagelvl)

csoundCreateMessageBuffer :: MonadIO m => Ptr () -> CInt -> m ()
csoundCreateMessageBuffer csoundptr tostdout = liftIO (csoundCreateMessageBuffer' csoundptr tostdout)

csoundGetFirstMessageAttr :: MonadIO m => Ptr () -> m CInt
csoundGetFirstMessageAttr csoundptr = liftIO (csoundGetFirstMessageAttr' csoundptr)

csoundPopFirstMessage :: MonadIO m => Ptr () -> m ()
csoundPopFirstMessage csoundptr = liftIO (csoundPopFirstMessage' csoundptr)

csoundGetMessageCnt :: MonadIO m => Ptr () -> m CInt
csoundGetMessageCnt csoundptr = liftIO (csoundGetMessageCnt' csoundptr)

csoundDestroyMessageBuffer :: MonadIO m => Ptr () -> m ()
csoundDestroyMessageBuffer csoundptr = liftIO (csoundDestroyMessageBuffer' csoundptr)
