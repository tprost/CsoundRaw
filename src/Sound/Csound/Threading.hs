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
module Sound.Csound.Threading (
    --csoundSetYieldCallback,
    --csoundCreateThread,
    csoundGetCurrentThreadId,
    --csoundJoinThread,
    csoundCreateThreadLock,
    --csoundWaitThreadLock,
    csoundWaitThreadLockNoTimeout,
    csoundNotifyThreadLock,
    csoundDestroyThreadLock,
    csoundCreateMutex,
    csoundLockMutex,
    csoundLockMutexNoWait,
    csoundUnlockMutex,
    csoundDestroyMutex,
    csoundCreateBarrier,
    csoundDestroyBarrier,
    csoundWaitBarrier
    --csoundSleep
) where

import Control.Monad.IO.Class
import Foreign.Ptr
import Foreign.C.Types

--foreign import ccall "csound.h csoundSetYieldCallback" csoundSetYieldCallback'
--foreign import ccall "csound.h csoundCreateThread" csoundCreateThread'
foreign import ccall "csound.h csoundGetCurrentThreadId" csoundGetCurrentThreadId' :: IO (Ptr ())
--foreign import ccall "csound.h csoundJoinThread" csoundJoinThread'
foreign import ccall "csound.h csoundCreateThreadLock" csoundCreateThreadLock' :: IO (Ptr ())
--foreign import ccall "csound.h csoundWaitThreadLock" csoundWaitThreadLock'
foreign import ccall "csound.h csoundWaitThreadLockNoTimeout" csoundWaitThreadLockNoTimeout' :: Ptr () -> IO ()
foreign import ccall "csound.h csoundNotifyThreadLock" csoundNotifyThreadLock' :: Ptr () -> IO ()
foreign import ccall "csound.h csoundDestroyThreadLock" csoundDestroyThreadLock' :: Ptr () -> IO ()
foreign import ccall "csound.h csoundCreateMutex" csoundCreateMutex' :: CInt -> IO (Ptr ())
foreign import ccall "csound.h csoundLockMutex" csoundLockMutex' :: Ptr () -> IO ()
foreign import ccall "csound.h csoundLockMutexNoWait" csoundLockMutexNoWait' :: Ptr () -> IO CInt
foreign import ccall "csound.h csoundUnlockMutex" csoundUnlockMutex' :: Ptr () -> IO ()
foreign import ccall "csound.h csoundDestroyMutex" csoundDestroyMutex' :: Ptr () -> IO ()
foreign import ccall "csound.h csoundCreateBarrier" csoundCreateBarrier' :: CUInt -> IO (Ptr ())
foreign import ccall "csound.h csoundDestroyBarrier" csoundDestroyBarrier' :: Ptr () -> IO CInt
foreign import ccall "csound.h csoundWaitBarrier" csoundWaitBarrier' :: Ptr () -> IO CInt
--foreign import ccall "csound.h csoundSleep" csoundSleep'

--csoundSetYieldCallback
--csoundSetYieldCallback

--csoundCreateThread
--csoundCreateThread

csoundGetCurrentThreadId :: MonadIO m => m (Ptr ())
csoundGetCurrentThreadId = liftIO csoundGetCurrentThreadId'

--csoundJoinThread
--csoundJoinThread

csoundCreateThreadLock :: MonadIO m => m (Ptr ())
csoundCreateThreadLock = liftIO csoundCreateThreadLock'

--csoundWaitThreadLock
--csoundWaitThreadLock

csoundWaitThreadLockNoTimeout :: MonadIO m => Ptr () -> m ()
csoundWaitThreadLockNoTimeout lock = liftIO (csoundWaitThreadLockNoTimeout' lock)

csoundNotifyThreadLock :: MonadIO m => Ptr () -> m ()
csoundNotifyThreadLock lock = liftIO (csoundNotifyThreadLock' lock)

csoundDestroyThreadLock :: MonadIO m => Ptr () -> m ()
csoundDestroyThreadLock lock = liftIO (csoundDestroyThreadLock' lock)

csoundCreateMutex :: MonadIO m => CInt -> m (Ptr ())
csoundCreateMutex isrecusive = liftIO (csoundCreateMutex' isrecusive)

csoundLockMutex :: MonadIO m => Ptr () -> m ()
csoundLockMutex mutex = liftIO (csoundLockMutex' mutex)

csoundLockMutexNoWait :: MonadIO m => Ptr () -> m CInt
csoundLockMutexNoWait mutex = liftIO (csoundLockMutexNoWait' mutex)
 
csoundUnlockMutex :: MonadIO m => Ptr () -> m ()
csoundUnlockMutex mutex = liftIO (csoundUnlockMutex' mutex)

csoundDestroyMutex :: MonadIO m => Ptr () -> m ()
csoundDestroyMutex mutex = liftIO (csoundDestroyMutex' mutex)

csoundCreateBarrier :: MonadIO m => CUInt -> m (Ptr ())
csoundCreateBarrier max = liftIO (csoundCreateBarrier' max)

csoundDestroyBarrier :: MonadIO m => Ptr () -> m CInt
csoundDestroyBarrier barrier = liftIO (csoundDestroyBarrier' barrier)
   
csoundWaitBarrier :: MonadIO m => Ptr () -> m CInt
csoundWaitBarrier barrier = liftIO (csoundWaitBarrier' barrier)

--csoundSleep
--csoundSleep
