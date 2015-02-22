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
    csoundCreateThreadLock
    --csoundWaitThreadLock,
    --csoundWaitThreadLockNoTimeout,
    --csoundNotifyThreadLock,
    --csoundDestroyThreadLock,
    --csoundCreateMutex,
    --csoundLockMutex,
    --csoundLockMutexNoWait,
    --csoundUnlockMutex,
    --csoundDestroyMutex,
    --csoundCreateBarrier,
    --csoundDestroyBarrier,
    --csoundWaitBarrier,
    --csoundSleep
) where

import Control.Monad.IO.Class
import Foreign.Ptr

--foreign import ccall "csound.h csoundSetYieldCallback" csoundSetYieldCallback'
--foreign import ccall "csound.h csoundCreateThread" csoundCreateThread'
foreign import ccall "csound.h csoundGetCurrentThreadId" csoundGetCurrentThreadId' :: IO (Ptr ())
--foreign import ccall "csound.h csoundJoinThread" csoundJoinThread'
foreign import ccall "csound.h csoundCreateThreadLock" csoundCreateThreadLock' :: IO (Ptr ())
--foreign import ccall "csound.h csoundWaitThreadLock" csoundWaitThreadLock'
--foreign import ccall "csound.h csoundWaitThreadLockNoTimeout" csoundWaitThreadLockNoTimeout'
--foreign import ccall "csound.h csoundNotifyThreadLock" csoundNotifyThreadLock'
--foreign import ccall "csound.h csoundDestroyThreadLock" csoundDestroyThreadLock'
--foreign import ccall "csound.h csoundCreateMutex" csoundCreateMutex'
--foreign import ccall "csound.h csoundLockMutex" csoundLockMutex'
--foreign import ccall "csound.h csoundLockMutexNoWait" csoundLockMutexNoWait'
--foreign import ccall "csound.h csoundUnlockMutex" csoundUnlockMutex'
--foreign import ccall "csound.h csoundDestroyMutex" csoundDestroyMutex'
--foreign import ccall "csound.h csoundWaitBarrier" csoundWaitBarrier'
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

--csoundWaitThreadLockNoTimeout
--csoundWaitThreadLockNoTimeout

--csoundNotifyThreadLock
--csoundNotifyThreadLock

--csoundDestroyThreadLock
--csoundDestroyThreadLock

--csoundCreateMutex
--csoundCreateMutex

--csoundLockMutex
--csoundLockMutex

--csoundLockMutexNoWait
--csoundLockMutexNoWait

--csoundUnlockMutex
--csoundUnlockMutex

--csoundDestroyMutex
--csoundDestroyMutex

--csoundCreateBarrier
--csoundCreateBarrier

--csoundDestroyBarrier
--csoundDestroyBarrier
   
--csoundWaitBarrier
--csoundWaitBarrier

--csoundSleep
--csoundSleep
