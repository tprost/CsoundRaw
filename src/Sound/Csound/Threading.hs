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
module Sound.Csound.Threading where

import Control.Monad.IO.Class
import Foreign
import Foreign.C.Types

foreign import ccall "csound.h csoundGetCurrentThreadId" csoundGetCurrentThreadId' :: IO (Ptr ())
foreign import ccall "csound.h csoundCreateThreadLock" csoundCreateThreadLock' :: IO (Ptr ())

csoundGetCurrentThreadId :: MonadIO m => m (Ptr ())
csoundGetCurrentThreadId = liftIO csoundGetCurrentThreadId'

csoundCreateThreadLock :: MonadIO m => m (Ptr ())
csoundCreateThreadLock = liftIO csoundCreateThreadLock'
