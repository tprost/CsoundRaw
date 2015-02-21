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
module Sound.Csound.Performance (
    csoundPerform,
    csoundPerformKsmps,
    csoundPerformBuffer,
    csoundStop,
    csoundCleanup,
    csoundReset
) where

import Control.Monad.IO.Class
import Foreign.Ptr
import Foreign.C.Types

foreign import ccall "csound.h csoundPerform" csoundPerform' :: Ptr () -> IO CInt
foreign import ccall "csound.h csoundPerformKsmps" csoundPerformKsmps' :: Ptr () -> IO CInt
foreign import ccall "csound.h csoundPerformBuffer" csoundPerformBuffer' :: Ptr () -> IO CInt
foreign import ccall "csound.h csoundStop" csoundStop' :: Ptr () -> IO ()
foreign import ccall "csound.h csoundCleanup" csoundCleanup' :: Ptr () -> IO CInt
foreign import ccall "csound.h csoundReset" csoundReset' :: Ptr () -> IO CInt

csoundPerform :: MonadIO m => Ptr () -> m CInt
csoundPerform csoundptr = liftIO (csoundPerform' csoundptr)

csoundPerformKsmps :: MonadIO m => Ptr () -> m CInt
csoundPerformKsmps csoundptr = liftIO (csoundPerformKsmps' csoundptr)

csoundPerformBuffer :: MonadIO m => Ptr () -> m CInt
csoundPerformBuffer csoundptr = liftIO (csoundPerformBuffer' csoundptr)

csoundStop :: MonadIO m => Ptr () -> m ()
csoundStop csoundptr = liftIO (csoundStop' csoundptr)

csoundCleanup :: MonadIO m => Ptr () -> m CInt
csoundCleanup csoundptr = liftIO (csoundCleanup' csoundptr)

csoundReset :: MonadIO m => Ptr () -> m CInt
csoundReset csoundptr = liftIO (csoundReset' csoundptr)
