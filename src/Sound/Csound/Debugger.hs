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
module Sound.Csound.Debugger (
    csoundDebuggerInit,
    csoundDebuggerClean,
    csoundClearBreakpoints,
    csoundDebuggerContinue,
    csoundDebugStop
) where

import Control.Monad.IO.Class
import Foreign
import Foreign.Ptr
import Foreign.C.Types

foreign import ccall "csound.h csoundDebuggerInit" csoundDebuggerInit' :: Ptr () -> IO ()
foreign import ccall "csound.h csoundDebuggerClean" csoundDebuggerClean' :: Ptr () -> IO ()
foreign import ccall "csound.h csoundClearBreakpoints" csoundClearBreakpoints' :: Ptr () -> IO ()
foreign import ccall "csound.h csoundDebuggerContinue" csoundDebuggerContinue' :: Ptr () -> IO ()
foreign import ccall "csound.h csoundDebugStop" csoundDebugStop' :: Ptr () -> IO ()

csoundDebuggerInit :: MonadIO m => Ptr () -> m ()
csoundDebuggerInit csoundptr = liftIO (csoundDebuggerInit' csoundptr)

csoundDebuggerClean :: MonadIO m => Ptr () -> m ()
csoundDebuggerClean csoundptr = liftIO (csoundDebuggerClean' csoundptr)

csoundClearBreakpoints :: MonadIO m => Ptr () -> m ()
csoundClearBreakpoints csoundptr = liftIO (csoundClearBreakpoints' csoundptr)

csoundDebuggerContinue :: MonadIO m => Ptr () -> m ()
csoundDebuggerContinue csoundptr = liftIO (csoundDebuggerContinue' csoundptr)

csoundDebugStop :: MonadIO m => Ptr () -> m ()
csoundDebugStop csoundptr = liftIO (csoundDebugStop' csoundptr)
