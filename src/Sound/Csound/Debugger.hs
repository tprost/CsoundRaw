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
    --csoundSetInstrumentBreakpoint,
    --csoundRemoveInstrumentBreakpoint,
    csoundClearBreakpoints,
    --csoundSetBreakpointCallback,
    csoundDebuggerContinue,
    csoundDebugStop,
    --csoundDebugGetInstrument
) where

import Control.Monad.IO.Class
import Foreign
import Foreign.Ptr
import Foreign.C.Types

foreign import ccall "csdebug.h csoundDebuggerInit" csoundDebuggerInit' :: Ptr () -> IO ()
foreign import ccall "csdebug.h csoundDebuggerClean" csoundDebuggerClean' :: Ptr () -> IO ()
--foreign import ccall "csdebug.h csoundSetInstrumentBreakpoint" csoundSetInstrumentBreakpoint'
--foreign import ccall "csdebug.h csoundRemoveInstrumentBreakpoint" csoundRemoveInstrumentBreakpoint'
foreign import ccall "csdebug.h csoundClearBreakpoints" csoundClearBreakpoints' :: Ptr () -> IO ()
--foreign import ccall "csdebug.h csoundSetBreakpointCallback" csoundSetBreakpointCallback'
foreign import ccall "csdebug.h csoundDebugContinue" csoundDebugContinue' :: Ptr () -> IO ()
foreign import ccall "csdebug.h csoundDebugStop" csoundDebugStop' :: Ptr () -> IO ()
--foreign import ccall "csdebug.h csoundDebugGetInstrument" csoundDebugGetInstrument'

csoundDebuggerInit :: MonadIO m => Ptr () -> m ()
csoundDebuggerInit csoundptr = liftIO (csoundDebuggerInit' csoundptr)

csoundDebuggerClean :: MonadIO m => Ptr () -> m ()
csoundDebuggerClean csoundptr = liftIO (csoundDebuggerClean' csoundptr)

--csoundSetInstrumentBreakpoint 
--csoundSetInstrumentBreakpoint 

--csoundRemoveInstrumentBreakpoint
--csoundRemoveInstrumentBreakpoint

csoundClearBreakpoints :: MonadIO m => Ptr () -> m ()
csoundClearBreakpoints csoundptr = liftIO (csoundClearBreakpoints' csoundptr)

--csoundSetBreakpointCallback
--csoundSetBreakpointCallback

csoundDebuggerContinue :: MonadIO m => Ptr () -> m ()
csoundDebuggerContinue csoundptr = liftIO (csoundDebuggerContinue' csoundptr)

csoundDebugStop :: MonadIO m => Ptr () -> m ()
csoundDebugStop csoundptr = liftIO (csoundDebugStop' csoundptr)

--csoundDebugGetInstrument
--csoundDebugGetInstrument
