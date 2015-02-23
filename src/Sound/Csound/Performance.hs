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
    --csoundParseOrc,
    --csoundCompileTree,
    --csoundDeleteTree,
    --csoundCompileOrc,
    --csoundEvalCode,
    --csoundInitializeCscore,
    --csoundCompileArgs,
    --csoundStart,
    --csoundCompile,
    --csoundCompileCsd,
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

--foreign import ccall "csound.h csoundParseOrc" csoundParseOrc'
--foreign import ccall "csound.h csoundCompileTree" csoundCompileTree'
--foreign import ccall "csound.h csoundDeleteTree" csoundDeleteTree'
--foreign import ccall "csound.h csoundCompileOrc" csoundCompileOrc'
--foreign import ccall "csound.h csoundEvalCode" csoundEvalCode'
--foreign import ccall "csound.h csoundInitializeCscore" csoundInitializeCscore'
--foreign import ccall "csound.h csoundCompileArgs" csoundCompileArgs'
--foreign import ccall "csound.h csoundStart" csoundStart'
--foreign import ccall "csound.h csoundCompile" csoundCompile'
--foreign import ccall "csound.h csoundCompileCsd" csoundCompileCsd'
foreign import ccall "csound.h csoundPerform" csoundPerform' :: Ptr () -> IO CInt
foreign import ccall "csound.h csoundPerformKsmps" csoundPerformKsmps' :: Ptr () -> IO CInt
foreign import ccall "csound.h csoundPerformBuffer" csoundPerformBuffer' :: Ptr () -> IO CInt
foreign import ccall "csound.h csoundStop" csoundStop' :: Ptr () -> IO ()
foreign import ccall "csound.h csoundCleanup" csoundCleanup' :: Ptr () -> IO CInt
foreign import ccall "csound.h csoundReset" csoundReset' :: Ptr () -> IO CInt

--csoundParseOrc
--csoundParseOrc

--csoundCompileTree
--csoundCompileTree

--csoundDeleteTree
--csoundDeleteTree

--csoundCompileOrc
--csoundCompileOrc

--csoundEvalCode
--csoundEvalCode

--csoundInitializeCscore
--csoundInitializeCscore

--csoundCompileArgs
--csoundCompileArgs

--csoundStart
--csoundStart

--csoundCompile
--csoundCompile

--csoundCompileCsd
--csoundCompileCsd

csoundPerform :: MonadIO m => Ptr () -> m CInt
csoundPerform csnd = liftIO (csoundPerform' csnd)

csoundPerformKsmps :: MonadIO m => Ptr () -> m CInt
csoundPerformKsmps csnd = liftIO (csoundPerformKsmps' csnd)

csoundPerformBuffer :: MonadIO m => Ptr () -> m CInt
csoundPerformBuffer csnd = liftIO (csoundPerformBuffer' csnd)

csoundStop :: MonadIO m => Ptr () -> m ()
csoundStop csnd = liftIO (csoundStop' csnd)

csoundCleanup :: MonadIO m => Ptr () -> m CInt
csoundCleanup csnd = liftIO (csoundCleanup' csnd)

csoundReset :: MonadIO m => Ptr () -> m CInt
csoundReset csnd = liftIO (csoundReset' csnd)
