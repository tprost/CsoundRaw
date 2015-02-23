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
module Sound.Csound.ScoreHandling (
    --csoundReadScore,
    csoundGetScoreTime,
    csoundIsScorePending,
    csoundSetScorePending,
    --csoundGetScoreOffsetSeconds,
    --csoundSetScoreOffsetSeconds,
    csoundRewindScore
    --csoundSetCscoreCallback,
    --csoundScoreSort,
    --csoundScoreExtract
) where

import Control.Monad.IO.Class
import Foreign.C.Types
import Foreign.Ptr

--foreign import ccall "csound.h csoundReadScore" csoundReadScore'
foreign import ccall "csound.h csoundGetScoreTime" csoundGetScoreTime' :: Ptr () -> IO CDouble
foreign import ccall "csound.h csoundIsScorePending" csoundIsScorePending' :: Ptr () -> IO CInt
foreign import ccall "csound.h csoundSetScorePending" csoundSetScorePending' :: Ptr () -> CInt -> IO ()
--foreign import ccall "csound.h csoundGetScoreOffsetSeconds" csoundGetScoreOffsetSeconds'
--foreign import ccall "csound.h csoundSetScoreOffsetSeconds" csoundSetScoreOffsetSeconds'
foreign import ccall "csound.h csoundRewindScore" csoundRewindScore' :: Ptr () -> IO ()
--foreign import ccall "csound.h csoundSetCscoreCallback" csoundSetCscoreCallback'
--foreign import ccall "csound.h csoundScoreSort" csoundScoreSort'
--foreign import ccall "csound.h csoundScoreExtract" csoundScoreExtract'

--csoundReadScore
--csoundReadScore

csoundGetScoreTime :: MonadIO m => Ptr () -> m CDouble
csoundGetScoreTime csnd = liftIO (csoundGetScoreTime' csnd)

csoundIsScorePending :: MonadIO m => Ptr () -> m CInt
csoundIsScorePending csnd = liftIO (csoundIsScorePending' csnd)

csoundSetScorePending :: MonadIO m => Ptr () -> CInt -> m ()
csoundSetScorePending csnd pending = liftIO (csoundSetScorePending' csnd pending)

--csoundGetScoreOffsetSeconds
--csoundGetScoreOffsetSeconds
   
--csoundSetScoreOffsetSeconds
--csoundSetScoreOffsetSeconds

csoundRewindScore :: MonadIO m => Ptr () -> m ()
csoundRewindScore csnd = liftIO (csoundRewindScore' csnd)

--csoundSetCscoreCallback
--csoundSetCscoreCallback
   
--csoundScoreSort
--csoundScoreSort

--csoundScoreExtract
--csoundScoreExtract
