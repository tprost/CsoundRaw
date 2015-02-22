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
    csoundGetScoreTime,
    csoundIsScorePending,
    csoundSetScorePending,
    csoundRewindScore
) where

import Control.Monad.IO.Class
import Foreign
import Foreign.C.Types
import Foreign.Ptr

foreign import ccall "csound.h csoundGetScoreTime" csoundGetScoreTime' :: Ptr () -> IO CDouble
foreign import ccall "csound.h csoundIsScorePending" csoundIsScorePending' :: Ptr () -> IO CInt
foreign import ccall "csound.h csoundSetScorePending" csoundSetScorePending' :: Ptr () -> CInt -> IO ()
foreign import ccall "csound.h csoundRewindScore" csoundRewindScore' :: Ptr () -> IO ()

csoundGetScoreTime :: MonadIO m => Ptr () -> m CDouble
csoundGetScoreTime csoundptr = liftIO (csoundGetScoreTime' csoundptr)

csoundIsScorePending :: MonadIO m => Ptr () -> m CInt
csoundIsScorePending csoundptr = liftIO (csoundIsScorePending' csoundptr)

csoundSetScorePending :: MonadIO m => Ptr () -> CInt -> m ()
csoundSetScorePending csoundptr pending = liftIO (csoundSetScorePending' csoundptr pending)

csoundRewindScore :: MonadIO m => Ptr () -> m ()
csoundRewindScore csoundptr = liftIO (csoundRewindScore' csoundptr)
