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
module Sound.Csound.FunctionTableDisplay (
    csoundSetIsGraphable
    --csoundSetMakeGraphCallback,
    --csoundSetDrawGraphCallback,
    --csoundSetKillGraphCallback,
    --csoundSetExitGraphCallback
) where

import Control.Monad.IO.Class
import Foreign.Ptr
import Foreign.C.Types

foreign import ccall "csound.h csoundSetIsGraphable" csoundSetIsGraphable' :: Ptr () -> CInt -> IO CInt
--foreign import ccall "csound.h csoundSetMakeGraphCallback" csoundSetMakeGraphCallback'
--foreign import ccall "csound.h csoundSetDrawGraphCallback" csoundSetDrawGraphCallback'
--foreign import ccall "csound.h csoundSetKillGraphCallback" csoundSetKillGraphCallback'
--foreign import ccall "csound.h csoundSetExitGraphCallback" csoundSetExitGraphCallback'

csoundSetIsGraphable :: MonadIO m => Ptr () -> CInt -> m CInt
csoundSetIsGraphable csnd isGraphable = liftIO (csoundSetIsGraphable' csnd isGraphable)

--csoundSetMakeGraphCallback
--csoundSetMakeGraphCallback

--csoundSetDrawGraphCallback
--csoundSetDrawGraphCallback

--csoundSetKillGraphCallback
--csoundSetKillGraphCallback

--csoundSetExitGraphCallback
--csoundSetExitGraphCallback
