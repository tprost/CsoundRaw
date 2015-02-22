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
module Sound.Csound.Opcodes (
    csoundGetNamedGens
    --csoundNewOpcodeList,
    --csoundDisposeOpcodeList,
    --csoundAppendOpcode
) where

import Control.Monad.IO.Class
import Foreign.Ptr

foreign import ccall "csound.h csoundGetNamedGens" csoundGetNamedGens' :: Ptr () -> IO (Ptr ())
--foreign import ccall "csound.h csoundNewOpcodeList" csoundNewOpcodeList'
--foreign import ccall "csound.h csoundDisposeOpcodeList" csoundDisposeOpcodeList'
--foreign import ccall "csound.h csoundAppendOpcode" csoundAppendOpcode'

csoundGetNamedGens :: MonadIO m => Ptr () -> m (Ptr ())
csoundGetNamedGens csnd = liftIO (csoundGetNamedGens' csnd)

--csoundNewOpcodeList
--csoundNewOpcodeList

--csoundDisposeOpcodeList
--csoundDisposeOpcodeList

--csoundAppendOpcode
--csoundAppendOpcode
