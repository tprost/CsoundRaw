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
module Sound.Csound.Miscellaneous (
    --csoundRunCommand,
    csoundInitTimerStruct,
    csoundGetRealTime
    --csoundGetCPUTime,
    --csoundGetRandomSeedFromTime,
    --csoundSetLanguage,
    --csoundGetEnv,
    --csoundSetGlobalEnv,
    --csoundCreateGlobalVariable,
    --csoundQueryGlobalVariable,
    --csoundQueryGlobalVariableNoCheck,
    --csoundDestroyGlobalVariable,
    --csoundRunUtility,
    --csoundListUtilities,
    --csoundDeleteUtilityList,
    --csoundGetUtilityDescription,
    --csoundRand31,
    --csoundSeedRandMT,
    --csoundRandMT,
    --csoundCreateCircularBuffer,
    --csoundReadCircularBuffer,
    --csoundPeekCircularBuffer,
    --csoundWriteCircularBuffer,
    --csoundFlushCircularBuffer,
    --csoundDestroyCircularBuffer,
    --csoundOpenLibrary,
    --csoundCloseLibrary,
    --csoundGetLibrarySymbol
) where

import Control.Monad.IO.Class
import Foreign.Ptr
import Foreign.C.Types

--foreign import ccall "csound.h csoundRunCommand" csoundRunCommand'
foreign import ccall "csound.h csoundInitTimerStruct" csoundInitTimerStruct' :: Ptr () -> IO ()
foreign import ccall "csound.h csoundGetRealTime" csoundGetRealTime' :: Ptr () -> IO CDouble
--foreign import ccall "csound.h csoundCPUTime" csoundCPUTime'
--foreign import ccall "csound.h csoundGetRandomSeedFromTime" csoundGetRandomSeedFromTime'
--foreign import ccall "csound.h csoundSetLanguage" csoundSetLanguage'
--foreign import ccall "csound.h csoundGetEnv" csoundGetEnv'
--foreign import ccall "csound.h csoundSetGlobalEnv" csoundSetGlobalEnv'
--foreign import ccall "csound.h csoundCreateGlobalVariable" csoundCreateGlobalVariable'
--foreign import ccall "csound.h csoundQueryGlobalVariable" csoundQueryGlobalVariable'
--foreign import ccall "csound.h csoundQueryGlobalVariableNoCheck" csoundQueryVariableNoCheck'
--foreign import ccall "csound.h csoundDestroyGlobalVariable" csoundDestroyGlobalVariable'
--foreign import ccall "csound.h csoundRunUtility" csoundRunUtility'
--foreign import ccall "csound.h csoundListUtilities" csoundListUtilities'
--foreign import ccall "csound.h csoundDeleteUtilityList" csoundDeleteUtilityList'
--foreign import ccall "csound.h csoundGetUtilityDescription" csoundGetUtilityDescription'
--foreign import ccall "csound.h csoundRand31" csoundRand31'
--foreign import ccall "csound.h csoundSeedRandMT" csoundSeedRandMT'
--foreign import ccall "csound.h csoundRandMT" csoundRandMT'
--foreign import ccall "csound.h csoundCreateCircularBuffer" csoundCreateCircularBuffer'
--foreign import ccall "csound.h csoundReadCircularBuffer" csoundReadCircularBuffer'
--foreign import ccall "csound.h csoundPeekCircularBuffer" csoundPeekCircularBuffer'
--foreign import ccall "csound.h csoundWriteCircularBuffer" csoundWriteCircularBuffer'
--foreign import ccall "csound.h csoundFlushCircularBuffer" csoundFlushCircularBuffer'
--foreign import ccall "csound.h csoundDestroyCircularBuffer" csoundDestroyCircularBuffer'
--foreign import ccall "csound.h csoundOpenLibrary" csoundOpenLibrary'
--foreign import ccall "csound.h csoundCloseLibrary" csoundCloseLibrary'
--foreign import ccall "csound.h csoundGetLibrarySymbol" csoundGetLibrarySymbol'

--csoundRunCommand
--csoundRunCommand

csoundInitTimerStruct :: MonadIO m => Ptr () -> m ()
csoundInitTimerStruct rtclock = liftIO (csoundInitTimerStruct' rtclock)

csoundGetRealTime :: MonadIO m => Ptr () -> m CDouble
csoundGetRealTime rtclock = liftIO (csoundGetRealTime' rtclock)

--csoundGetCPUTime
--csoundGetCPUTime

--csoundGetRandomSeedFromTime
--csoundGetRandomSeedFromTime

--csoundSetLanguage
--csoundSetLanguage

--csoundGetEnv
--csoundGetEnv

--csoundSetGlobalEnv
--csoundSetGlobalEnv

--csoundCreateGlobalVariable
--csoundCreateGlobalVariable

--csoundQueryGlobalVariable
--csoundQueryGlobalVariable

--csoundQueryGlobalVariableNoCheck
--csoundQueryGlobalVariableNoCheck

--csoundDestroyGlobalVariable
--csoundDestroyGlobalVariable

--csoundRunUtility
--csoundRunUtility

--csoundListUtilities
--csoundListUtilities

--csoundDeleteUtilityList
--csoundDeleteUtilityList

--csoundGetUtilityDescription
--csoundGetUtilityDescription

--csoundRand31
--csoundRand31

--csoundSeedRandMT
--csoundSeedRandMT

--csoundRandMT
--csoundRandMT

--csoundCreateCircularBuffer
--csoundCreateCircularBuffer

--csoundReadCircularBuffer
--csoundReadCircularBuffer

--csoundPeekCircularBuffer
--csoundPeekCircularBuffer

--csoundWriteCircularBuffer
--csoundWriteCircularBuffer

--csoundFlushCircularBuffer
--csoundFlushCircularBuffer

--csoundDestroyCircularBuffer
--csoundDestroyCircularBuffer

--csoundOpenLibrary
--csoundOpenLibrary

--csoundCloseLibrary
--csoundCloseLibrary

--csoundGetLibrarySymbol
--csoundGetLibrarySymbol
