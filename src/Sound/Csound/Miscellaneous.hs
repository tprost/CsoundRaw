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
    csoundGetRealTime,
    --csoundGetCPUTime,
    --csoundGetRandomSeedFromTime,
    --csoundSetLanguage,
    --csoundGetEnv,
    --csoundSetGlobalEnv,
    --csoundCreateGlobalVariable,
    csoundQueryGlobalVariable,
    csoundQueryGlobalVariableNoCheck,
    csoundDestroyGlobalVariable,
    --csoundRunUtility,
    --csoundListUtilities,
    --csoundDeleteUtilityList,
    csoundGetUtilityDescription,
    csoundRand31,
    --csoundSeedRandMT,
    --csoundRandMT,
    csoundCreateCircularBuffer,
    csoundReadCircularBuffer,
    csoundPeekCircularBuffer,
    csoundWriteCircularBuffer,
    csoundFlushCircularBuffer,
    csoundDestroyCircularBuffer,
    --csoundOpenLibrary,
    csoundCloseLibrary
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
foreign import ccall "csound.h csoundQueryGlobalVariable" csoundQueryGlobalVariable' :: Ptr () -> Ptr CChar -> IO (Ptr ())
foreign import ccall "csound.h csoundQueryGlobalVariableNoCheck" csoundQueryGlobalVariableNoCheck' :: Ptr () -> Ptr CChar -> IO (Ptr ())
foreign import ccall "csound.h csoundDestroyGlobalVariable" csoundDestroyGlobalVariable' :: Ptr () -> Ptr CChar -> IO CInt
--foreign import ccall "csound.h csoundRunUtility" csoundRunUtility'
--foreign import ccall "csound.h csoundListUtilities" csoundListUtilities'
--foreign import ccall "csound.h csoundDeleteUtilityList" csoundDeleteUtilityList'
foreign import ccall "csound.h csoundGetUtilityDescription" csoundGetUtilityDescription' :: Ptr () -> Ptr CChar -> IO (Ptr CChar)
foreign import ccall "csound.h csoundRand31" csoundRand31' :: Ptr CInt -> IO CInt
--foreign import ccall "csound.h csoundSeedRandMT" csoundSeedRandMT'
--foreign import ccall "csound.h csoundRandMT" csoundRandMT'
foreign import ccall "csound.h csoundCreateCircularBuffer" csoundCreateCircularBuffer' :: Ptr () -> CInt -> CInt -> IO (Ptr ())
foreign import ccall "csound.h csoundReadCircularBuffer" csoundReadCircularBuffer' :: Ptr () -> Ptr () -> Ptr () -> CInt -> IO CInt
foreign import ccall "csound.h csoundPeekCircularBuffer" csoundPeekCircularBuffer' :: Ptr () -> Ptr () -> Ptr () -> CInt -> IO CInt
foreign import ccall "csound.h csoundWriteCircularBuffer" csoundWriteCircularBuffer' :: Ptr () -> Ptr () -> Ptr () -> CInt -> IO CInt
foreign import ccall "csound.h csoundFlushCircularBuffer" csoundFlushCircularBuffer' :: Ptr () -> Ptr () -> IO ()
foreign import ccall "csound.h csoundDestroyCircularBuffer" csoundDestroyCircularBuffer' :: Ptr () -> Ptr () -> IO ()
--foreign import ccall "csound.h csoundOpenLibrary" csoundOpenLibrary'
foreign import ccall "csound.h csoundCloseLibrary" csoundCloseLibrary' :: Ptr () -> IO CInt
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

csoundQueryGlobalVariable :: MonadIO m => Ptr () -> Ptr CChar -> m (Ptr ())
csoundQueryGlobalVariable csnd name = liftIO (csoundQueryGlobalVariable' csnd name)

csoundQueryGlobalVariableNoCheck :: MonadIO m => Ptr () -> Ptr CChar -> m (Ptr ())
csoundQueryGlobalVariableNoCheck csnd name = liftIO (csoundQueryGlobalVariableNoCheck' csnd name)

csoundDestroyGlobalVariable :: MonadIO m => Ptr () -> Ptr CChar -> m CInt
csoundDestroyGlobalVariable csnd name = liftIO (csoundDestroyGlobalVariable' csnd name)

--csoundRunUtility
--csoundRunUtility

--csoundListUtilities
--csoundListUtilities

--csoundDeleteUtilityList
--csoundDeleteUtilityList

csoundGetUtilityDescription :: MonadIO m => Ptr () -> Ptr CChar -> m (Ptr CChar)
csoundGetUtilityDescription csnd utilname = liftIO (csoundGetUtilityDescription' csnd utilname)

csoundRand31 :: MonadIO m => Ptr CInt -> m CInt
csoundRand31 seedval = liftIO (csoundRand31' seedval)

--csoundSeedRandMT
--csoundSeedRandMT

--csoundRandMT
--csoundRandMT

csoundCreateCircularBuffer :: MonadIO m => Ptr () -> CInt -> CInt -> m (Ptr ())
csoundCreateCircularBuffer csnd numelem elemsize = liftIO (csoundCreateCircularBuffer' csnd numelem elemsize)

csoundReadCircularBuffer :: MonadIO m => Ptr () -> Ptr () -> Ptr () -> CInt -> m CInt
csoundReadCircularBuffer csnd circular_buffer out items = liftIO (csoundReadCircularBuffer' csnd circular_buffer out items)

csoundPeekCircularBuffer :: MonadIO m => Ptr () -> Ptr () -> Ptr () -> CInt -> m CInt
csoundPeekCircularBuffer csnd circular_buffer out items = liftIO (csoundPeekCircularBuffer' csnd circular_buffer out items)
 
csoundWriteCircularBuffer :: MonadIO m => Ptr () -> Ptr () -> Ptr () -> CInt -> m CInt
csoundWriteCircularBuffer csnd p inp items = liftIO (csoundWriteCircularBuffer' csnd p inp items)

csoundFlushCircularBuffer :: MonadIO m => Ptr () -> Ptr () -> m ()
csoundFlushCircularBuffer csnd p = liftIO (csoundFlushCircularBuffer' csnd p)

csoundDestroyCircularBuffer :: MonadIO m => Ptr () -> Ptr () -> m ()
csoundDestroyCircularBuffer csnd circularbuffer = liftIO (csoundDestroyCircularBuffer' csnd circularbuffer)

--csoundOpenLibrary
--csoundOpenLibrary

csoundCloseLibrary :: MonadIO m => Ptr () -> m CInt
csoundCloseLibrary library = liftIO (csoundCloseLibrary' library)

--csoundGetLibrarySymbol
--csoundGetLibrarySymbol
