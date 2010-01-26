{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Solanin.TagLib (TagFile,
                       Tag,
                       AudioProperties,

                       open,
                       isValid,

                       tag,
                       title,
                       artist,
                       album,
                       comment,
                       genre,
                       year,
                       track,

                       audioProperties,
                       length,
                       bitrate,
                       sampleRate,
                       channels) where

#include <taglib/tag_c.h>

{# context lib="tag" prefix="taglib" #}

import Prelude hiding (length)
import Foreign.C
import Foreign.C.String
import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.Marshal.Alloc
import Foreign.Marshal.Utils
import Control.Monad (liftM)

{# pointer *TagLib_File as TagFile foreign newtype #}
{# pointer *TagLib_Tag as Tag foreign newtype #}
{# pointer *TagLib_AudioProperties as AudioProperties foreign newtype #}

foreign import ccall unsafe "&taglib_file_free"
  taglib_file_free :: FinalizerPtr TagFile

ptfCString :: CString -> IO String
ptfCString cs = do
  s <- peekCString cs
  free cs
  return s

open :: String -> IO (Maybe TagFile)
open f = do
  {# call unsafe taglib_set_string_management_enabled #} (fromBool False)
  filePtr <- withCString f {# call unsafe taglib_file_new #}
  if filePtr == nullPtr then return Nothing else
    liftM (Just . TagFile) $ newForeignPtr taglib_file_free filePtr

isValid :: TagFile -> IO Bool
isValid tf = liftM toBool $
  withTagFile tf {# call unsafe taglib_file_is_valid #}

tag :: TagFile -> IO Tag
tag tf = do
  tagPtr <- withTagFile tf {# call unsafe taglib_file_tag #}
  liftM Tag $ newForeignPtr_ tagPtr

title :: Tag -> IO String
title t = withTag t {# call unsafe taglib_tag_title #} >>= ptfCString

artist :: Tag -> IO String
artist t = withTag t {# call unsafe taglib_tag_artist #} >>= ptfCString

album :: Tag -> IO String
album t = withTag t {# call unsafe taglib_tag_album #} >>= ptfCString

comment :: Tag -> IO String
comment t = withTag t {# call unsafe taglib_tag_comment #} >>= ptfCString

genre :: Tag -> IO String
genre t = withTag t {# call unsafe taglib_tag_genre #} >>= ptfCString

year :: Tag -> IO Integer
year t = liftM fromIntegral $ withTag t {# call unsafe taglib_tag_year #}

track :: Tag -> IO Integer
track t = liftM fromIntegral $ withTag t {# call unsafe taglib_tag_track #}

audioProperties :: TagFile -> IO AudioProperties
audioProperties tf = do
  apPtr <- withTagFile tf {# call unsafe taglib_file_audioproperties #}
  liftM AudioProperties $ newForeignPtr_ apPtr

length :: AudioProperties -> IO Integer
length ap = liftM fromIntegral $
  withAudioProperties ap {# call unsafe taglib_audioproperties_length #}

bitrate :: AudioProperties -> IO Integer
bitrate ap = liftM fromIntegral $
  withAudioProperties ap {# call unsafe taglib_audioproperties_bitrate #}

sampleRate :: AudioProperties -> IO Integer
sampleRate ap = liftM fromIntegral $
  withAudioProperties ap {# call unsafe taglib_audioproperties_samplerate #}

channels :: AudioProperties -> IO Integer
channels ap = liftM fromIntegral $
  withAudioProperties ap {# call unsafe taglib_audioproperties_channels #}
