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
{# pointer *TagLib_Tag as Tag_ foreign newtype #}
{# pointer *TagLib_AudioProperties as AudioProperties_ foreign newtype #}

-- Hold on to a reference of TagFile so it doesn't get collected
data Tag = Tag Tag_ TagFile
data AudioProperties = AudioProperties AudioProperties_ TagFile

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
  tag_ <- liftM Tag_ $ newForeignPtr_ tagPtr
  return $ Tag tag_ tf

title :: Tag -> IO String
title (Tag t _) = withTag_ t {# call unsafe taglib_tag_title #} >>= ptfCString

artist :: Tag -> IO String
artist (Tag t _) = withTag_ t {# call unsafe taglib_tag_artist #} >>= ptfCString

album :: Tag -> IO String
album (Tag t _) = withTag_ t {# call unsafe taglib_tag_album #} >>= ptfCString

comment :: Tag -> IO String
comment (Tag t _) = withTag_ t {# call unsafe taglib_tag_comment #} >>= ptfCString

genre :: Tag -> IO String
genre (Tag t _) = withTag_ t {# call unsafe taglib_tag_genre #} >>= ptfCString

year :: Tag -> IO Integer
year (Tag t _) = liftM fromIntegral $ withTag_ t {# call unsafe taglib_tag_year #}

track :: Tag -> IO Integer
track (Tag t _) = liftM fromIntegral $ withTag_ t {# call unsafe taglib_tag_track #}

audioProperties :: TagFile -> IO AudioProperties
audioProperties tf = do
  apPtr <- withTagFile tf {# call unsafe taglib_file_audioproperties #}
  ap_ <- liftM AudioProperties_ $ newForeignPtr_ apPtr
  return $ AudioProperties ap_ tf

length :: AudioProperties -> IO Integer
length (AudioProperties ap _) = liftM fromIntegral $
  withAudioProperties_ ap {# call unsafe taglib_audioproperties_length #}

bitrate :: AudioProperties -> IO Integer
bitrate (AudioProperties ap _) = liftM fromIntegral $
  withAudioProperties_ ap {# call unsafe taglib_audioproperties_bitrate #}

sampleRate :: AudioProperties -> IO Integer
sampleRate (AudioProperties ap _) = liftM fromIntegral $
  withAudioProperties_ ap {# call unsafe taglib_audioproperties_samplerate #}

channels :: AudioProperties -> IO Integer
channels (AudioProperties ap _) = liftM fromIntegral $
  withAudioProperties_ ap {# call unsafe taglib_audioproperties_channels #}
