{-# LANGUAGE MagicHash, BangPatterns #-}

module Main (main, test) where

import           Criterion.Main
import           Control.DeepSeq
import           Data.ByteString (ByteString)
import           Data.ByteString (breakByte)
import qualified Data.ByteString.Unsafe as BS
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Internal as Internal
import           Data.List
import           Data.Word
import           Foreign
import           GHC.Exts (Ptr(..))
import           GHC.Prim
import           GHC.Word
import           GHC.Base

import           Foreign
import           Foreign.C

import Debug.Trace

main :: IO ()
main = test >> defaultMain
  [ bench " " $ nf const 1 -- to make commenting out easy
  -- , bench "version1" $ nf version1 str1 -- String, slow
  -- , bench "version2" $ nf version2 str1 -- String, slow
  , bench "version3" $ nf version3 bs1
  , bench "version4" $ nf version4 bs1_0
  , bench "version5" $ nf version5 bs1_0
  , bench "version6" $ nf version6 bs1_0
  , bench "version7" $ nf version7 bs1_0
  , bench "versionInl1" $ nf versionInl1 bs1_0
  , bench "versionInl2" $ nf versionInl2 bs1_0
  , bench "version_unrolled4" $ nf version_unrolled4 bs1_0
  , bench "version32bits" $ nf version32bits bs1_0
  , bench "version32bitsLowLevel" $ nf version32bitsLowLevel bs1_0
  , bench "versionC" $ nf versionC bs1_0
  , bench "versionC32" $ nf versionC32 bs1_0
  , bench "versionC64" $ nf versionC64 bs1_0
  , bench "versionMemchr" $ nf versionMemchr bs1_0
  , bench "versionMemchr_simple" $ nf versionMemchr_simple bs1_0
  , bench "versionMemchr_nl1" $ nf versionMemchr_nl1 bs1_0
  , bench "versionMemchr_nl2" $ nf versionMemchr_nl2 bs1_0
  , bench "versionMemchr_nl3" $ nf versionMemchr_nl3 bs1_0
  , bench "versionMemchr_nl4" $ nf versionMemchr_nl4 bs1_0
  ]

test = do
  -- These should all print the same (40000 for `bs1`)
  print $    length $ fst $ version1 str1
  print $    length $ fst $ version2 str1
  print $ BS.length $ fst $ version3 bs1
  print $ BS.length $ fst $ version4 bs1_0
  print $ BS.length $ fst $ version5 bs1_0
  print $ BS.length $ fst $ version6 bs1_0
  print $ BS.length $ fst $ version7 bs1_0
  print $ BS.length $ fst $ version_unrolled4 bs1_0
  print $ BS.length $ fst $ version32bits bs1_0
  print $ BS.length $ fst $ version32bitsLowLevel bs1_0
  print $ BS.length $ fst $ versionC bs1_0
  print $ BS.length $ fst $ versionC32 bs1_0
  print $ BS.length $ fst $ versionC64 bs1_0
  print $ BS.length $ fst $ versionMemchr bs1_0
  print $ BS.length $ fst $ versionMemchr_simple bs1_0
  print $ BS.length $ fst $ versionMemchr_nl1 bs1_0
  print $ BS.length $ fst $ versionMemchr_nl2 bs1_0
  print $ BS.length $ fst $ versionMemchr_nl3 bs1_0
  print $ BS.length $ fst $ versionMemchr_nl4 bs1_0


str1 = (concat $ replicate 10000 "abcd") ++ "\n" ++ "hello"
bs1 = BS.pack str1
bs1_0 = byteString0 bs1


-- Version 1

version1 :: String -> (String, String)
version1 str = break test str
  where
    test x = x `elem` " \r\n$"


-- Version 2

version2 :: String -> (String, String)
version2 str = break test str
  where
    test x = x == ' ' || x == '\r' || x == '\n' || x == '$'


-- Version 3

version3 :: ByteString -> (ByteString, ByteString)
version3 str = BS.break test str
  where
    test x = x == ' ' || x == '\r' || x == '\n' || x == '$'


-- Version 4

newtype ByteString0 = BS0 ByteString deriving (Show)

instance NFData ByteString0 where rnf (BS0 b) = b `seq` ()

byteString0 :: ByteString -> ByteString0
byteString0 x = BS0 $ x `BS.snoc` '\0'

chr :: Ptr Word8 -> Char
chr x = Internal.w2c $ Internal.inlinePerformIO $ peek x

inc :: Ptr Word8 -> Ptr Word8
inc x = x `plusPtr` 1

{-# INLINE byteString0 #-}
{-# INLINE chr #-}
{-# INLINE inc #-}


break0_1 :: (Char -> Bool) -> ByteString0 -> (ByteString, ByteString0)
break0_1 f (BS0 bs) = (BS.unsafeTake i bs, BS0 $ BS.unsafeDrop i bs)
    where
        i = Internal.inlinePerformIO $ BS.unsafeUseAsCString bs $ \ptr -> do
            let start = castPtr ptr :: Ptr Word8
            let end = go start
            return $! end `minusPtr` start

        go s | c == '\0' || f c = s
             | otherwise = go $ inc s
            where c = chr s

version4 :: ByteString0 -> (ByteString, ByteString0)
version4 str = break0_1 test str
  where
    test x = x == ' ' || x == '\r' || x == '\n' || x == '$'


-- Version 5

break0_2 :: (Char -> Bool) -> ByteString0 -> (ByteString, ByteString0)
break0_2 f (BS0 bs) = (BS.unsafeTake i bs, BS0 $ BS.unsafeDrop i bs)
    where
        i = Internal.inlinePerformIO $ BS.unsafeUseAsCString bs $ \ptr -> do
            let start = castPtr ptr :: Ptr Word8
            let end = go start
            return $! Ptr end `minusPtr` start

        go s@(Ptr a) | c == '\0' || f c = a
                     | otherwise = go $ inc s
            where c = chr s

version5 :: ByteString0 -> (ByteString, ByteString0)
version5 str = break0_2 test str
  where
    test x = x == ' ' || x == '\r' || x == '\n' || x == '$'


-- Version 6

-- Exact duplicate of break0_2 - ghc 7.6.3 doesn't inline it if we use it in 2 places
break0_2copied :: (Char -> Bool) -> ByteString0 -> (ByteString, ByteString0)
break0_2copied f (BS0 bs) = (BS.unsafeTake i bs, BS0 $ BS.unsafeDrop i bs)
    where
        i = Internal.inlinePerformIO $ BS.unsafeUseAsCString bs $ \ptr -> do
            let start = castPtr ptr :: Ptr Word8
            let end = go start
            return $! Ptr end `minusPtr` start

        go s@(Ptr a) | c == '\0' || f c = a
                     | otherwise = go $ inc s
            where c = chr s

version6 :: ByteString0 -> (ByteString, ByteString0)
version6 str = break0_2copied test str
  where
    test x = x <= '$' && (x == ' ' || x == '\r' || x == '\n' || x == '$')


-- Version 7

break00_1 :: (Char -> Bool) -> ByteString0 -> (ByteString, ByteString0)
break00_1 f (BS0 bs) = (BS.unsafeTake i bs, BS0 $ BS.unsafeDrop i bs)
    where
        i = Internal.inlinePerformIO $ BS.unsafeUseAsCString bs $ \ptr -> do
            let start = castPtr ptr :: Ptr Word8
            let end = go start
            return $! Ptr end `minusPtr` start

        go s@(Ptr a) | f c = a
                     | otherwise = go $ inc s
            where c = chr s

version7 :: ByteString0 -> (ByteString, ByteString0)
version7 str = break00_1 test str
  where
    test x = x <= '$' && (x == ' ' || x == '\r' || x == '\n' || x == '$' || x == '\0')


-- Inlining issue

-- Version 6

break0_2pleaseinline :: (Char -> Bool) -> ByteString0 -> (ByteString, ByteString0)
break0_2pleaseinline f (BS0 bs) = (BS.unsafeTake i bs, BS0 $ BS.unsafeDrop i bs)
    where
        i = Internal.inlinePerformIO $ BS.unsafeUseAsCString bs $ \ptr -> do
            let start = castPtr ptr :: Ptr Word8
            let end = go start
            return $! Ptr end `minusPtr` start

        go s@(Ptr a) | c == '\0' || f c = a
                     | otherwise = go $ inc s
            where c = chr s

versionInl1 :: ByteString0 -> (ByteString, ByteString0)
versionInl1 str = break0_2pleaseinline test str
  where
    test x = x <= '$' && (x == ' ' || x == '\r' || x == '\n' || x == '$')

versionInl2 :: ByteString0 -> (ByteString, ByteString0)
versionInl2 str = break0_2pleaseinline test str
  where
    test x = x <= '$' && (x == ' ' || x == '\r' || x == '\n' || x == '$')


-- Version 32 bits

break00_unrolled4 :: (Char -> Bool) -> ByteString0 -> (ByteString, ByteString0)
break00_unrolled4 f (BS0 bs) = (BS.unsafeTake i bs, BS0 $ BS.unsafeDrop i bs)
    where
        i = Internal.inlinePerformIO $ BS.unsafeUseAsCString bs $ \ptr -> do
            let start = castPtr ptr :: Ptr Word8
            let end = go start
            return $! Ptr end `minusPtr` start

        go s@(Ptr a) | f c0 = a
                     | f c1 = a `plusAddr#` 1#
                     | f c2 = a `plusAddr#` 2#
                     | f c3 = a `plusAddr#` 3#
                     | otherwise = go $ s `plusPtr` 4
            where
              c0 = Internal.w2c $ Internal.inlinePerformIO $ peek s
              c1 = Internal.w2c $ Internal.inlinePerformIO $ peek (s `plusPtr` 1)
              c2 = Internal.w2c $ Internal.inlinePerformIO $ peek (s `plusPtr` 2)
              c3 = Internal.w2c $ Internal.inlinePerformIO $ peek (s `plusPtr` 3)

version_unrolled4 :: ByteString0 -> (ByteString, ByteString0)
version_unrolled4 str = break00_unrolled4 test str
  where
    test x = x <= '$' && (x == ' ' || x == '\r' || x == '\n' || x == '$' || x == '\0')


break00_32bits :: (Char -> Bool) -> ByteString0 -> (ByteString, ByteString0)
break00_32bits f (BS0 bs) = (BS.unsafeTake i bs, BS0 $ BS.unsafeDrop i bs)
    where
        i = Internal.inlinePerformIO $ BS.unsafeUseAsCString bs $ \ptr -> do
            let start = castPtr ptr :: Ptr Word32
            let end = go start
            return $! Ptr end `minusPtr` start

        go s@(Ptr a) | f c0 = a
                     | f c1 = a `plusAddr#` 1#
                     | f c2 = a `plusAddr#` 2#
                     | f c3 = a `plusAddr#` 3#
                     | otherwise = go (s `plusPtr` 4)
            where
              w32 = Internal.inlinePerformIO $ (peek s :: IO Word32)
              c0 = Internal.w2c $ fromIntegral $  w32                     .&. 0xFF
              c1 = Internal.w2c $ fromIntegral $ (w32  `unsafeShiftR` 8 ) .&. 0xFF
              c2 = Internal.w2c $ fromIntegral $ (w32  `unsafeShiftR` 16) .&. 0xFF
              c3 = Internal.w2c $ fromIntegral $ (w32  `unsafeShiftR` 24) .&. 0xFF

version32bits :: ByteString0 -> (ByteString, ByteString0)
version32bits str = break00_32bits test str
  where
    w8 = fromIntegral . ord
    test x = x <= '$' && (x == ' ' || x == '\r' || x == '\n' || x == '$' || x == '\0')


break00_32bitsAddr :: (Word8 -> Bool) -> ByteString0 -> (ByteString, ByteString0)
break00_32bitsAddr f (BS0 bs) = (BS.unsafeTake i bs, BS0 $ BS.unsafeDrop i bs)
    where
        i = Internal.inlinePerformIO $ BS.unsafeUseAsCString bs $ \ptr -> do
            let start = castPtr ptr :: Ptr Word32
            let !(Ptr startAddr) = start
            let end = go startAddr
            return $! Ptr end `minusPtr` start

        go :: Addr# -> Addr#
        go a         | f c0 = a
                     | f c1 = a `plusAddr#` 1#
                     | f c2 = a `plusAddr#` 2#
                     | f c3 = a `plusAddr#` 3#
                     | otherwise = go (a `plusAddr#` 4#)
            where
              w32 = indexWord32OffAddr# a 0#
              c0 = W8# (narrow8Word#  w32                          )
              c1 = W8# (narrow8Word# (w32  `uncheckedShiftRL#`  8#))
              c2 = W8# (narrow8Word# (w32  `uncheckedShiftRL#` 16#))
              c3 = W8# (narrow8Word# (w32  `uncheckedShiftRL#` 24#))

version32bitsLowLevel :: ByteString0 -> (ByteString, ByteString0)
version32bitsLowLevel str = break00_32bitsAddr test str
  where
    w8 = fromIntegral . ord
    test x = x <= w8 '$' && (x == w8 ' ' || x == w8 '\r' || x == w8 '\n' || x == w8 '$' || x == w8 '\0')


i2cX :: Word32 -> Word8
i2cX (W32# w) = W8# (narrow8Word# w)
{-# INLINE i2cX #-}

int :: CInt -> Int
int = fromIntegral
{-# INLINE int #-}


foreign import ccall unsafe break_c_8 :: Ptr CChar -> IO CInt

versionC :: ByteString0 -> (ByteString, ByteString0)
versionC (BS0 bs) = Internal.inlinePerformIO $ BS.unsafeUseAsCString bs $ \ptr -> do
  i <- int `fmap` break_c_8 ptr
  return (BS.unsafeTake i bs, BS0 $ BS.unsafeDrop i bs)


foreign import ccall unsafe break_c_32 :: Ptr CChar -> IO CInt

versionC32 :: ByteString0 -> (ByteString, ByteString0)
versionC32 (BS0 bs) = Internal.inlinePerformIO $ BS.unsafeUseAsCString bs $ \ptr -> do
  i <- int `fmap` break_c_32 ptr
  return (BS.unsafeTake i bs, BS0 $ BS.unsafeDrop i bs)


foreign import ccall unsafe break_c_64 :: Ptr CChar -> IO CInt

versionC64 :: ByteString0 -> (ByteString, ByteString0)
versionC64 (BS0 bs) = Internal.inlinePerformIO $ BS.unsafeUseAsCString bs $ \ptr -> do
  i <- int `fmap` break_c_64 ptr
  return (BS.unsafeTake i bs, BS0 $ BS.unsafeDrop i bs)



-- The follwing memchr functions don't care much about the original
-- `test` any more, only about finding the newline.

-- Just find newline, using breakByte (uses system memchr)

versionMemchr :: ByteString0 -> (ByteString, ByteString0)
versionMemchr (BS0 bs) = (l, BS0 r)
  where
    (l, r) = breakByte (fromIntegral $ ord '\n') bs


-- Just find newline, own memchr

foreign import ccall unsafe memchr_simple :: Ptr CChar -> Word8 -> IO CInt

versionMemchr_simple :: ByteString0 -> (ByteString, ByteString0)
versionMemchr_simple (BS0 bs) = Internal.inlinePerformIO $ BS.unsafeUseAsCString bs $ \ptr -> do
  i <- int `fmap` memchr_simple ptr (fromIntegral $ ord '\n')
  return (BS.unsafeTake i bs, BS0 $ BS.unsafeDrop i bs)


-- Just find newline, own memchr with hard-coded characters
-- Some of them do additional stuff, but that doesn't change the result
-- on `bs1` since that only contains [a-z] and '\n'.
-- Surprisingly, nl2 is fastest

foreign import ccall unsafe memchr_nl1 :: Ptr CChar -> IO CInt
foreign import ccall unsafe memchr_nl2 :: Ptr CChar -> IO CInt
foreign import ccall unsafe memchr_nl3 :: Ptr CChar -> IO CInt
foreign import ccall unsafe memchr_nl4 :: Ptr CChar -> IO CInt

versionMemchr_nl1 :: ByteString0 -> (ByteString, ByteString0)
versionMemchr_nl1 (BS0 bs) = Internal.inlinePerformIO $ BS.unsafeUseAsCString bs $ \ptr -> do
  i <- int `fmap` memchr_nl1 ptr
  return (BS.unsafeTake i bs, BS0 $ BS.unsafeDrop i bs)

versionMemchr_nl2 :: ByteString0 -> (ByteString, ByteString0)
versionMemchr_nl2 (BS0 bs) = Internal.inlinePerformIO $ BS.unsafeUseAsCString bs $ \ptr -> do
  i <- int `fmap` memchr_nl2 ptr
  return (BS.unsafeTake i bs, BS0 $ BS.unsafeDrop i bs)

versionMemchr_nl3 :: ByteString0 -> (ByteString, ByteString0)
versionMemchr_nl3 (BS0 bs) = Internal.inlinePerformIO $ BS.unsafeUseAsCString bs $ \ptr -> do
  i <- int `fmap` memchr_nl3 ptr
  return (BS.unsafeTake i bs, BS0 $ BS.unsafeDrop i bs)

versionMemchr_nl4 :: ByteString0 -> (ByteString, ByteString0)
versionMemchr_nl4 (BS0 bs) = Internal.inlinePerformIO $ BS.unsafeUseAsCString bs $ \ptr -> do
  i <- int `fmap` memchr_nl4 ptr
  return (BS.unsafeTake i bs, BS0 $ BS.unsafeDrop i bs)
