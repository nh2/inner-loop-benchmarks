module Main (main) where

import           Criterion.Main
import           Control.DeepSeq
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Unsafe as BS
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Internal as Internal
import           Data.List
import           Data.Word
import           Foreign
import           GHC.Exts (Ptr(..))


main :: IO ()
main = defaultMain
  [ bench " " $ nf const 1 -- to make commenting out easy
  , bench "version1" $ nf version1 str1
  , bench "version2" $ nf version2 str1
  , bench "version3" $ nf version3 bs1
  , bench "version4" $ nf version4 bs1_0
  , bench "version5" $ nf version5 bs1_0
  , bench "version6" $ nf version6 bs1_0
  , bench "version7" $ nf version6 bs1_0
  ]

test = do
  print $    length $ fst $ version1 str1
  print $    length $ fst $ version2 str1
  print $ BS.length $ fst $ version3 bs1
  print $ BS.length $ fst $ version4 bs1_0
  print $ BS.length $ fst $ version5 bs1_0
  print $ BS.length $ fst $ version6 bs1_0
  print $ BS.length $ fst $ version6 bs1_0

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

version6 :: ByteString0 -> (ByteString, ByteString0)
version6 str = break0_2 test str
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