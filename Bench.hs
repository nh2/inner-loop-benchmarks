module Bench (main) where

import           Criterion.Main
import           Control.DeepSeq
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Unsafe as BS
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Internal as Internal
import           Data.List
import           Data.Word
import           Foreign


main :: IO ()
main = defaultMain
  [ bench "version1" $ nf version1 str1
  , bench "version2" $ nf version2 str1
  , bench "version3" $ nf version3 bs1
  , bench "version4" $ nf version4 bs1_0
  ]


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

newtype ByteString0 = BS0 ByteString

instance NFData ByteString0 where rnf (BS0 b) = b `seq` ()

byteString0 :: ByteString -> ByteString0
byteString0 x = BS0 $ x `BS.snoc` '\0'

break0 :: (Char -> Bool) -> ByteString0 -> (ByteString, ByteString0)
break0 f (BS0 bs) = (BS.unsafeTake i bs, BS0 $ BS.unsafeDrop i bs)
    where
        i = Internal.inlinePerformIO $ BS.unsafeUseAsCString bs $ \ptr -> do
            let start = castPtr ptr :: Ptr Word8
            let end = go start
            return $! end `minusPtr` start

        go s | c == '\0' || f c = s
             | otherwise = go $ inc s
            where c = chr s

chr :: Ptr Word8 -> Char
chr x = Internal.w2c $ Internal.inlinePerformIO $ peek x

inc :: Ptr Word8 -> Ptr Word8
inc x = x `plusPtr` 1

version4 :: ByteString0 -> (ByteString, ByteString0)
version4 str = break0 test str
  where
    test x = x == ' ' || x == '\r' || x == '\n' || x == '$'
