-- MIT License
--
-- Copyright (c) 2020 Olivier Zeyen
-- 
-- Permission is hereby granted, free of charge, to any person obtaining a copy
-- of this software and associated documentation files (the "Software"), to deal
-- in the Software without restriction, including without limitation the rights
-- to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
-- copies of the Software, and to permit persons to whom the Software is
-- furnished to do so, subject to the following conditions:
-- 
-- The above copyright notice and this permission notice shall be included in all
-- copies or substantial portions of the Software.
-- 
-- THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
-- IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
-- FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
-- AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
-- LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
-- OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
-- SOFTWARE.

module Wave where

import Data.Int
import Data.Foldable
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Builder as B

sine :: Int -> Double -> Double -> [Double]
sine sr t f = map sin $ map (2 * pi * f *) $ map (/ fromIntegral sr) [0.0 .. (fromIntegral sr * t - 1)]

sineMult :: [Double] -> [Double] -> [Double]
sineMult a b = zipWith (*) a b

attackVec :: Int -> Double -> [Double]
attackVec sr t = map (/ k) [0.0 .. k]
    where k = fromIntegral sr * t - 1

releaseVec :: Int -> Double -> [Double]
releaseVec sr t = reverse $ attackVec sr t

vec :: Int -> Double -> Double -> Double -> [Double]
vec sr att rel tot = mconcat [sAtt, take lMid (cycle [1]), sRel]
    where sAtt = attackVec sr att
          sRel = releaseVec sr rel
          lsTot = fromIntegral sr * tot
          lMid = (round lsTot) - (length sAtt + length sRel)

toInt16 :: Double -> Int16
toInt16 f = round (f * (fromIntegral (maxBound :: Int16)))

save :: [Double] -> Int -> String -> IO ()
save wave sr path = B.writeFile path $ B.toLazyByteString $ fold l
    where len = fromIntegral (length wave) :: Int32
          l = [ B.string7 "RIFF"
              , B.int32LE (len * 2 + 36)
              , B.string7 "WAVEfmt "
              , B.int32LE 16
              , B.int16LE 1
              , B.int16LE 1
              , B.int32LE (fromIntegral sr)
              , B.int32LE (2 * (fromIntegral sr))
              , B.int16LE 2
              , B.int16LE 16
              , B.string7 "data"
              , B.int32LE (len * 2)
              ] ++ (map (B.int16LE . toInt16) wave)

