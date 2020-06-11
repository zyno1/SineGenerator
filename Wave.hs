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

import System.IO

sine :: Int -> Double -> Double -> [Double]
sine sr t f = map (\i -> sin (2 * pi * f * (i / fromIntegral sr))) [0.0 .. (fromIntegral sr * t - 1)]

sineMult :: [Double] -> [Double] -> [Double]
sineMult a b = zipWith (*) a b

attackVec :: Int -> Double -> [Double]
attackVec sr t = map (/ k) [0.0 .. k]
    where k = fromIntegral sr * t - 1

releaseVec :: Int -> Double -> [Double]
releaseVec sr t = reverse $ attackVec sr t

calc :: Double -> Double -> Double -> Double -> Double
calc nb attNb relNb i = if i < attNb
    then i / attNb
    else if nb - i < relNb
        then (nb - i) / relNb
        else 1

vec :: Int -> Double -> Double -> Double -> [Double]
vec sr att rel tot = map (calc nb attNb relNb) [0.0 .. (nb - 1)]
    where nb = tot * fromIntegral sr
          attNb = att * fromIntegral sr
          relNb = rel * fromIntegral sr

toInt16 :: Double -> Int16
toInt16 f = round (f * (fromIntegral (maxBound :: Int16)))

save :: [[Double]] -> Int -> String -> IO ()
save wave sr path = do
    let len = fromIntegral (length wave) :: Int32
    out <- openFile path WriteMode
    B.hPutBuilder out $ B.string7 "RIFF"
    B.hPutBuilder out $ B.int32LE (len * 2 + 36)
    B.hPutBuilder out $ B.string7 "WAVEfmt "
    B.hPutBuilder out $ B.int32LE 16
    B.hPutBuilder out $ B.int16LE 1
    B.hPutBuilder out $ B.int16LE 1
    B.hPutBuilder out $ B.int32LE (fromIntegral sr)
    B.hPutBuilder out $ B.int32LE (2 * (fromIntegral sr))
    B.hPutBuilder out $ B.int16LE 2
    B.hPutBuilder out $ B.int16LE 16
    B.hPutBuilder out $ B.string7 "data"
    B.hPutBuilder out $ B.int32LE (len * 2)
    mconcat $ mconcat (map ((map (B.hPutBuilder out) . (map (B.int16LE . toInt16) ))) wave)
    hClose out
