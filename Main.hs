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

import Wave
import Notes
import Parser

import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Builder as B
import Data.Foldable
import Data.Maybe

import System.Environment
import System.Exit

data State = State { volume :: Double
                   , bpm :: Double
                   , sampleRate :: Int
                   , samples :: [Double]
                   , attack :: Double
                   , release :: Double
                   } deriving (Show, Eq)

defaultState = State { volume = 0.5
                     , bpm = 100
                     , sampleRate = 44100
                     , samples = []
                     , attack = 0
                     , release = 0
                     }

generate :: State -> [Stmt] -> [Double]
generate s [] = samples s
generate s (BPM v:is) = generate (s {bpm = v}) is
generate s (Volume v:is) = generate (s {volume = v}) is
generate s (SampleRate v:is) = generate (s {sampleRate = v}) is
generate s (Attack v:is) = generate (s {attack = beatesToSeconds (bpm s) v}) is
generate s (Release v:is) = generate (s {release = beatesToSeconds (bpm s) v}) is
generate s (Silence b:is) = generate (s {samples = ns}) is
    where ns = mconcat [samples s, n]
          count = (beatesToSeconds (bpm s) b) * (fromIntegral $ sampleRate s)
          n = take (round count) (cycle [0])

generate s (Note note b:is) = if isJust f
    then generate (s {samples = ns}) is
    else error (mconcat ["note ", note, " not found"])
    where ns = mconcat [samples s, n]
          time = beatesToSeconds (bpm s) b
          attRelVec = vec (sampleRate s) (attack s) (release s) time
          n = sineMult attRelVec $ map (* (volume s)) $ sine (sampleRate s) time (fromJust f)
          f = findNote note


beatesToSeconds :: Double -> Double -> Double
beatesToSeconds bpm beats = beats * (60.0 / bpm)

firstSampleRate :: [Stmt] -> Int
firstSampleRate [] = 44100
firstSampleRate (SampleRate v:_) = v
firstSampleRate (_:ys) = firstSampleRate ys

main = do
    argv <- getArgs
    if length argv /= 2
        then do
            putStrLn "Usage:\n ./smg inputFile outputFile.wav"
            exitFailure
        else do
            (Seq input) <- parseFile (head argv)
            let wave = generate defaultState input
            save wave (firstSampleRate input) (head $ tail argv)
