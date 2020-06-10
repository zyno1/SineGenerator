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

module Notes where

import Data.Char (toLower)

-- C do
-- D re
-- E mi
-- F fa
-- G sol
-- A la
-- B si

lowerCase :: String -> String
lowerCase = map toLower

findNote :: String -> Maybe Double
findNote s = find (lowerCase s) notes

find :: String -> [(String, Double)] -> Maybe Double
find _ [] = Nothing
find s ((n, f):ys) = if n == s
    then Just f
    else find s ys

notes =
    [ ("c0"  ,16.35)
    , ("c0#" ,17.32)
    , ("d0b" ,17.32)
    , ("d0"  ,18.35)
    , ("d0#" ,19.45)
    , ("e0b" ,19.45)
    , ("e0"  ,20.60)
    , ("f0"  ,21.83)
    , ("f0#" ,23.12)
    , ("g0b" ,23.12)
    , ("g0 " ,24.50)
    , ("g0#" ,25.96)
    , ("a0b" ,25.96)
    , ("a0"  ,27.50)
    , ("a0#" ,29.14)
    , ("b0b" ,29.14)
    , ("b0"  ,30.87)

    , ("c1"  ,32.70)
    , ("c1#" ,34.65)
    , ("d1b" ,34.65)
    , ("d1"  ,36.71)
    , ("d1#" ,38.89)
    , ("e1b" ,38.89)
    , ("e1"  ,41.20)
    , ("f1"  ,43.65)
    , ("f1#" ,46.25)
    , ("g1b" ,46.25)
    , ("g1"  ,49.00)
    , ("g1#" ,51.91)
    , ("a1b" ,51.91)
    , ("a1"  ,55.00)
    , ("a1#" ,58.27)
    , ("b1b" ,58.27)
    , ("b1"  ,61.74)

    , ("c2" , 65.41)
    , ("c2#", 69.30)
    , ("d2b", 69.30)
    , ("d2" , 73.42)
    , ("d2#", 77.78)
    , ("e2b", 77.78)
    , ("e2" , 82.41)
    , ("f2" , 87.31)
    , ("f2#", 92.50)
    , ("g2b", 92.50)
    , ("g2" , 98.00)
    , ("g2#",103.83)
    , ("a2b",103.83)
    , ("a2" ,110.00)
    , ("a2#",116.54)
    , ("b2b",116.54)
    , ("b2" ,123.47)

    , ("c3" ,130.81)
    , ("c3#",138.59)
    , ("d3b",138.59)
    , ("d3" ,146.83)
    , ("d3#",155.56)
    , ("e3b",155.56)
    , ("e3" ,164.81)
    , ("f3" ,174.61)
    , ("f3#",185.00)
    , ("g3b",185.00)
    , ("g3" ,196.00)
    , ("g3#",207.65)
    , ("a3b",207.65)
    , ("a3" ,220.00)
    , ("a3#",233.08)
    , ("b3b",233.08)
    , ("b3" ,246.94)

    , ("c4" ,261.63)
    , ("c4#",277.18)
    , ("d4b",277.18)
    , ("d4" ,293.66)
    , ("d4#",311.13)
    , ("e4b",311.13)
    , ("e4" ,329.63)
    , ("f4" ,349.23)
    , ("f4#",369.99)
    , ("g4b",369.99)
    , ("g4" ,392.00)
    , ("g4#",415.30)
    , ("a4b",415.30)
    , ("a4" ,440.00)
    , ("a4#",466.16)
    , ("b4b",466.16)
    , ("b4" ,493.88)

    , ("c5" ,523.25)
    , ("c5#",554.37)
    , ("d5b",554.37)
    , ("d5" ,587.33)
    , ("d5#",622.25)
    , ("e5b",622.25)
    , ("e5" ,659.25)
    , ("f5" ,698.46)
    , ("f5#",739.99)
    , ("g5b",739.99)
    , ("g5" ,783.99)
    , ("g5#",830.61)
    , ("a5b",830.61)
    , ("a5" ,880.00)
    , ("a5#",932.33)
    , ("b5b",932.33)
    , ("b5" ,987.77)

    , ("c6" ,1046.50)
    , ("c6#",1108.73)
    , ("d6b",1108.73)
    , ("d6" ,1174.66)
    , ("d6#",1244.51)
    , ("e6b",1244.51)
    , ("e6" ,1318.51)
    , ("f6" ,1396.91)
    , ("f6#",1479.98)
    , ("g6b",1479.98)
    , ("g6" ,1567.98)
    , ("g6#",1661.22)
    , ("a6b",1661.22)
    , ("a6" ,1760.00)
    , ("a6#",1864.66)
    , ("b6b",1864.66)
    , ("b6" ,1975.53)

    , ("c7" ,2093.00)
    , ("c7#",2217.46)
    , ("d7b",2217.46)
    , ("d7" ,2349.32)
    , ("d7#",2489.02)
    , ("e7b",2489.02)
    , ("e7" ,2637.02)
    , ("f7" ,2793.83)
    , ("f7#",2959.96)
    , ("g7b",2959.96)
    , ("g7" ,3135.96)
    , ("g7#",3322.44)
    , ("a7b",3322.44)
    , ("a7" ,3520.00)
    , ("a7#",3729.31)
    , ("b7b",3729.31)
    , ("b7" ,3951.07)

    , ("c8" ,4186.01)
    , ("c8#",4434.92)
    , ("d8b",4434.92)
    , ("d8" ,4698.63)
    , ("d8#",4978.03)
    , ("e8b",4978.03)
    , ("e8" ,5274.04)
    , ("f8" ,5587.65)
    , ("f8#",5919.91)
    , ("g8b",5919.91)
    , ("g8" ,6271.93)
    , ("g8#",6644.88)
    , ("a8b",6644.88)
    , ("a8" ,7040.00)
    , ("a8#",7458.62)
    , ("b8b",7458.62)
    , ("b8" ,7902.13)
    ]

