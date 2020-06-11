# SineGenerator

This is a simple sine wave genearator.

Compiling:
```
ghc Main.hs
```
should be enough

Usage:
```
./executable input.n output.wav
```

The `input.n` file is a simple text file containing instructions:
```
bpm [float > 0]
vol [float 0.0 - 1.0]
n [note string] [float > 0]
s [float > 0]
sr [int > 0]
att [float > 0]
rel [float > 0]
```

`bpm` sets the beats per minute
`vol` sets the volume
`n` is for creating a note
`s` is for silence
`sr` is for setting the sample rate of the output file
`att` is to set the attack time for each note
`rel` is to set the release time for each note

so a simple example would be:

```
bpm 150.0
sr 48000
att 0.2
rel 0.2
vol 0.5

n c4 1.0
n d4 1.0
n e4 1.0
n f4 1.0
n g4 1.0
n a4 1.0
n b4 1.0
n c5 1.0
```

The available notes can be found in the Notes.hs file, but basically for a c sharp
the programm expects `c4#` and for a c flat type `c4b`.
The numbers can range from 1 to 8. If a note cannot be found in the program's list
the program will fail.

(the file can be found in the example directory with the resulting .wav file)

`bpm`, `att`, `rel` and `vol` can be used multiple times.
`sr` can also be used multiple times but only the first one will be used to set the sample rate
of the file so the others `sr` instructions will modify the following notes.

## C++ version

The cpp folder contains a simpler c++ version of the SineGenerator.
The language expected is basically the same except that one line is an instruction
if it begins with one of the 6 key-words, it is ignored otherwise. So a line
starting with '//' is ignored so you can have comments but they have to be at the beginning
of the line and the whole line will be ignored.

Compile:
```
g++ -lm -std=c++11 main.cpp
```

Usage:
```
./a.out in out
```

`in` and `out` should be file names, but if you use a '-' then stdin/stdout will be
used instead.
So if you want a more complex commenting system you could pipe the file through another
program that removes comments before piping it to the SineGenerator.

The C++ version is faster and uses less memory, but the haskell version uses
a real parser instead of the pseudo-parser used in the C++ version. However I
have not noticed any problem with that and I find the error messages of the C++
messages more readable.

Both versions have the same functionality for now, but the C++ version is a simpler
implementation of it.
