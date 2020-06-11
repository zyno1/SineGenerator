/*
MIT License

Copyright (c) 2020 Olivier Zeyen

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
*/

#include<iostream>
#include<fstream>
#include<vector>
#include<string>
#include<algorithm>
#include<sstream>

#include<cmath>
#include<cstring>

#include "notes.h"

double beatsToSeconds(double bpm, double t) {
    return t * 60.0 / bpm;
}

class Sine {
    public:
        int sampleRate;
        double bpm;
        double volume;
        double attack;
        double release;
        std::vector<double> samples;

    public:
        Sine() {
            sampleRate = 48000;
            bpm = 100;
            volume = 0.3;
            attack = 0;
            release = 0;
        }

        void appendSine(double f, double t) {
            t = beatsToSeconds(bpm, t);

            int nb = sampleRate * t;
            int att = beatsToSeconds(bpm, attack) * sampleRate;
            int rel = beatsToSeconds(bpm, release) * sampleRate;

            samples.reserve(samples.size() + nb);
            
            for(int i = 0; i < nb; i++) {
                double ti = (double)i / sampleRate;
                double val = volume * sin(2 * M_PI * f * ti);

                if(i < att) {
                    val *= (double)i / (att - 1);
                }
                if(nb - i < rel) {
                    double j = nb - i;
                    val *= j / (rel - 1);
                }

                samples.push_back(val);
            }
        }

        void appendSilence(double t) {
            int nb = sampleRate * beatsToSeconds(bpm, t);

            samples.reserve(samples.size() + nb);

            for(int i = 0; i < nb; i++) {
                samples.push_back(0);
            }
        }

        void write(std::ostream & out) {
            int d = 0;

            out.write("RIFF", 4);
            
            d = samples.size() * 2 + 36;
            out.write((char*)&d, 4);

            out.write("WAVEfmt ", 8);

            d = 16;
            out.write((char*)&d, 4);

            d = 1;
            out.write((char*)&d, 2);
            out.write((char*)&d, 2);

            out.write((char*)&sampleRate, 4);

            d = sampleRate * 2;
            out.write((char*) &d, 4);

            d = 2;
            out.write((char*)&d, 2);
            d = 16;
            out.write((char*)&d, 2);

            out.write("data", 4);

            d = samples.size() * 2;
            out.write((char*)&d, 4);

            for(int i = 0; i < samples.size(); i++) {
                int val = samples[i] * ((1 << 15) - 1);
                out.write((char*) &val, 2);
            }
        }
};

void parse(std::istream & in, std::ostream & out) {
    Sine sine;
    std::string line;
    int lc = 1;

    while(std::getline(in, line)) {
        std::string tmp;
        std::stringstream ss(line);
        ss >> tmp;
        if(tmp == "bpm") {
            double val;
            ss >> val;
            if(!ss) {
                std::cerr << "line " << lc << ": " << "number expected: " << line << "\n";
            }
            else {
                sine.bpm = val;
            }
        }
        else if(tmp == "vol") {
            double val;
            ss >> val;
            if(!ss) {
                std::cerr << "line " << lc << ": " << "number expected: " << line << "\n";
            }
            else {
                sine.volume = val;
            }
        }
        else if(tmp == "n") {
            std::string note;
            double t;
            ss >> note >> t;

            std::transform(note.begin(), note.end(), note.begin(), tolower);

            if(!ss) {
                std::cerr << "line " << lc << ": " << "note malformed: " << line << "\n";
                continue;
            }

            auto it = notes.find(note);
            if(it == notes.end()) {
                std::cerr << "line " << lc << ": " << "note " << note << " not found\n";
            }
            else {
                sine.appendSine(it->second, t);
            }
        }
        else if(tmp == "s") {
            double t;
            ss >> t;
            if(!ss) {
                std::cerr << "line " << lc << ": " << "number expected: " << line << "\n";
                continue;
            }
            sine.appendSilence(t);
        }
        else if(tmp == "sr") {
            int val;
            ss >> val;
            if(!ss) {
                std::cerr << "line " << lc << ": " << "integer expected: " << line << "\n";
                continue;
            }
            sine.sampleRate = val;
        }
        else if(tmp == "att") {
            double val;
            ss >> val;
            if(!ss) {
                std::cerr << "line " << lc << ": " << "number expected: " << line << "\n";
                continue;
            }
            sine.attack = val;
        }
        else if(tmp == "rel") {
            double val;
            ss >> val;
            if(!ss) {
                std::cerr << "line " << lc << ": " << "number expected: " << line << "\n";
                continue;
            }
            sine.release = val;
        }

        lc++;
    }

    sine.write(out);
}

int main(int argc, char** argv) {
    if(argc != 3) {
        std::cerr << "wrong usage\n";
        exit(1);
    }

    if(strcmp("-", argv[1]) != 0) {
        std::ifstream in(argv[1]);
        if(strcmp("-", argv[2]) != 0) {
            std::ofstream out(argv[2]);
            parse(in, out);
        }
        else {
            parse(in, std::cout);
        }
    }
    else {
        if(strcmp("-", argv[2]) != 0) {
            std::ofstream out(argv[2]);
            parse(std::cin, std::cout);
        }
        else {
            parse(std::cin, std::cout);
        }
    }

    return 0;
}

