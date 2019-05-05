# What is this?
A small generic Ada package with a handful of DSP functions.  I wrote this package since I needed to do some filtering inside a program of mine.  I thought that it could be useful to "extract" the code and make a project of its own in order to improve reusability.

Currently it only has code for complex FIR and IIR filters (why complex and not real?  Since in my program I needed complex filters).


# How do I use this?
## Installation 
You have two possibilities
1. Just put the files under `src/` where your compiler will find them
2. Save the whole dir somewhere and `with` in your project file `generic_dsp.gpr` (I am assuming that you are using `gprbuild` by AdaCore)

## Usage
You have again two possibilities
1. Instantiate `DSP.Generic_Functions` with the floating point/complex type of your choice **or**
2. Use directly `DSP.Functions` that is instantiated for `Float`
