# Basis set reader (s-bas)
This code is an example how to read basis sets in TURBOMOLE format. It also
features some basic IO like reading geometries form TURBOMOLE coordinate files 
or in XMOL format as well as a command line interface.

The basis sets in TURBOMOLE format can be obtained at the TURBOMOLE basis set
library or at EMSL Basis Set Exchange Portal.

## How to use?
Set up a ~/.soulrc with:

    # ~/.soulrc, config-file for s-bas
    $basdir /path/to/basis/set/library
    $end
    
Any basis set can be read in with

    sbas -b BASISNAME
    
By default the a geometry is required to set up the basis for the particular molecule, like

    sbas -b def2-QZVP coord
    
## What is it good for?
The program does actually nothing with the basis, but counting basis functions. 
You are encouraged to use it for some quantum chemical calculation.

To be honest this is rather a backup then an ongoing project. This code will be
incorporated in my QC code as soon as I have closed the gap between reading the basis
and doing a SCF.
