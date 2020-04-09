# PEPSI
PEPSI (Polarised Electron Proton Scattering Interactions) is a Monte Carlo generator for polarised deep inelastic scattering (pDIS). It is based on the LEPTO 6.5 Monte Carlo for unpolarised DIS.
See L. Mankiewicz, A. Schäfer and M. Veltri, Comp. Phys. Comm. 71, 305-318 (1992),
[PEPSI.paper.pdf](https://wiki.bnl.gov/eic/upload/PEPSI.paper.pdf).

# Installation

Can be built using "make".
The "install" target should be customized to your environment before using

Ignore warnings of the form:
```
 Warning: $ should be the last specifier in format
```
 Which should be okay (it is a g77 extension allowed by gfortran).
 As well as:
```
Warning: Deleted feature: PAUSE statement at (1)
```
This feature is deleted in F95; here, it should eventually be replaced by write() + read().

# CHANGES 
Multiple warnings like this:
```pepsi/setctq5.F:9.10:
     >   './pdf/cteq5hj.tbl',                                           
          1
Warning: Initialization string starting at (1) was truncated to fit the variable (16/17)
```

indicate a too short variable length. Changed to
```fortran
      Character Flnm(Isetmax)*100
```
in line 6 (of setctq5.F).


# Running
Note that the executables expect the pdf/ directory 
in the directory of execution. Easiest way to achieve this is a softlink (adapt to your location)
```sh
ln -s $EICDIRECTORY/PACKAGES/PEPSI/pdf
```

You can test with things like
```sh
./pepsieRHICnoRAD < STEER-FILES/input.EW_noradcor.eic.posi.test
```

# Known Issues
DO NOT TRY radiative corrections. Currently does not work.

With radiative corrections, you would also need a subdirectory
```sh
mkdir radcorr
```

Tables can be generated:
```sh
mkdir radgen
./pepsieRHICwithRAD < STEER-FILES/input.data_make-radcor.eic.pol.anti
```

But they cannot be used.
```sh
./pepsieRHICwithRAD < STEER-FILES/input.data_radcor.eic.pol.anti

At line 514 of file pepsiMaineRHIC_radcorr.v2.F (unit = 29, file =
'pepsi.ep.4x100.1Mevents.pol-anti.RadCor=1.txt')
Fortran runtime error: Expected REAL for item 42 in formatted
transfer, got INTEGER
),                                  34(E18.10,1x,$),I12,/)

```


this line for testing only
