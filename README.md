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

I do worry about multiple warnings like this:
```pepsi/setctq5.F:9.10:
     >   './pdf/cteq5hj.tbl',                                           
          1
Warning: Initialization string starting at (1) was truncated to fit the variable (16/17)
```
Could be done, for example, using
```fortran
      Character Flnm(Isetmax)*32 
```
in line 6 (of setctq5.F), but I'm leery of messing with fortran.


Note that the executables expect the pdf/ directory 
in the directory of execution. Easiest way to achieve this is a softlink
```sh
ln -s $EICDIRECTORY/PACKAGES/PEPSI/pdf
```


You can test with things like
```sh
./pepsieRHICnoRAD < STEER-FILES/input.EW_noradcor.eic.posi.test
```

With radiative corrections, you also need a subdirectory
```sh
mkdir radcorr
```

This may take a while (AND IS CURRENTLY NOT WORKING):
```sh
./pepsieRHICwithRAD < STEER-FILES/input.data_make-radcor.eic.pol.anti
./pepsieRHICwithRAD < STEER-FILES/input.data_radcor.eic.pol.anti
```


