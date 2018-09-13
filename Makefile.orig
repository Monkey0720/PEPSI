pepsi: gfortran -g -m32 -fno-inline -fno-automatic -Wall -I/afs/rhic.bnl.gov/eic/PACKAGES/PEPSI/include -c pepsi/*.F

gmc_random.o: gmc_random.f
	gfortran -g -m32 -fno-inline -fno-automatic -Wall -I/afs/rhic.bnl.gov/eic/PACKAGES/PEPSI/include -c gmc_random.f
pepsiMaineRHIC_noradcorr.o: pepsiMaineRHIC_noradcorr.f
	gfortran -g -m32 -fno-inline -fno-automatic -Wall -I/afs/rhic.bnl.gov/eic/PACKAGES/PEPSI/include -c pepsiMaineRHIC_noradcorr.f
pepsiMaineRHIC_radcorr.v2.o: pepsiMaineRHIC_radcorr.v2.f
	gfortran -g -m32 -fno-inline -fno-automatic -Wall -I/afs/rhic.bnl.gov/eic/PACKAGES/PEPSI/include -c pepsiMaineRHIC_radcorr.v2.f

pepsi_radgen_extras.: pepsi_radgen_extras.f
	gfortran -g -m32 -fno-inline -fno-automatic -Wall -I/afs/rhic.bnl.gov/eic/PACKAGES/PEPSI/include -c pepsi_radgen_extras.f
radgen_event.o: radgen_event.f
	gfortran -g -m32 -fno-inline -fno-automatic -Wall -I/afs/rhic.bnl.gov/eic/PACKAGES/PEPSI/include -c radgen_event.f  
radgen.o: radgen.f
	gfortran -g -m32 -fno-inline -fno-automatic -Wall -I/afs/rhic.bnl.gov/eic/PACKAGES/PEPSI/include -c radgen.f
radgen_init.o: radgen_init.f
	gfortran -g -m32 -fno-inline -fno-automatic -Wall -I/afs/rhic.bnl.gov/eic/PACKAGES/PEPSI/include -c radgen_init.f
pepsieRHICnoRAD: pepsieRHICnoRAD
	gfortran -g -m32 -fno-inline -fno-automatic -I/afs/rhic.bnl.gov/eic/PACKAGES/PEPSI/include -o pepsieRHICnoRAD pepsi/*.o pepsi_radgen_extras.o gmc_random.o pepsiMaineRHIC_noradcorr.o  -L/cern/pro/lib -lpdflib804 -lmathlib -lkernlib -lpacklib_noshift -ldl -lm

pepsieRHICwithRAD: pepsieRHICwithRAD
	gfortran -g -m32 -fno-inline -fno-automatic -I/afs/rhic.bnl.gov/eic/PACKAGES/PEPSI/include -o pepsieRHICwithRAD pepsi/*.o pepsi_radgen_extras.o radgen.o radgen_event.o radgen_init.o gmc_random.o pepsiMaineRHIC_radcorr.v2.o  -L/cern/pro/lib -lpdflib804 -lmathlib -lkernlib -lpacklib_noshift -ldl -lm

clean:
	rm *.o; o; rm pepsieRHIC*
