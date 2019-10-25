## Compiler
myfortran 	= gfortran

## Compile flags
# fopts 		= -g -m32 -fno-inline -fno-automatic -Wall 
fopts 		= -g -m64  

PEPSIDIR	= pepsi
INCDIR		= include
INCS		= $(INCDIR)/$(wildcard *.inc)

###############################################################################
# Rules

# Default targets (have to come first.....
all: pepsieRHICnoRAD pepsieRHICwithRAD

# PEPSI objects ###
pepsisrc	= $(wildcard $(PEPSIDIR)/*.F)
pepsiobj	= $(pepsisrc:.F=.o)

$(PEPSIDIR)/%.o : $(PEPSIDIR)/%.F $(INCS)
	@echo 
	@echo COMPILING
	$(myfortran) $(fopts) -I$(INCDIR) -c $< -o $@

$(PEPSIDIR)/libPepsi.a : $(pepsiobj)
	@echo 
	@echo MAKING LIBRARY
	ar rcf $@ $^

# Top level objects ###
%.o : ./%.f $(INCS)
	@echo 
	@echo COMPILING
	$(myfortran) $(fopts) -I$(INCDIR) -c $< -o $@

%.o : ./%.F $(INCS)
	@echo 
	@echo COMPILING
	$(myfortran) $(fopts) -I$(INCDIR) -c $< -o $@

# Executables ###
pepsieRHICnoRAD: pepsiMaineRHIC_noradcorr.o pepsi_radgen_extras.o gmc_random.o $(PEPSIDIR)/libPepsi.a
	@echo 
	@echo LINKING
	$(myfortran) $(fopts) $^ -L/cern64/pro/lib -lpdflib804 -lmathlib -lkernlib -lpacklib_noshift -ldl -lm -o $@
#	$(myfortran) $(fopts) $^ -L$(PEPSIDIR) -lPepsi -L/cern64/pro/lib -lpdflib804 -lmathlib -lkernlib -lpacklib_noshift -ldl -lm -o $@

pepsieRHICwithRAD: pepsiMaineRHIC_radcorr.v2.o pepsi_radgen_extras.o radgen.o radgen_event.o radgen_init.o gmc_random.o  $(PEPSIDIR)/libPepsi.a
	@echo 
	@echo LINKING
	$(myfortran) $(fopts) $^ -L/cern64/pro/lib -lpdflib804 -lmathlib -lkernlib -lpacklib_noshift -ldl -lm -o $@
#	$(myfortran) $(fopts) $^ -L$(PEPSIDIR) -lPepsi -L/cern64/pro/lib -lpdflib804 -lmathlib -lkernlib -lpacklib_noshift -ldl -lm -o $@

install: pepsieRHICwithRAD pepsieRHICnoRAD 
	@echo INSTALLING
	cp -v $^ $(EICDIRECTORY)/bin

clean :
	@echo 
	@echo CLEANING
	rm -vf ./$(PEPSIDIR)/*.o
	rm -vf ./$(PEPSIDIR)/lib*.a
	rm -vf ./*.o
	rm -vf pepsieRHIC*

.PHONY : clean all install

