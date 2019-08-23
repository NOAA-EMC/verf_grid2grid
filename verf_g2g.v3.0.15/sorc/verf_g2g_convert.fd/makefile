#!/bin/sh
#
#  This makefile is to compile ensembel production generator
#  Author: Binbin Zhou
#  OS: Intel WCOSS
#

FC=     ifort
INC= /nwprod/lib/incmod/g2_4
BINDIR= ../../exec
LIBS=  -L/nwprod/lib -lip_4 -lw3nco_4 -lw3emc_4  -lbacio_4 -lsp_4 -lg2_4 -ljasper -lpng -lz
FFLAGS =        -O3 -g -convert big_endian -I $(INC)
CMD= verf_g2g_convert

all:	clean $(CMD) install 

$(CMD):
	$(FC) -o $(@) $(FFLAGS) convert.f $(LIBS)
clean:
	-rm -f *.o $(CMD)
install:
	-mv $(CMD) ${BINDIR}/
check_prereqs:
	/nwprod/spa_util/check_libs.bash $(LIBS)
	/nwprod/spa_util/check_incs.bash $(INC)
#
