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
CMD=re-set-time 
CMD4stamp=re-set-time4stamp

all:	clean $(CMD) install1

$(CMD):
	$(FC) -o $(@) $(FFLAGS) re-set-time.f $(LIBS)
clean:
	-rm -f *.o $(CMD)
install1:
	-mv $(CMD) ${BINDIR}/
#
