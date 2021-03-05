#!/bin/sh
#
#  This makefile is to compile ensembel production generator
#  Author: Binbin Zhou
#  OS: Intel WCOSS
#

FC=     ifort -convert big_endian
INC= ${G2_INC4}
BINDIR= ../../exec
LIBS= ${IP_LIB4} ${W3NCO_LIB4} ${W3EMC_LIB4} ${BACIO_LIB4}  ${SP_LIB4} ${G2_LIB4} ${JASPER_LIB} ${PNG_LIB} ${Z_LIB}
FFLAGS =        -O3 -auto -I ${INC} 
CMD= verf_g2g_convert

all:    $(CMD) install clean

$(CMD):
	$(FC) $(FFLAGS) -o $(@)  convert.f $(LIBS) 
clean:
	-rm -f *.o $(CMD)
install:
	-mv $(CMD) ${BINDIR}/
#
