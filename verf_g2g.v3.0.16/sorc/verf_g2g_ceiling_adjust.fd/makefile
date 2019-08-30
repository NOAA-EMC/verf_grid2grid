#!/bin/sh
#
#  This makefile is to compile ensembel production generator
#  Author: Binbin Zhou
#  OS: Intel WCOSS
#

FC=     ifort -convert big_endian
INC= ${G2_INC4}
LIBS= ${IP_LIB4} ${W3NCO_LIB4} ${W3EMC_LIB4} ${BACIO_LIB4}  ${SP_LIB4} ${G2_LIB4} ${JASPER_LIB} ${PNG_LIB} ${Z_LIB}
FFLAGS = -O3 -auto -I $(INC)
BINDIR= ../../exec
CMD= verf_g2g_ceiling_adjust

#all:	check_prereqs $(CMD)
all:	clean $(CMD) install

$(CMD):
	$(FC) -o $(@) $(FFLAGS) ceiling_adjust.f $(LIBS)
clean:
	-rm -f *.o $(CMD)
install:
	-mv $(CMD) ${BINDIR}/
check_prereqs:
	/nwprod/spa_util/check_libs.bash $(LIBS)
	/nwprod/spa_util/check_incs.bash $(INC)
#
