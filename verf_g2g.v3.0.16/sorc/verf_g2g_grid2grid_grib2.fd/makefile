#!/bin/sh
#
#  This makefile is to compile grid-to-grid all codes on Dell
#  Author: Binbin Zhou
#          20 Nov 2018

FC=     ifort -convert big_endian
INC= -I ${IP_INC4} -I ${G2_INC4}
LIBS=  ${IP_LIB4} ${W3NCO_LIB4} ${W3EMC_LIB4} ${BACIO_LIB4} ${SP_LIB4} ${G2_LIB4} ${JASPER_LIB} ${PNG_LIB} ${Z_LIB}
FFLAGS = -O3 -auto $(INC)   
#IP= /gpfs/hps/nco/ops/nwprod/lib/ip/v2.0.0/intel/libip_v2.0.0_4.a

BINDIR= ../../exec

SRCS=   gtgdef.f readcntl.f setarea.f \
	setfcst.f setlevel.f setmodel.f setobtyp.f  \
	stclst.f stldsp.f stlstr.f stnumb.f stread.f strmbl.f \
	strxbl.f getregion.f getnumber.f getGRIB2data.f \
	read_area.f sl1l2.f vl1l2.f fho.f getfho.f get_hasdata.f \
	getHGTsfc.f getTndGRIBdata.f getMeanClimData.f sal1l2.f \
	getAnomlyLevMrk.f val1l2.f afho.f FITWAV_1D.f \
	esl1l2.f evl1l2.f rhnt.f rhet.f sl1l2_S1.f efho.f \
	EFS.f getHiresClimData.f \
	DIST.f PROB.f naefs.f fho_fss.f FSS.f \
	readGRIB2.f getClimData_2.5deg.f getNAEFSanalysis.f \
	fhop.f readProbGRIB2.f DIST_special.f  \

OBJ=${SRCS:.f=.o}
CMD= verf_g2g_grid2grid_grib2

all:    clean $(CMD) install

$(CMD):	$(OBJ)
	$(FC) $(FFLAGS) -o $(@) grid2grid.f $(OBJ) $(LIBS) 
clean:
	-rm -f $(OBJ) $(CMD)
install:
	-mv $(CMD) ${BINDIR}/
check_prereqs:
	/nwprod/spa_util/check_libs.bash $(LIBS)
	/nwprod/spa_util/check_incs.bash $(INC)
########################################################
