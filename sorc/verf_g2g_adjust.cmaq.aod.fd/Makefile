#######################################################
CMD = verf_g2g_adjust.cmaq.aod 

SRC = adjust.cmaq.aod.f90 
EXE = $(CMD)
# libraries must be in this order
#FC=     ifort -convert big_endian
FC=     ftn -convert big_endian
INC= ${G2_INC4}
LIBS= ${IP_LIB4} ${W3NCO_LIB4} ${W3EMC_LIB4} ${BACIO_LIB4}  ${SP_LIB4} ${G2_LIB4} ${JASPER_LIB} ${PNG_LIB} ${Z_LIB}
FFLAGS = -O3 -auto -I $(INC)
BINDIR= ../../exec

all:	$(CMD) install clean
$(CMD) : $(SRC) 
	$(FC) -o $(@) $(FFLAGS) $(SRC) $(LIBS)         
install:
	-mv $(CMD) ${BINDIR}/
clean:
	rm -f *.o
	rm -f $(CMD)
