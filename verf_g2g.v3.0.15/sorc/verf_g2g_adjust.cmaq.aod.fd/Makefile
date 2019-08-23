#######################################################
CMD = verf_g2g_adjust.cmaq.aod 

SRC = adjust.cmaq.aod.f90 
BINDIR= ../../exec
EXE = $(CMD)
# libraries must be in this order
LIB = /nwprod/lib/libw3nco_d.a \
      /nwprod/lib/libip_d.a \
      /nwprod/lib/libsp_d.a \
      /nwprod/lib/libbacio_8.a 


#INC  = $(DIR)/lib/arl_hysplit

INTL = -O -FR -assume byterecl -convert big_endian 

CFLAGS = $(INTL)

FC = ifort
all:	clean $(CMD) install
$(CMD) : $(SRC) 
	$(FC) -o $(EXE) $(CFLAGS) $(SRC) $(LIB)         
install:
	-mv $(CMD) ${BINDIR}/
clean:
	rm -f *.o
	rm -f $(CMD)
