#######################################################
CMD = verf_g2g_adjust.nesdis.index 

SRC = adjust_nesdis_index.f90 
BINDIR= ../../exec
EXE = $(CMD)
# libraries must be in this order
LIB = /nwprod/lib/libw3nco_4.a \
      /nwprod/lib/libip_4.a \
      /nwprod/lib/libbacio_4.a 


#INC  = $(DIR)/lib/arl_hysplit

INTL = -O -FR -assume byterecl -convert big_endian  -g -fpe0 -traceback

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
