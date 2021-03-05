##SHELL=/bin/sh
#

SRCS=	adjust.cmaq.aod.f90

OBJS=	adjust.cmaq.aod.o

INCLS=

F90=xlf90_r

LIBS= -L/nwprod/lib -lw3_d -lip_d -lsp_d -lbacio_4
#LDFLAG= -lessl_r -qsmp=noauto -bnoquiet
LDFLAG= -lessl_r -qsmp=noauto

## DEFAULT SETTING IS -qarch=auto -qmaxmem=-1 -qfullpath -O -g -qstrict
##FFLAG90= -O3 -qrealsize=8 -qstrict -qxlf77=leadzero -qmaxmem=-1 -qnolm 
FFLAG90= -O3 -qrealsize=8 -qsmp=noauto -qnolm

EXEC = verf_g2g_adjust.cmaq.aod

.SUFFIXES: .o .f90

all:	$(EXEC)

$(EXEC): $(OBJS)
	$(F90) $(LDFLAG) -o $(EXEC) $(OBJS) $(LIBS) $(ESMFLIB)

.f90.o:	$(INCLS)
	$(F90) $(FFLAG90) -c $<


adjust.cmaq.aod.o:	adjust.cmaq.aod.f90 

clean:
	rm -f $(OBJS)

clobber:	clean
	rm -f $(EXEC)
