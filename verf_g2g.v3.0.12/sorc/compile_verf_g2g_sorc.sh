#!/bin/sh
set -aux

#module purge
#module load ../modulefiles/v3.0.12
#module list


SORCverf_g2g=`pwd`
EXEC=`pwd`/../exec

#compile product generator sorc
#cd $SORCverf_g2g/verf_g2g_grid2grid_grib2.fd
#pwd
#make all

#compile ceiling_adjust  code
#cd $SORCverf_g2g/verf_g2g_ceiling_adjust.fd
#pwd
#make all

#compile convert code
cd $SORCverf_g2g/verf_g2g_convert.fd
pwd
make all


cd $SORCverf_g2g/verf_g2g_adjust.cmaq.aod.fd
pwd
#make clean
#make 
# copy the exec

#cd $SORCverf_g2g/verf_g2g_adjust.nesdis.index.fd
#pwd
#make clean
#make
# copy the exec

cd $SORCverf_g2g/verf_g2g_reset.fd
pwd
make clean
make
echo "All sorc codes are compiled"  
exit 
