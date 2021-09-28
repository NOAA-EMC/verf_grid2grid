#!/bin/sh
#set -aux

#source /etc/profile.d/lmod.sh

SORCverf_g2g=`pwd`

#module purge
#module use $SORCverf_g2g/../modulefiles
#module load v3.2.0
#module list

source ../versions/build.ver

source ../modulefiles/v3.2.0

set -x

SORCverf_g2g=`pwd`

#export G2_INC4=/gpfs/dell1/nco/ops/nwprod/lib/g2/v3.1.0/ips/18.0.1/include/g2_v3.1.0_4
#export G2_LIB4=/gpfs/dell1/nco/ops/nwprod/lib/g2/v3.1.0/ips/18.0.1/libg2_v3.1.0_4.a


#compile product generator sorc
cd $SORCverf_g2g/verf_g2g_grid2grid_grib2.fd
make all

#exit

#compile ceiling_adjust  code
cd $SORCverf_g2g/verf_g2g_ceiling_adjust.fd
make all

#compile convert code
cd $SORCverf_g2g/verf_g2g_convert.fd
make all


cd $SORCverf_g2g/verf_g2g_adjust.cmaq.aod.fd
make all

cd $SORCverf_g2g/verf_g2g_adjust.nesdis.index.fd
make all

cd $SORCverf_g2g/verf_g2g_reset.fd
make clean
make
echo "All sorc codes are compiled"  
exit 
