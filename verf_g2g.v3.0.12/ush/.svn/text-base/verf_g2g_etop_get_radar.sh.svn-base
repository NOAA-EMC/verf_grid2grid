#!/bin/ksh
#####################################################################
# This script obtains the Radar Mosaic Data
# and interpolate them onto 212 grid as for the NAM grid
# History:   Binbin Zhou  -- May 2010, original version
#            Julia Zhu  -- June 30th, second version
#            Binbin Zhou  -- Dec 2014, grib1 -> grib2
#####################################################################
set -x

export copygb2=${copygb:-/nwprod/util/exec/copygb2}
export wgrib2=${wgrib:-/nwprod/util/exec/wgrib2}

model=$1
vday=$2

cd $DATA


if [ $model = mosaic ]; then
  COMMOSAIC=${COMMOSAIC:-/com/hourly/prod/radar}
  #COMMOSAIC=/meso/noscrub/Shun.Liu/com/hourly/prod/radar
  for cyc in 00 06 12 18 ; do
    mosaicfile=$COMMOSAIC.$vday/refd3d.t${cyc}z.grb2f00
    $copygb2 -g"30 6 0 0 0 0 0 0 1473 1025 12190000 226541000 8 25000000 265000000 5079000 5079000 0 64 25000000 25000000 0 0" -i2,1 -x $mosaicfile $COMOUT/refd3d.t${cyc}z.grid227.f00
   echo 'copygb2 mosaic ' $cyc ' done!'
  done
fi

# NAMnest and Hiresw ARW and NMMB already are grid227



