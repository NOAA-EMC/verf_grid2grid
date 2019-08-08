#!/bin/ksh
#####################################################################
# This script obtains daily NESDIS SMOKE concentration data
#
# Author:  Jianping Huang 
# History: 2011-11-01 Modified by Julia Zhu for running in production
#          environment
#  2016-01-4: Binbin Zhou: Modify to grib2 version
#
#
# Usage:   verf_g2g_getnesdis_daily.sh NEST VDAY
#####################################################################
set -x

export nest=$1
export vday=$2

export COPYGB=${COPYGB:-/gpfs/dell1/nco/ops/nwprod/grib_util.v1.1.0/exec/copygb}
export cnvgrib=${CNVGRIB:-/gpfs/dell1/nco/ops/nwprod/grib_util.v1.1.0/exec/cnvgrib}

case $nest in
   conus) nesdisdir=$DCOM/$vday/wgrbbul/dust
         ftype="MYDdust"
         otype="modis-dust"
         varvn=aod_conc.v6.3.4
         jmax=601
         imax=251
         vgrid="255";; 
esac

#------jp for testing  ---------------
#
export OUTDIR=$COM_OUT/dust.$vday

# Select NESDIS SAT Algorithm
export algorithm=5

# fhead should be in consistent with file prefix opened in sorc/convert_*.f
#export fhead=aod-smoke

tbeg=${tbeg:-11}
tend=${tend:-23}

 cp $nesdisdir/${ftype}.${varvn}.$vday.hr*grib .

if [ $SENDCOM = YES ] ; then
  echo "DO NOT SAVE THE ORIGINAL FILES"
  cp $nesdisdir/${ftype}.${varvn}.$vday.hr*grib $OUTDIR/.
fi

t=$tbeg
while [ $t -le ${tend} ] ; do

 file=$OUTDIR/${ftype}.${varvn}.$vday.hr${t}.grib
 $cnvgrib -g12 $file $OUTDIR/${ftype}.t${t}z.f00
 t=`expr $t + 1`

done

