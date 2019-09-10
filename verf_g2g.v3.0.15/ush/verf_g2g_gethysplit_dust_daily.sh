#!/bin/ksh
#####################################################################
# This script obtains daily HYSPLIT SMOKE concentration data
#
# Author:  Jianping Huang
# History: 2011-11-01 Modified by Julia Zhu for running in production
#          environment
#          2012-04-05 Modified by Jianping Huang for dust verification
#
#   2016-01-14: Binbin Zhou: Modified to grib2 version
#
# Usage:   verf_g2g_gethysplit_daily.sh NEST VDAY
#####################################################################
set -x

if [[ $# -lt 2 ]]; then
  MSG="USAGE $0 NEST VDAY"
  echo $MSG
  err_exit
fi

export nest=$1
export vday=$2

export COPYGB=${COPYGB:-/nwprod/util/exec/copygb}
export COMHYSPT=${COMHYSPT:-/com2/hysplit/prod}
export WGRIB2=/nwprod/util/exec/wgrib2

export gribfile_conus_dust=dustcs.t06z.pbl.1hr.grib2

case $nest in
  conus) hysplitdir=$COMHYSPT/dustcs
         OUTDIR=$COM_OUT/dust.$vday
         binfile=bin_pbl.1hr
         gribfile=$gribfile_conus_dust
         fhead=hysplit-dust-cs
         jmax=601
         imax=251
         vgrid=255;;
  ak)    hysplitdir=$COMHYSPT/dustak
         OUTDIR=$COM_OUT/dust.$vday
         binfile=binak_pbl.1hr_198
         gribfile=$gribfile_ak_dust
         fhead=aod-dust-ak 
         vgrid=198
         jmax=825
         imax=553 ;;
  hi)    hysplitdir=$COMHYSPT/dusthi
         OUTDIR=$COM_OUT/dust.$vday
         binfile=binhi_pbl.1hr_196
         gribfile=$gribfile_hi_dust
         fhead=aod-dust-hi
         vgrid=196
         jmax=321
         imax=225 ;;
esac


# Select NESDIS SAT Algorithm
#jp export algorithm=5

# fhead should be in consistent with file prefix opened in sorc/convert_*.f
export fhead=$fhead


if [ -s ${hysplitdir}.$vday/$gribfile ]
then
   cp ${hysplitdir}.$vday/$gribfile .
   cp ${hysplitdir}.$vday/$gribfile ${OUTDIR}/.
fi

if [ $nest = conus -o $nest = ak -o $nest = hi ] && [ $SENDCOM = YES ] ; then

  fst_beg=1
  fst_end=48

  while [ $fst_beg -le $fst_end ] ; do
   hr1=$((fst_beg-1))
   hh=$fst_beg
   typeset -Z2 hh
   $WGRIB2 -match ":${hr1}-${fst_beg} hour" $gribfile|$WGRIB2 -i $gribfile -grib $OUTDIR/${fhead}.t06z.f$hh
   fst_beg=$((fst_beg+1))
  done

fi

echo "Done hypslit data conversion"
