#!/bin/ksh
#####################################################################
# This script obtains daily HYSPLIT SMOKE concentration data
#
# Author:  Jianping Huang
# History: 2011-11-01 Modified by Julia Zhu for running in production
#          environment
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

export WGRIB2=/nwprod/util/exec/wgrib2
export COPYGB=${COPYGB:-/nwprod/util/exec/copygb}

export COMHYSPT=${COMHYSPT:-/com2/hysplit/prod}

export gribfile_conus_smoke=smokecs.t06z.pbl.1hr.grib2
export gribfile_ak_smoke=smokeak.t06z.pbl.1hr_198.grib2
export gribfile_hi_smoke=smokehi.t06z.pbl.1hr_196.grib2

case $nest in
  conus) hysplitdir=$COMHYSPT/smokecs
         OUTDIR=$COM_OUT/smoke.$vday
         gribfile=$gribfile_conus_smoke
         fhead=aod-smoke-cs
         jmax=801
         imax=534
         vgrid=255;;
  ak)    hysplitdir=$COMHYSPT/smokeak
         OUTDIR=$COM_OUT/smoke.$vday
         gribfile=$gribfile_ak_smoke
         fhead=aod-smoke-ak 
         vgrid=198
         jmax=825
         imax=553 ;;
  hi)    hysplitdir=$COMHYSPT/smokehi
         OUTDIR=$COM_OUT/smoke.$vday
         gribfile=$gribfile_hi_smoke
         fhead=aod-smoke-hi
         vgrid=196
         jmax=321
         imax=225 ;;
esac

# below added by jphuang for testing 
##
# Select NESDIS SAT Algorithm
# jp export algorithm=5

# fhead should be in consistent with file prefix opened in sorc/convert_*.f
export fhead=$fhead

if [ -s ${hysplitdir}.$vday/$gribfile ]
then
   cp ${hysplitdir}.$vday/$gribfile .
   cp ${hysplitdir}.$vday/$gribfile ${OUTDIR}/.
   #Before grib2 smoke files are available, use grib1 to convert
#   cnvgrib -g12 ${hysplitdir}.$vday/$gribfile ${gribfile}.grib2
#   cnvgrib -g12 ${hysplitdir}.$vday/$gribfile ${OUTDIR}/${gribfile}.grib2
   #otherwise: 
#    cp ${hysplitdir}.$vday/$gribfile ${gribfile}.grib2
#    cp ${hysplitdir}.$vday/$gribfile ${OUTDIR}/${gribfile}.grib2
fi

  fst_beg=1
  fst_end=48


    #if [ $SENDCOM = YES ]; then
    #  cp -rp ${fhead}.t06z.f* $OUTDIR/.
    #fi

  while [ $fst_beg -le $fst_end ] ; do
   hr1=$((fst_beg-1))
   hh=$fst_beg
   typeset -Z2 hh
   $WGRIB2 -match ":${hr1}-${fst_beg} hour"  ${OUTDIR}/${gribfile}|$WGRIB2 -i ${OUTDIR}/${gribfile}  -grib $OUTDIR/${fhead}.t06z.f$hh
   fst_beg=$((fst_beg+1))
  done


echo "Done hypslit data conversion"
