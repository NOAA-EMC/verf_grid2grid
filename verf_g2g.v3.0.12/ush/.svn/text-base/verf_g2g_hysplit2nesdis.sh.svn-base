#!/bin/ksh
#####################################################################
# This script converts the HYSPLIT data to NESDIS data format using 
#           copygb
#
# Author:  Jianping Huang
# History: 2011-11-01 Modified by Julia Zhu for running in production
#          environment
#
# Usage:   verf_g2g_hysplit2nesids.sh NEST DAY
#####################################################################
set -x

nest=$1
yyyymmdd=$2

grid='255  0 801 534 0 -175000 128 80000 -55000 150 150 64 0 0 0 0 0'
export COPYGB=${COPYGB:-/nwprod/util/exec/copygb}

fst_beg=1
fst_end=48

case $nest in
  ak) ftype=aod-smoke-ak ;;
  hi) ftype=aod-smoke-hi ;;
esac

input_dir=$COM_IN/smoke.${yyyymmdd}
output_dir=$COM_OUT/smoke.${yyyymmdd}

fst=${fst_beg}

while [[ ${fst} -le ${fst_end} ]]; do
  if [[ ${fst} -le 9 ]]; then
    export file1=${input_dir}/${ftype}.t06z.f"0"${fst}
    export file2=${output_dir}/${ftype}.t06z.grib255.f"0"${fst}
  else
    export file1=${input_dir}/${ftype}.t06z.f${fst}
    export file2=${output_dir}/${ftype}.t06z.grib255.f${fst}
  fi
  echo "copygb at hour: "$fst
   $COPYGB -g"$grid" -i0,1 -x $file1 $file2
   $COPYGB -g"$grid" -i2,1 -x $file1 $file2
   let fst=fst+1
done

