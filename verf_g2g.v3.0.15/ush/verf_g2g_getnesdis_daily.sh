#!/bin/ksh
#####################################################################
# This script obtains daily NESDIS SMOKE concentration data
#
# Author:  Jianping Huang 
# History: 2011-11-01 Modified by Julia Zhu for running in production
#          environment
#
# Usage:   verf_g2g_getnesdis_daily.sh NEST VDAY
#####################################################################
set -x

export nest=$1
export vday=$2

export COPYGB=${COPYGB:-/nwprod/util/exec/copygb}
export cnvgrib=${cnvgrib:-/nwprod/util/exec/cnvgrib}

export DCOM=${DCOM:-/dcom/us007003}

case $nest in
  conus) nesdisdir=$DCOM/$vday/wgrdbul/smoke
         jmax=801
         imax=534
         vgrid="255" 
         ftype="G13" ;;
  ak)    nesdisdir=$DCOM/$vday/wgrdbul/smoke_west
         jmax=825
         imax=553 
         vgrid="198"
         ftype="GW" ;;
  hi)    nesdisdir=$DCOM/$vday/wgrdbul/smoke_hawaii
         jmax=401
         imax=201
         vgrid="196"
         ftype="GWHI" ;;
esac

#------jp for testing  ---------------
#
export OUTDIR=$COM_OUT/smoke.$vday

# Select NESDIS SAT Algorithm
export algorithm=5

# fhead should be in consistent with file prefix opened in sorc/convert_*.f
#export fhead=aod-smoke

tbeg=${tbeg:-11}
tend=${tend:-23}

cp $nesdisdir/${ftype}.$vday*.2smoke.combaod.hmshysplitcomb2.NAM3.grd .
if [ $SENDCOM = YES ]
then
  echo "DO NOT SAVE THE ORIGINAL FILES"
  #cp $nesdisdir/${ftype}.$vday*.2smoke.combaod.hmshysplitcomb2.NAM3.grd $OUTDIR/.
  cp $nesdisdir/${ftype}.$vday*.2smoke.combaod.hmshysplitcomb2.NAM3.grd $OUTDIR/.
fi

t=$( printf "%02d" $tbeg )
while [ $t -le ${tend} ]
do
  if [ $SENDCOM = YES ]; then
    file=$OUTDIR/${ftype}.$vday${t}.2smoke.combaod.hmshysplitcomb2.NAM3.grd
    if [ $nest = ak -o $nest = hi ] ; then 
      $COPYGB -g${vgrid} -i2,1 -x $file $OUTDIR/${ftype}-grib${vgrid}.t${t}z.f00
      $cnvgrib -g12 $OUTDIR/${ftype}-grib${vgrid}.t${t}z.f00 $OUTDIR/${ftype}.t${t}z.f00
    else
      $cnvgrib -g12 $file $OUTDIR/${ftype}.t${t}z.f00
    fi
  fi

   t=$( printf "%02d" `expr $t + 1` )
done

# Get the first 3 hours of the next day's data as well for AK and HI region:
if [ $nest = ak -o $nest = hi ]
then
  tbeg1=0
  tend1=3
  

  next_day=`/nwprod/util/ush/finddate.sh $vday d+1`
  nesdisdir_p=$DCOM/$next_day/wgrdbul/smoke_west
  if [ $nest = hi ]
    then
    nesdisdir_p=$DCOM/$next_day/wgrdbul/smoke_hawaii
  fi

  OUTDIR_P=$COM_OUT/smoke.${next_day}

  if [ ! -d $OUTDIR_P ]; then mkdir -p $OUTDIR_P; fi

  if [ -e $nesdisdir_p ]
  then
    cp $nesdisdir_p/${ftype}.${next_day}*.2smoke.combaod.hmshysplitcomb2.NAM3.grd $OUTDIR_P/.
    if [ $SENDCOM = YES ]
      then
       echo "DO NOT SAVE THE ORIGINAL FILES"
      cp $nesdisdir_p/${ftype}.${next_day}*.2smoke.combaod.hmshysplitcomb2.NAM3.grd $OUTDIR_P/.
    fi
 
    t=$( printf "%02d" $tbeg1 )
    while [ $t -le ${tend1} ]
    do
      file=$OUTDIR_P/${ftype}.${next_day}${t}.2smoke.combaod.hmshysplitcomb2.NAM3.grd

      $COPYGB -g${vgrid} -i2,1 -x $file   $OUTDIR_P/${ftype}-grib${vgrid}.t${t}z.f00
      if [ $SENDCOM = YES ]; then
       #cp ${ftype}*t${t}z.f00 $OUTDIR_P/.
       $cnvgrib -g12 $OUTDIR_P/${ftype}-grib${vgrid}.t${t}z.f00  $OUTDIR_P/${ftype}.t${t}z.f00
      fi
      t=$( printf "%02d" `expr $t + 1` )
    done
  else
       echo "NESDIS SMOKE data is not available in /dcom"
       echo "Skipping..."
  fi
fi
