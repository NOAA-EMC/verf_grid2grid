#!/bin/ksh
#############################################################################
# script: verf_g2g_get_all_on_firewx.sh
#         to convert MOSAIC, NAMNEST and HRRR to Firewx dynamic domain 
# Author: Binbin Zhou EMC/NCEP
#         Dec 5, 2014
#############################################################################
set -x

export copygb2=${copygb2:-/nwprod/util/exec/copygb2}
export wgrib2=${wgrib2:-/nwprod/util/exec/wgrib2}
export degrib2=${degrib2:-/nwprod/util/exec/degrib2}

modnam=$1
RUN_DAY=$2
NEXT1=$3
NEXT2=$4
RUN_CYC=$5


VALID_DIR=$COM_OUT  #/com/verf/dev or /com/verf/prod
nam_firewx=$COMNAM.${RUN_DAY}/nam.t${RUN_CYC}z.firewxnest.hiresf12.tm00.grib2


$wgrib2 -match ":TMP:2 m" $nam_firewx|$wgrib2 -i $nam_firewx -grib $DATA/t2m.t${RUN_CYC}z.firewxnest.f12
$degrib2 $DATA/t2m.t${RUN_CYC}z.firewxnest.f12|grep "GRID TEMPLATE" > $DATA/griddef.${RUN_DAY}.${RUN_CYC}
read LINE < $DATA/griddef.${RUN_DAY}.${RUN_CYC}
echo "LINE:"$LINE
x=${LINE:21} 
firewxgrid="30 "$x
echo "firewxgrid=$firewxgrid" 

if [ $modnam = mosaic ] ; then
 #Convert MOSAIC grid to Firewx grid
 for hh in 00 01 02 03 04 05 06 07 08 09 10 11 12 13 14 15 16 17 18 19 20 21 22 23 ; do
  for DAY in $RUN_DAY $NEXT1 $NEXT2 ; do  #NEXT2 NEXT2 for longer forecast hours verification (foreward mode)
    VALID=`/nwprod/util/exec/ndate +$hh ${DAY}${RUN_CYC}`
    VALID_DAY=${VALID:0:8}
    VALID_CYC=${VALID:8:2}
    mkdir -p ${VALID_DIR}/firewx.${VALID_DAY}
    echo ${DAY}${RUN_CYC} $VALID $VALID_DAY $VALID_CYC
    mosaic=$COMMOSAIC.${VALID_DAY}/refd3d.t${VALID_CYC}z.grb2f00
    $copygb2 -g"$firewxgrid" -i2,1 -x $mosaic $VALID_DIR/firewx.${VALID_DAY}/refd3d.t${VALID_CYC}z.firewx.${RUN_DAY}${RUN_CYC}.grib2
    echo copygb2: $mosaic $VALID_DIR/firewx.${VALID_DAY}/refd3d.t${VALID_CYC}z.firewx.${RUN_DAY}${RUN_CYC}.grib2  done!
  done
 done

 #For g2g source's reading fort.15 file two times
 echo $firewxgrid  > $DATA/firewx.kgds.${RUN_DAY}${RUN_CYC}
 echo $firewxgrid  >> $DATA/firewx.kgds.${RUN_DAY}${RUN_CYC}
fi


if [ $modnam = hrrr ] ; then
  #Convert HRRR grid to Firewx grid
  hrrr_orig=/com2/hrrr/prod/hrrr.${RUN_DAY}
  for hh in 01 02 03 04 05 06 07 08 09 10 11 12 13 14 15 ; do
    file=$hrrr_orig/hrrr.t${RUN_CYC}z.wrfsfcf${hh}.grib2
    $wgrib2 -match ":REF" $file|$wgrib2 -i $file -grib  $DATA/temp.${RUN_CYC}.${hh}
    $copygb2 -g"$firewxgrid" -i2,1 -x $DATA/temp.${RUN_CYC}.${hh}  ${VALID_DIR}/firewx.${RUN_DAY}/hrrr.t${RUN_CYC}z.firewx.f${hh}.grib2
    echo copygb2: $file ${VALID_DIR}/firewx.${RUN_DAY}/hrrr.t${RUN_CYC}z.firewx.f${hh}.grib2 done!
  done
  rm -f $DATA/temp.*
fi

if [ $modnam = namnest ] ; then
 #Convert NAMnest  grid to Firewx grid
 namnest_orig=/com2/nam/prod/nam.${RUN_DAY}
 for hh in 01 02 03 04 05 06 07 08 09 10 11 12 13 14 15 18 24 30 36 42 48 ; do
   file=$namnest_orig/nam.t${RUN_CYC}z.conusnest.hiresf${hh}.tm00.grib2 
   $wgrib2 -match ":REF" $file|$wgrib2 -i $file -grib  $DATA/temp.${RUN_CYC}.${hh}
   $copygb2 -g"$firewxgrid" -i2,1 -x $DATA/temp.${RUN_CYC}.${hh}  ${VALID_DIR}/firewx.${RUN_DAY}/namnest.t${RUN_CYC}z.firewx.f${hh}.grib2
   echo copygb2: $file ${VALID_DIR}/firewx.${RUN_DAY}/namnest.t${RUN_CYC}z.firewx.f${hh}.grib2 done!
 done
   rm -f $DATA/temp.*
fi

exit

#Convert HiresARW and HiresNMMB grid to Firewx grid
hiresw_orig=${hiresw_orig:-/gpfs/hps/nco/ops/com/hiresw/prod/hiresw.${RUN_DAY}}
mkdir -p $hiresw
cd $hiresw
if [ $RUN_CYC = '00' ] || [ $RUN_CYC = '12' ] ; then
for hh in 01 02 03 04 05 06 07 08 09 10 11 12 13 14 15 18 24 30 36 ; do
  $copygb -g"$firewxgrid" -x $hiresw_orig/hiresw.t${RUN_CYC}z.arw_5km.f${hh}.conus.grib2 $hiresw/hiresw.t${RUN_CYC}z.arw_5km.f${hh}.firewx.grib2
  $copygb -g"$firewxgrid" -x $hiresw_orig/hiresw.t${RUN_CYC}z.nmmb_5km.f${hh}.conus.grib2 $hiresw/hiresw.t${RUN_CYC}z.nmmb_5km.f${hh}.firewx.grib2
done
fi


exit
