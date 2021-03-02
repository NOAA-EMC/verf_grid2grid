#!/bin/ksh


ref=$1 
grid=$2

 read LINE
  echo $LINE
 read LINE
  echo $LINE
 read LINE
  echo $LINE
 set -A tfcst $LINE
 t=1
 while [ $t -le ${tfcst[0]} ] ; do
  read LINE
  echo $LINE
  yyyy=`echo ${LINE} | cut -c 1-4`
    mm=`echo ${LINE} | cut -c 5-6`
    dd=`echo ${LINE} | cut -c 7-8`
   cyc=`echo ${LINE} | cut -c 9-10`
    ff=`echo ${LINE} | cut -c 11-12`

    ref_data=$COM_OUT/${RUN}.$yyyy$mm$dd

  if [ $grid = 255 ] ; then
    cp $ref_data/${ref}.t${cyc}z.apcp.f${ff}.grib2  $DATA/clim.mean.$yyyy$mm$dd$cyc$ff
    cp $ref_data/${ref}.t${cyc}z.apcp.f${ff}.grib2  $DATA/clim.sprd.$yyyy$mm$dd$cyc$ff
  else
    cp $ref_data/${ref}.t${cyc}z.grid${grid}.f${ff}.grib2  $DATA/clim.mean.$yyyy$mm$dd$cyc$ff
    cp $ref_data/${ref}.t${cyc}z.grid${grid}.f${ff}.grib2  $DATA/clim.sprd.$yyyy$mm$dd$cyc$ff 
  fi

  t=`expr $t + 1` 
 done 
exit

