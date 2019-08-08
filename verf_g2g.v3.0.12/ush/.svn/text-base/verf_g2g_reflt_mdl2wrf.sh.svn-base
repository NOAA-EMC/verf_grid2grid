#!/bin/ksh
#############################################################################
# script: verf_g2g_mdl2wrf_grid.sh $model
#         to convert NAM, RAP and SREF-NMM/ARW grids to Hi-res WRF grid
# Author: Binbin Zhou /SAIC
#         March 25, 2010
# B. Zhou Nov 19, 2014 upgraded to grib2
#############################################################################
set -x

modnam=$1

export vday=${vday:-$PDYm1}
export vdate=${vdate:-$vday$cyc}

export copygb=${copygb:-/nwprod/util/exec/copygb}
export copygb2=${copygb2:-/nwprod/util/exec/copygb2}
export cnvgrib=${cnvgrib:-/nwprod/util/exec/cnvgrib}
export wgrib2=${wgrib2:-/nwprod/util/exec/wgrib2}



#############################################
#1:Get convert mosaic to Hires-WRF grid#227 
#############################################
if [ $modnam = mosaic ]; then
  COMMOSAIC=${COMMOSAIC:-/com/hourly/prod/radar}
  #COMMOSAIC=/meso/noscrub/Shun.Liu/com/hourly/prod/radar
  cycles="00 03 06 09 12 15 18 21"
  for cyc in $cycles
  do
    mosaic=$COMMOSAIC.$vday/refd3d.t${cyc}z.grb2f00
    $copygb2 -g"30 6 0 0 0 0 0 0 1473 1025 12190000 226541000 8 25000000 265000000 5079000 5079000 0 64 25000000 25000000 0 0" -i2,1 -x $mosaic $COMOUT/refd3d.t${cyc}z.grid227.f00
    echo 'copygb2 mosaic ' $cyc ' done!'
  done
fi

########################################
#2:Get convert NAM to Hires-WRF grid#227
########################################
if [ $modnam = nam ]; then
  COMNAM=${COMNAM:-/com/nam/prod/nam}
  cycles="00 06 12 18"
  for cyc in  $cycles
  do
    for fhr in 06 12 18 24 30 36 42 48 54 60 66 72 78 84
    do
      nam=$COMNAM.$vday/nam.t${cyc}z.awphys${fhr}.tm00.grib2
      $wgrib2 -match ":REFC:entire atmosphere" $nam |$wgrib2 -i $nam -grib $DATA/temp.${cyc}.${fhr}
      $wgrib2 -match ":REFD:1000 m" $nam       |$wgrib2 -i $nam -grib $DATA/ref1k.${cyc}.${fhr}
      cat $DATA/ref1k.${cyc}.${fhr} >> $DATA/temp.${cyc}.${fhr} 
      $copygb2 -g"30 6 0 0 0 0 0 0 1473 1025 12190000 226541000 8 25000000 265000000 5079000 5079000 0 64 25000000 25000000 0 0" -i2,1 -x  $DATA/temp.${cyc}.${fhr} $COMOUT/nam.t${cyc}z.grid227.f${fhr}
      echo 'copygb2 nam ' $cyc $fhr ' done!'
    done
  done
  rm -f $DATA/temp* $DATA/ref1k*
fi

########################################
#:Get convert NAMNEST to Hires-WRF grid#227
# NAMNest is already grid227, not to convert
########################################


#######################################
#3:Get convert RAP to Hires-WRF grid#227
#######################################
if [ $modnam = rap ]; then
  COMRAP=${COMRAP:-/com2/rap/prod/rap}
  cycles="00 06 12 18"
  for cyc in $cycles
  do
    for fhr in 06 12 18
    do 
      rap1=$COMRAP.$vday/rap.t${cyc}z.awp130pgrbf${fhr}.grib2
      $wgrib2 -match ":REFC:entire atmosphere" $rap1|$wgrib2 -i $rap1 -grib $DATA/temp.${cyc}.${fhr}
      $wgrib2 -match ":REFD:1000 m" $rap1      |$wgrib2 -i $rap1 -grib $DATA/ref1k.${cyc}.${fhr}
      cat $DATA/ref1k.${cyc}.${fhr} >> $DATA/temp.${cyc}.${fhr}
      $copygb2 -g"30 6 0 0 0 0 0 0 1473 1025 12190000 226541000 8 25000000 265000000 5079000 5079000 0 64 25000000 25000000 0 0" -i2,1 -x  $DATA/temp.${cyc}.${fhr} $COMOUT/rap.t${cyc}z.grid227.f${fhr}
      echo 'copygb2 rap ' $cyc $fhr ' done!'
    done
  done
  rm -f $DATA/temp* $DATA/ref1k*
  rm -f $DATA/*pgrb13*
fi

if [ $modnam = hrrr ]; then
  COMHRRR=${COMHRRR:-/com2/hrrr/prod/hrrr}
  cycles="00 06 12 18"
  for cyc in $cycles
  do
    for fhr in 06 12 18
    do
      file_grib2=$COMHRRR.$vday/hrrr.t${cyc}z.wrfsfcf${fhr}.grib2
      $wgrib2 -match ":REFC:entire atmosphere" $file_grib2 |$wgrib2 -i $file_grib2 -grib $DATA/temp.${cyc}.${fhr}
      $wgrib2 -match ":REFD:1000 m"       $file_grib2 |$wgrib2 -i $file_grib2 -grib $DATA/ref1k.${cyc}.${fhr}
      cat $DATA/ref1k.${cyc}.${fhr} >> $DATA/temp.${cyc}.${fhr}
      $copygb2 -g"30 6 0 0 0 0 0 0 1473 1025 12190000 226541000 8 25000000 265000000 5079000 5079000 0 64 25000000 25000000 0 0" -i2,1 -x  $DATA/temp.${cyc}.${fhr} $COMOUT/hrrr.t${cyc}z.grid227.f${fhr}
      echo 'copygb2 hrrr ' $cyc $fhr ' done!'
    done
  done
  rm -f $DATA/temp* $DATA/ref1k*
fi


#####################################################
#4: Get convert SREF nmm control grid to grid#227
#####################################################
if [ $modnam = srefnmm ]; then 
  COMSREF=${COMSREF:-/com2/sref/prod/sref}
  cycles="03 09 15 21"
  mdl=nmm
  for cyc in $cycles
  do
    for fhr in 06 12 18 24 30 36 42 48 54 60 66 72 78 84
    do
      srefmdl=$COMSREF.$vday/$cyc/pgrb/sref_${mdl}.t${cyc}z.pgrb216.ctl.f${fhr}.grib2
      $wgrib2 -match ":REFC:entire atmosphere" $srefmdl |$wgrib2 -i $srefmdl -grib $DATA/temp.${cyc}.${fhr}
      $copygb2 -g"30 6 0 0 0 0 0 0 1473 1025 12190000 226541000 8 25000000 265000000 5079000 5079000 0 64 25000000 25000000 0 0" -i2,1 -x $DATA/temp.${cyc}.${fhr} $COMOUT/${modnam}.t${cyc}z.grid227.f${fhr}
      echo 'copygb2 srefnmm ' $cyc $fhr ' done!'
    done
  done
  rm -f $DATA/temp*
fi

#####################################################
#4: Get convert SREF arw control grid to grid#227
#####################################################
if [ $modnam = srefarw ]; then 
  COMSREF=${COMSREF:-/com2/sref/prod/sref}
  cycles="03 09 15 21"
  mdl=em
  for cyc in $cycles
  do
    for fhr in 06 12 18 24 30 36 42 48 54 60 66 72 78 84
    do
      srefmdl=$COMSREF.$vday/$cyc/pgrb/sref_${mdl}.t${cyc}z.pgrb216.ctl.f${fhr}.grib2
      $wgrib2 -match ":REFC:entire atmosphere" $srefmdl|$wgrib2 -i $srefmdl -grib $DATA/temp.${cyc}.${fhr}
      $copygb2 -g"30 6 0 0 0 0 0 0 1473 1025 12190000 226541000 8 25000000 265000000 5079000 5079000 0 64 25000000 25000000 0 0" -i2,1 -x $DATA/temp.${cyc}.${fhr} $COMOUT/${modnam}.t${cyc}z.grid227.f${fhr}
      echo 'copygb2 srefarw ' $cyc $fhr ' done!'
    done
  done
  rm -f $DATA/temp*
fi

#####################################################
#4: Get convert SREF nmmb control grid to grid#227
#####################################################
if [ $modnam = srefnmmb ]; then
  COMSREF=${COMSREF:-/com2/sref/prod/sref}
  cycles="03 09 15 21"
  mdl=nmb
  for cyc in $cycles
  do
    for fhr in 06 12 18 24 30 36 42 48 54 60 66 72 78 84
    do
      srefmdl=$COMSREF.$vday/$cyc/pgrb/sref_${mdl}.t${cyc}z.pgrb216.ctl.f${fhr}.grib2
      $wgrib2 -match ":REFC:entire atmosphere" $srefmdl|$wgrib2 -i $srefmdl -grib $DATA/temp.${cyc}.${fhr}
      $copygb2 -g"30 6 0 0 0 0 0 0 1473 1025 12190000 226541000 8 25000000 265000000 5079000 5079000 0 64 25000000 25000000 0 0" -i2,1 -x $DATA/temp.${cyc}.${fhr} $COMOUT/${modnam}.t${cyc}z.grid227.f${fhr}
      echo 'copygb2 srefarw ' $cyc $fhr ' done!'
    done
  done
  rm -f $DATA/temp*
fi



exit
