#!/bin/ksh
#####################################################################
# This script obtains the AFWA, CLAVR, GFS and HIRESW cloud data
# fnd interpolate them onto 212 grid as for the NAM grid
# History:   Binbin Zhou  -- May 2010, original version
#            Julia Zhu  -- June 30th, second version
#            B. Zhou    -- Nov. 15, 2014 upgraded to grib2
# Usage: verf_g2g_get_cloud.sh DATA_TYPE(MODEL NAME)
#####################################################################
set -x

model_name=$1
vday=$2
domain=$3

export copygb=${copygb:-/nwprod/util/exec/copygb}
export copygb2=${copygb2:-/nwprod/util/exec/copygb2}
export cnvgrib=${cnvgrib:-/nwprod/util/exec/cnvgrib}
export wgrib2=${wgrib2:-/nwprod/util/exec/wgrib2}

if [ $domain = 'CONUS' ] ; then

 if [ $model_name = afwa ]; then    # AFWA data
  AFWADIR=${AFWADIR:-/dcom/us007003}
  OUTDIR=$COMOUT
  for HH in 00 06 12 18
  do
    $copygb -B-1 -K-1 -x -g212 $AFWADIR/$vday/wgrbbul/cloud/grbclouds.${vday}${HH}00 $COMOUT/afwa.t${HH}z.grd212.f00
    $cnvgrib -g12  $COMOUT/afwa.t${HH}z.grd212.f00 $COMOUT/afwa.t${HH}z.grd212.f00.grib2
    echo "afwa.t${HH}z.grd212.f00.grib2 done"
  done

 elif [ $model_name = clavr ]; then   # CLAVR data
  CLAVRDIR=${CLAVRDIR:-/dcom/us007003}
  for HH in 00 06 12 18
  do
    $copygb -B-1 -K-1  -x -g212 -i0 $CLAVRDIR/$vday/wgrbbul/clavrx/clvrxgrb.${vday}${HH} $COMOUT/clavr.t${HH}z.grd212.f00
    $cnvgrib -g12 $COMOUT/clavr.t${HH}z.grd212.f00 $COMOUT/clavr.t${HH}z.grd212.f00.grib2
     echo "clavr.t${HH}z.grd212.f00.grib2 done"
  done

 elif [ $model_name = gfs ]; then   # GFS data
  COMGFS=${COMGFS:-/com2/gfs/prod/gfs}
  for HH in 00 06 12 18
  do
    fhr=00
    typeset -Z3 fh
    while [ $fhr -le 84 ]
    do
       fh=$fhr
       file1=$COMGFS.$vday/gfs.t${HH}z.pgrb2.0p50.f${fh}
       $wgrib2 -match ":TCDC:entire atmosphere" $file1|$wgrib2 -i $file1 -grib retrieved1
       #convert to 212 grid
       $copygb2 -g"30 6 0 0 0 0 0 0 185 129 12190000 226541000 8 25000000 265000000 40635000 40635000 0 64 25000000 25000000 0 0" -i3 -x  retrieved1 $COMOUT/gfs.t${HH}z.grd212.f${fhr}.grib2 
    
       rm -f  retrieved1 
 
       fhr=`expr $fhr + 6`
       if [ $fhr -lt 10 ]; then fhr=0$fhr; fi
    done
  done
  
  echo "copying of GFS done"

 elif [ $model_name = hiresw ]; then   # HIRESW data
  COMHIRESW=${COMHIRESW:-/com/hiresw/prod/hiresw}
  # Only runs at 00/12Z
  for HH in 00 12
  do
    fhr=00
    while [ $fhr -le 48 ]
    do
      file1=$COMHIRESW.$vday/hiresw.t${HH}z.arw_5km.f${fhr}.conus.grib2
      $wgrib2 -match ":TCDC:entire atmosphere" $file1|$wgrib2 -i $file1 -grib retrieved1
      $copygb2 -g"30 6 0 0 0 0 0 0 185 129 12190000 226541000 8 25000000 265000000 40635000 40635000 0 64 25000000 25000000 0 0" -i3 -x retrieved1 $COMOUT/conusarw.t${HH}z.grd212.f${fhr}.grib2
      rm -f retrieved1

      file2=$COMHIRESW.$vday/hiresw.t${HH}z.nmmb_5km.f${fhr}.conus.grib2
      $wgrib2 -match ":TCDC:entire atmosphere" $file2|$wgrib2 -i $file2 -grib retrieved2
      $copygb2 -g"30 6 0 0 0 0 0 0 185 129 12190000 226541000 8 25000000 265000000 40635000 40635000 0 64 25000000 25000000 0 0" -i3 -x  retrieved2 $COMOUT/conusnmmb.t${HH}z.grd212.f${fhr}.grib2
      rm -f retrieved2

      fhr=`expr $fhr + 3`
      if [ $fhr -lt 10 ]; then fhr=0$fhr; fi
    done
  done

  echo "copying of HIRESW done"

 elif [ $model_name = rap ] ; then
  for HH in 00 06 12 18 ; do 
   fhr=00 
   while [ $fhr -le 15 ] ; do 
     COMHRRR=${COMHRRR:-/com2/hrrr/prod/hrrr}
     file1=$COMHRRR.$vday/hrrr.t${HH}z.wrfsfcf${fhr}.grib2
     $wgrib2 -match ":TCDC:entire atmosphere" $file1 |$wgrib2  -i $file1 -grib  retrieved1
     $copygb2 -g"30 6 0 0 0 0 0 0 185 129 12190000 226541000 8 25000000 265000000 40635000 40635000 0 64 25000000 25000000 0 0" -i3 -x retrieved1 $COMOUT/hrrr.t${HH}z.grd212.f${fhr}.grib2
     rm -f retrieved1

     COMRAP=${COMRAP:-/com2/rap/prod/rap}
     file2=$COMRAP.$vday/rap.t${HH}z.awp130pgrbf${fhr}.grib2
     $wgrib2 -match ":TCDC:entire atmosphere" $file2|$wgrib2  -i $file2 -grib retrieved2
     $copygb2 -g"30 6 0 0 0 0 0 0 185 129 12190000 226541000 8 25000000 265000000 40635000 40635000 0 64 25000000 25000000 0 0" -i3 -x retrieved2 $COMOUT/rap.t${HH}z.grd212.f${fhr}.grib2
      rm -f retrieved2

      fhr=`expr $fhr + 3`
      if [ $fhr -lt 10 ]; then fhr=0$fhr; fi
    done
  done

  echo "copying of RAP/HRRR done"

 elif [ $model_name = nam ] ; then
   for HH in 00 06 12 18 ; do
    fhr=00
    while [ $fhr -le 84 ] ; do 
     COMNAMNEST=${COMNAMNEST:-/com/nam/prod/nam}
     file1=$COMNAMNEST.$vday/nam.t${HH}z.conusnest.hiresf${fhr}.tm00.grib2
     $wgrib2 -match ":TCDC:entire atmosphere" $file1|$wgrib2 -i $file1 -grib retrieved1
     $copygb2 -g"30 6 0 0 0 0 0 0 185 129 12190000 226541000 8 25000000 265000000 40635000 40635000 0 64 25000000 25000000 0 0" -i3 -x retrieved1 $COMOUT/namnest.t${HH}z.grd212.f${fhr}.grib2
     rm -f retrieved1

     fhr=`expr $fhr + 6`
     if [ $fhr -lt 10 ]; then fhr=0$fhr; fi
    done
  done

  echo "copying of NAMNEST done"
 fi

else        # and of domain=CONUS
  #following are for Alaska, GFS, NAM, NAMnest and RAP are included, NAM already has grid#242 files   

 if [ $model_name = afwa ]; then    # AFWA data

  AFWADIR=${AFWADIR:-/dcom/us007003}
  OUTDIR=$COMOUT

  for HH in 00 06 12 18
  do
    $copygb -B-1 -K-1 -x -g242 $AFWADIR/$vday/wgrbbul/cloud/grbclouds.${vday}${HH}00 $COMOUT/afwa.t${HH}z.grd242.f00
    $cnvgrib -g12  $COMOUT/afwa.t${HH}z.grd242.f00 $COMOUT/afwa.t${HH}z.grd242.f00.grib2
    echo "afwa.t${HH}z.grd242.f00.grib2 done"
  done

 elif [ $model_name = clavr ]; then   # CLAVR data
  CLAVRDIR=${CLAVRDIR:-/dcom/us007003}
  for HH in 00 06 12 18
  do
    $copygb -B-1 -K-1  -x -g242 -i0 $CLAVRDIR/$vday/wgrbbul/clavrx/clvrxgrb.${vday}${HH} $COMOUT/clavr.t${HH}z.grd242.f00
    $cnvgrib -g12 $COMOUT/clavr.t${HH}z.grd242.f00 $COMOUT/clavr.t${HH}z.grd242.f00.grib2
     echo "clavr.t${HH}z.grd242.f00.grib2 done"
  done

 elif [ $model_name = gfs ]; then   # GFS data
  COMGFS=${COMGFS:-/com2/gfs/prod/gfs}
  for HH in 00 06 12 18
  do
    fhr=00
    typeset -Z3 fh
    while [ $fhr -le 84 ]
    do
       fh=$fhr 
       file1=$COMGFS.$vday/gfs.t${HH}z.pgrb2.0p50.f${fh}
       $wgrib2 -match ":TCDC:entire atmosphere" $file1|$wgrib2 -i $file1 -grib retrieved1
       $copygb2 -g"20 6 0 0 0 0 0 0 553 425 30000000 187000000 8 60000000 225000000 11250000 11250000 0 64" -i3 -x retrieved1 $COMOUT/gfs.t${HH}z.grd242.f${fhr}.grib2

       rm -f  retrieved1

       fhr=`expr $fhr + 6`
       if [ $fhr -lt 10 ]; then fhr=0$fhr; fi
    done
  done

  echo "copying of GFS 242 done"

 elif [ $model_name = nam ] ; then
   for HH in 00 06 12 18 ; do
    fhr=00
    while [ $fhr -le 84 ] ; do
     COMNAMNEST=${COMNAMNEST:-/com/nam/prod/nam}
     file1=$COMNAMNEST.$vday/nam.t${HH}z.alaskanest.hiresf${fhr}.tm00.grib2
     $wgrib2  -match ":TCDC:entire atmosphere"  $file1|$wgrib2  -i  $file1  -grib retrieved1
     $copygb2  -g"20 6 0 0 0 0 0 0 553 425 30000000 187000000 8 60000000 225000000 11250000 11250000 0 64"  -i3 -x  retrieved1 $COMOUT/namnest.t${HH}z.grd242.f${fhr}.grib2
     rm -f retrieved1

     fhr=`expr $fhr + 6`
     if [ $fhr -lt 10 ]; then fhr=0$fhr; fi
    done
  done

  echo "copying of NAMNEST 242 done"
 fi

fi


