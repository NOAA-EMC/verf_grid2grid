#!/bin/ksh

set -x

export copygb=${copygb2:-/nwprod/util/exec/copygb}
export copygb2=${copygb2:-/nwprod/util/exec/copygb2}
export cnvgrib=${cnvgrib:-/nwprod/util/exec/cnvgrib}
export wgrib2=${wgrib2:-/nwprod/util/exec/wgrib2}
export wgrib=${wgrib:-/nwprod/util/exec/wgrib}


modnam=$1

if [ $modnam = urma ]; then

   COMURMA=${COMURMA:-/com2/urma/prod/urma2p5}
  vgrid=184
  for cyc in 00 03 06 09 12 15 18 21 ; do

     urma=$COMURMA.$vday/urma2p5.t${cyc}z.2dvaranl_ndfd.grb2
     cp $urma $COMOUT/urma2p5.t${cyc}z.grid${vgrid}.f00.grib2
     echo 'get urma2p5 grib2 ' $cyc ' done!'

  done

fi

if [ $modnam = mosaic ]; then
  COMMOSAIC=${COMMOSAIC:-/com2/hourly/prod/radar}
  cycles="00 03 06 09 12 15 18 21"
  for cyc in $cycles ; do
    mosaic=$COMMOSAIC.$vday/refd3d.t${cyc}z.grb2f00
    $copygb2 -g"30 6 0 0 0 0 0 0 1473 1025 12190000 226541000 8 25000000 265000000 5079000 5079000 0 64 25000000 25000000 0 0" -i2,1 -x $mosaic $COMOUT/refd3d.t${cyc}z.grid227.f00
    echo 'copygb2 mosaic ' $cyc ' done!'
  done
fi

if [ $modnam = ccpa ] ; then
  COMCCPA=${COMCCPA:-/com2/ccpa/prod/ccpa}
   cycles="06 12 18"
   for cyc in $cycles ; do
     ccpa=$COMCCPA.${vday}/$cyc/ccpa.t${cyc}z.03h.hrap.conus.gb2
     cp $ccpa $COMOUT/ccpa.t${cyc}z.g255.f00 
   done
fi 
    


if [ $modnam = reference ] ; then

  COMHRRR=${COMHRRR:-/com2/hrrr/prod/hrrr}
  cycles="00 03 06 09 12 15 18 21"

  for cyc in $cycles ; do
   for fhr in 03 06 09 12 15 18 ; do 

     hrrr=${COMHRRR}.${vday}/hrrr.t${cyc}z.wrfsfcf${fhr}.grib2
     >$COMOUT/hrrr.t${cyc}z.grid184.f${fhr}.grib2
     >$COMOUT/hrrr.t${cyc}z.grid227.f${fhr}.grib2
     $wgrib2 -match ":VIS:surface" $hrrr |$wgrib2 -i  $hrrr -grib  $DATA/vis
     $copygb2 -g"30 6 0 0 0 0 0 0 2145 1377 20191999 238445999 8 25000000 265000000 2539703 2539703 0 64 25000000 25000000 -90000000 0" -i2,1 -x  $DATA/vis $DATA/hrrr_vis
     $wgrib2 -match ":TCDC:entire atmosphere" $hrrr |$wgrib2 -i  $hrrr -grib  $DATA/cloud
     $copygb2 -g"30 6 0 0 0 0 0 0 2145 1377 20191999 238445999 8 25000000 265000000 2539703 2539703 0 64 25000000 25000000 -90000000 0" -i2,1 -x  $DATA/cloud $DATA/hrrr_cloud
     $wgrib2 -match ":TMP:2 m" $hrrr |$wgrib2 -i  $hrrr -grib  $DATA/t2m
     $copygb2 -g"30 6 0 0 0 0 0 0 2145 1377 20191999 238445999 8 25000000 265000000 2539703 2539703 0 64 25000000 25000000 -90000000 0" -i2,1 -x  $DATA/t2m $DATA/hrrr_t2m

     $wgrib2 -match ":DPT:2 m" $hrrr |$wgrib2 -i  $hrrr -grib  $DATA/dpt
     $copygb2 -g"30 6 0 0 0 0 0 0 2145 1377 20191999 238445999 8 25000000 265000000 2539703 2539703 0 64 25000000 25000000 -90000000 0" -i2,1 -x  $DATA/dpt $DATA/hrrr_dpt


     cat $DATA/hrrr_vis >>   $COMOUT/hrrr.t${cyc}z.grid184.f${fhr}.grib2
     cat $DATA/hrrr_cloud >> $COMOUT/hrrr.t${cyc}z.grid184.f${fhr}.grib2
     cat $DATA/hrrr_t2m >>   $COMOUT/hrrr.t${cyc}z.grid184.f${fhr}.grib2
     cat $DATA/hrrr_dpt >>   $COMOUT/hrrr.t${cyc}z.grid184.f${fhr}.grib2

     $wgrib2 -match ":REFC:entire atmosphere" $hrrr |$wgrib2 -i $hrrr -grib $DATA/ref
     $wgrib2 -match ":RETOP" $hrrr |$wgrib2 -i $hrrr -grib $DATA/etp
     $copygb2 -g"30 6 0 0 0 0 0 0 1473 1025 12190000 226541000 8 25000000 265000000 5079000 5079000 0 64 25000000 25000000 0 0" -i2,1 -x  $DATA/ref $DATE/hrrr_ref
     $copygb2 -g"30 6 0 0 0 0 0 0 1473 1025 12190000 226541000 8 25000000 265000000 5079000 5079000 0 64 25000000 25000000 0 0" -i2,1 -x  $DATA/etp $DATE/hrrr_etp
     cat $DATA/hrrr_ref >> $COMOUT/hrrr.t${cyc}z.grid227.f${fhr}.grib2
     cat $DATA/hrrr_etp >> $COMOUT/hrrr.t${cyc}z.grid227.f${fhr}.grib2

     rm -f $DATA/hrrr_vis $DATA/hrrr_cloud $DATA/hrrr_t2m $DATA/hrrr_dpt $DATA/vis $DATA/cloud $DATA/t2m $DATA/ref $DATA/dpt  $DATA/hrrr_etp

    done
   done

fi

if [ $modnam = reference2 ] ; then

  COMNAM=${COMNAM:-/com2/nam/prod/nam}
  cycles="00 06 12 18"

  for cyc in $cycles ; do
   for fhr in 06 12 18 24 30 36 ; do

     namnest=$COMNAM.${vday}/nam.t${cyc}z.conusnest.hiresf${fhr}.tm00.grib2
     >$COMOUT/namnest.t${cyc}z.grid184.f${fhr}.grib2
     >$COMOUT/namnest.t${cyc}z.grid227.f${fhr}.grib2
     $wgrib2 -match ":VIS:surface" $namnest |$wgrib2 -i  $namnest -grib  $DATA/vis
     $copygb2 -g"30 6 0 0 0 0 0 0 2145 1377 20191999 238445999 8 25000000 265000000 2539703 2539703 0 64 25000000 25000000 -90000000 0" -i2,1 -x  $DATA/vis $DATA/namnest_vis
     $wgrib2 -match ":TCDC:entire atmosphere" $namnest |$wgrib2 -i  $namnest -grib  $DATA/cloud
     $copygb2 -g"30 6 0 0 0 0 0 0 2145 1377 20191999 238445999 8 25000000 265000000 2539703 2539703 0 64 25000000 25000000 -90000000 0" -i2,1 -x  $DATA/cloud $DATA/namnest_cloud
     $wgrib2 -match ":TMP:2 m" $namnest |$wgrib2 -i  $namnest -grib  $DATA/t2m
     $copygb2 -g"30 6 0 0 0 0 0 0 2145 1377 20191999 238445999 8 25000000 265000000 2539703 2539703 0 64 25000000 25000000 -90000000 0" -i2,1 -x  $DATA/t2m $DATA/namnest_t2m
     $wgrib2 -match ":DPT:2 m" $namnest |$wgrib2 -i  $namnest -grib  $DATA/dpt
     $copygb2 -g"30 6 0 0 0 0 0 0 2145 1377 20191999 238445999 8 25000000 265000000 2539703 2539703 0 64 25000000 25000000 -90000000 0" -i2,1 -x  $DATA/dpt $DATA/namnest_dpt
     $wgrib2 -match ":APCP:surface" $namnest |$wgrib2 -i  $namnest -grib  $DATA/apcp
     $copygb2 -g"20 6 0 0 0 0 0 0 1121 881 23117000 240977000 8 60000000 255000000 4763000 4763000 0 64" -i2,1 -x $DATA/apcp $COMOUT/namnest.t${cyc}z.apcp.f${fhr}.grib2
     cat $DATA/namnest_vis >>   $COMOUT/namnest.t${cyc}z.grid184.f${fhr}.grib2
     cat $DATA/namnest_cloud >> $COMOUT/namnest.t${cyc}z.grid184.f${fhr}.grib2
     cat $DATA/namnest_t2m >>   $COMOUT/namnest.t${cyc}z.grid184.f${fhr}.grib2
     cat $DATA/namnest_dpt >>   $COMOUT/namnest.t${cyc}z.grid184.f${fhr}.grib2
     $wgrib2 -match ":REFC:entire atmosphere" $namnest |$wgrib2 -i $namnest -grib $DATA/ref 
     $copygb2 -g"30 6 0 0 0 0 0 0 1473 1025 12190000 226541000 8 25000000 265000000 5079000 5079000 0 64 25000000 25000000 0 0" -i2,1 -x  $DATA/ref $DATA/namnest_ref 
     $wgrib2 -match "RETOP" $namnest |$wgrib2 -i $namnest -grib $DATA/etp
     $copygb2 -g"30 6 0 0 0 0 0 0 1473 1025 12190000 226541000 8 25000000 265000000 5079000 5079000 0 64 25000000 25000000 0 0" -i2,1 -x  $DATA/etp $DATA/namnest_etp
     cat $DATA/namnest_ref >> $COMOUT/namnest.t${cyc}z.grid227.f${fhr}.grib2
     cat $DATA/namnest_etp >> $COMOUT/namnest.t${cyc}z.grid227.f${fhr}.grib2     

     rm -f $DATA/namnest_vis $DATA/namnest_cloud $DATA/namnest_t2m $DATA/namnest_dpt $DATA/vis $DATA/cloud $DATA/t2m $DATA/ref $DATA/apcp $DATA/dpt $DATA/etp

    done
   done

fi


if [ $modnam = href ] ; then

  yyyy=${vday:0:4}
  mm=${vday:4:2}
  dd=${vday:6:2}

  for cycle in 00 06 12 18 ; do
    #COMHREF=$(get_pdy_dir $vday $cycle)
    for fhr in 06 12 18 24 30; do
      for mbr in 01 02 03 04 05 06 07 08 ; do
        >$DATA/href.ens${mbr}.t${cycle}z.grid184.f${fhr}
#        href=$COMHREF/${fhr}/href.m${mbr}.t${cycle}z.f${fhr}
        href=${COMHREF}.${vday}/verf_g2g/href.m${mbr}.t${cycle}z.conus.f${fhr}

        $wgrib2 -match ":APCP:surface" $href |$wgrib2 -i $href -grib $DATA/apcp
        $copygb2 -g"20 6 0 0 0 0 0 0 1121 881 23117000 240977000 8 60000000 255000000 4763000 4763000 0 64" -i2,1 -x $DATA/apcp $DATA/href.ens${mbr}.t${cycle}z.apcp.f${fhr}

        #HREFv2 grid resolution has been changed
        
        $wgrib2 -match ":REFC:entire atmosphere" $href |$wgrib2 -i $href -grib $DATA/refc
        $copygb2 -g"30 6 0 0 0 0 0 0 1473 1025 12190000 226541000 8 25000000 265000000 5079000 5079000 0 64 25000000 25000000 0 0" -i2,1 -x $DATA/refc $DATA/href.ens${mbr}.t${cycle}z.grid227.f${fhr}

        $wgrib2 -match ":RETOP" $href |$wgrib2 -i $href -grib $DATA/etop
        $copygb2 -g"30 6 0 0 0 0 0 0 1473 1025 12190000 226541000 8 25000000 265000000 5079000 5079000 0 64 25000000 25000000 0 0" -i2,1 -x $DATA/etop $DATA/etop.grid227
        cat $DATA/etop.grid227 >> $DATA/href.ens${mbr}.t${cycle}z.grid227.f${fhr}

        $wgrib2 -match ":VIS:surface" $href |$wgrib2 -i  $href -grib  $DATA/urma
        $copygb2 -g"30 6 0 0 0 0 0 0 2145 1377 20191999 238445999 8 25000000 265000000 2539703 2539703 0 64 25000000 25000000 -90000000 0" -i2,1 -x  $DATA/urma $DATA/href.ens${mbr}.t${cycle}z.grid184.f${fhr}

        $wgrib2 -match ":TCDC:entire atmosphere" $href |$wgrib2 -i  $href -grib  $DATA/urma
        $copygb2 -g"30 6 0 0 0 0 0 0 2145 1377 20191999 238445999 8 25000000 265000000 2539703 2539703 0 64 25000000 25000000 -90000000 0" -i2,1 -x  $DATA/urma urma.grid184 
        cat $DATA/urma.grid184 >> $DATA/href.ens${mbr}.t${cycle}z.grid184.f${fhr}
    
        $wgrib2 -match ":TMP:2 m" $href |$wgrib2 -i  $href -grib  $DATA/urma
        $copygb2 -g"30 6 0 0 0 0 0 0 2145 1377 20191999 238445999 8 25000000 265000000 2539703 2539703 0 64 25000000 25000000 -90000000 0" -i2,1 -x  $DATA/urma urma.grid184
        cat $DATA/urma.grid184 >> $DATA/href.ens${mbr}.t${cycle}z.grid184.f${fhr}

        $wgrib2 -match ":DPT:2 m" $href |$wgrib2 -i  $href -grib  $DATA/urma
        $copygb2 -g"30 6 0 0 0 0 0 0 2145 1377 20191999 238445999 8 25000000 265000000 2539703 2539703 0 64 25000000 25000000 -90000000 0" -i2,1 -x  $DATA/urma urma.grid184
        cat $DATA/urma.grid184 >> $DATA/href.ens${mbr}.t${cycle}z.grid184.f${fhr}
        rm -f $DATA/urma $DATA/urma.grid184

        echo "227 $yyyy $mm $dd $cycle $fhr href.ens${mbr}.t${cycle}z.grid227.f${fhr}"|$EXECverf_g2g/re-set-time
         mv href.ens${mbr}.t${cycle}z.grid227.f${fhr}.new $COMOUT/href.ens${mbr}.t${cycle}z.grid227.f${fhr}.grib2 
        echo "184 $yyyy $mm $dd $cycle $fhr href.ens${mbr}.t${cycle}z.grid184.f${fhr}"|$EXECverf_g2g/re-set-time
         mv href.ens${mbr}.t${cycle}z.grid184.f${fhr}.new $COMOUT/href.ens${mbr}.t${cycle}z.grid184.f${fhr}.grib2 
        echo "255 $yyyy $mm $dd $cycle $fhr href.ens${mbr}.t${cycle}z.apcp.f${fhr}"|$EXECverf_g2g/re-set-time
         mv href.ens${mbr}.t${cycle}z.apcp.f${fhr}.new $COMOUT/href.ens${mbr}.t${cycle}z.apcp.f${fhr}.grib2
      done

       #cp $COMHREF/${fhr}/filename $COMOUT/filename.t${cycle}z.f${fhr}
 
    done
  done 

fi
