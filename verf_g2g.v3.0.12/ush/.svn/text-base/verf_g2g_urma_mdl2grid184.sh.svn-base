#!/bin/ksh
#############################################################################
# script: verf_g2g_mdl2wrf_grid.sh $model
#         to convert NAM and URMA grids to grid#184 (2.5km) 
# Author: Binbin Zhou /SAIC
#         March 25, 2010
#############################################################################
set -x

modnam=$1
vgrid=$2

export vday=${vday:-$PDYm1}
export vdate=${vdate:-$vday$cyc}


export copygb=${copygb:-/nwprod/util/exec/copygb}
export copygb2=${copygb2:-/nwprod/util/exec/copygb2}
export cnvgrib=${cnvgrib:-/nwprod/util/exec/cnvgrib}
export wgrib2=${wgrib2:-/nwprod/util/exec/wgrib2}

############################################
#1:Get convert mosaic to Hires-WRF grid#227 
#############################################
if [ $modnam = urma ]; then

  echo $modnam is print here ...............

  if [ $vgrid = '184' ] ; then
    COMURMA=${COMURMA:-/com2/urma/prod/urma2p5}
  fi
  if [ $vgrid = '91' ] ; then
    COMURMA=/com2/rtma/prod/akrtma       #Alaska URMA not available, use RTMA instead
  fi

  for cyc in 00 03 06 09 12 15 18 21 ; do

     if [ $vgrid = '184' ] ; then
      urma=$COMURMA.$vday/urma2p5.t${cyc}z.2dvaranl_ndfd.grb2
     fi

     if [ $vgrid = '91' ] ; then
      urma=$COMURMA.$vday/akrtma.t${cyc}z.2dvaranl_ndfd_3p0.grb2
     fi

     cp $urma $COMOUT/urma2p5.t${cyc}z.grid${vgrid}.f00.grib2
     echo 'get urma2p5 grib2 ' $cyc ' done!'

     if [ $vgrid = '184' ] ; then   # over CONUS, use HRRR's 0hr as ceiling truth
     
       COMHRRR=${COMHRRR:-/com2/hrrr/prod/hrrr}

       file=$COMHRRR.$vday/hrrr.t${cyc}z.wrfsfcf00.grib2
       #$wgrib2 -match ":HGT:cloud ceiling" $file |$wgrib2 -i  $file -grib  $DATA/temp.${cyc}.00
       #$copygb2 -g"30 6 0 0 0 0 0 0 2145 1377 20191999 238445999 8 25000000 265000000 2539703 2539703 0 64 25000000 25000000 -90000000 0" -i2,1 -x  $DATA/temp.${cyc}.00 ceil.${cyc}.00
       #cat ceil.${cyc}.00 >> $COMOUT/urma2p5.t${cyc}z.grid${vgrid}.f00.grib2 
       #rm -f temp.* ceil.* 
       #adjust HRRR's 0hr ceiling height 
       ln -sf $file ceiling.t${cyc}z.hrrr.grib2
       echo ceiling.t${cyc}z.hrrr.grib2|${EXECverf_g2g}/verf_g2g_ceiling_adjust
       $copygb2 -g"30 6 0 0 0 0 0 0 2145 1377 20191999 238445999 8 25000000 265000000 2539703 2539703 0 64 25000000 25000000 -90000000 0" -i2,1 -x ceiling.t${cyc}z.hrrr.grib2.adjusted ceiling.t${cyc}z.grid${vgrid}.f00.grib2 
       cat ceiling.t${cyc}z.grid${vgrid}.f00.grib2 >> $COMOUT/urma2p5.t${cyc}z.grid${vgrid}.f00.grib2
       rm -f ceiling* 

     fi 

      

  done
fi

########################################
#2:Get convert NAM to 2.5km grid184 or 3km Alaska grid#91
########################################
if [ $modnam = nam ] || [ $modnam = namnest ] ; then
  COMNAM=${COMNAM:-/com2/nam/prod/nam}
  cycles="00 06 12 18"
  for cyc in  $cycles ; do
    for fhr in 03 06 09 12 15 18 21 24 27 30 33 36 ; do
    
      if [ $modnam = nam ] ; then
       #file=$COMNAM.$vday/nam.t${cyc}z.awphys${fhr}.grb2.tm00  # awphys has no HGTceil data
       file=$COMNAM.$vday/nam.t${cyc}z.awip12${fhr}.tm00.grib2     
        if [ $vgrid = '91' ] ; then
          file=$COMNAM.$vday/nam.t${cyc}z.awak3d${fhr}.grb2.tm00
        fi
      else
       file=$COMNAM.$vday/nam.t${cyc}z.conusnest.hiresf${fhr}.tm00.grib2
         if [ $vgrid = '91' ] ; then
          file=$COMNAM.$vday/nam.t${cyc}z.alaskanest.hiresf${fhr}.tm00.grib2
         fi
      fi

      $wgrib2 -match ":VIS:surface" $file |$wgrib2 -i  $file -grib  $DATA/vis.${cyc}.${fhr}
      $wgrib2 -match ":HGT:cloud ceiling" $file |$wgrib2 -i  $file -grib  $DATA/ceil.${cyc}.${fhr}
      cat $DATA/ceil.${cyc}.${fhr} >> $DATA/temp.${cyc}.${fhr}
      cat $DATA/vis.${cyc}.${fhr} >> $DATA/temp.${cyc}.${fhr}

      if [ $vgrid = '184' ] ; then
       $copygb2 -g"30 6 0 0 0 0 0 0 2145 1377 20191999 238445999 8 25000000 265000000 2539703 2539703 0 64 25000000 25000000 -90000000 0" -i2,1 -x  $DATA/temp.${cyc}.${fhr} $COMOUT/${modnam}.t${cyc}z.grid${vgrid}.f${fhr}.grib2
      else 
         $copygb2 -g"20 6 0 0 0 0 0 0 1649 1105 40530101 181429000 8 60000000 210000000 2976563 2976563 0 64" -i2,1 -x  $DATA/temp.${cyc}.${fhr} $COMOUT/${modnam}.t${cyc}z.grid${vgrid}.f${fhr}.grib2
      fi
       
      echo 'copygb2 ' $modnam $cyc $fhr ' done!'
    done
  done
  rm -f  $DATA/temp* $DATA/vis.* $DATA/ceil.*
fi


#############################################################
#2:Get convert 5km Hiresw WRF ARW and NMMB to 2.5km grid184
#############################################################

if [ $modnam = conusarw ] || [ $modnam = conusnmmb ] ; then   # HIRESW data
  modnam_new=`echo $modnam |sed -e s/conus//g`
  COMHIRESW=${COMHIRESW:-/gpfs/hps/nco/ops/com/hiresw/prod/hiresw}
  # Only runs at 00/12Z
  for cyc in 00 12 ; do 
    for fhr in 03 06 09 12 15 18 21 24 27 30 33 36 ; do 
      file=$COMHIRESW.$vday/hiresw.t${cyc}z.${modnam_new}_5km.f${fhr}.conus.grib2 
      $wgrib2 -match ":VIS:surface" $file |$wgrib2 -i  $file -grib  $DATA/vis.${cyc}.${fhr}
      cat $DATA/vis.${cyc}.${fhr} >> $DATA/temp.${cyc}.${fhr}

      $copygb2 -g"30 6 0 0 0 0 0 0 2145 1377 20191999 238445999 8 25000000 265000000 2539703 2539703 0 64 25000000 25000000 -90000000 0" -i2,1 -x  $DATA/temp.${cyc}.${fhr} $COMOUT/${modnam}.t${cyc}z.grid184.f${fhr}.grib2
      echo 'copygb2 ' $modnam $cyc $fhr ' done!'
    done
  done
  rm -f $DATA/temp* $DATA/vis.*
fi

#############################################################
#3:Get convert 13km RAP  to 2.5km grid184
#############################################################

if [ $modnam = rap ] ; then    
  COMRAP=${COMRAP:-/com2/rap/prod/rap}
  # Only runs at 00/12Z
  for cyc in 00 03 06 09 12 15 18 21 ; do
    for fhr in 03 06 09 12 15 18 ; do
      file=$COMRAP.$vday/rap.t${cyc}z.awp130pgrbf${fhr}.grib2
      if [ $vgrid = '91' ] ; then
       file=$COMRAP.$vday/rap.t${cyc}z.awp242f${fhr}.grib2
      fi

      $wgrib2 -match ":VIS:surface" $file |$wgrib2 -i  $file -grib  $DATA/vis.${cyc}.${fhr}
      cat $DATA/vis.${cyc}.${fhr} >> $DATA/temp.${cyc}.${fhr}

      if [ $vgrid = '184' ] ; then
       $copygb2 -g"30 6 0 0 0 0 0 0 2145 1377 20191999 238445999 8 25000000 265000000 2539703 2539703 0 64 25000000 25000000 -90000000 0" -i2,1 -x  $DATA/temp.${cyc}.${fhr} $COMOUT/${modnam}.t${cyc}z.grid${vgrid}.f${fhr}.grib2
      else
         $copygb2 -g"20 6 0 0 0 0 0 0 1649 1105 40530101 181429000 8 60000000 210000000 2976563 2976563 0 64" -i2,1 -x  $DATA/temp.${cyc}.${fhr} $COMOUT/${modnam}.t${cyc}z.grid${vgrid}.f${fhr}.grib2
      fi

      echo 'copygb2 ' $modnam $cyc $fhr ' done!'
    done
  done
  rm -f $DATA/temp*  $DATA/vis.*

fi

#############################################################
#4:Get convert 13km NARRE-Mean  to 2.5km grid184
#############################################################

if [ $modnam = narremean ] ; then
  COMNARRE=${COMNARRE:-/com2/rap/prod/narre}
  for cyc in 00 03 06 09 12 15 18 21 ; do
    for fhr in 03 06 09 12 ; do
      file=$COMNARRE.$vday/ensprod/narre.t${cyc}z.mean.grd130.f${fhr}.grib2
      if [ $vgrid = '91' ] ; then
        file=$COMNARRE.$vday/ensprod/narre.t${cyc}z.mean.grd242.f${fhr}.grib2
      fi

      $wgrib2 -match ":VIS:surface" $file |$wgrib2 -i  $file -grib  $DATA/vis.${cyc}.${fhr}
      cat $DATA/vis.${cyc}.${fhr} >> $DATA/temp.${cyc}.${fhr}

      if [ $vgrid = '184' ] ; then
       $copygb2 -g"30 6 0 0 0 0 0 0 2145 1377 20191999 238445999 8 25000000 265000000 2539703 2539703 0 64 25000000 25000000 -90000000 0" -i2,1 -x  $DATA/temp.${cyc}.${fhr} $COMOUT/${modnam}.t${cyc}z.grid${vgrid}.f${fhr}.grib2
      else
         $copygb2 -g"20 6 0 0 0 0 0 0 1649 1105 40530101 181429000 8 60000000 210000000 2976563 2976563 0 64" -i2,1 -x  $DATA/temp.${cyc}.${fhr} $COMOUT/${modnam}.t${cyc}z.grid${vgrid}.f${fhr}.grib2
      fi

      echo 'copygb2 ' $modnam $cyc $fhr ' done!'
    done
  done
  rm -f $DATA/temp*  $DATA/vis.*

fi




#############################################################
#5:Get convert 3km HRRR  to 2.5km grid184
#############################################################
if [ $modnam = hrrr ] ; then
 if [ $vgrid = '184' ] ; then
  COMHRRR=${COMHRRR:-/com2/hrrr/prod/hrrr}
  for cyc in 00 03 06 09 12 15 18 21 ; do
    for fhr in 03 06 09 12 15 ; do
      file=$COMHRRR.$vday/hrrr.t${cyc}z.wrfsfcf${fhr}.grib2

      $wgrib2 -match ":VIS:surface" $file |$wgrib2 -i  $file -grib  $DATA/vis.${cyc}.${fhr}
      #$wgrib2 -match ":HGT:cloud ceiling" $file |$wgrib2 -i  $file -grib  $DATA/ceil.${cyc}.${fhr}
      cat $DATA/vis.${cyc}.${fhr} >> $DATA/temp.${cyc}.${fhr}
      #cat $DATA/ceil.${cyc}.${fhr} >> $DATA/temp.${cyc}.${fhr}

      ln -sf $file hrrr.${cyc}.${fhr}.ceiling
      echo hrrr.${cyc}.${fhr}.ceiling|${EXECverf_g2g}/verf_g2g_ceiling_adjust
      cat hrrr.${cyc}.${fhr}.ceiling.adjusted >> $DATA/temp.${cyc}.${fhr}     


      $copygb2 -g"30 6 0 0 0 0 0 0 2145 1377 20191999 238445999 8 25000000 265000000 2539703 2539703 0 64 25000000 25000000 -90000000 0" -i2,1 -x  $DATA/temp.${cyc}.${fhr} $COMOUT/${modnam}.t${cyc}z.grid${vgrid}.f${fhr}.grib2

      echo 'copygb2 ' $modnam $cyc $fhr ' done!'
    done
  done
  rm -f $DATA/temp*  $DATA/vis.* hrrr.*.ceiling
 
 fi 
fi


#############################################################
#6:Get convert GLAMP  to 2.5km grid184
#############################################################
if [ $modnam = glmp ] ; then
 if [ $vgrid = '184' ] ; then
  COMGLMP=${COMGLMP:-/com2/glmp/prod/glmp}
  for cyc in 00 03 06 09 12 15 18 21 ; do

    file=$COMGLMP.$vday/glmp_fcsts_vis.co.gb2.g.t${cyc}z

    for fhr in 3 6 9 12 15 18 21 24 ; do
      if [ fhr -lt 10 ] ; then
       hh=0$fhr
      else
       hh=$fhr
      fi

      $wgrib2 -match ":VIS:surface:$fhr hour " $file |$wgrib2 -i  $file -grib  $DATA/glmp.${cyc}.${hh}

      $copygb2 -g"30 6 0 0 0 0 0 0 2145 1377 20191999 238445999 8 25000000 265000000 2539703 2539703 0 64 25000000 25000000 -90000000 0" -i2,1 -x  $DATA/glmp.${cyc}.${hh} $COMOUT/${modnam}.t${cyc}z.grid${vgrid}.f${hh}.grib2

      echo 'copygb2 ' $modnam $cyc $fhr ' done!'

    done
  done


 fi
fi


#############################################################
#7:Get convert 16km  SREFMEAN  to 2.5km grid184 or grid91 over Alasks
#############################################################
if [ $modnam = srefmean ] ; then
  COMSREF=${COMSREF:-/com2/sref/prod/sref}
  for cyc in 03 09 15 21 ; do
   for t in  3 6 9 12 15 18 21 24 27 30 33 36 ; do

       if [ $vgrid = '184' ] ; then
         bigfile=$COMSREF.$vday/$cyc/ensprod/sref.t${cyc}z.pgrb132.mean_3hrly.grib2
       else
         bigfile=$COMSREF.$vday/$cyc/ensprod/sref.t${cyc}z.pgrb216.mean_3hrly.grib2
       fi

       if [ $t -le 9 ] ; then
        fhr="0"$t
       else
        fhr=$t
       fi

       $wgrib2 ${bigfile} |grep ":${t} hour fcst:" |$wgrib2 -i ${bigfile} -grib $DATA/sref.${cyc}.${fhr}
       $wgrib2 -match ":VIS:surface" $DATA/sref.${cyc}.${fhr}|$wgrib2 -i $DATA/sref.${cyc}.${fhr} -grib  $DATA/vis.${cyc}.${fhr}
       $wgrib2 -match ":HGT:cloud ceiling" $DATA/sref.${cyc}.${fhr}|$wgrib2 -i $DATA/sref.${cyc}.${fhr} -grib  $DATA/ceil.${cyc}.${fhr}
       cat $DATA/vis.${cyc}.${fhr} >> $DATA/temp.${cyc}.${fhr}
       cat $DATA/ceil.${cyc}.${fhr} >> $DATA/temp.${cyc}.${fhr}

       if [ $vgrid = '184' ] ; then   
         $copygb2 -g"30 6 0 0 0 0 0 0 2145 1377 20191999 238445999 8 25000000 265000000 2539703 2539703 0 64 25000000 25000000 -90000000 0" -i2,1 -x $DATA/temp.${cyc}.${fhr} $COMOUT/srefmean.t${cyc}z.grid${vgrid}.f${fhr}.grib2
       else
         $copygb2 -g"20 6 0 0 0 0 0 0 1649 1105 40530101 181429000 8 60000000 210000000 2976563 2976563 0 64" -i2,1 -x $DATA/temp.${cyc}.${fhr} $COMOUT/srefmean.t${cyc}z.grid${vgrid}.f${fhr}.grib2
       fi
   done
  done

  rm -f $DATA/temp*  $DATA/vis.* $DATA/ceil.* $DATA/sref.*

fi


#############################################################
# For probability~event ensemble verifications
#############################################################

if [ $modnam = href ] ; then   # HREF prob data
  COMHREF=${COMHREF:-/com2/hiresw/prod/href}
  for cyc in 00 06 12 18 ; do 
    for fhr in 03 06 09 12 15 18 21 24 27 30 33 36 ; do 
      file=$COMHREF.$vday/href.t${cyc}z.prob.f${fhr}.grib2
      $wgrib2 -match ":VIS:surface" $file |$wgrib2 -i  $file -grib  $DATA/vis.${cyc}.${fhr}
      cat $DATA/vis.${cyc}.${fhr} >> $DATA/temp.${cyc}.${fhr}

      $copygb2 -g"30 6 0 0 0 0 0 0 2145 1377 20191999 238445999 8 25000000 265000000 2539703 2539703 0 64 25000000 25000000 -90000000 0" -i2,1 -x  $DATA/temp.${cyc}.${fhr} $COMOUT/${modnam}.t${cyc}z.grid184.f${fhr}.grib2
      echo 'copygb2 ' $modnam $cyc $fhr ' done!'
    done
  done
  rm -f $DATA/temp* $DATA/vis.*
fi


if [ $modnam = hrefmean ] ; then   # HREF Mean data
 COMHREF=${COMHREF:-/com2/hiresw/prod/href}
 for cyc in 00 06 12 18 ; do
    for fhr in 03 06 09 12 15 18 21 24 27 30 33 36 ; do
      file=$COMHREF.$vday/ensprod/href.t${cyc}z.conus.mean.f${fhr}.grib2
      $wgrib2 -match ":VIS:surface" $file |$wgrib2 -i  $file -grib  $DATA/vis.${cyc}.${fhr}
      cat $DATA/vis.${cyc}.${fhr} >> $DATA/temp.${cyc}.${fhr}

      $copygb2 -g"30 6 0 0 0 0 0 0 2145 1377 20191999 238445999 8 25000000 265000000 2539703 2539703 0 64 25000000 25000000 -90000000 0" -i2,1 -x  $DATA/temp.${cyc}.${fhr} $COMOUT/${modnam}.t${cyc}z.grid184.f${fhr}.grib2
      echo 'copygb2 ' $modnam $cyc $fhr ' done!'
    done
  done
  rm -f $DATA/temp* $DATA/vis.*
fi


#############################################################
#4:Get convert 13km NARRE-Mean  to 2.5km grid184
#############################################################

if [ $modnam = narre ] ; then
  COMNARRE=${COMNARRE:-/com2/rap/prod/narre}
  for cyc in 00 03 06 09 12 15 18 21 ; do
    for fhr in 03 06 09 12 ; do
      file=$COMNARRE.$vday/ensprod/narre.t${cyc}z.prob.grd130.f${fhr}.grib2
      if [ $vgrid = '91' ] ; then
        file=$COMNARRE.$vday/ensprod/narre.t${cyc}z.prob.grd242.f${fhr}.grib2
      fi

      $wgrib2 -match ":VIS:surface" $file |$wgrib2 -i  $file -grib  $DATA/vis.${cyc}.${fhr}
      cat $DATA/vis.${cyc}.${fhr} >> $DATA/temp.${cyc}.${fhr}

      if [ $vgrid = '184' ] ; then
       $copygb2 -g"30 6 0 0 0 0 0 0 2145 1377 20191999 238445999 8 25000000 265000000 2539703 2539703 0 64 25000000 25000000 -90000000 0" -i2,1 -x  $DATA/temp.${cyc}.${fhr} $COMOUT/${modnam}.t${cyc}z.grid${vgrid}.f${fhr}.grib2
      else
         $copygb2 -g"20 6 0 0 0 0 0 0 1649 1105 40530101 181429000 8 60000000 210000000 2976563 2976563 0 64" -i2,1 -x  $DATA/temp.${cyc}.${fhr} $COMOUT/${modnam}.t${cyc}z.grid${vgrid}.f${fhr}.grib2
      fi

      echo 'copygb2 ' $modnam $cyc $fhr ' done!'
    done
  done
  rm -f $DATA/temp*  $DATA/vis.*

fi



#############################################################
#7:Get convert 16km  SREFMEAN  to 2.5km grid184 or grid91 over Alasks
#############################################################
if [ $modnam = sref ] ; then
  COMSREF=${COMSREF:-/com2/sref/prod/sref}
  for cyc in 03 09 15 21 ; do
   for t in  6 12 18 24 30 36 ; do

       if [ $vgrid = '184' ] ; then
         bigfile=$COMSREF.$vday/$cyc/ensprod/sref.t${cyc}z.pgrb132.prob_3hrly.grib2
       else
         bigfile=$COMSREF.$vday/$cyc/ensprod/sref.t${cyc}z.pgrb216.prob_3hrly.grib2
       fi

       if [ $t -le 9 ] ; then
        fhr="0"$t
       else
        fhr=$t
       fi

       $wgrib2 ${bigfile} |grep ":${t} hour fcst:" |$wgrib2 -i ${bigfile} -grib $DATA/sref.${cyc}.${fhr}
       $wgrib2 -match ":VIS:surface" $DATA/sref.${cyc}.${fhr}|$wgrib2 -i $DATA/sref.${cyc}.${fhr} -grib  $DATA/vis.${cyc}.${fhr}
       #$wgrib2 -match ":HGT:cloud ceiling" $DATA/sref.${cyc}.${fhr}|$wgrib2 -i $DATA/sref.${cyc}.${fhr} -grib  $DATA/ceil.${cyc}.${fhr}
       cat $DATA/vis.${cyc}.${fhr} >> $DATA/temp.${cyc}.${fhr}
       #cat $DATA/ceil.${cyc}.${fhr} >> $DATA/temp.${cyc}.${fhr}

       if [ $vgrid = '184' ] ; then   
         $copygb2 -g"30 6 0 0 0 0 0 0 2145 1377 20191999 238445999 8 25000000 265000000 2539703 2539703 0 64 25000000 25000000 -90000000 0" -i2,1 -x $DATA/temp.${cyc}.${fhr} $COMOUT/sref.t${cyc}z.grid${vgrid}.f${fhr}.grib2
       else
         $copygb2 -g"20 6 0 0 0 0 0 0 1649 1105 40530101 181429000 8 60000000 210000000 2976563 2976563 0 64" -i2,1 -x $DATA/temp.${cyc}.${fhr} $COMOUT/sref.t${cyc}z.grid${vgrid}.f${fhr}.grib2
       fi
   done
  done

  rm -f $DATA/temp*  $DATA/vis.* $DATA/ceil.* $DATA/sref.*

fi

exit

