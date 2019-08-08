#!/bin/ksh
#############################################################################
# script: verf_g2g_get_ensemble.sh $model
#         to get global and NAM analysis data, climatology data and member data  
# Author: Binbin Zhou /EMC/NCEP
#         Dec 18, 2014
#############################################################################
set -x

modnam=$1

export vday=${vday:-$PDYm2}    #for ensemble, use past-2 day as validation day
export vdate=${vdate:-$vday$cyc}


export copygb2=${copygb2:-/nwprod/util/exec/copygb2}
export cnvgrib=${cnvgrib:-/nwprod/util/exec/cnvgrib}
export wgrib2=${wgrib2:-/nwprod/util/exec/wgrib2}

#############################################################
#1:Get gfs analysis grib2 data in GRID#3 (1-degree global)
############################################################
if [ $modnam = gfsanl ]; then

  echo $modnam is print here ...............

    COMGFSANL=${COMGFSANL:-/com2/gfs/prod/gfs}

  for cyc in 00 06 12 18 ; do

    gfsanl=$COMGFSANL.$vday/gfs.t${cyc}z.pgrbf00.grib2
    if [ -s $gfsanl ] ; then 
      cp $gfsanl $COMOUT/gfs.t${cyc}z.pgrbf00.grib2
    else 
      $cnvgrib -g12 $COMGFSANL.$vday/gfs.t${cyc}z.pgrbf00 $COMOUT/gfsanl.t${cyc}z.grd3.f00.grib2
    fi

  done
fi

##########################################################
#2:Get climatology grib2 data in GRID#3 (1-degree global) 
##########################################################
if [ $modnam = gfsclim ] ; then

   echo $modnam is print here ...............
    
   FIXGFSCLIM=${FIXGFSCLIM:-/nwprod/naefs.v4.0.1/fix}
   mm=`echo $vday|cut -c5-6`
   dd=`echo $vday|cut -c7-8`

    for head in cmean_1d cstdv_1d ; do

      if [ $head = 'cmean_1d' ] ; then
        ens='mean'
      else
        ens='stdv'
      fi
    
      for cyc in 00 06 12 18 ; do
        clim1d_grib2=$FIXGFSCLIM/${head}.1979${mm}${dd}
        $wgrib2 -match "1979$mm$dd$cyc" $clim1d_grib2|$wgrib2 -i $clim1d_grib2 -grib $COMOUT/clim.grid3.${ens}.${mm}${dd}${cyc}.grib2
      done
    done
fi

#############################################################
#3:Get ndas grib2 data in GRID#212 for sref verification
############################################################
if [ $modnam = ndas ]; then

   echo $modnam is print here ...............
   COMNDAS=${COMNDAS:-/com2/nam/prod/nam}
   
   DAY1=` /nwprod/util/exec/ndate +24 ${vday}09`
   tom=`echo ${DAY1} | cut -c 1-8`
   ln -sf  $COMNDAS.$tom/nam.t00z.awip3d00.tm03.grib2 $COMOUT/ndas.t21z.awip3d00.f00.grib2
   ln -sf $COMNDAS.$vday/nam.t06z.awip3d00.tm03.grib2 $COMOUT/ndas.t03z.awip3d00.f00.grib2
   ln -sf $COMNDAS.$vday/nam.t12z.awip3d00.tm03.grib2 $COMOUT/ndas.t09z.awip3d00.f00.grib2
   ln -sf $COMNDAS.$vday/nam.t18z.awip3d00.tm03.grib2 $COMOUT/ndas.t15z.awip3d00.f00.grib2
   for cyc in 03 09 15 21 ; do
        $copygb2 -g"30 6 0 0 0 0 0 0 185 129 12190000 226541000 8 25000000 265000000 40635000 40635000 0 64 25000000 25000000 0 0" -x $COMOUT/ndas.t${cyc}z.awip3d00.f00.grib2 $COMOUT/ndas.t${cyc}z.grd212.f00.grib2
   done

fi

#############################################################
#4:Get climatology grib2 data in GRID#212 for sref verification
############################################################

if [ $modnam = srefclim ] ; then

   echo $modnam is print here ...............

   #FIXGFSCLIM=/meso/noscrub/Binbin.Zhou/clim
   FIXGFSCLIM=${FIXGFSCLIM:-/nwprod/naefs.v4.0.1/fix}
   mm=`echo $vday|cut -c5-6`
   dd=`echo $vday|cut -c7-8`

    for head in cmean_1d cstdv_1d ; do

      if [ $head = 'cmean_1d' ] ; then
        ens='mean'
      else
        ens='stdv'
      fi

      #clim_grib2=$FIXGFSCLIM/${head}.1979${mm}${dd}.grid212.grib2
      clim_grib2=$FIXGFSCLIM/${head}.1979${mm}${dd}
      for cyc in 03 09 15 21 ; do
        $copygb2 -g"30 6 0 0 0 0 0 0 185 129 12190000 226541000 8 25000000 265000000 40635000 40635000 0 64 25000000 25000000 0 0" -x $clim_grib2 $COMOUT/clim.grid212.${ens}.${mm}${dd}${cyc}.grib2
        #$wgrib2 -match "1979$mm$dd$cyc" $clim_grib2|$wgrib2 -i $clim_grib2 -grib $COMOUT/clim.grid212.${ens}.${mm}${dd}${cyc}.grib2
      done
    done

fi

###########################################
#5:Get GFS 20 member grib2 file in grid3 
###########################################
if [ $modnam = gefs ] ; then
  COMGEFS=${COMGEFS:-/com2/gens/prod/gefs}
  total=20

  for dd in 0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 ; do

   hrs=`expr $dd \* 24`
   day=` /nwprod/util/exec/ndate -$hrs ${vday}09 |cut -c1-8`
   echo $day
   outdata=$COM_OUT/ens.$day

   if [ ! -h $outdata/gefs.ens20.t18z.grd3.f240 ] ; then
    mkdir -p $outdata

    for cyc in 00 06 12 18 ; do
  
      origin=$COMGEFS.$day/$cyc/pgrb2a  

      mbr=1
       while [ $mbr -le $total ] ; do
         mb=$mbr
         typeset -Z2 mb
         nfhrs=0

         while [ $nfhrs -le 384 ] ; do

           hh=$nfhrs

           if [ $nfhrs -le 100 ] ; then
             typeset -Z2 hh
           else
             typeset -Z3 hh
           fi
           if [ ! -h $outdata/gefs.ens${mb}.t${cyc}z.grd3.f${hh} ] ; then 
             ln -sf $origin/gep${mb}.t${cyc}z.pgrb2af${hh}  $outdata/gefs.ens${mb}.t${cyc}z.grd3.f${hh}
           fi 
           nfhrs=`expr $nfhrs + 6`
         done

         mbr=`expr $mbr + 1`
      done

    done   #for cyc

   fi 
  done    #for dd

fi




###########################################
#6:Get NAEFS 40 member grib2 file in grid3 
###########################################

if [ $modnam = naefs ] ; then
  COMNAEFS=${COMNAEFS:-/com2/gens/prod/cmce}
  total=40

  for dd in 0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 ; do

    hrs=`expr $dd \* 24`
    day=` /nwprod/util/exec/ndate -$hrs ${vday}09 |cut -c1-8`
    echo $day

   outdata=$COM_OUT/ens.$day
   if [ ! -e $outdata/naefs.cmc40.t18z.grd3.f240 ] ; then
    mkdir -p $outdata

    for cyc in 00 12 ; do

      origin=$COMNAEFS.$day/$cyc/pgrb2a

      mbr=21
       while [ $mbr -le $total ] ; do

         mb=$mbr
         typeset -Z2 mb
         mmb=`expr $mbr - 20`
         typeset -Z2 mmb 

         nfhrs=24
         while [ $nfhrs -le 384 ] ; do

           hh=$nfhrs

           if [ $nfhrs -le 100 ] ; then
             typeset -Z2 hh
           else
             typeset -Z3 hh
           fi
           if [ $hh = '020' ] ; then
             hh='120'
           fi

           naefs=$origin/cmc_gep${mmb}.t${cyc}z.pgrb2af${hh}

           if [ ! -s  $outdata/naefs.cmc${mb}.t${cyc}z.grd3.f${hh} ] ; then

             for var in  TMP UGRD VGRD HGT ; do
               for p in 500 700 850 ; do
                $wgrib2  $naefs|grep "${var}:${p} mb"|$wgrib2 -i $naefs -grib $DATA/output.${cyc}.f${hh}
                cat $DATA/output.${cyc}.f${hh} >> $outdata/naefs.cmc${mb}.t${cyc}z.grd3.f${hh}
               done
             done

             $wgrib2 $naefs|grep "TMP:2 m"|$wgrib2 -i $naefs -grib $DATA/output.${cyc}.f${hh}
             cat $DATA/output.${cyc}.f${hh} >> $outdata/naefs.cmc${mb}.t${cyc}z.grd3.f${hh}

             $wgrib2 $naefs|grep "UGRD:10 m above ground"|$wgrib2 -i $naefs -grib $DATA/output.${cyc}.f${hh}
             cat $DATA/output.${cyc}.f${hh} >> $outdata/naefs.cmc${mb}.t${cyc}z.grd3.f${hh}
             $wgrib2 $naefs|grep "VGRD:10 m above ground"|$wgrib2 -i $naefs -grib $DATA/output.${cyc}.f${hh}
             cat $DATA/output.${cyc}.f${hh} >> $outdata/naefs.cmc${mb}.t${cyc}z.grd3.f${hh}
           fi

           nfhrs=`expr $nfhrs + 24`

         done

         mbr=`expr $mbr + 1`
      done

    done
 fi

 done

fi

###########################################
#7:Get SREF 21 member grib2 file in GRID#212 
###########################################

if [ $modnam = sref ] ; then
  COMSREF=${COMSREF:-/com2/sref/prod/sref}
  total=26

mdl[1]=sref_arw
mbr[1]=ctl
mdl[2]=sref_arw
mbr[2]=p1
mdl[3]=sref_arw
mbr[3]=n1
mdl[4]=sref_arw
mbr[4]=p2
mdl[5]=sref_arw
mbr[5]=n2
mdl[6]=sref_arw
mbr[6]=p3
mdl[7]=sref_arw
mbr[7]=n3
mdl[8]=sref_arw
mbr[8]=p4
mdl[9]=sref_arw
mbr[9]=n4
mdl[10]=sref_arw
mbr[10]=p5
mdl[11]=sref_arw
mbr[11]=n5
mdl[12]=sref_arw
mbr[12]=p6
mdl[13]=sref_arw
mbr[13]=n6

mdl[14]=sref_nmb
mbr[14]=ctl
mdl[15]=sref_nmb
mbr[15]=p1
mdl[16]=sref_nmb
mbr[16]=n1
mdl[17]=sref_nmb
mbr[17]=p2
mdl[18]=sref_nmb
mbr[18]=n2
mdl[19]=sref_nmb
mbr[19]=p3
mdl[20]=sref_nmb
mbr[20]=n3
mdl[21]=sref_nmb
mbr[21]=p4
mdl[22]=sref_nmb
mbr[22]=n4
mdl[23]=sref_nmb
mbr[23]=p5
mdl[24]=sref_nmb
mbr[24]=n5
mdl[25]=sref_nmb
mbr[25]=p6
mdl[26]=sref_nmb
mbr[26]=n6


 for dd in 0 1 2 3 ; do

   hrs=`expr $dd \* 24`
   day=` /nwprod/util/exec/ndate -$hrs ${vday}09 |cut -c1-8`
   echo $day

   outdata=$COM_OUT/ens.$day
   if [ ! -h $outdata/sref.ens21.t21z.grd212.f87.grib2 ] ; then 
     mkdir -p $outdata

    for cyc in 03 09 15 21 ; do

     origin=$COMSREF.$day/$cyc/pgrb

      m=1
      while [ $m -le $total ] ; do

       mb=$m
       typeset -Z2 mb

       nfhrs=0
       while [ $nfhrs -le 87 ] ; do

         hh=$nfhrs
         typeset -Z2 hh
         if [ ! -h $outdata/sref.ens${mb}.t${cyc}z.grd212.f${hh}.grib2 ] ; then
           ln -sf $origin/${mdl[$mb]}.t${cyc}z.pgrb212.${mbr[$mb]}.f${hh}.grib2  $outdata/sref.ens${mb}.t${cyc}z.grd212.f${hh}.grib2
         fi 
         nfhrs=`expr $nfhrs + 6`

        done

        m=`expr $m + 1`
      done

    done

   fi

done

fi

###########################################
#8:Get CMCE 20 member grib2 file in grid3 
###########################################
if [ $modnam = cmce ] ; then
  COMCMCE=${COMCMCE:-/com2/gens/prod/cmce}
  total=20

  for dd in 0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 ; do

   hrs=`expr $dd \* 24`
   day=` /nwprod/util/exec/ndate -$hrs ${vday}09 |cut -c1-8`
   echo $day
   outdata=$COM_OUT/ens.$day

   if [ ! -e $outdata/cmce.ens20.t18z.grd3.f240 ] ; then
    mkdir -p $outdata

    for cyc in 00 06 12 18 ; do

      origin=$COMCMCE.$day/$cyc/pgrb2a

      mbr=1
       while [ $mbr -le $total ] ; do
         mb=$mbr
         typeset -Z2 mb

         nfhrs=24
         while [ $nfhrs -le 384 ] ; do

           hh=$nfhrs

           if [ $nfhrs -le 100 ] ; then
             typeset -Z2 hh
           else
             typeset -Z3 hh
           fi
           if [ $hh = '020' ] ; then
             hh='120'
           fi


           cmce=$origin/cmc_gep${mb}.t${cyc}z.pgrb2af${hh}

           if [ ! -s $outdata/cmce.ens${mb}.t${cyc}z.grd3.f${hh} ] ; then
             for var in  TMP UGRD VGRD HGT ; do
               for p in 500 700 850 ; do
                $wgrib2  $cmce|grep "${var}:${p} mb"|$wgrib2 -i $cmce -grib $DATA/output.${cyc}.f${hh}
                cat $DATA/output.${cyc}.f${hh} >> $outdata/cmce.ens${mb}.t${cyc}z.grd3.f${hh}
               done
             done

             $wgrib2 $cmce|grep "TMP:2 m"|$wgrib2 -i $cmce -grib $DATA/output.${cyc}.f${hh}
             cat $DATA/output.${cyc}.f${hh} >> $outdata/cmce.ens${mb}.t${cyc}z.grd3.f${hh}

             $wgrib2 $cmce|grep "UGRD:10 m above ground"|$wgrib2 -i $cmce -grib $DATA/output.${cyc}.f${hh}
             cat $DATA/output.${cyc}.f${hh} >> $outdata/cmce.ens${mb}.t${cyc}z.grd3.f${hh}
             $wgrib2 $cmce|grep "VGRD:10 m above ground"|$wgrib2 -i $cmce -grib $DATA/output.${cyc}.f${hh}
             cat $DATA/output.${cyc}.f${hh} >> $outdata/cmce.ens${mb}.t${cyc}z.grd3.f${hh}
           fi

           nfhrs=`expr $nfhrs + 24`
         done

         mbr=`expr $mbr + 1`
      done

    done   #for cyc

   fi
  done    #for dd

fi

