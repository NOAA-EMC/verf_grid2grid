#!/bin/ksh
##############################################################################
# Script Name: verf_g2g_cloud_afwa.sh
# Purpose:  This script processes the grid-to-grid verification of the model
#           output against the afwa input data
#
# History:  2011-11-18  Julia Zhu modified from Binbin Zhou's version to 
#                       prepare for production implementation
#           2014-11-15 B. Zhou upgraded to grib2
# Usage:  verf_g2g_cloud_afwa.sh vday model obsv
###############################################################################
set -x

PAST1=$1
model=$2
obsv=$3
domain=$4


OBSVDIR=${OBSVDIR:-/com/verf/prod/cloud}
FCSTDIR=${FCSTDIR:-/com/verf/prod/cloud}

#(1) Set parameters
# Except for NAM from operational /com/nam/prod, all others from
# $FCSTDIR where copygbed model files over grid212 are stored   

if [ $domain = 'CONUS' ] ; then 
vgrid=212

case $model in 
  nam)      export fcstdir=${COMNAM:-/com2/nam/prod/nam}
            export fhead=nam
            export fgrbtype=awip3d
            export ftm=.tm00.grib2
            export mdl=NAM
            ;;
  gfs)      export fcstdir=$FCSTDIR
            export fhead=gfs
            export fgrbtype=grd212.f
            export ftm=.grib2
            export mdl=GFS
            ;;
  conusarw) export fcstdir=$FCSTDIR
            export fhead=conusarw
            export fgrbtype=grd212.f
            export ftm=.grib2
            export mdl=CONUSARW
            ;;
 conusnmmb) export fcstdir=$FCSTDIR
            export fhead=conusnmmb
            export fgrbtype=grd212.f
            export ftm=.grib2
            export mdl=CONUSNMMB
            ;;
  rap)      export fcstdir=$FCSTDIR
            export fhead=rap
            export fgrbtype=grd212.f
            export ftm=.grib2
            export mdl=RAP
            ;;
  hrrr)     export fcstdir=$FCSTDIR
            export fhead=hrrr
            export fgrbtype=grd212.f
            export ftm=.grib2
            export mdl=HRRR
            ;;
  namnest)  export fcstdir=$FCSTDIR
            export fhead=namnest
            export fgrbtype=grd212.f
            export ftm=.grib2
            export mdl=NAMNEST
            ;;
esac

if [ $obsv = "afwa" ] ; then
 export obsvdir=$OBSVDIR
 export ohead=afwa
 export ogrbtype=grd212
 export otm=
 export otail=.f00.grib2
 export obsvdata=AFWA
elif [ $obsv = "clavr" ] ; then
 export obsvdir=$OBSVDIR
 export ohead=clavr
 export ogrbtype=grd212
 export otm=
 export otail=.f00.grib2
 export obsvdata=CLAVR
else
 echo $obsv " is wrong type of satellite data"
 exit
fi
 
#(2) tendency options
export tnd03='close'
export tnd06='close'
export tnd12='close'
export tnd24='close'

#(3) cloud base starting from where
export cloud_base_from_sfc="no"

#(4) lat_weight="no"

#(5) Prepare OBS and FCST input files and run grid2grid to generate VSDB files
for HH in 00 06 12 18
do
  if [ $model = gfs -o $model = nam ]; then
     cp $PARMverf_g2g/verf_g2g_cloud.${model} .
     sed -e "s/MODNAM/${mdl}_212/g" -e "s/VDATE/${PAST1}${HH}/g" \
         -e "s/OBSTYPE/$obsvdata/g" verf_g2g_cloud.${model} >user.ctl
  elif [ $model = conusarw -o $model = conusnmmb ]; then
     cp $PARMverf_g2g/verf_g2g_cloud.hiresw .
     sed -e "s/MODNAM/${mdl}_212/g" -e "s/VDATE/${PAST1}${HH}/g" \
         -e "s/OBSTYPE/$obsvdata/g" verf_g2g_cloud.hiresw >user.ctl
  elif [ $model = namnest ] ; then 
      cp $PARMverf_g2g/verf_g2g_cloud.nam .
       sed -e "s/MODNAM/${mdl}_212/g" -e "s/VDATE/${PAST1}${HH}/g" \
         -e "s/OBSTYPE/$obsvdata/g" verf_g2g_cloud.nam >user.ctl
  elif [ $model = rap -o $model = hrrr ] ; then
       cp $PARMverf_g2g/verf_g2g_cloud.rap .
       sed -e "s/MODNAM/${mdl}_212/g" -e "s/VDATE/${PAST1}${HH}/g" \
         -e "s/OBSTYPE/$obsvdata/g" verf_g2g_cloud.rap >user.ctl
  else
     echo "$model has not been included in the verification"
     exit
  fi
     
  $USHverf_g2g/verf_g2g_prepg2g_grib2.sh < user.ctl >output.prepg2g.${obsv}.${model}

  $USHverf_g2g/verf_g2g_fitsg2g_grib2.sh<temp

  echo "verf_g2g_ref.sh done for " ${PAST1}${HH} $vgrid
done

# Combine the vsdb files for each model
if [ ! -d $COMVSDB/cloud ]; then
  mkdir -p $COMVSDB/cloud
fi

rm -rf ${model}_${PAST1}.vsdb
MODEL=`echo $model | tr '[a-z]' '[A-Z]'`
for HH in 00 06 12 18
do
  cat ${MODEL}_${vgrid}_${PAST1}${HH}.vsdb >> $COMVSDB/cloud/${model}_${obsv}_${PAST1}.vsdb
done

#rm -rf *${MODEL}*.vsdb

else  # for Alaska domain grid#242:
vgrid=242
 case $model in
  nam)      export fcstdir=${COMNAM:-/com22/nam/prod/nam}
            export fhead=nam
            export fgrbtype=awak3d
            export ftm=.grb2.tm00
            export mdl=NAM
            ;;
  gfs)      export fcstdir=$FCSTDIR
            export fhead=gfs
            export fgrbtype=grd242.f
            export ftm=.grib2
            export mdl=GFS
            ;;
  rap)      export fcstdir=/com2/rap/prod/rap
            export fhead=rap
            export fgrbtype=awp242f
            export ftm=.grib2
            export mdl=RAP
            ;;
  namnest)  export fcstdir=$FCSTDIR
            export fhead=namnest
            export fgrbtype=grd242.f
            export ftm=.grib2
            export mdl=NAMNEST
            ;;
 esac

  if [ $obsv = "afwa" ] ; then
   export obsvdir=$OBSVDIR
   export ohead=afwa
   export ogrbtype=grd242
   export otm=
   export otail=.f00.grib2
   export obsvdata=AFWA
  elif [ $obsv = "clavr" ] ; then
   export obsvdir=$OBSVDIR
   export ohead=clavr
   export ogrbtype=grd242
   export otm=
   export otail=.f00.grib2
   export obsvdata=CLAVR
  else
   echo $obsv " is wrong type of satellite data"
  exit
 fi

# tendency options
export tnd03='close'
export tnd06='close'
export tnd12='close'
export tnd24='close'

# cloud base starting from where
export cloud_base_from_sfc="no"

# lat_weight="no"

# Prepare OBS and FCST input files and run grid2grid to generate VSDB files
for HH in 00 06 12 18
do
  if [ $model = gfs -o $model = nam ]; then
     cp $PARMverf_g2g/verf_g2g_cloud.${model} .
     sed -e "s/MODNAM/${mdl}_242/g" -e "s/VDATE/${PAST1}${HH}/g" \
         -e "s/OBSTYPE/$obsvdata/g" -e "s/G212/G242/g" verf_g2g_cloud.${model} >user.ctl
  elif [ $model = namnest ] ; then
      cp $PARMverf_g2g/verf_g2g_cloud.nam .
       sed -e "s/MODNAM/${mdl}_242/g" -e "s/VDATE/${PAST1}${HH}/g" \
         -e "s/OBSTYPE/$obsvdata/g" -e "s/G212/G242/g" verf_g2g_cloud.nam >user.ctl
  elif [ $model = rap ] ; then
       cp $PARMverf_g2g/verf_g2g_cloud.rap .
       sed -e "s/MODNAM/${mdl}_242/g" -e "s/VDATE/${PAST1}${HH}/g" \
         -e "s/OBSTYPE/$obsvdata/g" -e "s/G212/G242/g" verf_g2g_cloud.rap >user.ctl
  else
     echo "$model has not been included in the verification"
     exit
  fi

  $USHverf_g2g/verf_g2g_prepg2g_grib2.sh < user.ctl >output.prepg2g.${obsv}.${model}

  $USHverf_g2g/verf_g2g_fitsg2g_grib2.sh<temp

  echo "verf_g2g_ref.sh done for " ${PAST1}${HH}  $vgrid
done

# Combine the vsdb files for each model
if [ ! -d $COMVSDB/cloud ]; then
  mkdir -p $COMVSDB/cloud
fi

rm -rf ${model}_${PAST1}.vsdb
MODEL=`echo $model | tr '[a-z]' '[A-Z]'`
for HH in 00 06 12 18
do
  cat ${MODEL}_${vgrid}_${PAST1}${HH}.vsdb >> $COMVSDB/cloud/${model}_${obsv}_${PAST1}.vsdb
done

#rm -rf *${MODEL}*.vsdb


fi 




