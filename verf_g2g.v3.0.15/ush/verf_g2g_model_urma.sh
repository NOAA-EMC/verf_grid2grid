#!/bin/ksh
#############################################################
#  verf_g2g_model_urma.sh: to run g2g urma package 
#  Author: Binbin Zhou/IMSG
#          Apr. 1, 2014     
#############################################################
#set -x

PAST1=$1
core=$2
model=$3
vgrid=$4

OBSVDIR=${OBSVDIR:-/com/verf/prod/urma}
FCSTDIR=${FCSTDIR:-/com/verf/prod/urma}

# following parameters are exported to prepg2g.sh ###############
# (1) for observation and forecaste file's names/directories (forecsat's are put
#     in following if block

export obsvdir=${obsvdir:-$OBSVDIR}
export ohead=urma2p5
export ogrbtype=grid${vgrid}.f
export otm=.grib2
export otail=00

#(2) tendency options
export tnd03='close'
export tnd06='close'
export tnd12='close'
export tnd24='close'

#(4) cloud base starting from where
export cloud_base_from_sfc="yes"

#(5) lat_weight="no"
###################################################################

if [ $model = 'SREFMEAN' ] ; then
 HH=03
 END=21
else
 HH=00
 END=18
fi

typeset -Z2 HH

while [ $HH -le $END ]   # loop for different validation time  

do

######Step 1:  generate user-defined control file for prepg2g.sh  ############# 

if [ $model = 'CONUSNMMB' ] || [ $model = 'CONUSARW' ] ; then

     export fcstdir=${fcstdir:-$FCSTDIR}
     export fhead=${core}
     export fgrbtype=grid${vgrid}.f
     export ftm=.grib2

     cp $PARMverf_g2g/verf_g2g_urma.hiresw .
     sed -e "s/MODNAM/$model/g" -e "s/VDATE/${PAST1}${HH}/g"  verf_g2g_urma.hiresw >user.ctl 

elif [ $model = 'NAM' ] ; then

  export fcstdir=${fcstdir:-$FCSTDIR}
  export fhead=nam
  export fgrbtype=grid${vgrid}.f
  export ftm=.grib2

  cp $PARMverf_g2g/verf_g2g_urma.nam .
  sed -e "s/MODNAM/$model/g" -e "s/VDATE/${PAST1}${HH}/g" -e "s/G184/G${vgrid}/g" verf_g2g_urma.nam >user.ctl 

elif [ $model = NAMNEST ] ; then

  export fcstdir=${fcstdir:-$FCSTDIR}
  export fhead=namnest
  export fgrbtype=grid${vgrid}.f
  export ftm=.grib2

  cp $PARMverf_g2g/verf_g2g_urma.nam .
  sed -e "s/MODNAM/$model/g" -e "s/VDATE/${PAST1}${HH}/g" -e "s/G184/G${vgrid}/g" verf_g2g_urma.nam >user.ctl 

elif [ $model = RAP ] ; then

  export fcstdir=${fcstdir:-$FCSTDIR}
  export fhead=rap
  export fgrbtype=grid${vgrid}.f
  export ftm=.grib2

  cp $PARMverf_g2g/verf_g2g_urma.rap .
  sed -e "s/MODNAM/$model/g" -e "s/VDATE/${PAST1}${HH}/g" -e "s/G184/G${vgrid}/g" verf_g2g_urma.rap >user.ctl 

elif [ $model = NARREMEAN ] ; then

  export fcstdir=${fcstdir:-$FCSTDIR}
  export fhead=narremean
  export fgrbtype=grid${vgrid}.f
  export ftm=.grib2

  cp $PARMverf_g2g/verf_g2g_urma.narremean .
  sed -e "s/MODNAM/$model/g" -e "s/VDATE/${PAST1}${HH}/g"  -e "s/G184/G${vgrid}/g" verf_g2g_urma.narremean >user.ctl


elif [ $model = HREFMEAN ] ; then

  export fcstdir=${fcstdir:-$FCSTDIR}
  export fhead=hrefmean
  export fgrbtype=grid${vgrid}.f
  export ftm=.grib2

  cp $PARMverf_g2g/verf_g2g_urma.hrefmean .
  sed -e "s/MODNAM/$model/g" -e "s/VDATE/${PAST1}${HH}/g"  -e "s/G184/G${vgrid}/g" verf_g2g_urma.hrefmean >user.ctl


elif [ $model = HRRR ] ; then
  export fcstdir=${fcstdir:-$FCSTDIR}
  export fhead=hrrr
  export fgrbtype=grid184.f
  export ftm=.grib2

  cp $PARMverf_g2g/verf_g2g_urma.hrrr .
  sed -e "s/MODNAM/$model/g" -e "s/VDATE/${PAST1}${HH}/g" verf_g2g_urma.hrrr >user.ctl

elif [ $model = GLMP ] ; then
  export fcstdir=${fcstdir:-$FCSTDIR}
  export fhead=glmp
  export fgrbtype=grid184.f
  export ftm=.grib2

  cp $PARMverf_g2g/verf_g2g_urma.glmp .
  sed -e "s/MODNAM/$model/g" -e "s/VDATE/${PAST1}${HH}/g" verf_g2g_urma.glmp >user.ctl


elif [ $model = SREFMEAN ] ; then

  export fcstdir=${fcstdir:-$FCSTDIR}
  export fhead=srefmean
  export fgrbtype=grid${vgrid}.f
  export ftm=.grib2

  cp $PARMverf_g2g/verf_g2g_urma.srefmean .
  sed -e "s/MODNAM/$model/g" -e "s/VDATE/${PAST1}${HH}/g" -e "s/G184/G${vgrid}/g" verf_g2g_urma.srefmean >user.ctl

else

  echo 'Wrong' $model
  exit

fi 

echo "model-done" $model

#### Step 2: call prepg2g.sh to prepare both forecast and observation GRIB files

$USHverf_g2g/verf_g2g_prepg2g_grib2.sh < user.ctl >output.prepg2g.$model

echo "prepg2g.sh done for "${PAST1}${HH} 

# Step 3: call run_g2g.sh to arange forecast and observation GRIB files and then
#         call g2g executable to generate SL1L2 and FHO VSDB files

$USHverf_g2g/verf_g2g_fitsg2g_grib2.sh<temp

echo "verf_g2g_fitsg2g_grib2.sh done for " ${PAST1}${HH}
 
HH=`expr $HH + 6`
done

exit

