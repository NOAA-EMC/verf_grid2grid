#!/bin/ksh
#############################################################
#  verf_g2g_reflt_mosaic.sh: to run g2g reflectivity package 
#  Author: Binbin Zhou/IMSG
#          Apr. 1, 2014     
#  B ZHou Nov. 18, 2014, upgraded to grib2       
#############################################################
#set -x

PAST2=$1
core=$2
model=$3

echo "verf_g2g_firewx_mosaic.sh, PAST2=" $PAST2

OBSVDIR=${OBSVDIR:-/com/verf/prod/reflt}
FCSTDIR=${FCSTDIR:-/com/verf/prod/reflt}

# following parameters are exported to prepg2g.sh ###############
# (1) for observation and forecaste file's names/directories (forecsat's are put
#     in following if block

export obsvdir=${obsvdir:-$OBSVDIR}
export ohead=refd3d
export ogrbtype=firewx
export otm=
#export otail=00

#(2) tendency options
export tnd03='close'
export tnd06='close'
export tnd12='close'
export tnd24='close'

#(4) cloud base starting from where
export cloud_base_from_sfc="yes"

#(5) lat_weight="no"
###################################################################

if [ $model = 'SREFNMM' ] || [ $model = 'SREFARW' ] || [ $model = 'SREFNMMB' ] ; then   
 HH=03
 END=21
else
 HH=00
 END=18
fi

typeset -Z2 HH

while [ $HH -le $END ]   # loop for different validation time  

do

#####S#tep 1:  generate user-defined control file for prepg2g.sh  ############# 

export otail=.${PAST2}${HH}.grib2

ln -sf $DATA/firewx.kgds.${PAST2}${HH} fort.15

if [ $model = 'CONUSNMMB' ] || [ $model = 'CONUSARW' ] ; then

     export fcstdir=${fcstdir:-$FCSTDIR}
     export fhead=${core}
     export fgrbtype=awp5kmf
     export ftm=.grib2

     cp $PARMverf_g2g/verf_g2g_reflt.hiresw .
     sed -e "s/MODNAM/$model/g" -e "s/VDATE/${PAST2}${HH}/g"  verf_g2g_reflt.hiresw >user.ctl 

elif [ $model = NAMNEST ] ; then

  export fcstdir=${fcstdir:-$FCSTDIR}
  export fhead=namnest
  export fgrbtype=firewx.f
  export ftm=.grib2

  cp $PARMverf_g2g/verf_g2g_firewx.nam .
  sed -e "s/MODNAM/$model/g" -e "s/VDATE/${PAST2}${HH}/g" verf_g2g_firewx.nam >user.ctl 

elif [ $model = 'HRRR' ] ; then

  export fcstdir=${fcstdir:-$FCSTDIR}
  export fhead=hrrr
  export fgrbtype=firewx.f
  export ftm=.grib2

  cp $PARMverf_g2g/verf_g2g_firewx.hrrr .
  sed -e "s/MODNAM/$model/g" -e "s/VDATE/${PAST2}${HH}/g" verf_g2g_firewx.hrrr >user.ctl

elif [ $model = 'NAMFIREWX' ] ; then

  export fcstdir=$COMNAMFIREWX
  export fhead=nam
  export fgrbtype=firewxnest.hiresf
  export ftm=.tm00.grib2

  cp $PARMverf_g2g/verf_g2g_firewx.nam .
  sed -e "s/MODNAM/$model/g" -e "s/VDATE/${PAST2}${HH}/g" verf_g2g_firewx.nam >user.ctl
else

  echo 'Wrong' $model
  exit

fi 

echo "model-done" $model

#### Step 2: call prepg2g.sh to prepare both forecast and observation GRIB files

$USHverf_g2g/verf_g2g_prepg2g_grib2.sh < user.ctl >output.prepg2g.$model

echo "prepg2g.sh done for "${PAST2}${HH} 

# Step 3: call run_g2g.sh to arange forecast and observation GRIB files and then
#         call g2g executable to generate SL1L2 and FHO VSDB files

if [ -s fcst.grib.$model ] && [ -s obsv.grib.$model ] ; then
 $USHverf_g2g/verf_g2g_fitsg2g_grib2.sh<temp
 echo "verf_g2g_fitsg2g_grib2.sh done for " ${PAST2}${HH}
fi
 
HH=`expr $HH + 6`
done

exit

