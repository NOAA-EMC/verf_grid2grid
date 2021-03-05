#!/bin/ksh
#############################################################
#  verf_g2g_ensemble.sh: to run g2g ensemble package 
#  Author: Binbin Zhou/IMSG
#          Dec 12, 2014     
#############################################################
set -x

VDAY=$1
model=$2
export var=$3
ref=$4
MM=`echo ${VDAY} | cut -c 5-6`
DD=`echo ${VDAY} | cut -c 7-8`

OBSVDIR=${OBSVDIR:-/com/verf/prod/href}
FCSTDIR=${FCSTDIR:-/com/verf/prod/href}

# following parameters are exported to prepg2g.sh ###############
# (1) for observation and forecaste file's names/directories (forecsat's are put
#     in following if block

export obsvdir=${obsvdir:-$OBSVDIR}
export fcstdir=${fcstdir:-$FCSTDIR}

#(2) tendency options
export tnd03='close'
export tnd06='close'
export tnd12='close'
export tnd24='close'

#(4) cloud base starting from where
export cloud_base_from_sfc="no"

#(5) lat_weight="no"
###################################################################

if [ $var = 'vis' ] || [ $var = 't2m' ] || [ $var = 'cld' ] || [ $var = 'dpt' ] ; then
  grid=184
  for m in 01 02 03 04 05 06 07 08 09 10 11 ; do
     MODEL[${m}]=href.ens${m}
  done
  HH=00
  END=18
  #HH=18
  #END=18
  NMODEL=8
  export ohead=urma2p5
  export ogrbtype=grid184.f00.grib2
  export otm=
  export otail=
  export fgrbtype=grid184.f
  export ftm=.grib2

elif [ $var = 'pc3' ] ; then

  grid=255
  for m in 01 02 03 04 05 06 07 08 09 10 11 ; do
     MODEL[${m}]=href.ens${m}
  done 
  HH=06
  END=18

  NMODEL=8
  export ohead=ccpa
  export ogrbtype=g255.f00
  export otm=
  export otail=
  export fgrbtype=apcp.f
  export ftm=.grib2

elif [ $var = 'ref' ] || [ $var = 'etp' ] ; then

  grid=227

  for m in 01 02 03 04 05 06 07 08 09 10 11 ; do
     MODEL[${m}]=href.ens${m}
  done

  HH=00
  END=18
  #HH=18
  #END=18

  NMODEL=8

  export ohead=refd3d
  export ogrbtype=grid227.f00
  export otm=
  export otail=

  export fgrbtype=grid227.f
  export ftm=.grib2



fi

typeset -Z2 HH

while [ $HH -le $END ] ; do  # loop for different validation time  
   cp $PARMverf_g2g/verf_g2g_href.$var .
   rm -f g2g.ctl.ensemble

   m=1
   while [ $m -le $NMODEL ] ; do
     export fhead=${MODEL[$m]} 
     sed -e "s/MODNAM/${MODEL[$m]}/g" -e "s/VDATE/$vday$HH/g" verf_g2g_href.$var > user.ctl.${MODEL[$m]} 

     $USHverf_g2g/verf_g2g_prepg2g_grib2.sh < user.ctl.${MODEL[$m]} > output.prepg2g.${MODEL[$m]}

     cat g2g.ctl.${MODEL[$m]} >> g2g.ctl.ensemble

     echo "prepg2g_grib2.sh done for " ${MODEL[$m]}

     m=`expr $m + 1`
   done 

   $USHverf_g2g/cp_single_Ref_exref.sh $ref $grid <g2g.ctl.ensemble

   $USHverf_g2g/verf_g2g_fitsg2g_grib2.sh<temp

   echo "verf_g2g_fitsg2g_grib2.sh done for " ${VDAY}${HH}

   HH=`expr $HH + 6`

done

exit

