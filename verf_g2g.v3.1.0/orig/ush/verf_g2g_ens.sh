#!/bin/ksh
#############################################################
#  verf_g2g_ensemble.sh: to run g2g ensemble package 
#  Author: Binbin Zhou/IMSG
#          Dec 12, 2014     
#############################################################
#set -x

PAST1=$1
core=$2
model=$3
MM=`echo ${PAST1} | cut -c 5-6`
DD=`echo ${PAST1} | cut -c 7-8`

OBSVDIR=${OBSVDIR:-/com/verf/prod/ens}
FCSTDIR=${FCSTDIR:-/com/verf/prod/ens}

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

if [ $model = 'GEFS' ] ; then
   for m in 01 02 03 04 05 06 07 08 09 10 11 12 13 14 15 16 17 18 19 20 ; do
     MODEL[${m}]=gefs.ens${m}
   done

   HH=00
   END=18
   NMODEL=20

   export ohead=gfsanl
   export ogrbtype=grd3.f00.grib2
   export otm=
   export otail=

   export fgrbtype=grd3.f
   export ftm=
  

   for hh in 00 06 12 18 ; do   # hh same as validation time
     #ln -s $obsvdir.$vday/clim.grid3.mean.${MM}${DD}${hh}.grib2  clim.mean.$MM$DD$hh
     #ln -s $obsvdir.$vday/clim.grid3.stdv.${MM}${DD}${hh}.grib2  clim.stdv.$MM$DD$hh
     cp $obsvdir.$vday/clim.grid3.mean.${MM}${DD}${hh}.grib2  clim.mean.$MM$DD$hh
     cp $obsvdir.$vday/clim.grid3.stdv.${MM}${DD}${hh}.grib2  clim.stdv.$MM$DD$hh
   done  


elif [ $model = 'CMCE' ] ; then
   for m in 01 02 03 04 05 06 07 08 09 10 11 12 13 14 15 16 17 18 19 20 ; do
     MODEL[${m}]=cmce.ens${m}
   done

   HH=00
   END=12
   NMODEL=20

   export ohead=gfsanl
   export ogrbtype=grd3.f00.grib2
   export otm=
   export otail=

   export fgrbtype=grd3.f
   export ftm=


   for hh in 00 12 ; do   # hh same as validation time
     #ln -s $obsvdir.$vday/clim.grid3.mean.${MM}${DD}${hh}.grib2  clim.mean.$MM$DD$hh
     #ln -s $obsvdir.$vday/clim.grid3.stdv.${MM}${DD}${hh}.grib2  clim.stdv.$MM$DD$hh
      cp $obsvdir.$vday/clim.grid3.mean.${MM}${DD}${hh}.grib2  clim.mean.$MM$DD$hh
      cp $obsvdir.$vday/clim.grid3.stdv.${MM}${DD}${hh}.grib2  clim.stdv.$MM$DD$hh
   done


elif [ $model = 'NAEFS' ] ; then

  for m in 01 02 03 04 05 06 07 08 09 10 11 12 13 14 15 16 17 18 19 20 ; do
     MODEL[${m}]=gefs.ens${m}
  done
  for m in 21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40 ; do
     MODEL[${m}]=naefs.cmc${m}
  done

   HH=00
   END=12
   NMODEL=40

   export ohead=gfsanl
   export ogrbtype=grd3.f00.grib2
   export otm=
   export otail=

   export fgrbtype=grd3.f
   export ftm=

   for hh in 00 12 ; do   # hh same as validation time
     #ln -s $obsvdir.$vday/clim.grid3.mean.${MM}${DD}${hh}.grib2  clim.mean.$MM$DD$hh
     #ln -s $obsvdir.$vday/clim.grid3.stdv.${MM}${DD}${hh}.grib2  clim.stdv.$MM$DD$hh
     cp $obsvdir.$vday/clim.grid3.mean.${MM}${DD}${hh}.grib2  clim.mean.$MM$DD$hh
     cp $obsvdir.$vday/clim.grid3.stdv.${MM}${DD}${hh}.grib2  clim.stdv.$MM$DD$hh
   done

elif [ $model = 'SREF' ] ; then

  for m in 01 02 03 04 05 06 07 08 09 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 ; do
     MODEL[${m}]=sref.ens${m}
  done

  HH=03
  END=21

  NMODEL=26

   export ohead=ndas
   export ogrbtype=grd212.f00.grib2
   export otm=
   export otail=

   export fgrbtype=grd212.f
   export ftm=.grib2

   for hh in 03 09 15 21 ; do   # hh same as validation time
     #ln -s $obsvdir.$vday/clim.grid212.mean.${MM}${DD}${hh}.grib2  clim.mean.$MM$DD$hh
     #ln -s $obsvdir.$vday/clim.grid212.stdv.${MM}${DD}${hh}.grib2  clim.stdv.$MM$DD$hh
     cp $obsvdir.$vday/clim.grid212.mean.${MM}${DD}${hh}.grib2  clim.mean.$MM$DD$hh
     cp $obsvdir.$vday/clim.grid212.stdv.${MM}${DD}${hh}.grib2  clim.stdv.$MM$DD$hh
   done


fi

typeset -Z2 HH

while [ $HH -le $END ] ; do  # loop for different validation time  
   cp $PARMverf_g2g/verf_g2g_ens.$core .
   rm -f g2g.ctl.ensemble

   m=1
   while [ $m -le $NMODEL ] ; do
     export fhead=${MODEL[$m]} 
     sed -e "s/MODNAM/${MODEL[$m]}/g" -e "s/VDATE/$vday$HH/g" verf_g2g_ens.$core > user.ctl.${MODEL[$m]} 

     $USHverf_g2g/verf_g2g_prepg2g_grib2.sh < user.ctl.${MODEL[$m]} > output.prepg2g.${MODEL[$m]}

     cat g2g.ctl.${MODEL[$m]} >> g2g.ctl.ensemble

     echo "prepg2g_grib2.sh done for " ${MODEL[$m]}

     m=`expr $m + 1`
   done 

   $USHverf_g2g/verf_g2g_fitsg2g_grib2.sh<temp

   echo "verf_g2g_fitsg2g_grib2.sh done for " ${PAST1}${HH}

  if [ $model = 'NAEFS' ] || [ $model = 'CMCE' ] ; then
   HH=`expr $HH + 12`
  else  
   HH=`expr $HH + 6`
  fi

done

exit

