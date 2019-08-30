#!/bin/ksh
#
# script     obtained from variable set in run_all.sh
# TODAY      obtained from variable set in run_all.sh
# aqm        obtained from variable set in run_all.sh
# nesdis     obtained from variable set in run_all.sh
# datadir    obtained from variable set in run_all.sh
# fhead      obtained from variable set in run_all.sh
#
set -x

vmodel=$1
nest=$2
vday=$3

########### following parameters are exported to prepg2g.sh #####################
#  forecast files at NCEP are in the following file naming, stored
#  following directoy naming format
#  forecast     directory:   .../model_name.YYYYMMDD, e.g., aod.20061205
#  observation  directory:   .../source.YYYYMMDD,     e.g., nesdis.20061205
#  forecast file name:   fhead.tHHz.fgrbtypeFF.ftm,   e.g., aod.t06z.f36
#                                                 (HH: cycle, FF: forecast time)
#  observation file name: ohead.tHHz.ogrbtype.otm.f00, G13.t12z.f00
################################################################################
OBSVDIR=${OBSVDIR:-/com/verf/prod/etop}

if [ $vmodel = hiresw ]; then
  export vcycles="00 06 12 18"
  case $nest in
    conusnmmb) MDL='CONUSNMMB'
               ;;
    conusarw)  MDL='CONUSARW'
               ;;
  esac

  export fcstdir=${COMHIRESW:-/com/hiresw/prod/hiresw}
  export obsvdir=${obsvdir:-$OBSVDIR}
  export fhead=hiresw
  export ohead=refd3d
  export fgrbtype=`echo $nest|sed -e s/conus//g`_5km.f
  export ogrbtype=grid227
  export ftm=.conus.grib2
  export otm=
  export otail=.f00
elif [ $vmodel = nam ]; then
  export vcycles="00 06 12 18"
  export MDL='NAMNEST'
  export fcstdir=${COMNAM:-/com/nam/prod/nam}
  export obsvdir=${obsvdir:-$OBSVDIR}
  export fhead=nam
  export ohead=refd3d
  export fgrbtype=conusnest.hiresf
  export ogrbtype=grid227
  export ftm=.tm00.grib2
  export otm=
  export otail=.f00
fi

#(3) tendency options
export tnd03='close'
export tnd06='close'
export tnd12='close'
export tnd24='close'

#(4) cloud base starting from where
export cloud_base_from_sfc="yes"

export lat_weight="no"

export workdir=$DATA
cd $workdir

# Loop for different validation time
for vcyc in $vcycles
do
  cp $PARMverf_g2g/verf_g2g_etop.${vmodel} .
  sed -e "s/MODNAM/$MDL/g" -e "s/VDATE/$vday$vcyc/g" verf_g2g_etop.${vmodel} >verf_g2g_etop.${vmodel}.$nest

  $USHverf_g2g/verf_g2g_prepg2g_grib2.sh < verf_g2g_etop.${vmodel}.$nest
  $USHverf_g2g/verf_g2g_fitsg2g_grib2.sh <$workdir/temp
done

# Concatenate the vsdb file
rm -rf ${nest}_${vday}.vsdb
for vcyc in $vcycles
do
  cat ${MDL}_${vday}${vcyc}.vsdb >>${nest}_${vday}.vsdb
done

if [ $SENDCOM = YES ]; then
  cp ${nest}_${vday}.vsdb $COMVSDB/etop/.
fi
