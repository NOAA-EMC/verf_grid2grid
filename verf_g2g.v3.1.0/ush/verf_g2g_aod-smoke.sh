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
let tbeg=11
let tend=23


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
if [ $vmodel = cmaq ]; then
  export fcstdir=$COM_OUT/aod
  export obsvdir=$COM_OUT/aod
  export fhead=aod
  export ohead=G13
  export fgrbtype=f
  export vtype=cmaq
  export nest=""
  export odata=nesdis
elif [ $vmodel = hysplit ]; then
  export fcstdir=$COM_OUT/smoke
  export obsvdir=$COM_OUT/smoke
  export vtype=smoke
  case $nest in
    conus) export fhead=aod-smoke-cs
##           export ohead=G13-grib227
#grib2           export ohead=G13-grib255
           export ohead=G13
           export odata=smoke-cs
           export fgrbtype=f  
           ;;
    ak)    export fhead=aod-smoke-ak
           export ohead=GW
           export odata=smoke-ak
           export fgrbtype=f    #for NAM-218 12km
           ;;
    hi)    export fhead=aod-smoke-hi
           export ohead=GWHI
           export odata=smoke-hi
           export fgrbtype=f    #for NAM-218 12km
           ;;
  esac
fi

export ogrbtype=
export ftm=
export otm=
export otail=f00

export tnd03='close'
export tnd06='close'
export tnd12='close'
export tnd24='close'
export cloud_base_from_sfc="no"
export lat_weight="no"

##
## smoke made only 1 prediciton per day and at 06 Z
## therefore, forecast time alway need to count back to 06 Z
## deom observed time (t)
## VT1 means the number of hour to be dial back for nearest day of forecast
## VT2 means the number of hour to be dialed back for previous day of VT1 (+24)


let V10=6

let t=tbeg

while [[ ${t} -le ${tend} ]]; do

  if [[ $t -le 9 ]]; then
    HH="0"$t
  else
    HH=$t
  fi 


  if [[ ${t} -ge 11 && ${t} -le 23 ]]; then  ## make sure VT1 >=0
    if [[ -e $obsvdir.${vday}/${ohead}.t${t}z.f00 ]]; then

      echo $HH
      let VT1=t-V10
      let VT2=VT1+24

      cp $PARMverf_g2g/verf_g2g_${vtype}${nest} .
      sed -e "s/VT1/$VT1/g" -e "s/VT2/$VT2/g" -e "s/VTIME/$vday$HH/g" verf_g2g_${vtype}${nest} >user.ctl

      $USHverf_g2g/verf_g2g_prepg2g_grib2.sh < user.ctl >output_prepg2g.${vtype}${nest}
      $USHverf_g2g/verf_g2g_fitsg2g_grib2.sh <temp
    fi
 
  else
    echo $t"Z of of range, VT1 and VT2 not defined, Skip VSDB"
  fi

  let t=t+1

done


#Combile all vsdb files winthin one days into one vasb file  change names of files

while read LINE
do
 set -A x $LINE
 model=${x[0]}
 ymdh=${x[1]}
done < temp

ymd=`echo ${ymdh} | cut -c 1-8`

workfile=${model}_${ymd}00.vsdb
##savefile=${vmodel}.${odata}_${ymd}.vsdb
savefile=${vmodel}_${ymd}.vsdb

if [ -e $workfile ]; then
  rm -f $workfile
fi

let hh=tbeg
while [[ ${hh} -le ${tend} ]]; do
  if [[ -e ${model}_${ymd}${hh}.vsdb ]]; then
    echo "cat "${model}_${ymd}${hh}."vsdb"
    cat ${model}_${ymd}${hh}.vsdb >> $workfile
  fi
  let hh=hh+1
done

##mv -f $workfile $savefile

cat $workfile >> $savefile

##
## move vsdb data to tempest for FVS processing
##
if [ $SENDCOM = YES ]; then
##  cp $savefile $COMVSDB/${RUN}/.
    cat $savefile >> $COMVSDB/${RUN}/$savefile

fi
rm -f usr.ctl

exit
