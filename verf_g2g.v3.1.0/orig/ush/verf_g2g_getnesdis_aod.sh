#!/bin/sh
###############################################################
# Script Name: verf_g2g_getnesdis_aod.sh
# Purpose:  To ingest the NESIDS AOD data
#
# Log History:   Original author: Jianping Huang
#                2011-10-03:   Julia Zhu   
# Usage:  verf_g2g_getnesdis_aod.sh $data_type $vday
###############################################################
set -x

data_type=$1
vday=$2
 
### Set the Begin and End date for searching of the Nesdis and CMAQ data
nbeg=${nbeg:-11}
nend=${nend:-23}
cbeg=${cbeg:-1}
cend=${cend:-48}

COMNESDIS=${COMNESDIS:-/dcom/us007003/$vday/wgrdbul/smoke}
COMCMAQ=${COMCMAQ:-/com2/aqm/prod/aqm.$vday}

case $data_type in
  goes) INDIR=$COMNESDIS
        OUTDIR=$COMOUT;;
  cmaq) INDIR=$COMCMAQ
        OUTDIR=$COMOUT;;
esac

if [ $data_type = goes ]; then
  let cpflag=0
  let t=$nbeg
  while [ ${t} -le ${nend} ]; do
    if [[ ${cpflag} -ne 1 ]]; then
      if [[ -e ${INDIR}/G13.$vday${t}15.all.aod_conc.NAM3.grd ]]; then
         cp ${INDIR}/G13.$vday${t}15.all.aod_conc.NAM3.grd .
         $EXECverf_g2g/verf_g2g_adjust.nesdis.index $vday $t >adjust.nesdis.out 2>&1
         export err=$?; err_chk

         if [ $SENDCOM = YES ]; then
           cp G13.t${t}z.f00 $OUTDIR/.
           cp X13.t${t}z.f00 $OUTDIR/.
         fi
      else
         echo "NESDIS GOES data is not available in /dcom"
         echo "Skipping..."
      fi
    fi
    let t=t+1
  done

elif [ $data_type = cmaq ]; then
  let cpflag=0
  let t=$cbeg
  while [[ ${t} -le ${cend} ]]; do
    if [[ ${t} -le 9 ]]; then
      HH="0"$t
    else
      HH=$t
    fi
    if [[ -e ${INDIR}/aqm.t06z.aot${HH} ]]; then
       let cpflag=1
  #     break
    fi
    let t=t+1
  done

  if [[ ${cpflag} -eq 1 ]]; then
    cp $INDIR/aqm.t06z.aot* .
    $EXECverf_g2g/verf_g2g_adjust.cmaq.aod $vday >adjust.cmaq.out 2>&1
    export err=$?; err_chk

    if [ $SENDCOM = YES ]; then
       cp aod.t06z.* $OUTDIR/.
    fi
  else
    echo "No CMAQ data available for $vday"
    err_exit
  fi
fi
