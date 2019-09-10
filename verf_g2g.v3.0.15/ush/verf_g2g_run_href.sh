#!/bin/ksh
###################################################################
# script: 
#         To run grid-to-grid program for all of single models
#  Author: Binbin Zhou, Apr. 9, 2014
###################################################################
set -x

model=$1
var=$2
ref=namnest

MODEL=`echo $model | tr '[a-z]' '[A-Z]'`

  $USHverf_g2g/verf_g2g_href.sh $vday $model $var $ref
  
  cycles="00 06 12 18"

  if [ ! -d $COMVSDB/${RUN} ]; then
    mkdir -p $COMVSDB/${RUN}
  fi

  for ncyc in $cycles ; do
    cat ${MODEL}_${var}_${vday}${ncyc}.vsdb >> $COMVSDB/${RUN}/${model}_${vday}.vsdb
  done
  #sed -e "s!HREF!HREFv2!g" $COMVSDB/${RUN}/${model}_${vday}.vsdb > $COMVSDB/${RUN}/${model}v2_${vday}.vsdb
  #rm -f $COMVSDB/${RUN}/${model}_${vday}.vsdb
  #rm -rf *${MODEL}*
