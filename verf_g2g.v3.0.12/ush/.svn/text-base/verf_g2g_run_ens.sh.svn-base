#!/bin/ksh
###################################################################
# script: 
#         To run grid-to-grid program for all of single models
#  Author: Binbin Zhou, Apr. 9, 2014
###################################################################
set -x

models=$1

for model in $models
do
  MODEL=`echo $model | tr '[a-z]' '[A-Z]'`
  $USHverf_g2g/verf_g2g_ens.sh $vday $model $MODEL 
  
  if [ `echo $model |cut -c1-4` = "sref" ] ; then
    cycles="03 09 15 21"
  elif [ `echo $model |cut -c1-4` = "gefs" ] ; then
    cycles="00 06 12 18"
  elif [ `echo $model |cut -c1-4` = "cmce" ] ; then
    cycles="00 12"
  elif [ `echo $model |cut -c1-4` = "naef" ] ; then
    cycles="00 12 "
  else
    echo "Other system"
    exit
  fi

  if [ ! -d $COMVSDB/${RUN} ]; then
    mkdir -p $COMVSDB/${RUN}
  fi

  for ncyc in $cycles ; do
    cat ${MODEL}_${vday}${ncyc}.vsdb >> $COMVSDB/${RUN}/${model}_${vday}.vsdb
  done

  #rm -rf *${MODEL}*
done
