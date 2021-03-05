#!/bin/ksh
###################################################################
# script: run_mosaic_model.sh
#         To run grid-to-grid program for all of single models
#  Author: Binbin Zhou/SAIC
#         Apr. 5, 2010
#   2014-11-20 B.Zhou upgraded to grib2
###################################################################
set -x

models=$1

for model in $models
do
  MODEL=`echo $model | tr '[a-z]' '[A-Z]'`
  $USHverf_g2g/verf_g2g_reflt_mosaic.sh $vday $model $MODEL
   
  if [ `echo $model |cut -c1-4` = "sref" ]
  then 
    cycles="03 09 15 21"
  else
    cycles="00 06 12 18"
  fi

  if [ ! -d $COMVSDB/${RUN} ]; then
    mkdir -p $COMVSDB/${RUN}
  fi
  for ncyc in $cycles
  do
    cat ${MODEL}_${vday}${ncyc}.vsdb >> $COMVSDB/${RUN}/${model}_${vday}.vsdb
  done

  #rm -rf *${MODEL}*
done
