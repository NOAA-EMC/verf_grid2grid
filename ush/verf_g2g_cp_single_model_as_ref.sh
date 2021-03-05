#!/bin/ksh
#read g2g.ctl.ensemble to get forecast time: yyyy,mm,dd,cyc, and lead time(ff)

 read LINE
  echo $LINE
 read LINE
  echo $LINE
 read LINE
  echo $LINE
 set -A tfcst $LINE
 t=1
 while [ $t -le ${tfcst[0]} ] ; do
  read LINE
  set -A time $LINE
  #echo $LINE
  yyyy=${time[0]:0:4}
    mm=${time[0]:4:2}
    dd=${time[0]:6:2}
   cyc=${time[0]:8:2}
    ff=${time[0]:10}
    #ff=`echo $} | cut -c 11-12`

    cp ${FCSTDIR}.$yyyy$mm$dd/clim.mean.$yyyy$mm$dd$cyc$ff   $DATA/clim.mean.$yyyy$mm$dd$cyc$ff
    cp ${FCSTDIR}.$yyyy$mm$dd/clim.sprd.$yyyy$mm$dd$cyc$ff   $DATA/clim.sprd.$yyyy$mm$dd$cyc$ff

  t=`expr $t + 1` 
 done 
exit

