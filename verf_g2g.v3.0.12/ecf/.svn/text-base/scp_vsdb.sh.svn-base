#!/bin/ksh

. /u/Binbin.Zhou/.kshrc

dev=`cat /etc/dev`
if [ $dev = 'gyre' ] ; then

YYYY=`date +%Y`
TODAY=`date +%Y%m%d`
#TODAY=$1

H=09
DAY1=` /nwprod/util/exec/ndate -24 $TODAY$H`
PAST1=`echo ${DAY1} | cut -c 1-8`
DAY2=` /nwprod/util/exec/ndate -48 $TODAY$H`
PAST2=`echo ${DAY2} | cut -c 1-8`

sftp wd20bz@emc-lw-bzhou.ncep.noaa.gov << EOF

cd /export/emc-lw-bzhou/wd20bz/g2g/vsdb/vsdb/href
lcd /ptmpp2/Binbin.Zhou/g2g/RFC2/com/verf/dev/vsdb/grid2grid/hrefv2
#mput *${PAST1}*.vsdb
mput *.vsdb

cd /export/emc-lw-bzhou/wd20bz/g2g/vsdb/vsdb/cloud
lcd /ptmpp2/Binbin.Zhou/g2g/RFC2/com/verf/dev/vsdb/grid2grid/cloud
#mput *${PAST2}*.vsdb
mput *.vsdb

cd /export/emc-lw-bzhou/wd20bz/g2g/vsdb/vsdb/ens
#lcd /com/verf/prod/vsdb/grid2grid/ens
lcd /ptmpp2/Binbin.Zhou/g2g/RFC2/com/verf/dev/vsdb/grid2grid/ens
#mput *${PAST2}*.vsdb
mput *.vsdb

cd /export/emc-lw-bzhou/wd20bz/g2g/vsdb/vsdb/etop
lcd /ptmpp2/Binbin.Zhou/g2g/RFC2/com/verf/dev/vsdb/grid2grid/etop
#mput *${PAST1}*.vsdb
mput *.vsdb

cd /export/emc-lw-bzhou/wd20bz/g2g/vsdb/vsdb/firewx
lcd /ptmpp2/Binbin.Zhou/g2g/RFC2/com/verf/dev/vsdb/grid2grid/firewx
#mput *${PAST2}*.vsdb
mput *.vsdb

cd /export/emc-lw-bzhou/wd20bz/g2g/vsdb/vsdb/reflt
lcd /ptmpp2/Binbin.Zhou/g2g/RFC2/com/verf/dev/vsdb/grid2grid/reflt
#mput *${PAST1}*.vsdb
mput *.vsdb

cd /export/emc-lw-bzhou/wd20bz/g2g/vsdb/vsdb/urma
lcd /ptmpp2/Binbin.Zhou/g2g/RFC2/com/verf/dev/vsdb/grid2grid/urma
#mput *${PAST1}*.vsdb
mput *.vsdb

#cd /export/emc-lw-bzhou/wd20bz/g2g/vsdb/vsdb/para/vsdb/cape
#lcd /ptmpp2/Binbin.Zhou/g2g/RFC2/com/verf/dev/vsdb/grid2grid/cape
#mput *${PAST1}*.vsdb
mput *.vsdb

cd /export/emc-lw-bzhou/wd20bz/g2g/vsdb/vsdb/enscloud
lcd /ptmpp2/Binbin.Zhou/g2g/RFC2/com/verf/dev/vsdb/grid2grid/ens
#mput *${PAST2}*.vsdb 
mput *.vsdb

cd /export/emc-lw-bzhou/wd20bz/g2g/vsdb/vsdb/ensicing
lcd /ptmpp2/Binbin.Zhou/g2g/RFC2/com/verf/dev/vsdb/grid2grid/ensicing
#mput *${PAST2}*.vsdb 
mput *.vsdb

EOF


else

YYYY=`date +%Y`
TODAY=`date +%Y%m%d`

H=09
DAY1=` /nwprod/util/exec/ndate -24 $TODAY$H`
PAST1=`echo ${DAY1} | cut -c 1-8`
DAY2=` /nwprod/util/exec/ndate -48 $TODAY$H`
PAST2=`echo ${DAY2} | cut -c 1-8`

sftp wd20bz@emc-lw-bzhou.ncep.noaa.gov << EOF

cd /export/emc-lw-bzhou/wd20bz/g2g/vsdb/vsdb/href
lcd /ptmpp2/Binbin.Zhou/g2g/RFC2/com/verf/prod/vsdb/grid2grid/href
#mput *${PAST1}*.vsdb
mput *{PAST1}*.vsdb

EOF

fi
