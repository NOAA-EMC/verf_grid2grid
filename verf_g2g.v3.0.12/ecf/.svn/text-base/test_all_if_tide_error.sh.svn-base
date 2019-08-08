#!/bin/ksh

. /u/Binbin.Zhou/.kshrc


bsub < /u/Binbin.Zhou/work/grid2grid/verf_g2g.v3.0.11/ecf/jverf_grid2grid_cloud_00.ecf
bsub < /u/Binbin.Zhou/work/grid2grid/verf_g2g.v3.0.11/ecf/jverf_grid2grid_etop_00.ecf  
bsub < /u/Binbin.Zhou/work/grid2grid/verf_g2g.v3.0.11/ecf/jverf_grid2grid_reflectivity_00.ecf 
bsub < /u/Binbin.Zhou/work/grid2grid/verf_g2g.v3.0.11/ecf/jverf_grid2grid_urma_00.ecf
bsub < /u/Binbin.Zhou/work/grid2grid/verf_g2g.v3.0.11/ecf/jverf_grid2grid_firewx_00.ecf
bsub < /u/Binbin.Zhou/work/grid2grid/verf_g2g.v3.0.11/ecf/jverf_grid2grid_dust_00.ecf
bsub < /u/Binbin.Zhou/work/grid2grid/verf_g2g.v3.0.11/ecf/jverf_grid2grid_smoke_00.ecf
bsub < /u/Binbin.Zhou/work/grid2grid/verf_g2g.v3.0.11/ecf/jverf_grid2grid_ens_00.ecf
bsub < /u/Binbin.Zhou/work/grid2grid/verf_g2g.v3.0.10_has_ens2p5/ecf/jverf_grid2grid_ens2p5_00.ecf
/u/Binbin.Zhou/work/grid2grid/verf_g2g.v3.0.11/ecf/jverf_grid2grid_href_00.ecf 1>/u/Binbin.Zhou/work/grid2grid/verf_g2g.v3.0.11/ecf/a 2>/u/Binbin.Zhou/work/grid2grid/verf_g2g.v3.0.11/ecf/b


#just run gefs cloud
#bsub < /u/Binbin.Zhou/work/grid2grid/verf_g2g.v3.0.11/ecf/jverf_grid2grid_ens_00.ecf
bsub < /u/Binbin.Zhou/work/grid2grid/verf_g2g.v5.0.0/ecf/jverf_grid2grid_ens_00.ecf
bsub < /u/Binbin.Zhou/work/grid2grid/verf_g2g.v5.0.0/ecf/jverf_grid2grid_ensicing_00.ecf

###bsub < /u/Binbin.Zhou/work/grid2grid/verf_g2g.para/ecf/jverf_grid2grid_reflectivity_00.ecf
###bsub < /u/Binbin.Zhou/work/grid2grid/verf_g2g.para/ecf/jverf_grid2grid_urma_00.ecf
###bsub < /u/Binbin.Zhou/work/grid2grid/verf_g2g.para/ecf/jverf_grid2grid_etop_00.ecf
###bsub < /u/Binbin.Zhou/work/grid2grid/verf_g2g.para/ecf/jverf_grid2grid_cape_00.ecf

#/u/Binbin.Zhou/work/grid2grid/verf_g2g.hrefv2/ecf/jverf_grid2grid_href_00.ecf 1>/u/Binbin.Zhou/work/grid2grid/verf_g2g.hrefv2/ecf/a 2>/u/Binbin.Zhou/work/grid2grid/verf_g2g.hrefv2/ecf/b

bsub < /u/Binbin.Zhou/work/grid2grid/verf_g2g.aod/ecf/jverf_grid2grid_aod_00.ecf

/u/Binbin.Zhou/work/grid2grid/verf_g2g.met_href/ecf/jverf_grid2grid_ap3h.ecf 1>/u/Binbin.Zhou/work/grid2grid/verf_g2g.met_href/ecf/a 2>/u/Binbin.Zhou/work/grid2grid/verf_g2g.met_href/ecf/b 

/u/Binbin.Zhou/work/grid2grid/verf_g2g.met.nam.vis/ecf/jverf_grid2grid_vis_00.ecf 1>/u/Binbin.Zhou/work/grid2grid/verf_g2g.met.nam.vis/ecf/a 2>/u/Binbin.Zhou/work/grid2grid/verf_g2g.met.nam.vis/ecf/b

echo "Test begins" 

sleep 8000

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

