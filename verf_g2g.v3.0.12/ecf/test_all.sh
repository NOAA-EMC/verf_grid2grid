#!/bin/ksh


ecf=/gpfs/dell2/emc/verification/noscrub/Binbin.Zhou/grid2grid/verf_g2g.v3.0.12/ecf

$ecf/jverf_grid2grid_firewx_00.ecf 1>$ecf/firewx.1 2>$ecf/firewx.2

$ecf/jverf_grid2grid_href_00.ecf 1>$ecf/href.1 2>$ecf/href.2

$ecf/jverf_grid2grid_reflectivity_00.ecf 1>$ecf/reflectivity.1 2>$ecf/reflectivity.2 

$ecf/jverf_grid2grid_urma_00.ecf 1>$ecf/urma.1 2>$ecf/urma.2

$ecf/jverf_grid2grid_ens_00.ecf 1>$ecf/ens.1 2>$ecf/ens.2

$ecf/jverf_grid2grid_dust_00.ecf 1>$ecf/dust.1 2>$ecf/dust.2
exit

$ecf/jverf_grid2grid_cloud_00.ecf 1>$ecf/cloud.1 2>$ecf/cloud.2

$ecf/jverf_grid2grid_etop_00.ecf 1>$ecf/etop.1 2>$ecf/etop.2

exit

