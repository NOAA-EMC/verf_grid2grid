#!/bin/ksh

qsub jverf_grid2grid_cloud_00.ecf
qsub jverf_grid2grid_etop_00.ecf  
qsub jverf_grid2grid_reflectivity_00.ecf 
qsub jverf_grid2grid_urma_00.ecf
qsub jverf_grid2grid_firewx_00.ecf
qsub jverf_grid2grid_dust_00.ecf
qsub jverf_grid2grid_smoke_00.ecf
qsub jverf_grid2grid_ens_00.ecf
qsub jverf_grid2grid_href_00.ecf


