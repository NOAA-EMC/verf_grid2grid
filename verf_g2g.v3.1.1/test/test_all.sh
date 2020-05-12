#!/bin/ksh

. /u/Binbin.Zhou/.kshrc


#bsub < /u/Binbin.Zhou/work/grid2grid/verf_g2g.v3.0.11/ecf/jverf_grid2grid_cloud_00.ecf
#bsub < /u/Binbin.Zhou/work/grid2grid/verf_g2g.v3.0.11/ecf/jverf_grid2grid_etop_00.ecf  
#bsub < /u/Binbin.Zhou/work/grid2grid/verf_g2g.v3.0.11/ecf/jverf_grid2grid_reflectivity_00.ecf 
#bsub < /u/Binbin.Zhou/work/grid2grid/verf_g2g.v3.0.11/ecf/jverf_grid2grid_urma_00.ecf
#bsub < /u/Binbin.Zhou/work/grid2grid/verf_g2g.v3.0.11/ecf/jverf_grid2grid_firewx_00.ecf
#bsub < /u/Binbin.Zhou/work/grid2grid/verf_g2g.v3.0.11/ecf/jverf_grid2grid_dust_00.ecf
#bsub < /u/Binbin.Zhou/work/grid2grid/verf_g2g.v3.0.11/ecf/jverf_grid2grid_smoke_00.ecf
#bsub < /u/Binbin.Zhou/work/grid2grid/verf_g2g.v3.0.11/ecf/jverf_grid2grid_ens_00.ecf
#bsub < /u/Binbin.Zhou/work/grid2grid/verf_g2g.v3.0.11/ecf/jverf_grid2grid_href_00.ecf

./jverf_grid2grid_cloud_00.ecf        1>cloud.1 2>cloud.2
echo "jverf_grid2grid_cloud_00.ecf done" 
./jverf_grid2grid_etop_00.ecf         1>etop.1 2>etop.2
echo "./jverf_grid2grid_etop_00.ecf done"
./jverf_grid2grid_reflectivity_00.ecf 1>ref.1 2>ref.2
echo "./jverf_grid2grid_reflectivity_00.ecf done"
./jverf_grid2grid_urma_00.ecf         1>urma.1 2>urma.2
echo "./jverf_grid2grid_urma_00.ecf  done"
./jverf_grid2grid_firewx_00.ecf       1>fire.1 2>fire.2
echo "./jverf_grid2grid_firewx_00.ecf done"
./jverf_grid2grid_dust_00.ecf         1>dust.1 2>dust.2
echo "./jverf_grid2grid_dust_00.ecf done"
./jverf_grid2grid_smoke_00.ecf        1>smoke.1 2>smoke.2
echo "./jverf_grid2grid_smoke_00.ecf done"
./jverf_grid2grid_href_00.ecf         1>href.1 2>href.2
echo "./jverf_grid2grid_href_00.ecf done"
./jverf_grid2grid_ens_00.ecf  20200416   1>ens.1 2>ens.2
echo "./jverf_grid2grid_ens_00.ecf done"
