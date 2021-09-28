#!/bin/bash

module load intel/19.1.3.304
module load craype
module load cray-mpich
module avail w3
module load wgrib2/2.0.8_wmo
module load python/3.8.6

export COMPATH=/lfs/h1/ops/canned/com/gefs

export COMGFS=${COMGFS:-$(compath.py gfs/v16.2)/gfs}
