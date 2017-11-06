#!/bin/bash
#PBS -P e14
#PBS -q express
#PBS -lsoftware=intel-fc
#PBS -l wd
#PBS -l ncpus=1,mem=64Gb,walltime=09:50:00

for file in CESM1-*_gridT.nc
do

ncap2 -O -s "vosaline=vosaline*1000.0" $file $file
echo "..."
ncatted -O -a units,vosaline,m,c,'1.e-3' $file $file
echo "$file [oK]"

done
