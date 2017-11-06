#!/bin/bash
#PBS -P e14
#PBS -q express
#PBS -lsoftware=intel-fc
#PBS -l wd
#PBS -l ncpus=1,mem=64Gb,walltime=04:50:00

module load netcdf

export NC_INC='-I /apps/netcdf/4.2.1.1/include'
export NC_LIB='-L /apps/netcdf/4.2.1.1/lib -lnetcdf -lnetcdff'

rm -f calculate_mean_ALL_CMIP5_OCE calculate_mean_ALL_CMIP5_OCE.o
ifort -c $NC_INC calculate_mean_ALL_CMIP5_OCE.f90
ifort -o calculate_mean_ALL_CMIP5_OCE calculate_mean_ALL_CMIP5_OCE.o $NC_LIB
./calculate_mean_ALL_CMIP5_OCE

echo " "
echo "[End]"
