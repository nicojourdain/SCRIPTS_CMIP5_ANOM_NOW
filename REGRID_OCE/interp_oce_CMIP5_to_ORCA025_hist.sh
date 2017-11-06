#!/bin/bash
#PBS -P e14
#PBS -q normal
#PBS -lsoftware=intel-fc
#PBS -l wd
#PBS -l ncpus=1,mem=64Gb,walltime=47:50:00

module load netcdf
NC_INC='-I /apps/netcdf/4.2.1.1/include'
NC_LIB='-L /apps/netcdf/4.2.1.1/lib -lnetcdf -lnetcdff'


for MODEL in ACCESS1-0 ACCESS1-3 BNU-ESM CCSM4 CESM1-BGC CESM1-CAM5 CESM1-WACCM CMCC-CESM CMCC-CM CMCC-CMS CNRM-CM5 CSIRO-Mk3-6-0 CanESM2 EC-EARTH FGOALS-g2 FIO-ESM GFDL-CM3 GFDL-ESM2G GFDL-ESM2M HadGEM2-CC HadGEM2-ES IPSL-CM5A-LR IPSL-CM5A-MR IPSL-CM5B-LR MIROC-ESM MIROC-ESM-CHEM MIROC5 MPI-ESM-LR MPI-ESM-MR MRI-CGCM3 NorESM1-M NorESM1-ME bcc-csm1-1 inmcm4
do

echo " "
echo "########################################################################"
echo "## ${MODEL}"
echo " "

cp -p interp_oce_CMIP5_to_ORCA025_hist.f90 tjodu.f90

##-----
## test to see whether lon,lat variables are double or simple precision :
ncdump -h ../CLIMO_OCE/uo_${MODEL}_climo_1989_2009.nc | grep "double lon(" | sed -e "s/double //g ; s/ \;//g" > tmpp
DBLE=`cat tmpp | wc -c`
rm -f tmpp
if [ $DBLE -gt 1 ]; then
  DBLE=2
  sed -e "s/\!##DB /     /g" tjodu.f90 > tmpp
  echo "Double precision"
else
  DBLE=1
  sed -e "s/\!##FL /     /g" tjodu.f90 > tmpp
  echo "Simple precision"
fi
mv -f tmpp tjodu.f90

##-----
## test to see whether lon,lat are 1D or 2D variables :
if [ $DBLE == 2 ]; then
  ncdump -h ../CLIMO_OCE/uo_${MODEL}_climo_1989_2009.nc | grep "double lon(" | sed -e "s/double //g ; s/ \;//g ; s/ //g" > tmpp
elif [ $DBLE == 1 ]; then
  ncdump -h ../CLIMO_OCE/uo_${MODEL}_climo_1989_2009.nc | grep "float lon(" | sed -e "s/float //g ; s/ \;//g ; s/ //g" > tmpp
fi
LONDIM=`cat tmpp`
echo "LONDIM : $LONDIM"
rm -f tmpp
if [ $LONDIM == 'lon(lon)' ] || [ $LONDIM == 'lon(i)' ] || [ $LONDIM == 'lon(rlon)' ] ; then
  sed -e "s/\!##1D //g" tjodu.f90 > tmpp
else
  if [ $LONDIM == 'lon(j,i)' ] || [ $LONDIM == 'lon(i,j)' ] || [ $LONDIM == 'lon(rlat,rlon)' ] || [ $LONDIM == 'lon(rlon,rlat)' ]; then
    sed -e "s/\!##2D //g" tjodu.f90 > tmpp
  else
     echo "#@%^&*! Adapt sh script for this file : script not built for such dimensions"
     echo "   ===> STOP"
     exit
  fi
fi
mv -f tmpp tjodu.f90

##-----
sed -e "s/MMMM/${MODEL}/g" tjodu.f90 > tmpp
mv -f tmpp tjodu.f90

rm -f tjodu tjodu.o
ifort -c $NC_INC tjodu.f90
ifort -o tjodu tjodu.o $NC_LIB
time ./tjodu &> interp_oce_CMIP5_to_ORCA025_hist.log 

done
