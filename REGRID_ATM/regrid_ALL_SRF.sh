#!/bin/bash

module load netcdf

tmpmodels=`more allmodels.txt`
allmodels=`echo $tmpmodels |sed -e "s/\:\:\:\:\:\:\:\:\:\:\:\:\:\: allmodels\.txt \:\:\:\:\:\:\:\:\:\:\:\:\:\://g"`
echo $allmodels

#for MODEL in $allmodels
#for MODEL in ACCESS1-0 ACCESS1-3 BNU-ESM CCSM4 CESM1-BGC CESM1-CAM5 CESM1-CAM5-1-FV2 CESM1-FASTCHEM CESM1-WACCM CMCC-CESM CMCC-CM CMCC-CMS CNRM-CM5 CSIRO-Mk3-6-0 CanESM2 EC-EARTH FGOALS-g2 FIO-ESM GFDL-CM3 GFDL-ESM2G GFDL-ESM2M HadCM3 HadGEM2-CC HadGEM2-ES IPSL-CM5A-LR IPSL-CM5A-MR IPSL-CM5B-LR MIROC-ESM MIROC-ESM-CHEM MIROC5 MPI-ESM-LR MPI-ESM-MR MRI-CGCM3 MRI-ESM1 NorESM1-M NorESM1-ME bcc-csm1-1 inmcm4
for MODEL in CanESM2
do

echo "#####################################################################################"
echo "#####################################################################################"

sed -e "s/MMMM/${MODEL}/g" regrid_GENERIC_SRF.f90 > regrid_${MODEL}.f90

ifort -c $NC_INC regrid_${MODEL}.f90
ifort -o regrid_${MODEL} regrid_${MODEL}.o $NC_LIB
./regrid_${MODEL}

NTEST=`ls -al *_${MODEL}_climo_ERAIgrid.nc | wc -l | sed -e "s/ //g"`
NTEST="0$NTEST"
echo "$NTEST files created"
if [ $NTEST -eq 0 ]; then
  echo "No climato found for model ${MODEL} >>>>> skip it !"
elif [ $NTEST -ne 12 ]; then ## 12 because 5 3D variables already done (7 otherwise)
  echo "Some variables are missing for model ${MODEL} (only $NTEST found) >>>> stop"
  exit
else
  rm -f regrid_${MODEL} regrid_${MODEL}.o regrid_${MODEL}.f90
fi

done
