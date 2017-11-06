#!/bin/bash

for MODEL in ACCESS1-0 ACCESS1-3 BNU-ESM CCSM4 CESM1-BGC CESM1-CAM5 CESM1-WACCM CMCC-CESM CMCC-CM CMCC-CMS CNRM-CM5 CSIRO-Mk3-6-0 CanESM2 FGOALS-g2 FIO_ESM GFDL-CM3 GFDL-ESM2G GFDL-ESM2M HadGEM2-CC HadGEM2-ES IPSL-CM5A-LR IPSL-CM5A-MR IPSL-CM5B-LR MIROC-ESM MIROC-ESM-CHEM MIROC5 MPI-ESM-LR MPI-ESM-MR MRI-CGCM3 NorESM1-M NorESM1-ME bcc-csm1-1 inmcm4
do

for VAR in votemper vosaline vozocrtx vomecrty
do

if [ "$VAR" == "votemper" ]; then
  GRID="gridT"
elif [ "$VAR" == "vosaline" ]; then
  GRID="gridT"
elif [ "$VAR" == "vozocrtx" ]; then
  GRID="gridU"
elif [ "$VAR" == "vomecrty" ]; then
  GRID="gridV"
fi

echo "cancel data/all"                             >  toto.jnl
echo "use ${MODEL}_hist_1989-2009_${GRID}.nc"      >> toto.jnl
echo "use ${MODEL}_rcp85_2080-2100_${GRID}.nc"     >> toto.jnl
echo "shade ${VAR}[k=26,l=1,d=2]-${VAR}[k=26,l=1,d=1]"      >> toto.jnl
echo "frame/file=toto_${VAR}_${MODEL}.gif"         >> toto.jnl
echo "exit"                                        >> toto.jnl

ferret -gif < toto.jnl

if [ -f toto_${VAR}_${MODEL}.gif ]; then
  echo "toto_${VAR}_${MODEL}.gif [oK]"
else
  echo "PROBLEM WITH toto_${VAR}_${MODEL}.gif >>>> stop"
  exit
fi

done

done
