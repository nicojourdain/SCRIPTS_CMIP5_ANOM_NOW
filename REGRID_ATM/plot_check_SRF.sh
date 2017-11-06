#!/bin/bash

for MODEL in CanESM2
#for MODEL in ACCESS1-0 ACCESS1-3 BNU-ESM CCSM4 CESM1-BGC CESM1-CAM5 CESM1-WACCM CMCC-CESM CMCC-CM CMCC-CMS CNRM-CM5 CSIRO-Mk3-6-0 CanESM2 FGOALS-g2 FIO-ESM GFDL-CM3 GFDL-ESM2G GFDL-ESM2M HadGEM2-CC HadGEM2-ES IPSL-CM5A-LR IPSL-CM5A-MR IPSL-CM5B-LR MIROC-ESM MIROC-ESM-CHEM MIROC5 MPI-ESM-LR MPI-ESM-MR MRI-CGCM3 NorESM1-M NorESM1-ME bcc-csm1-1 inmcm4
do

for VAR in uas vas tas ts psl ps hurs
do

if [ "$VAR" == "uas" ]; then
  LEV="(-15,15,1.5)"
elif [ "$VAR" == "vas" ]; then
  LEV="(-10,10,1)"
elif [ "$VAR" == "tas" ]; then
  LEV="(240,310,3.5)"
elif [ "$VAR" == "ts" ]; then
  LEV="(220,320,5)"
elif [ "$VAR" == "psl" ]; then
  LEV="(94000,104000,500)"
elif [ "$VAR" == "ps" ]; then
  LEV="(55000,105000,2500)"
elif [ "$VAR" == "hurs" ]; then
  LEV="(0,100,5)"
fi

echo "cancel data/all"                             >  toto.jnl
echo "use ${VAR}_${MODEL}_climo_ERAIgrid.nc"       >> toto.jnl
echo "set viewport UPPER"                          >> toto.jnl
echo "shade/lev=${LEV} ${VAR}_his[Z=100000,l=12]"  >> toto.jnl
echo "set viewport LOWER"                          >> toto.jnl
echo "shade/lev=${LEV} ${VAR}_rcp[Z=100000,l=12]"  >> toto.jnl
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
