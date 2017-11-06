#!/bin/bash

for MODEL in ACCESS1-0 ACCESS1-3 BNU-ESM CCSM4 CESM1-BGC CESM1-CAM5 CESM1-WACCM CMCC-CESM CMCC-CM CMCC-CMS CNRM-CM5 CSIRO-Mk3-6-0 CanESM2 FGOALS-g2 FIO-ESM GFDL-CM3 GFDL-ESM2G GFDL-ESM2M HadGEM2-CC HadGEM2-ES IPSL-CM5A-LR IPSL-CM5A-MR IPSL-CM5B-LR MIROC-ESM MIROC-ESM-CHEM MIROC5 MPI-ESM-LR MPI-ESM-MR MRI-CGCM3 NorESM1-M NorESM1-ME bcc-csm1-1 inmcm4
do

VAR="tas"

echo "cancel mode logo"                                                                                  >  toto.jnl
echo "use tas_${MODEL}_climo_ERAIgrid.nc"                                                                >> toto.jnl
echo "define viewport/xlim=0,1/ylim=0.6,1.0 Upp"                                                         >> toto.jnl
echo "define viewport/xlim=0,1/ylim=0.0,0.4 Low"                                                         >> toto.jnl
echo "define viewport/xlim=0,1/ylim=0.3,0.7 Cent"                                                        >> toto.jnl
echo "set viewport Upp"                                                                                  >> toto.jnl
echo 'sha/lev="(-60,-30,10)(-30,30,5)(30,60,10)"/palette=blue_darkred TAS_HIS[l=7]-TAS_HIS[l=1]'         >> toto.jnl
echo "go land"                                                                                           >> toto.jnl
echo "set viewport Cent"                                                                                 >> toto.jnl
echo 'sha/lev="(-60,-30,10)(-30,30,5)(30,60,10)"/palette=blue_darkred TAS_RCP[l=7]-TAS_RCP[l=1]'         >> toto.jnl
echo "go land"                                                                                           >> toto.jnl
echo "set viewport Low"                                                                                  >> toto.jnl
echo 'sha/lev="(-20,20,2)"/palette=blue_darkred (TAS_RCP[l=7]-TAS_HIS[l=7])-(TAS_RCP[l=1]-TAS_HIS[l=1])' >> toto.jnl
echo "go land"                                                                                           >> toto.jnl
echo "frame/file=toto_${VAR}_${MODEL}.gif"                                                               >> toto.jnl
echo "exit"                                                                                              >> toto.jnl

ferret -gif < toto.jnl

if [ -f toto_${VAR}_${MODEL}.gif ]; then
  echo "toto_${VAR}_${MODEL}.gif [oK]"
else
  echo "PROBLEM WITH toto_${VAR}_${MODEL}.gif >>>> stop"
  exit
fi

done
