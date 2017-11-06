#! /bin/bash

############ SCRIPT para lanzar el UNGRIB ######################

set -xe

START_YEAR=$1
END_YEAR=$2

THIS_DIR=$(pwd)
GCM_DIR="/srv/ccrc/data13/z3393020/MIROC3.2/20C3M-SRES_A2/WRFint"
################################################################
###                                                          ###
###              END OF USER MODIFICATION                    ###    
################################################################
function sedcmd_switch() {
cat << EOF > sedcmd
s#$1#$2#g
EOF
}

function sedcmd_addswitch() {
cat << EOF >> sedcmd
s#$1#$2#g
EOF
}

function days_in_month(){
case $2 in 
01|03|05|07|08|10|12) days=31;;
04|06|09|11) days=30;;
02)
	if [$(($1 % 4)) -ne "0"]; then
	days=28	
	else
	days=29
	echo "this is a leap year"
	fi
esac
}







datadir=/srv/ccrc/data13/z3393020/ERA-interim/
ln -sf Vtable.ERAIN.SOIL1ststep Vtable
cp $datadir/SOILCLIM.grb .

for year in $(seq $START_YEAR $END_YEAR); do
for month in $(seq -f "%02g" 1 12); do


######### WRITE NAMELIST.WPS ############
cat > ./namelist.wps << EOF_namelist
&share
 wrf_core = 'ARW',
 max_dom = 2,
 start_date = FYEAR_SWITCH,
 end_date   = LYEAR_SWITCH,
 interval_seconds = 21600,
 io_form_geogrid = 2,
 opt_output_from_geogrid_path = './',
 debug_level = 0,
/

&geogrid
 parent_id         = 1,1,
 parent_grid_ratio = 1,5,
 i_parent_start    = 1,82,
 j_parent_start    = 1,36,
 e_we          = 216,326,
 e_sn          = 145,201,
 geog_data_res = '10m','2m',
 dx = 0.44,
 dy = 0.44,
 map_proj =  'lat-lon',
 ref_lat   = -24.26,
 ref_lon   = 147.63,
 pole_lat  = 60.31,
 pole_lon  = 321.38,
 stand_lon = 32.37,
 geog_data_path = '/srv/ccrc/data02/z3236814/data/WRF_geog/geog',
/

&ungrib
 out_format = 'WPS',
 prefix = 'EIN_SOIL',
/

&metgrid
 fg_name = 'MIROC','SST_MIROC','EIN_SOIL',
 io_form_metgrid = 2,
/

&mod_levs
 press_pa = 201300 , 200100 , 100000 ,
             95000 ,  90000 ,
             85000 ,  80000 ,
             75000 ,  70000 ,
             65000 ,  60000 ,
             55000 ,  50000 ,
             45000 ,  40000 ,
             35000 ,  30000 ,
             25000 ,  20000 ,
             15000 ,  10000 ,
              5000 ,   1000
/
EOF_namelist


#START_TIME="18:00:00"
START_TIME="00:00:00"

#days_in_month $START_YEAR $START_MONTH
days=01
cdo setdate,${year}-${month}-${days} SOILCLIM.grb aux.grb
cdo settime,${START_TIME} aux.grb SOILCLIM.startdate.grb
rm aux.grb 


./link_grib.csh ./SOILCLIM.startdate.grb

# Change the start and end hour in the script below to use a time different from 00



rundate="${year}-${month}-${days}_${START_TIME}"
echo $rundate

	sedcmd_switch    FYEAR_SWITCH    "'${rundate}','${rundate}','${rundate}'"
        sedcmd_addswitch LYEAR_SWITCH    "'${rundate}','${rundate}','${rundate}'"

sed -i -f sedcmd namelist.wps
rm -f sedcmd 

./ungrib.exe

#ln -sf $GCM_DIR/MIROC\:${year}-${month}-${days}* .
#ln -sf $GCM_DIR/SST_MIROC\:${year}-${month}-${days}* .
#./metgrid.exe
#rm -f MIROC\:* SST_MIROC\:*

done
done
