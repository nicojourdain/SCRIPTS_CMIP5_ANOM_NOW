#!/bin/bash
# Script to generate WRF boundary conditons from ERA Interim
# Must be located in WRF/run
# e.g. ./myrunWPSandreal.sh 1990 1991
#+==============================
# Daniel Argueso
# University of New South Wales
# 07 July 2012
# Last version: 24 Mayy 2013
# email: d.argueso@unsw.edu.au
#==============================

#set -xe


START_YEAR=$1
END_YEAR=$2

START_MONTH=1
END_MONTH=1

THIS_DIR=$(pwd)
metem_DIR="../../WPS/"
output_DIR="/srv/ccrc/data13/z3393020/ERA-interim/ERA-interim_CMIP5anom/WRF_boundary/"
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


for runyear in $(seq $START_YEAR $END_YEAR);do	
for month in $(seq $START_MONTH $END_MONTH);do


#####################
##### WPS PART ######
#####################

cd ../../WPS/

ln -sf ungrib/Variable_Tables/Vtable.ERA-interim.pl Vtable
ERAINT_DIR="/srv/ccrc/data13/z3393020/ERA-interim"
WRFint_DIR="/srv/ccrc/data13/z3393020/ERA-interim/ERA-interim_CMIP5anom/WRF_intermediate"
######### WRITE NAMELIST.WPS ############
cat > ./namelist.wps << EOF_namelist
&share
 wrf_core = 'ARW',
 max_dom = 1,
 start_date = 'THIS_YEAR-THIS_MONTH-01_00:00:00'
 end_date   = 'NEXT_YEAR-NEXT_MONTH-01_00:00:00'
 interval_seconds = 21600,
 io_form_geogrid = 2,
 opt_output_from_geogrid_path = './',
 debug_level = 0,
/

&geogrid
 parent_id         = 1,
 parent_grid_ratio = 1,
 i_parent_start    = 1,
 j_parent_start    = 1,
 e_we          = 216,
 e_sn          = 145,
 geog_data_res = '10m',
 dx = 0.22,
 dy = 0.22,
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
 prefix = 'EIN',
/

&metgrid
 fg_name = 'EIN',
 io_form_metgrid = 2,
 constants_name='LSEIN:2009-01-01_00','EIN_SOIL:1989-01-01_00',
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


###### GEOGRID AND UNGRIB ######


if [ $month == 12 ]; then

  sedcmd_switch THIS_YEAR $runyear
  sedcmd_addswitch NEXT_YEAR $((${runyear}+1))
  sedcmd_addswitch THIS_MONTH 12
  sedcmd_addswitch NEXT_MONTH 01
 sed -i -f sedcmd  namelist.wps
 rm -f sedcmd

  ./link_grib.csh ${ERAINT_DIR}/EIN${runyear}12* ${ERAINT_DIR}/EIN$((${runyear}+1))01* 
else

  sedcmd_switch THIS_YEAR ${runyear}
  sedcmd_addswitch NEXT_YEAR ${runyear}
  sedcmd_addswitch THIS_MONTH $(printf "%02d" ${month})
  sedcmd_addswitch NEXT_MONTH $(printf "%02d" $((${month}+1)))
 sed -i -f sedcmd  namelist.wps
 rm -f sedcmd
  ./link_grib.csh ${ERAINT_DIR}/EIN${runyear}$(printf "%02d" ${month})* ${ERAINT_DIR}/EIN${runyear}$(printf "%02d" $((${month}+1)))*
  echo "month = ${month}"
  echo "year = ${runyear}"
  echo "end date = ${runyear}-$(printf "%02d" $((${month}+1)))-01"
fi

#THIS HAS TO BE RUN ONLY THE FIRST TIME
# Comment otherwise if they are already generated
#if [ $runyear == $START_YEAR ] && [ $month == $START_MONTH ]; then
#./geogrid.exe >& geogrid.log
#fi

###### METGRID ######

if [ $month == 12 ]; then

ln -sf ${WRFint_DIR}/EIN_SOIL\:${runyear}-12* .      	
ln -sf ${WRFint_DIR}/EIN\:${runyear}-12* .
ln -sf ${WRFint_DIR}/EIN\:$((${runyear}+1))-01-01_00 .

else
  ln -sf ${WRFint_DIR}/EIN_SOIL\:${runyear}-$(printf "%02d" ${month})* .	
  ln -sf ${WRFint_DIR}/EIN\:${runyear}-$(printf "%02d" ${month})* .
  ln -sf ${WRFint_DIR}/EIN\:${runyear}-$(printf "%02d" $((${month}+1)))-01_00 .
  
fi


# sedcmd_switch START_YEAR $runyear
# sed -i -f sedcmd  namelist.wps
# rm -f sedcmd

./metgrid.exe >& metgrid.log

rm EIN\:${runyear}*

 cd ../WRFV3/run/



################################################################
#####################
##### REAL PART #####
#####################

cat > namelist.input << EOF_namelist
 &time_control
 run_days                            = 0,
 run_hours                           = 0,
 run_minutes                         = 0,
 run_seconds                         = 0,
 start_year                          = FYEAR_SWITCH,
 start_month                         = FMONTH_SWITCH,
 start_day                           = FDAY_SWITCH,
 start_hour                          = FHOUR_SWITCH,
 start_minute                        = 00,	00,	00,
 start_second                        = 00,	00,	00,
 end_year                            = LYEAR_SWITCH,,
 end_month                           = LMONTH_SWITCH,
 end_day                             = LDAY_SWITCH,
 end_hour                            = LHOUR_SWITCH,
 end_minute                          = 000,	00,	00,
 end_second                          = 00,	00,	00,
 interval_seconds                    = 21600
 input_from_file                     = .true.,.true.,.false.,
 history_interval                    = 180, 180,   60,
 frames_per_outfile                  = 8, 8, 1000,
 restart                             = .false.
 restart_interval                    = 1440
 io_form_history                     = 2
 io_form_restart                     = 2
 io_form_input                       = 2
 io_form_boundary                    = 2
 debug_level                         = 0
 auxinput4_inname                    = "wrflowinp_d<domain>"
 auxinput4_interval                  = 360,360,
 io_form_auxinput4                   = 2
 auxhist3_outname                    = 'wrfxtrm_d<domain>_<date>'
 io_form_auxhist3                    = 2
 auxhist3_interval                   = 1440, 1440
 frames_per_auxhist3                 = 31, 31
 auxhist4_outname                    = 'wrfrain_d<domain>_<date>'
 io_form_auxhist4                    = 2
 auxhist4_interval                   = 60,60
 frames_per_auxhist4                 = 31, 31
 auxhist5_outname                    = 'wrf24h_d<domain>_<date>'
 io_form_auxhist5                    = 2
 auxhist5_interval                   = 1440,1440
 frames_per_auxhist5                 = 31, 31
 /

 &diagnostics
 clwrf_variables                     = 1,
 max_rain_5m                         = 1,
 max_wind_5m                         = 1,
 max_rain_10m                        = 1,
 max_wind_10m                        = 1,
 max_rain_20m                        = 1,
 max_wind_20m                        = 1,
 max_rain_30m                        = 1,
 max_wind_30m                        = 1,
 max_rain_60m                        = 1,
 max_wind_60m                        = 1,
 max_window                          = 12,
 /

 &domains
 time_step                           = 300,
 time_step_fract_num                 = 0,
 time_step_fract_den                 = 1,
 max_dom                             = 1,
 s_we                                = 1,     1,     1,
 e_we                                = 432,    326,   94,
 s_sn                                = 1,     1,     1,
 e_sn                                = 290,    201,   91,
 s_vert                              = 1,     1,     1,
 e_vert                              = 30,    30,    28,
 eta_levels = 1.00,0.995,0.99,0.98,0.97,0.96,0.94,0.92,0.89,0.86,0.83,0.80,0.77,0.72,0.67,0.62,0.57,0.52,0.47,0.42,0.37,0.32,0.27,0.22,0.17,0.12,0.07,0.04,0.02,0.00
 num_metgrid_levels                  = 38
 num_metgrid_soil_levels 	     = 4 
 dx                                  = 24459.05, 9783.618,  3333,
 dy                                  = 24459.05, 9783.618,  3333,
 grid_id                             = 1,     2,     3,
 parent_id                           = 0,     1,     2,
 i_parent_start                      = 1,     82,    30,
 j_parent_start                      = 1,     36,    30,
 parent_grid_ratio                   = 1,     5,     3,
 parent_time_step_ratio              = 1,     5,     3,
 feedback                            = 0,
 smooth_option                       = 0
 /

 &physics
 mp_physics                          = 14,     14,     3,
 ra_lw_physics                       = 1,     1,     1,
 ra_sw_physics                       = 1,     1,     1,
 radt                                = 10,    10,    10,
 cam_abs_freq_s                      = 10800
 levsiz                              = 59
 paerlev                             = 29
 cam_abs_dim1                        = 4
 cam_abs_dim2                        = 28
 sf_sfclay_physics                   = 2,     2,     1,
 sf_surface_physics                  = 2,     2,     1,
 bl_pbl_physics                      = 2,     2,     1,
 bldt                                = 0,     0,     0,
 cu_physics                          = 1,     1,     0,
 cudt                                = 0,     0,     5,
 isfflx                              = 1,
 surface_input_source                = 1,
 num_soil_layers                     = 4,
 sf_urban_physics                    = 0,
 sst_update                          = 1,
 tmn_update                          = 1,
 lagday                              = 150,
 sst_skin                            = 1,
 usemonalb                           = .true.
 rdmaxalb                            = .true.,
 slope_rad                           = 1,
 topo_shading                        = 1,
 shadlen                             = 25000.,
 bucket_mm                           = 1000000.,
 /

 &fdda
 grid_fdda                           = 2,0,
 gfdda_inname                        = "wrffdda_d<domain>",
 gfdda_interval_m                    = 360,
 gfdda_end_h                         = 999999999, 
 xwavenum                            = 3, 
 ywavenum                            = 3, 
 if_zfac_uv                          = 1, 
 k_zfac_uv                           = 19, 
 if_zfac_t                           = 1, 
 k_zfac_t                            = 30, 
 if_zfac_q                           = 1, 
 k_zfac_q                            = 30, 
 if_zfac_ph                          = 1, 
 k_zfac_ph                           = 19, 
 guv                                 = 0.0003,
 gt                                  = 0.000, 
 gq                                  = 0.000, 
 gph                                 = 0.0003, 
 if_ramping                          = 1,
 dtramp_min                          = 60.0,
 io_form_gfdda                       = 2,
 /

 &dynamics
 rk_ord                              = 3,
 w_damping                           = 0,
 diff_opt                            = 1,
 km_opt                              = 4,
 diff_6th_opt                        = 0,
 diff_6th_factor                     = 0.12,
 base_temp                           = 290.
 damp_opt                            = 3,
 zdamp                               = 5000.,  5000.,  5000.,
 dampcoef                            = 0.2,   0.2,   0.01
 khdif                               = 0,      0,      0,
 kvdif                               = 0,      0,      0,
 non_hydrostatic                     = .true., .true., .true.,
 moist_adv_opt                       = 1,      1,      1,     
 scalar_adv_opt                      = 1,      1,      1, 
 gwd_opt                             = 1, 
 /

 &bdy_control
 spec_bdy_width                      = 5,
 spec_zone                           = 1,
 relax_zone                          = 4,
 specified                           = .true., .false.,.false.,
 nested                              = .false., .true., .true.,
 /

 &grib2
 /

 &namelist_quilt
 nio_tasks_per_group = 0,
 nio_groups = 1,
 /
EOF_namelist



if [ $month == 12 ]; then
	
	sedcmd_switch    FYEAR_SWITCH    "${runyear}"
  sedcmd_addswitch LYEAR_SWITCH    "$((${runyear}+1))"
	sedcmd_addswitch FMONTH_SWITCH	 "12"
	sedcmd_addswitch LMONTH_SWITCH	 "01"
	sedcmd_addswitch FDAY_SWITCH	 "01"
	sedcmd_addswitch LDAY_SWITCH	 "01"
	sedcmd_addswitch FHOUR_SWITCH	 "00"
	sedcmd_addswitch LHOUR_SWITCH	 "00"

else
	
	next_month=$(printf "%02d" $((${month}+1)))
	echo $next_month
	sedcmd_switch    FYEAR_SWITCH    "${runyear},"
  sedcmd_addswitch LYEAR_SWITCH    "$((${runyear}))"
	sedcmd_addswitch FMONTH_SWITCH	 "$(printf "%02d" ${month})"
	sedcmd_addswitch LMONTH_SWITCH	 "${next_month}"
	sedcmd_addswitch FDAY_SWITCH	 "01"
	sedcmd_addswitch LDAY_SWITCH	 "01"
	sedcmd_addswitch FHOUR_SWITCH	 "00"
	sedcmd_addswitch LHOUR_SWITCH	 "00"
fi



sed -i -f sedcmd namelist.input
rm -f sedcmd 

if [ $month == 12 ]; then
ln -sf ${metem_DIR}/met_em.d0?.${runyear}-$(printf "%02d" ${month})* .
ln -sf ${metem_DIR}/met_em.d0?.$((${runyear}+1))-01-01_00:00:00.nc .

else
next_month=$(printf "%02d" $((${month}+1)))


ln -sf ${metem_DIR}/met_em.d0?.${runyear}-$(printf "%02d" ${month})* .
ln -sf ${metem_DIR}/met_em.d0?.${runyear}-${next_month}-01_00:00:00.nc .

fi 

time mpirun -np 1 -report-bindings real.exe 

#bound_files=$(ls wrf*_d0?)
# for file in $bound_files; do
# mv ${file} ${output_DIR}/${file}_${runyear}-${month}
# done
#rm met_em.d0* 

mv wrfbdy_d01 ${output_DIR}/wrfbdy_d01_${runyear}-$(printf "%02d" ${month})
mv wrflowinp_d01 ${output_DIR}/wrflowinp_d01_${runyear}-$(printf "%02d" ${month})
mv wrffdda_d01 ${output_DIR}/wrffdda_d01_${runyear}-$(printf "%02d" ${month})
mv wrfinput_d01 ${output_DIR}/wrfinput_d01_${runyear}-$(printf "%02d" ${month})


rm ../../WPS/met_em.d0*
rm met_em.d0*
# 
 done
 done
