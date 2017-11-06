#!/usr/bin/env python

""" write_intermediate_ERAI+CMIP5anom.py

Author: Daniel Argueso @ CCRC, UNSW. Sydney (Australia)
email: d.argueso@ unsw.edu.au
Created: Wed Jun 17 14:08:31 AEST 2015

"""

import netCDF4 as nc
import pygrib as pygrib
import numpy as np
from constants import const
import glob as glob
from optparse import OptionParser
import calendar
import outputInter as f90
import datetime as dt
import sys

import pdb


def calc_midmonth(year):
  
  midm_date=[]
   
  for month in range(1,13):
    minit=dt.datetime(year,month,01,00)
    if month==12:
      mend=dt.datetime(year+1,1,01,01)
    else:
       mend=dt.datetime(year,month+1,01,01)
    tdiference=(mend-minit).total_seconds()/2
    midm_date=midm_date+[minit+dt.timedelta(seconds=tdiference)]
  
  tdiference=(dt.datetime(year,1,01,01)-dt.datetime(year-1,12,01,01)).total_seconds()/2
  midm_date=[dt.datetime(year-1,12,01,01)+dt.timedelta(seconds=tdiference)]+midm_date
  
  tdiference=(dt.datetime(year+1,2,01,01)-dt.datetime(year+1,1,01,01)).total_seconds()/2
  midm_date=midm_date+[dt.datetime(year+1,1,01,01)+dt.timedelta(seconds=tdiference)]
  
  return midm_date

def calc_relhum(dewpt,t):
  """ Function to calculate relative humidity 
      from dew point temperature and temperature
  """

  relhum=100.*(np.exp((const.es_Abolton*dewpt)/(const.es_Bbolton+dewpt))/np.exp((const.es_Abolton*t)/(const.es_Bbolton+t)))
  return relhum

### Options
parser = OptionParser()
parser.add_option("-s", "--syear",type="int", dest="syear",
help="first year to process", metavar="input argument")
parser.add_option("-e", "--eyear",type="int", dest="eyear",
help="last year to process", metavar="input argument")

(opts, args) = parser.parse_args()
###

syear = opts.syear
eyear = opts.eyear
nyears = eyear - syear + 1

vars3d=['hus','ta','ua','va','zg']
vars3d_codes={'hus':157,'ta':130,'ua':131,'va':132,'zg':129}
vars3d_oname={'hus':157,'ta':130,'ua':131,'va':132,'zg':129}
vars2d=['hurs','tas','uas','vas','ps','psl','ts']
vars2d_codes={'dew':168,'tas':167,'uas':165,'vas':166,'ps':134,'psl':151,'ts':235}

nfields3d=5
nfields2d=6

CMIP5anom_dir="/srv/ccrc/data22/z3381502/CMIP5_ANOMALY"
ERAI_dir="/srv/ccrc/data19/z3393020/ERA-interim_CMIP5anom"


plvs=[100000.0 , 97500.00,  95000.00,  92500.00 , 90000.00 , 87500.00 , 85000.00 , 82500.00 ,
      80000.00 , 77500.00 , 75000.00 , 70000.00 , 65000.00 , 60000.00 , 55000.00 , 
      50000.00 , 45000.00 , 40000.00 , 35000.00 , 30000.00 , 25000.00 , 22500.00 , 20000.00 ,
      17500.00 , 15000.00 , 12500.00 , 10000.00 ,  7000.00 ,  5000.00 ,  3000.00 ,  2000.00 , 
      1000.000,    700.00 ,   500.00 ,   300.00 ,   200.00 ,   100.00 ]

nlat=251
nlon=500


file_ref=nc.Dataset("%s/EIN199001_an_sfc.nc" %(ERAI_dir),'r')
lat=file_ref.variables['lat'][:]
lon=file_ref.variables['lon'][:]
#Extracting land-sea mask from SST
masklm=file_ref.variables['var34'][0,:,:]
masklm[~masklm.mask]=0
masklm[masklm.mask]=1
masklm=masklm.data

for y in range(nyears):
  year=y+syear
  
  midmonth=calc_midmonth(year)
  
  for month in range(1,13):
    print "processing year %s month %02d" %(year, month)
    

    

    
    ferapl=nc.Dataset("%s/EIN%s%02d_an_pl.nc" %(ERAI_dir,year,month),'a')
    ferasfc=nc.Dataset("%s/EIN%s%02d_an_sfc.nc" %(ERAI_dir,year,month),'a') 
    
    
    date_init = dt.datetime(year,month,01,00)
    date_end  = dt.datetime(year,month,31,18)
    
    time_filepl=ferapl.variables['time']
    time_filesfc=ferasfc.variables['time'] 
    
    date1 = nc.date2index(date_init,time_filepl,calendar='standard',select='exact')
    date2 = nc.date2index(date_end,time_filepl,calendar='standard',select='exact')
    ndays=(date_end-date_init).total_seconds()/86400.+1
    nsteps=int((date_end-date_init).total_seconds()/86400.*4.+1)
    
    vout={}
    print "Looping over timesteps in original ERA-Interim file"
    
    for nt in range(date1,date2+1):
      proc_date=nc.num2date(time_filepl[nt],units=time_filepl.units,calendar='standard')
      print 'processing 3Dvar time: ',proc_date
    
      tdelta=np.asarray([(midmonth[i]-proc_date).total_seconds() for i in range(len(midmonth))])
      tdelta_min=np.argmin(np.abs(tdelta))
      if tdelta[tdelta_min]<0:
        i1=(tdelta_min-1)%12
        i2=(tdelta_min)%12
        tdelta_before=np.abs(tdelta[tdelta_min])
        tdelta_mid_month=(midmonth[tdelta_min+1]-midmonth[tdelta_min]).total_seconds()
      else:
        i1=(tdelta_min-2)%12
        i2=(tdelta_min-1)%12
        tdelta_before=np.abs(tdelta[tdelta_min-1])
        tdelta_mid_month=(midmonth[tdelta_min]-midmonth[tdelta_min-1]).total_seconds()
    
    
      for var in vars3d:
      
        print "Processing variable %s" %(var)
      
        fanom=nc.Dataset("%s/%s_anom_ALL_CMIP5.nc" %(CMIP5anom_dir,var))
        var_era=ferapl.variables['var%s' %(vars3d_codes[var])][nt,:,:,:]
        
       
        if np.argmin(np.abs(tdelta))==0:
          var_anom=fanom.variables["%s_anom" %(var)][i1,:,:,:]
        
        else:
          var_anom_1=fanom.variables["%s_anom" %(var)][i1,:,:,:]
          var_anom_2=fanom.variables["%s_anom" %(var)][i2,:,:,:]
          
          var_anom=var_anom_1+(var_anom_1+var_anom_2)*(tdelta_before)/tdelta_mid_month
        
        
        #vout[var]=var_era+np.nan_to_num(var_anom)
        ferapl.variables['var%s' %(vars3d_codes[var])][nt,:,:,:]=var_era+np.nan_to_num(var_anom)
        fanom.close()
        
      
      for var in vars2d:  
        print "Processing variable %s" %(var)
        if var=='hurs': 
          #Surface relative humidity doesn't exist in original ERA-INt, must be calculated from T2 and DEWPT
          dew_era=ferasfc.variables['var168'][nt,:,:]-const.tkelvin
          tas_era=ferasfc.variables['var167'][nt,:,:]-const.tkelvin
          
          var_era=calc_relhum(dew_era,tas_era)
          
        else:
          var_era=ferasfc.variables['var%s' %(vars2d_codes[var])][nt,:,:]
        
        
        fanom=nc.Dataset("%s/%s_anom_ALL_CMIP5.nc" %(CMIP5anom_dir,var))
        
        if np.min(np.abs(tdelta))==0:
          var_anom=fanom.variables["%s_anom" %(var)][i1,:,:]
        else:
          var_anom_1=fanom.variables["%s_anom" %(var)][i1,:,:]
          var_anom_2=fanom.variables["%s_anom" %(var)][i2,:,:]
          
          var_anom=var_anom_1+(var_anom_1+var_anom_2)*(tdelta_before)/tdelta_mid_month
        

        #vout[var]=var_era+np.nan_to_num(var_anom)
        
        if var=='hurs':
          ferasfc.variables['var168'][nt,:,:]=var_era+np.nan_to_num(var_anom)
        else:
          ferasfc.variables['var%s' %(vars2d_codes[var])][nt,:,:]=var_era+np.nan_to_num(var_anom)
        
        
        fanom.close()
        
        
      
      
      
      
      
        
      # ####################  Writing to WRF intermediate format  #############################
      # 
      # filedate=proc_date.strftime('%Y-%m-%d_%H-%M-%S')
      # 
      # fields3d    = np.ndarray(shape=(nfields3d,len(plvs),nlat,nlon),dtype='float32')#,order='Fortran')
      # fields2d    = np.ndarray(shape=(nfields2d,nlat,nlon),dtype='float32')#,order='Fortran')
      # 
      # startlat=lat[0]
      # startlon=lon[0]
      # deltalon=0.72
      # deltalat=0.72
      # 
      #      
      #      
      # 
      # fields3d[0,:,:,:]=vout['hus'][:]
      # fields3d[1,:,:,:]=vout['ta'][:]
      # fields3d[2,:,:,:]=vout['ua'][:]
      # fields3d[3,:,:,:]=vout['va'][:]
      # fields3d[4,:,:,:]=vout['zg'][:]
      # 
      # fields2d[0,:,:]=vout['uas'][:]
      # fields2d[1,:,:]=vout['vas'][:]
      # fields2d[2,:,:]=vout['hurs'][:]
      # fields2d[3,:,:]=vout['ps'][:]
      # fields2d[4,:,:]=vout['psl'][:]
      # fields2d[5,:,:]=vout['tas'][:]
      # 
      # #f90.writeint(plvs,fields3d,fields2d,filedate,nlat,nlon,startlat,startlon,deltalon,deltalat)
      #   
      # ####################  Writing  SST to WRF intermediate format  #############################
      # 
      # filedate=proc_date.strftime('%Y-%m-%d_%H-%M-%S')
      # 
      # fieldsst   = np.ndarray(shape=(nlat,nlon),dtype='float32')#,order='Fortran')
      # fieldmask  = np.ndarray(shape=(nlat,nlon),dtype='float32')#,order='Fortran')
      # 
      # startlat=lat[0]
      # startlon=lon[0]
      # deltalon=lon[1]-lon[0]
      #  
      # 
      # fieldsst=vout['ts'][:]
      # fieldmask=masklm[:]
      # 
      # f90.writeintsst(fieldsst,fieldmask,filedate,nlat,nlon,startlat,startlon,deltalon)

      
      
      
      
    
    

