
#!/usr/bin/env python

""" grib2netcdf.py

Author: Daniel Argueso @ CCRC, UNSW. Sydney (Australia)
email: d.argueso@ unsw.edu.au
Created: Wed Jun 17 15:22:24 AEST 2015

"""

import netCDF4 as nc
import numpy as np
from constants import const
import glob as glob
import os

import pdb


files=sorted(glob.glob("EIN2*_an_sfc.grb"))

for fin in files:
  fout=fin.split("grb")[0]+"nc"
  os.system("cdo -f nc copy %s ERA-interim_CMIP5anom/%s" %(fin,fout))



