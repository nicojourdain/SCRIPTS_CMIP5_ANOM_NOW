# SCRIPTS CMIP5 ANOM NOW

**Purpose**: Scripts to build forcing from CMIP5 anomalies (future-present) for NOW (NEMO-OASIS-WRF) simulations.

**Contributors**: Nicolas Jourdain, Daniel Argüeso, Alejandro Di Luca, Alexander Sen Gupta. 

In its current form, it calculates present as 1989-2009 (historical) and future as 2081-2100 (rcp85).

* seasonal\_climatology3\_Atm.sh : used to build climatologies (both 1989-2009 and 2081-2100) of 3d atmospheric variables (stored in CLIMO\_ATM)

* seasonal\_climatology3\_Atm\_SRF.sh : used to build climatologies (both 1989-2009 and 2081-2100) of 2d atmospheric variables (stored in CLIMO\_ATM).

* REGRID\_ATM/regrid\_ALL\_3D.sh : regrid all CMIP5 3d atm files onto the REAinterim grid.

* REGRID\_ATM/regrid\_ALL\_SRF.sh : regrid all CMIP5 surface atm files onto the REAinterim grid.

* REGRID\_ATM/calculate\_mean\_ALL\_CMIP5\_3D.f90 : calculate multi-model mean of 3d atm variables. 

* REGRID\_ATM/calculate\_mean\_ALL\_CMIP5\_SRF.f90 : calculate multi-model mean of surface atm variables. 

* REGRID\_OCE/interp\_oce\_CMIP5\_to\_ORCA025\_hist.sh : interpolate ocean historical climatologies onto the ORCA025 grid.

* REGRID\_OCE/interp\_oce\_CMIP5\_to\_ORCA025\_rcp85.sh : interpolate ocean rcp85 climatologies onto the ORCA025 grid.

* REGRID\_OCE/calculate\_mean\_ALL\_CMIP5\_OCE.sh : calculate the multi-model mean for the ocean variables.

* WRF : python scripts to put the CMIP5 anomalies onto WRF's format.

**References**: 

Dutheil C., Bador M., Lengaigne M., Lefevre J., Jourdain N. C., Jullien S. ,Vialard J., Peltier A., and Menkes C. (2019). Impact of surface temperature biases on climate change projections of the South Pacific Convergence Zone. _Climate Dynamics (in press)_. [doi:10.1007/s00382-019-04692-6](https://doi.org/10.1007/s00382-019-04692-6)
