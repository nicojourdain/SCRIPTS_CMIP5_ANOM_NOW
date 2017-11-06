program modif                                         

USE netcdf                                            

IMPLICIT NONE                                         

INTEGER :: fidA, status, dimID_time, dimID_lev, dimID_lat, dimID_lon, mtime, mlev, mlat, mlon, lat_ID, lon_ID, lev_ID, time_ID, var_rcp_ID, var_his_ID, fidM, kvar, kmod, Nvar, Nmod, var_anom_ID, NN_ID, var_ID, ki, kj, kk, mm

CHARACTER(LEN=150) :: file_in, file_out

CHARACTER(LEN=10) :: varnam_his, varnam_rcp, varnam_anom

CHARACTER(LEN=5),ALLOCATABLE,DIMENSION(:) :: varnam

CHARACTER(LEN=14),ALLOCATABLE,DIMENSION(:) :: modnam

REAL*8,ALLOCATABLE,DIMENSION(:) :: lat, lon, lev, time

REAL*4,ALLOCATABLE,DIMENSION(:,:,:,:) :: var_rcp, var_his, mean_anom

!!INTEGER*2,ALLOCATABLE,DIMENSION(:,:,:,:) :: NN
REAL*4,ALLOCATABLE,DIMENSION(:,:,:,:) :: NN

REAL*4 :: misval

!--------------------------------------------------------

Nmod=33
Nvar=5

ALLOCATE( modnam(Nmod), varnam(Nvar) )

modnam=(/ 'ACCESS1-0     ', &
&         'ACCESS1-3     ', &
&         'BNU-ESM       ', &
&         'CCSM4         ', &
&         'CESM1-BGC     ', &
&         'CESM1-CAM5    ', &
&         'CESM1-WACCM   ', &
&         'CMCC-CESM     ', &
&         'CMCC-CM       ', &
&         'CMCC-CMS      ', &
&         'CNRM-CM5      ', &
&         'CSIRO-Mk3-6-0 ', &
&         'CanESM2       ', &
&         'FGOALS-g2     ', &
&         'FIO-ESM       ', &
&         'GFDL-CM3      ', &
&         'GFDL-ESM2G    ', &
&         'GFDL-ESM2M    ', &
&         'HadGEM2-CC    ', &
&         'HadGEM2-ES    ', &
&         'IPSL-CM5A-LR  ', &
&         'IPSL-CM5A-MR  ', &
&         'IPSL-CM5B-LR  ', &
&         'MIROC-ESM     ', &
&         'MIROC-ESM-CHEM', &
&         'MIROC5        ', &
&         'MPI-ESM-LR    ', &
&         'MPI-ESM-MR    ', &
&         'MRI-CGCM3     ', &
&         'NorESM1-M     ', &
&         'NorESM1-ME    ', &
&         'bcc-csm1-1    ', &
&         'inmcm4        '   /)

varnam = (/ 'hus  ', 'ua   ', 'va   ','ta   ', 'zg   ' /) 

!------------------------------------------------------------

DO kvar=1,Nvar

  write(*,*) '###########################################'
  write(*,*) '###########################################'
  write(*,*) 'variable ', TRIM(varnam(kvar))

  write(file_out,101) TRIM(varnam(kvar))
  101 FORMAT(a,'_anom_ALL_CMIP5.nc')                
                                   
  write(varnam_his,102) TRIM(varnam(kvar))
  102 FORMAT(a,'_his')

  write(varnam_rcp,103) TRIM(varnam(kvar))
  103 FORMAT(a,'_rcp')

  write(varnam_anom,104) TRIM(varnam(kvar))
  104 FORMAT(a,'_anom')

  DO kmod=1,Nmod
       
    write(file_in,201) TRIM(varnam(kvar)), TRIM(modnam(kmod))
    201 FORMAT(a,'_',a,'_climo_ERAIgrid.nc')
                                                           
    !---------------------------------------                   
    ! Read netcdf input file :                                 

    write(*,*) 'Reading ', TRIM(file_in)

    status = NF90_OPEN(TRIM(file_in),0,fidA)          
    call erreur(status,.TRUE.,"read") 

    if ( kmod .eq. 1 ) then

         status = NF90_INQ_DIMID(fidA,"time",dimID_time)
         call erreur(status,.TRUE.,"inq_dimID_time")
         status = NF90_INQ_DIMID(fidA,"lev",dimID_lev)
         call erreur(status,.TRUE.,"inq_dimID_lev")
         status = NF90_INQ_DIMID(fidA,"lat",dimID_lat)
         call erreur(status,.TRUE.,"inq_dimID_lat")
         status = NF90_INQ_DIMID(fidA,"lon",dimID_lon)
         call erreur(status,.TRUE.,"inq_dimID_lon")
                                                               
         status = NF90_INQUIRE_DIMENSION(fidA,dimID_time,len=mtime)
         call erreur(status,.TRUE.,"inq_dim_time")
         status = NF90_INQUIRE_DIMENSION(fidA,dimID_lev,len=mlev)
         call erreur(status,.TRUE.,"inq_dim_lev")
         status = NF90_INQUIRE_DIMENSION(fidA,dimID_lat,len=mlat)
         call erreur(status,.TRUE.,"inq_dim_lat")
         status = NF90_INQUIRE_DIMENSION(fidA,dimID_lon,len=mlon)
         call erreur(status,.TRUE.,"inq_dim_lon")
                               
         ALLOCATE(  lat(mlat)  ) 
         ALLOCATE(  lon(mlon)  ) 
         ALLOCATE(  lev(mlev)  ) 
         ALLOCATE(  time(mtime)  ) 
         ALLOCATE(  var_rcp(mlon,mlat,mlev,mtime)  ) 
         ALLOCATE(  var_his(mlon,mlat,mlev,mtime)  )
         ALLOCATE(  mean_anom(mlon,mlat,mlev,mtime)  )
         ALLOCATE(  NN(mlon,mlat,mlev,mtime)  ) 

         mean_anom(:,:,:,:) = 0.0
         NN(:,:,:,:) = 0

         status = NF90_INQ_VARID(fidA,"lat",lat_ID)
         call erreur(status,.TRUE.,"inq_lat_ID")
         status = NF90_INQ_VARID(fidA,"lon",lon_ID)
         call erreur(status,.TRUE.,"inq_lon_ID")
         status = NF90_INQ_VARID(fidA,"lev",lev_ID)
         call erreur(status,.TRUE.,"inq_lev_ID")
         status = NF90_INQ_VARID(fidA,"time",time_ID)
         call erreur(status,.TRUE.,"inq_time_ID")

         status = NF90_GET_VAR(fidA,lat_ID,lat)
         call erreur(status,.TRUE.,"getvar_lat")
         status = NF90_GET_VAR(fidA,lon_ID,lon)
         call erreur(status,.TRUE.,"getvar_lon")
         status = NF90_GET_VAR(fidA,lev_ID,lev)
         call erreur(status,.TRUE.,"getvar_lev")
         status = NF90_GET_VAR(fidA,time_ID,time)
         call erreur(status,.TRUE.,"getvar_time")

    endif

    status = NF90_INQ_VARID(fidA,TRIM(varnam_rcp),var_rcp_ID)
    call erreur(status,.TRUE.,"inq_var_rcp_ID")
    status = NF90_INQ_VARID(fidA,TRIM(varnam_his),var_his_ID)
    call erreur(status,.TRUE.,"inq_var_his_ID")

    status = NF90_GET_VAR(fidA,var_rcp_ID,var_rcp)
    call erreur(status,.TRUE.,"getvar_var_rcp")
    status = NF90_GET_VAR(fidA,var_his_ID,var_his)
    call erreur(status,.TRUE.,"getvar_var_his")
                                                  
    status = NF90_GET_ATT(fidA,var_his_ID,"missing_value",misval)
    call erreur(status,.TRUE.,"get_att_missval_ID")

    status = NF90_CLOSE(fidA)                      
    call erreur(status,.TRUE.,"fin_lecture")     
                                                              
    !---------------------------------------                      

    do ki=1,mlon
    do kj=1,mlat
    do kk=1,mlev
      do mm=1,12
        if ( var_his(ki,kj,kk,mm) .ne. misval .and. var_rcp(ki,kj,kk,mm) .ne. misval ) then
          mean_anom(ki,kj,kk,mm) = mean_anom(ki,kj,kk,mm) + var_rcp(ki,kj,kk,mm) - var_his(ki,kj,kk,mm)
          NN(ki,kj,kk,mm) = NN(ki,kj,kk,mm) + 1.0
        endif
      enddo
    enddo
    enddo
    enddo                                                              
    
  ENDDO !- kmod

  do ki=1,mlon
  do kj=1,mlat
  do kk=1,mlev
  do mm=1,12
    if ( NN(ki,kj,kk,mm) .lt. 0.5 ) then
      mean_anom(ki,kj,kk,mm) = misval
    else
      mean_anom(ki,kj,kk,mm) = mean_anom(ki,kj,kk,mm) / NN(ki,kj,kk,mm)
    endif
  enddo
  enddo
  enddo
  enddo

  !---------------------------------------                      
  ! Writing netcdf output file :                                   
                
  write(*,*) 'Writing ', TRIM(file_out)
                                              
  status = NF90_CREATE(TRIM(file_out),NF90_NOCLOBBER,fidM)
  call erreur(status,.TRUE.,'create')                     
                
  status = NF90_DEF_DIM(fidM,"time",NF90_UNLIMITED,dimID_time)
  call erreur(status,.TRUE.,"def_dimID_time")
  status = NF90_DEF_DIM(fidM,"lev",mlev,dimID_lev)
  call erreur(status,.TRUE.,"def_dimID_lev")
  status = NF90_DEF_DIM(fidM,"lat",mlat,dimID_lat)
  call erreur(status,.TRUE.,"def_dimID_lat")
  status = NF90_DEF_DIM(fidM,"lon",mlon,dimID_lon)
  call erreur(status,.TRUE.,"def_dimID_lon")

  status = NF90_DEF_VAR(fidM,"lat",NF90_DOUBLE,(/dimID_lat/),lat_ID)
  call erreur(status,.TRUE.,"def_var_lat_ID")
  status = NF90_DEF_VAR(fidM,"lon",NF90_DOUBLE,(/dimID_lon/),lon_ID)
  call erreur(status,.TRUE.,"def_var_lon_ID")
  status = NF90_DEF_VAR(fidM,"lev",NF90_DOUBLE,(/dimID_lev/),lev_ID)
  call erreur(status,.TRUE.,"def_var_lev_ID")
  status = NF90_DEF_VAR(fidM,"time",NF90_DOUBLE,(/dimID_time/),time_ID)
  call erreur(status,.TRUE.,"def_var_time_ID")
  status = NF90_DEF_VAR(fidM,TRIM(varnam_anom),NF90_FLOAT,(/dimID_lon,dimID_lat,dimID_lev,dimID_time/),var_anom_ID)
  call erreur(status,.TRUE.,"def_var_anomaly_ID")
  status = NF90_DEF_VAR(fidM,"NN",NF90_FLOAT,(/dimID_lon,dimID_lat,dimID_lev,dimID_time/),NN_ID)
  call erreur(status,.TRUE.,"def_var_NN_ID")

  SELECT CASE (kvar)
    CASE(1) !! hus
      status = NF90_PUT_ATT(fidM,var_anom_ID,"standard_name","specific_humidity")
      call erreur(status,.TRUE.,"put_att_var_anom_ID")
      status = NF90_PUT_ATT(fidM,var_anom_ID,"long_name","Specific Humidity Anomaly = (2080-2100)_rcp85 - (1989-2009)_hist")
      call erreur(status,.TRUE.,"put_att_var_anom_ID")
      status = NF90_PUT_ATT(fidM,var_anom_ID,"units","1")
      call erreur(status,.TRUE.,"put_att_var_anom_ID")
    CASE(2) !! ua
      status = NF90_PUT_ATT(fidM,var_anom_ID,"standard_name","eastward_wind")
      call erreur(status,.TRUE.,"put_att_var_anom_ID")
      status = NF90_PUT_ATT(fidM,var_anom_ID,"long_name","Eastward Wind Anomaly = (2080-2100)_rcp85 - (1989-2009)_hist")
      call erreur(status,.TRUE.,"put_att_var_anom_ID")
      status = NF90_PUT_ATT(fidM,var_anom_ID,"units","m s-1")
      call erreur(status,.TRUE.,"put_att_var_anom_ID")
    CASE(3) !! va
      status = NF90_PUT_ATT(fidM,var_anom_ID,"standard_name","northward_wind")
      call erreur(status,.TRUE.,"put_att_var_anom_ID")
      status = NF90_PUT_ATT(fidM,var_anom_ID,"long_name","Northward Wind Anomaly = (2080-2100)_rcp85 - (1989-2009)_hist")
      call erreur(status,.TRUE.,"put_att_var_anom_ID")
      status = NF90_PUT_ATT(fidM,var_anom_ID,"units","m s-1")
      call erreur(status,.TRUE.,"put_att_var_anom_ID")
    CASE(4) !! ta
      status = NF90_PUT_ATT(fidM,var_anom_ID,"standard_name","air_temperature")
      call erreur(status,.TRUE.,"put_att_var_anom_ID")
      status = NF90_PUT_ATT(fidM,var_anom_ID,"long_name","Air Temperature Anomaly = (2080-2100)_rcp85 - (1989-2009)_hist")
      call erreur(status,.TRUE.,"put_att_var_anom_ID")
      status = NF90_PUT_ATT(fidM,var_anom_ID,"units","K")
      call erreur(status,.TRUE.,"put_att_var_anom_ID")
    CASE(5) !! zg
      status = NF90_PUT_ATT(fidM,var_anom_ID,"standard_name","geopotential_height")
      call erreur(status,.TRUE.,"put_att_var_anom_ID")
      status = NF90_PUT_ATT(fidM,var_anom_ID,"long_name","Geopotential Height Anomaly = (2080-2100)_rcp85 - (1989-2009)_hist")
      call erreur(status,.TRUE.,"put_att_var_anom_ID")
      status = NF90_PUT_ATT(fidM,var_anom_ID,"units","m")
      call erreur(status,.TRUE.,"put_att_var_anom_ID")
    CASE DEFAULT
      write(*,*) '~!@#$%^* ERROR : unknown variable for kvar = ', kvar
      write(*,*) '         >>>>>>>>> STOP'
      stop
  END SELECT
  status = NF90_PUT_ATT(fidM,var_anom_ID,"missing_value",misval)
  call erreur(status,.TRUE.,"put_att_var_anom_ID")
  status = NF90_PUT_ATT(fidM,var_anom_ID,"_FillValue",misval)
  call erreur(status,.TRUE.,"put_att_var_anom_ID")

  status = NF90_PUT_ATT(fidM,NN_ID,"long_name","Nb of models used to calculate the mean")
  call erreur(status,.TRUE.,"put_att_NN_ID")
  status = NF90_PUT_ATT(fidM,lat_ID,"units","degrees_north")
  call erreur(status,.TRUE.,"put_att_lat_ID")
  status = NF90_PUT_ATT(fidM,lat_ID,"axis","Y")
  call erreur(status,.TRUE.,"put_att_lat_ID")
  status = NF90_PUT_ATT(fidM,lat_ID,"long_name","latitude")
  call erreur(status,.TRUE.,"put_att_lat_ID")
  status = NF90_PUT_ATT(fidM,lat_ID,"standard_name","latitude")
  call erreur(status,.TRUE.,"put_att_lat_ID")
  status = NF90_PUT_ATT(fidM,lon_ID,"units","degrees_east")
  call erreur(status,.TRUE.,"put_att_lon_ID")
  status = NF90_PUT_ATT(fidM,lon_ID,"axis","X")
  call erreur(status,.TRUE.,"put_att_lon_ID")
  status = NF90_PUT_ATT(fidM,lon_ID,"long_name","longitude")
  call erreur(status,.TRUE.,"put_att_lon_ID")
  status = NF90_PUT_ATT(fidM,lon_ID,"standard_name","longitude")
  call erreur(status,.TRUE.,"put_att_lon_ID")
  status = NF90_PUT_ATT(fidM,lev_ID,"units","Pa")
  call erreur(status,.TRUE.,"put_att_lev_ID")
  status = NF90_PUT_ATT(fidM,lev_ID,"axis","Z")
  call erreur(status,.TRUE.,"put_att_lev_ID")
  status = NF90_PUT_ATT(fidM,lev_ID,"positive","down")
  call erreur(status,.TRUE.,"put_att_lev_ID")
  status = NF90_PUT_ATT(fidM,lev_ID,"long_name","pressure")
  call erreur(status,.TRUE.,"put_att_lev_ID")
  status = NF90_PUT_ATT(fidM,lev_ID,"standard_name","air_pressure")
  call erreur(status,.TRUE.,"put_att_lev_ID")
  status = NF90_PUT_ATT(fidM,time_ID,"units","days since 0001-01-01")
  call erreur(status,.TRUE.,"put_att_time_ID")
  status = NF90_PUT_ATT(fidM,time_ID,"calendar","proleptic_gregorian")
  call erreur(status,.TRUE.,"put_att_time_ID")
  status = NF90_PUT_ATT(fidM,time_ID,"axis","T")
  call erreur(status,.TRUE.,"put_att_time_ID")
  status = NF90_PUT_ATT(fidM,time_ID,"long_name","time")
  call erreur(status,.TRUE.,"put_att_time_ID")
  status = NF90_PUT_ATT(fidM,time_ID,"standard_name","time")
  call erreur(status,.TRUE.,"put_att_time_ID")

  status = NF90_PUT_ATT(fidM,NF90_GLOBAL,"history","Created using calculate_mean_ALL_CMIP5_3D.f90")
  call erreur(status,.TRUE.,"put_att_GLOBAL_ID")

  status = NF90_ENDDEF(fidM)                   
  call erreur(status,.TRUE.,"fin_definition") 

  status = NF90_PUT_VAR(fidM,lat_ID,lat)
  call erreur(status,.TRUE.,"var_lat_ID")
  status = NF90_PUT_VAR(fidM,lon_ID,lon)
  call erreur(status,.TRUE.,"var_lon_ID")
  status = NF90_PUT_VAR(fidM,lev_ID,lev)
  call erreur(status,.TRUE.,"var_lev_ID")
  status = NF90_PUT_VAR(fidM,time_ID,time)
  call erreur(status,.TRUE.,"var_time_ID")
  status = NF90_PUT_VAR(fidM,var_anom_ID,mean_anom)
  call erreur(status,.TRUE.,"var_anom_ID")
  write(*,*) 'toto1'
  status = NF90_PUT_VAR(fidM,NN_ID,NN)
  call erreur(status,.TRUE.,"var_NN_ID")
  write(*,*) 'toto2'

  status = NF90_CLOSE(fidM)                    
  call erreur(status,.TRUE.,"final")         

  !-----

  DEALLOCATE( lat, lon, lev, time, var_rcp, var_his, mean_anom, NN )

ENDDO !! kvar

end program modif



SUBROUTINE erreur(iret, lstop, chaine)
  ! pour les messages d'erreur
  USE netcdf
  INTEGER, INTENT(in)                     :: iret
  LOGICAL, INTENT(in)                     :: lstop
  CHARACTER(LEN=*), INTENT(in)            :: chaine
  !
  CHARACTER(LEN=80)                       :: message
  !
  IF ( iret .NE. 0 ) THEN
    WRITE(*,*) 'ROUTINE: ', TRIM(chaine)
    WRITE(*,*) 'ERREUR: ', iret
    message=NF90_STRERROR(iret)
    WRITE(*,*) 'CA VEUT DIRE:',TRIM(message)
    IF ( lstop ) STOP
  ENDIF
  !
END SUBROUTINE erreur
