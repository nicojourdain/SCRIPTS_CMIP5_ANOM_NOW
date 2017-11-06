program modif                                         

USE netcdf                                            

IMPLICIT NONE                                         

INTEGER :: fidA, fidB, status, dimID_time, dimID_y, dimID_x, mtime, my, mx, nav_lat_ID, nav_lon_ID, time_ID, var_rcp_ID, var_his_ID, fidM, kvar, kmod, Ngrid, Nmod, var_anom_ID, NN_ID, var_ID, ki, kj, kk, mm, kgrid, depth_ID, z_ID, dimID_z, dimID_depth, mdepth, dimID_time2

INTEGER, ALLOCATABLE, DIMENSION(:) :: Nvar

INTEGER, DIMENSION(12) :: time

CHARACTER(LEN=150) :: file_in_his, file_in_rcp, file_out

CHARACTER(LEN=15) :: varnam_anom, varnam

CHARACTER(LEN=5),ALLOCATABLE,DIMENSION(:) :: gridnam

CHARACTER(LEN=8),ALLOCATABLE,DIMENSION(:) :: varnam1, varnam2

CHARACTER(LEN=14),ALLOCATABLE,DIMENSION(:) :: modnam

REAL*4,ALLOCATABLE,DIMENSION(:) :: depth

REAL*4,ALLOCATABLE,DIMENSION(:,:) :: nav_lat, nav_lon

REAL*4,ALLOCATABLE,DIMENSION(:,:,:,:) :: var_rcp, var_his, mean_anom

REAL*4,ALLOCATABLE,DIMENSION(:,:,:,:) :: NN

REAL*4 :: misval

!--------------------------------------------------------

time = (/ 1., 2., 3., 4., 5., 6., 7., 8., 9., 10., 11., 12. /)

Nmod=33
Ngrid=3

ALLOCATE( modnam(Nmod), gridnam(Ngrid), Nvar(Ngrid) )
ALLOCATE( varnam1(Ngrid), varnam2(Ngrid) )

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
&         'FIO_ESM       ', &
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

gridnam = (/ 'gridT', 'gridU', 'gridV' /)

Nvar(1)=2
Nvar(2)=1
Nvar(3)=1

varnam1 = (/ 'votemper', 'vozocrtx', 'vomecrty' /)
varnam2 = (/ 'vosaline', 'not_used', 'not_used' /)

!------------------------------------------------------------

DO kgrid=1,Ngrid

DO kvar=1,Nvar(kgrid)

  if ( kvar .eq. 1 ) then
    varnam = varnam1(kgrid)
  elseif ( kvar .eq. 2 ) then
    varnam = varnam2(kgrid)
  else
    write(*,*) '~!@#$%^* ADAPT THIS SCRIPT FOR MORE VARIABLES >>>>>>> stop !!'
    stop
  endif

  write(*,*) '###########################################'
  write(*,*) '###########################################'
  write(*,*) 'variable ', TRIM(varnam)

  write(file_out,101) TRIM(varnam), TRIM(gridnam(kgrid))
  101 FORMAT('Oce_anom_ALL_CMIP5_',a,'_',a,'.nc')                
              
  write(*,*) 'Output file for this variable: ', TRIM(file_out)
                     
  write(varnam_anom,104) TRIM(varnam)
  104 FORMAT(a,'_anom')

  write(*,*) '(', TRIM(varnam_anom), ')'

  DO kmod=1,Nmod
       
    write(file_in_his,201) TRIM(modnam(kmod)), TRIM(gridnam(kgrid))
    201 FORMAT(a,'_hist_1989-2009_',a,'.nc')

    write(file_in_rcp,202) TRIM(modnam(kmod)), TRIM(gridnam(kgrid))
    202 FORMAT(a,'_rcp85_2080-2100_',a,'.nc')
                                                           
    !---------------------------------------                   
    ! Read historical climatology : 

    write(*,*) 'Reading ', TRIM(file_in_his)

    status = NF90_OPEN(TRIM(file_in_his),0,fidA)          
    call erreur(status,.TRUE.,"read historical climatology") 

    if ( kmod .eq. 1 ) then

         status = NF90_INQ_DIMID(fidA,"x",dimID_x)
         call erreur(status,.TRUE.,"inq_dimID_x")
         status = NF90_INQ_DIMID(fidA,"y",dimID_y)
         call erreur(status,.TRUE.,"inq_dimID_y")
         status = NF90_INQ_DIMID(fidA,"deptht",dimID_depth)
         if ( status .ne. 0 ) status = NF90_INQ_DIMID(fidA,"depthu",dimID_depth)
         if ( status .ne. 0 ) status = NF90_INQ_DIMID(fidA,"depthv",dimID_depth)
         call erreur(status,.TRUE.,"inq_dimID_depth")
         status = NF90_INQ_DIMID(fidA,"time_counter",dimID_time)
         call erreur(status,.TRUE.,"inq_dimID_time_counter")

         status = NF90_INQUIRE_DIMENSION(fidA,dimID_x,len=mx)
         call erreur(status,.TRUE.,"inq_dim_x")
         status = NF90_INQUIRE_DIMENSION(fidA,dimID_y,len=my)
         call erreur(status,.TRUE.,"inq_dim_y")
         status = NF90_INQUIRE_DIMENSION(fidA,dimID_depth,len=mdepth)
         call erreur(status,.TRUE.,"inq_dim_deptht")
         status = NF90_INQUIRE_DIMENSION(fidA,dimID_time,len=mtime)
         call erreur(status,.TRUE.,"inq_dim_time_counter")

         write(*,*) '(mx,my,mdepth,mtime) = ', mx, my,mdepth,  mtime

         ALLOCATE(  nav_lat(mx,my)  ) 
         ALLOCATE(  nav_lon(mx,my)  ) 
         ALLOCATE(  depth(mdepth)   )
         ALLOCATE(  var_rcp(mx,my,mdepth,mtime)  ) 
         ALLOCATE(  var_his(mx,my,mdepth,mtime)  )
         ALLOCATE(  mean_anom(mx,my,mdepth,mtime)  )
         ALLOCATE(  NN(mx,my,mdepth,mtime)  ) 

         mean_anom(:,:,:,:) = 0.0
         NN(:,:,:,:) = 0

         status = NF90_INQ_VARID(fidA,"nav_lon",nav_lon_ID)
         call erreur(status,.TRUE.,"inq_nav_lon_ID")
         status = NF90_INQ_VARID(fidA,"nav_lat",nav_lat_ID)
         call erreur(status,.TRUE.,"inq_nav_lat_ID")
         status = NF90_INQ_VARID(fidA,"deptht",depth_ID)
         if ( status .ne. 0 ) status = NF90_INQ_VARID(fidA,"depthu",depth_ID)
         if ( status .ne. 0 ) status = NF90_INQ_VARID(fidA,"depthv",depth_ID)
         call erreur(status,.TRUE.,"inq_depth_ID")

         status = NF90_GET_VAR(fidA,nav_lon_ID,nav_lon)
         call erreur(status,.TRUE.,"getvar_nav_lon")
         status = NF90_GET_VAR(fidA,nav_lat_ID,nav_lat)
         call erreur(status,.TRUE.,"getvar_nav_lat")
         status = NF90_GET_VAR(fidA,depth_ID,depth)
         call erreur(status,.TRUE.,"getvar_depth")

         write(*,*) 'depth = ', depth(1), depth(2), depth(30), depth(74), depth(75)

    endif

    status = NF90_INQ_VARID(fidA,TRIM(varnam),var_his_ID)
    call erreur(status,.TRUE.,"inq_var_his_ID")

    status = NF90_GET_VAR(fidA,var_his_ID,var_his)
    call erreur(status,.TRUE.,"getvar_var_his")
                                                  
    status = NF90_GET_ATT(fidA,var_his_ID,"missing_value",misval)
    call erreur(status,.TRUE.,"get_att_missval_ID")

    status = NF90_CLOSE(fidA)                      
    call erreur(status,.TRUE.,"fin_lecture_historical")     
                                  
    !---------------------------------------                   
    ! Read rcp85 climatology : 

    write(*,*) 'Reading ', TRIM(file_in_rcp)

    status = NF90_OPEN(TRIM(file_in_rcp),0,fidB)
    call erreur(status,.TRUE.,"read rcp85 climatology")
                            
    status = NF90_INQ_VARID(fidB,TRIM(varnam),var_rcp_ID)
    call erreur(status,.TRUE.,"inq_var_rcp_ID")

    status = NF90_GET_VAR(fidB,var_rcp_ID,var_rcp)
    call erreur(status,.TRUE.,"getvar_var_rcp")

    status = NF90_CLOSE(fidB)
    call erreur(status,.TRUE.,"fin_lecture_rcp85")

    !---------------------------------------                      

    do ki=1,mx
    do kj=1,my
    do kk=1,mdepth
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

  do ki=1,mx
  do kj=1,my
  do kk=1,mdepth
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
  call erreur(status,.TRUE.,'create output file') 
 
  status = NF90_DEF_DIM(fidM,"x",mx,dimID_x)
  call erreur(status,.TRUE.,"def_dimID_x")
  status = NF90_DEF_DIM(fidM,"y",my,dimID_y)
  call erreur(status,.TRUE.,"def_dimID_y")
  status = NF90_DEF_DIM(fidM,"z",mdepth,dimID_z)
  call erreur(status,.TRUE.,"def_dimID_depth")
  status = NF90_DEF_DIM(fidM,"time",mtime,dimID_time2)
  call erreur(status,.TRUE.,"def_dimID_time")

  status = NF90_DEF_VAR(fidM,"nav_lon",NF90_FLOAT,(/dimID_x,dimID_y/),nav_lon_ID)
  call erreur(status,.TRUE.,"def_var_nav_lon_ID")
  status = NF90_DEF_VAR(fidM,"nav_lat",NF90_FLOAT,(/dimID_x,dimID_y/),nav_lat_ID)
  call erreur(status,.TRUE.,"def_var_nav_lat_ID")
  status = NF90_DEF_VAR(fidM,"z",NF90_FLOAT,(/dimID_z/),depth_ID)
  call erreur(status,.TRUE.,"def_var_depth_ID")
  status = NF90_DEF_VAR(fidM,"time",NF90_FLOAT,(/dimID_time2/),time_ID)
  call erreur(status,.TRUE.,"def_var_time_ID")
  status = NF90_DEF_VAR(fidM,TRIM(varnam_anom),NF90_FLOAT,(/dimID_x,dimID_y,dimID_z,dimID_time2/),var_anom_ID)
  call erreur(status,.TRUE.,"def_var_anomaly_ID")

  if ( kgrid .eq. 1 ) then
    if ( kvar .eq. 1 ) then
      status = NF90_PUT_ATT(fidM,var_anom_ID,"standard_name","Oce Temp anom")
      call erreur(status,.TRUE.,"put_att_var_anom_ID")
      status = NF90_PUT_ATT(fidM,var_anom_ID,"long_name","Ocean Temperature Anomaly = (2080-2100)_rcp85 - (1989-2009)_hist")
      call erreur(status,.TRUE.,"put_att_var_anom_ID")
      status = NF90_PUT_ATT(fidM,var_anom_ID,"units","K")
      call erreur(status,.TRUE.,"put_att_var_anom_ID")
    elseif ( kvar .eq. 2 ) then
      status = NF90_PUT_ATT(fidM,var_anom_ID,"standard_name","Oce Sal anom")
      call erreur(status,.TRUE.,"put_att_var_anom_ID")
      status = NF90_PUT_ATT(fidM,var_anom_ID,"long_name","Ocean Salinity Anomaly = (2080-2100)_rcp85 - (1989-2009)_hist")
      call erreur(status,.TRUE.,"put_att_var_anom_ID")
      status = NF90_PUT_ATT(fidM,var_anom_ID,"units","psu")
      call erreur(status,.TRUE.,"put_att_var_anom_ID")
    else
      write(*,*) '~!@#$%^* kvar value not defined, check script >>>>>>> stop !!'
      stop
    endif
  elseif ( kgrid .eq. 2 ) then
      status = NF90_PUT_ATT(fidM,var_anom_ID,"standard_name","uoce anom")
      call erreur(status,.TRUE.,"put_att_var_anom_ID")
      status = NF90_PUT_ATT(fidM,var_anom_ID,"long_name","Ocean zonal velocity Anomaly = (2080-2100)_rcp85 - (1989-2009)_hist")
      call erreur(status,.TRUE.,"put_att_var_anom_ID")
      status = NF90_PUT_ATT(fidM,var_anom_ID,"units","m/s")
      call erreur(status,.TRUE.,"put_att_var_anom_ID")
  elseif ( kgrid .eq. 3 ) then
      status = NF90_PUT_ATT(fidM,var_anom_ID,"standard_name","voce anom")
      call erreur(status,.TRUE.,"put_att_var_anom_ID")
      status = NF90_PUT_ATT(fidM,var_anom_ID,"long_name","Ocean meridional velocity Anomaly = (2080-2100)_rcp85 - (1989-2009)_hist")
      call erreur(status,.TRUE.,"put_att_var_anom_ID")
      status = NF90_PUT_ATT(fidM,var_anom_ID,"units","m/s")
      call erreur(status,.TRUE.,"put_att_var_anom_ID")
  else
    write(*,*) '~!@#$%^* kgrid value not defined, check script >>>>>>> stop !!'
    stop
  endif
  status = NF90_PUT_ATT(fidM,var_anom_ID,"missing_value",misval)
  call erreur(status,.TRUE.,"put_att_var_anom_ID")
  status = NF90_PUT_ATT(fidM,var_anom_ID,"_FillValue",misval)
  call erreur(status,.TRUE.,"put_att_var_anom_ID")

  status = NF90_PUT_ATT(fidM,nav_lon_ID,"units","degrees_east")
  call erreur(status,.TRUE.,"put_att_nav_lon_ID")
  status = NF90_PUT_ATT(fidM,nav_lon_ID,"valid_min",-180.)
  call erreur(status,.TRUE.,"put_att_nav_lon_ID")
  status = NF90_PUT_ATT(fidM,nav_lon_ID,"valid_max",180.)
  call erreur(status,.TRUE.,"put_att_nav_lon_ID")
  status = NF90_PUT_ATT(fidM,nav_lon_ID,"long_name","Longitude")
  call erreur(status,.TRUE.,"put_att_nav_lon_ID")
  status = NF90_PUT_ATT(fidM,nav_lon_ID,"nav_model","Default grid")
  call erreur(status,.TRUE.,"put_att_nav_lon_ID")
  status = NF90_PUT_ATT(fidM,nav_lat_ID,"units","degrees_north")
  call erreur(status,.TRUE.,"put_att_nav_lat_ID")
  status = NF90_PUT_ATT(fidM,nav_lat_ID,"valid_min",-77.01048)
  call erreur(status,.TRUE.,"put_att_nav_lat_ID")
  status = NF90_PUT_ATT(fidM,nav_lat_ID,"valid_max",89.94787)
  call erreur(status,.TRUE.,"put_att_nav_lat_ID")
  status = NF90_PUT_ATT(fidM,nav_lat_ID,"long_name","Latitude")
  call erreur(status,.TRUE.,"put_att_nav_lat_ID")
  status = NF90_PUT_ATT(fidM,nav_lat_ID,"nav_model","Default grid")
  call erreur(status,.TRUE.,"put_att_nav_lat_ID")
  status = NF90_PUT_ATT(fidM,depth_ID,"units","m")
  call erreur(status,.TRUE.,"put_att_depth_ID")
  status = NF90_PUT_ATT(fidM,depth_ID,"positive","unknown")
  call erreur(status,.TRUE.,"put_att_depth_ID")
  status = NF90_PUT_ATT(fidM,depth_ID,"valid_min",0.)
  call erreur(status,.TRUE.,"put_att_depth_ID")
  status = NF90_PUT_ATT(fidM,depth_ID,"valid_max",75.)
  call erreur(status,.TRUE.,"put_att_depth_ID")
  status = NF90_PUT_ATT(fidM,depth_ID,"title","depth")
  call erreur(status,.TRUE.,"put_att_depth_ID")
  status = NF90_PUT_ATT(fidM,depth_ID,"long_name","Vertical levels")
  call erreur(status,.TRUE.,"put_att_depth_ID")
  status = NF90_PUT_ATT(fidM,time_ID,"units","calendar month in the climatology")
  call erreur(status,.TRUE.,"put_att_time_ID")
  status = NF90_PUT_ATT(fidM,time_ID,"title","Time")
  call erreur(status,.TRUE.,"put_att_time_ID")

  status = NF90_PUT_ATT(fidM,NF90_GLOBAL,"history","Created using calculate_mean_ALL_CMIP5_OCE.f90")
  call erreur(status,.TRUE.,"put_att_GLOBAL_ID")

  status = NF90_ENDDEF(fidM)
  call erreur(status,.TRUE.,"fin_definition") 

  write(*,*) 'size(depth) = ', SIZE(depth)

  status = NF90_PUT_VAR(fidM,nav_lon_ID,nav_lon)
  call erreur(status,.TRUE.,"var_nav_lon_ID")
  status = NF90_PUT_VAR(fidM,nav_lat_ID,nav_lat)
  call erreur(status,.TRUE.,"var_nav_lat_ID")
  status = NF90_PUT_VAR(fidM,depth_ID,depth)
  call erreur(status,.TRUE.,"var_depth_ID")
  status = NF90_PUT_VAR(fidM,time_ID,time)
  call erreur(status,.TRUE.,"var_time_ID")
  status = NF90_PUT_VAR(fidM,var_anom_ID,mean_anom)
  call erreur(status,.TRUE.,"var_anom_ID")

  status = NF90_CLOSE(fidM)
  call erreur(status,.TRUE.,"final")

  !-----

  DEALLOCATE( nav_lat, nav_lon, depth, var_rcp, var_his, mean_anom, NN )

ENDDO !! kvar

ENDDO !! kgrid

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
