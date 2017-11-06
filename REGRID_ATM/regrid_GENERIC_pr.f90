program modif                                         
                                                      
USE netcdf                                            
                                                      
IMPLICIT NONE                                         
                                                      
INTEGER :: fidGRID, status, dimID_lon, dimID_lat, mlon, mlat, lon_ID, lat_ID

CHARACTER(LEN=150) :: file_GRID

REAL*8,ALLOCATABLE,DIMENSION(:) :: lon, lat          

INTEGER :: fidA, dimID_bnds, dimID_lon_mod, dimID_lat_mod, dimID_time, mbnds, mlon_mod, mlat_mod, mtime, time_bnds_ID, time_ID, lon_bnds_ID, lon_mod_ID, lat_bnds_ID, lat_mod_ID, var_ID, var_his_ID, var_rcp_ID, fidM

CHARACTER(LEN=150) :: file_mod_his, file_mod_rcp, file_out

REAL*8,ALLOCATABLE,DIMENSION(:) :: time, lon_mod, lat_mod 

REAL*8,ALLOCATABLE,DIMENSION(:,:) :: time_bnds, lon_bnds, lat_bnds        

REAL*4,ALLOCATABLE,DIMENSION(:,:,:) :: var_his, var_rcp, his_regrid, rcp_regrid, Zhis, Zrcp

REAL*4 :: FillValue, missing_value, aa, bb

CHARACTER(LEN=20) :: varnam, varnam_his, varnam_rcp, modnam

CHARACTER(LEN=100) :: long_name, standard_name, long_name_his, standard_name_his, long_name_rcp, standard_name_rcp

INTEGER :: yeari, yearf, ki, kj, kk, km0, kk_mod, ki_mod, kj_mod, kvar, mm

INTEGER,ALLOCATABLE,DIMENSION(:) :: iminf, imsup, jminf, jmsup

REAL*4,DIMENSION(12) :: tmpinf, tmpsup

!---------------------------------------

modnam = 'MMMM'

file_GRID = 'ERAI_grid.nc'

!---------------------------------------                   
! Read netcdf grid file : 

status = NF90_OPEN(TRIM(file_GRID),0,fidGRID)          
call erreur(status,.TRUE.,"read") 

  status = NF90_INQ_DIMID(fidGRID,"lon",dimID_lon)
  call erreur(status,.TRUE.,"inq_dimID_lon")
  status = NF90_INQ_DIMID(fidGRID,"lat",dimID_lat)
  call erreur(status,.TRUE.,"inq_dimID_lat")
  
  status = NF90_INQUIRE_DIMENSION(fidGRID,dimID_lon,len=mlon)
  call erreur(status,.TRUE.,"inq_dim_lon")
  status = NF90_INQUIRE_DIMENSION(fidGRID,dimID_lat,len=mlat)
  call erreur(status,.TRUE.,"inq_dim_lat")
      
  ALLOCATE(  lon(mlon)  ) 
  ALLOCATE(  lat(mlat)  ) 
        
  status = NF90_INQ_VARID(fidGRID,"lon",lon_ID)
  call erreur(status,.TRUE.,"inq_lon_ID")
  status = NF90_INQ_VARID(fidGRID,"lat",lat_ID)
  call erreur(status,.TRUE.,"inq_lat_ID")
 
  status = NF90_GET_VAR(fidGRID,lon_ID,lon)
  call erreur(status,.TRUE.,"getvar_lon")
  status = NF90_GET_VAR(fidGRID,lat_ID,lat)
  call erreur(status,.TRUE.,"getvar_lat")

status = NF90_CLOSE(fidGRID)                      
call erreur(status,.TRUE.,"fin_lecture")     

!---------------

DO kvar=1,1

  SELECT CASE (kvar)
    CASE(1)
      varnam = 'pr'
    CASE DEFAULT
      varnam = 'xxxxxxxx'
  END SELECT

  write(file_mod_his,101) TRIM(varnam), TRIM(modnam)
  write(file_mod_rcp,102) TRIM(varnam), TRIM(modnam)
  101 FORMAT('../CLIMO_ATM/',a'_',a,'_climo_1989_2009.nc')
  102 FORMAT('../CLIMO_ATM/',a'_',a,'_climo_2080_2100.nc')

  write(file_out,103) TRIM(varnam), TRIM(modnam)
  103 FORMAT(a,'_',a,'_climo_ERAIgrid.nc')

  write(varnam_his,201) TRIM(varnam)
  201 FORMAT(a,'_his')
  write(varnam_rcp,202) TRIM(varnam)
  202 FORMAT(a,'_rcp')

  !--------------------------------------------------              
  ! Read netcdf model climatological 1989-2009 file :
                                                
  write(*,*) 'Reading ', TRIM(file_mod_his)           
  status = NF90_OPEN(TRIM(file_mod_his),0,fidA)          
  call erreur(status,.TRUE.,"read 1989-2009 file") 
                                                           
         status = NF90_INQ_DIMID(fidA,"bnds",dimID_bnds)
         call erreur(status,.TRUE.,"inq_dimID_bnds")
         status = NF90_INQ_DIMID(fidA,"lon",dimID_lon_mod)
         call erreur(status,.TRUE.,"inq_dimID_lon_mod")
         status = NF90_INQ_DIMID(fidA,"lat",dimID_lat_mod)
         call erreur(status,.TRUE.,"inq_dimID_lat_mod")
         status = NF90_INQ_DIMID(fidA,"time",dimID_time)
         call erreur(status,.TRUE.,"inq_dimID_time")
                                                               
         status = NF90_INQUIRE_DIMENSION(fidA,dimID_bnds,len=mbnds)
         call erreur(status,.TRUE.,"inq_dim_bnds")
         status = NF90_INQUIRE_DIMENSION(fidA,dimID_lon_mod,len=mlon_mod)
         call erreur(status,.TRUE.,"inq_dim_lon")
         status = NF90_INQUIRE_DIMENSION(fidA,dimID_lat_mod,len=mlat_mod)
         call erreur(status,.TRUE.,"inq_dim_lat")
         status = NF90_INQUIRE_DIMENSION(fidA,dimID_time,len=mtime)
         call erreur(status,.TRUE.,"inq_dim_time")
 
         write(*,*) '    > dimensions :', mlon_mod, mlat_mod, mtime                               

         ALLOCATE(  time_bnds(mbnds,mtime)  ) 
         ALLOCATE(  time(mtime)  ) 
         ALLOCATE(  lon_mod(mlon_mod)  ) 
         ALLOCATE(  lat_mod(mlat_mod)  ) 
         ALLOCATE(  var_his(mlon_mod,mlat_mod,mtime)  )          
                        
         status = NF90_INQ_VARID(fidA,"time_bnds",time_bnds_ID)
         call erreur(status,.TRUE.,"inq_time_bnds_ID")
         status = NF90_INQ_VARID(fidA,"time",time_ID)
         call erreur(status,.TRUE.,"inq_time_ID")
         status = NF90_INQ_VARID(fidA,"lon",lon_mod_ID)
         call erreur(status,.TRUE.,"inq_lon_mod_ID")
         status = NF90_INQ_VARID(fidA,"lat",lat_mod_ID)
         call erreur(status,.TRUE.,"inq_lat_mod_ID")
         status = NF90_INQ_VARID(fidA,TRIM(varnam),var_ID)
         call erreur(status,.TRUE.,"inq_var_ID")
                                                              
         status = NF90_GET_VAR(fidA,time_bnds_ID,time_bnds)
         call erreur(status,.TRUE.,"getvar_time_bnds")
         status = NF90_GET_VAR(fidA,time_ID,time)
         call erreur(status,.TRUE.,"getvar_time")
         status = NF90_GET_VAR(fidA,lon_mod_ID,lon_mod)
         call erreur(status,.TRUE.,"getvar_lon_mod")
         status = NF90_GET_VAR(fidA,lat_mod_ID,lat_mod)
         call erreur(status,.TRUE.,"getvar_lat_mod")
         status = NF90_GET_VAR(fidA,var_ID,var_his)
         call erreur(status,.TRUE.,"getvar_var_his")

         status = NF90_GET_ATT(fidA,var_ID,"_FillValue",FillValue)
         if ( status .ne. 0 ) FillValue=1.e+20
         !call erreur(status,.TRUE.,"put_att1_var_ID")
         status = NF90_GET_ATT(fidA,var_ID,"missing_value",missing_value)
         if ( status .ne. 0 ) missing_value=1.e+20
         !call erreur(status,.TRUE.,"put_att2_var_ID")
         status = NF90_GET_ATT(fidA,var_ID,"long_name",long_name)
         call erreur(status,.TRUE.,"put_att3_var_ID")
         status = NF90_GET_ATT(fidA,var_ID,"standard_name",standard_name)
         call erreur(status,.TRUE.,"put_att4_var_ID")
        
         write(long_name_his,301) TRIM(long_name)
         write(standard_name_his,301) TRIM(standard_name)
         301 FORMAT(a,' over 1989-2009 (hist & rcp85)')
         write(long_name_rcp,302) TRIM(long_name)
         write(standard_name_rcp,302) TRIM(standard_name)
         302 FORMAT(a,' over 2080-2100 (rcp85)')
 
  status = NF90_CLOSE(fidA)                      
  call erreur(status,.TRUE.,"fin_lecture_his")     

  !--------------------------------------------------              
  ! Read netcdf model climatological 2080-2100 file :
                   
  write(*,*) 'Reading ', TRIM(file_mod_rcp)                                        
  status = NF90_OPEN(TRIM(file_mod_rcp),0,fidA)          
  call erreur(status,.TRUE.,"read 2080-2100 file") 
                                                           
         ALLOCATE(  var_rcp(mlon_mod,mlat_mod,mtime)  )          
                        
         status = NF90_INQ_VARID(fidA,TRIM(varnam),var_rcp_ID)
         call erreur(status,.TRUE.,"inq_var_rcp_ID")
                                                              
         status = NF90_GET_VAR(fidA,var_rcp_ID,var_rcp)
         call erreur(status,.TRUE.,"getvar_var_rcp")

  status = NF90_CLOSE(fidA)                      
  call erreur(status,.TRUE.,"fin_lecture_his")     
                                                              
  !---------------------------------------                      
  ! Modification of the variables :                             
    
  ALLOCATE( his_regrid(mlon,mlat,mtime), rcp_regrid(mlon,mlat,mtime) )
  ALLOCATE( iminf(mlon), imsup(mlon), jminf(mlat), jmsup(mlat) )

  !- interpolate horizontally : 
  iminf(:)=0 ; imsup(:)=0
  jminf(:)=0 ; jmsup(:)=0
  do ki=1,mlon
    do ki_mod=1,mlon_mod-1
      if ( lon(ki) .ge. lon_mod(ki_mod) .and. lon(ki) .lt. lon_mod(ki_mod+1) ) then
        iminf(ki)=ki_mod
        imsup(ki)=ki_mod+1
        exit
      endif
    enddo
    if ( lon(ki) .ge. lon_mod(mlon_mod) .or. lon(ki) .lt. lon_mod(1) ) then
      iminf(ki)=mlon_mod
      imsup(ki)=1
    endif
  enddo                       
  !-
  do kj=1,mlat
    do kj_mod=1,mlat_mod-1
      if ( lat(kj) .ge. lat_mod(kj_mod) .and. lat(kj) .le. lat_mod(kj_mod+1) ) then
        jminf(kj)=kj_mod
        jmsup(kj)=kj_mod+1
        exit
      endif
    enddo
  enddo
  !-
  do ki=1,mlon
  do kj=1,mlat
   do mm=1,12
    if ( var_his(iminf(ki),jminf(kj),mm) .ne. missing_value .and. var_his(imsup(ki),jminf(kj),mm) .ne. missing_value ) then
      aa = ( lon_mod(imsup(ki)) - lon(ki) ) / ( lon_mod(imsup(ki)) - lon_mod(iminf(ki)) )
      bb = ( lon(ki) - lon_mod(iminf(ki)) ) / ( lon_mod(imsup(ki)) - lon_mod(iminf(ki)) )
    elseif ( var_his(iminf(ki),jminf(kj),mm) .ne. missing_value ) then
      aa = 1.0
      bb = 0.0
    else
      aa = 0.0
      bb = 1.0
    endif
    tmpinf(mm) = aa * var_his(iminf(ki),jminf(kj),mm) + bb * var_his(imsup(ki),jminf(kj),mm)
    !-
    if ( var_his(iminf(ki),jmsup(kj),mm) .ne. missing_value .and. var_his(imsup(ki),jmsup(kj),mm) .ne. missing_value ) then
      aa = ( lon_mod(imsup(ki)) - lon(ki) ) / ( lon_mod(imsup(ki)) - lon_mod(iminf(ki)) )
      bb = ( lon(ki) - lon_mod(iminf(ki)) ) / ( lon_mod(imsup(ki)) - lon_mod(iminf(ki)) )
    elseif ( var_his(iminf(ki),jmsup(kj),mm) .ne. missing_value ) then
      aa = 1.0
      bb = 0.0
    else
      aa = 0.0
      bb = 1.0
    endif
    tmpsup(mm) = aa * var_his(iminf(ki),jmsup(kj),mm) + bb * var_his(imsup(ki),jmsup(kj),mm)
    !-
    if ( tmpinf(mm) .ne. missing_value .and. tmpsup(mm) .ne. missing_value ) then
      aa = ( lat_mod(jmsup(kj)) - lat(kj) ) / ( lat_mod(jmsup(kj)) - lat_mod(jminf(kj)) )
      bb = ( lat(kj) - lat_mod(jminf(kj)) ) / ( lat_mod(jmsup(kj)) - lat_mod(jminf(kj)) )
    elseif ( tmpinf(mm) .ne. missing_value ) then
      aa = 1.0
      bb = 0.0
    else
      aa = 0.0
      bb = 1.0
    endif
    his_regrid(ki,kj,mm) = aa * tmpinf(mm) + bb * tmpsup(mm)
   enddo
  enddo
  enddo
  !-
  do ki=1,mlon
  do kj=1,mlat
   do mm=1,12
    if ( var_rcp(iminf(ki),jminf(kj),mm) .ne. missing_value .and. var_rcp(imsup(ki),jminf(kj),mm) .ne. missing_value ) then
      aa = ( lon_mod(imsup(ki)) - lon(ki) ) / ( lon_mod(imsup(ki)) - lon_mod(iminf(ki)) )
      bb = ( lon(ki) - lon_mod(iminf(ki)) ) / ( lon_mod(imsup(ki)) - lon_mod(iminf(ki)) )
    elseif ( var_rcp(iminf(ki),jminf(kj),mm) .ne. missing_value ) then
      aa = 1.0
      bb = 0.0
    else
      aa = 0.0
      bb = 1.0
    endif
    tmpinf(mm) = aa * var_rcp(iminf(ki),jminf(kj),mm) + bb * var_rcp(imsup(ki),jminf(kj),mm)
    !-
    if ( var_rcp(iminf(ki),jmsup(kj),mm) .ne. missing_value .and. var_rcp(imsup(ki),jmsup(kj),mm) .ne. missing_value ) then
      aa = ( lon_mod(imsup(ki)) - lon(ki) ) / ( lon_mod(imsup(ki)) - lon_mod(iminf(ki)) )
      bb = ( lon(ki) - lon_mod(iminf(ki)) ) / ( lon_mod(imsup(ki)) - lon_mod(iminf(ki)) )
    elseif ( var_rcp(iminf(ki),jmsup(kj),mm) .ne. missing_value ) then
      aa = 1.0
      bb = 0.0
    else
      aa = 0.0
      bb = 1.0
    endif
    tmpsup(mm) = aa * var_rcp(iminf(ki),jmsup(kj),mm) + bb * var_rcp(imsup(ki),jmsup(kj),mm)
    !-
    if ( tmpinf(mm) .ne. missing_value .and. tmpsup(mm) .ne. missing_value ) then
      aa = ( lat_mod(jmsup(kj)) - lat(kj) ) / ( lat_mod(jmsup(kj)) - lat_mod(jminf(kj)) )
      bb = ( lat(kj) - lat_mod(jminf(kj)) ) / ( lat_mod(jmsup(kj)) - lat_mod(jminf(kj)) )
    elseif ( tmpinf(mm) .ne. missing_value ) then
      aa = 1.0
      bb = 0.0
    else
      aa = 0.0
      bb = 1.0
    endif
    rcp_regrid(ki,kj,mm) = aa * tmpinf(mm) + bb * tmpsup(mm)
   enddo
  enddo
  enddo
                                                      
  !---------------------------------------                      
  ! Writing new netcdf file :                                   
                                               
   write(*,*) 'Writing ', TRIM(file_out)               
   status = NF90_CREATE(TRIM(file_out),NF90_NOCLOBBER,fidM)
   call erreur(status,.TRUE.,'create')                     
                                                                
         status = NF90_DEF_DIM(fidM,"bnds",mbnds,dimID_bnds)
         call erreur(status,.TRUE.,"def_dimID_bnds")
         status = NF90_DEF_DIM(fidM,"lon",mlon,dimID_lon)
         call erreur(status,.TRUE.,"def_dimID_lon")
         status = NF90_DEF_DIM(fidM,"lat",mlat,dimID_lat)
         call erreur(status,.TRUE.,"def_dimID_lat")
         status = NF90_DEF_DIM(fidM,"time",NF90_UNLIMITED,dimID_time)
         call erreur(status,.TRUE.,"def_dimID_time")
                     
         status = NF90_DEF_VAR(fidM,"time_bnds",NF90_DOUBLE,(/dimID_bnds,dimID_time/),time_bnds_ID)
         call erreur(status,.TRUE.,"def_var_time_bnds_ID")
         status = NF90_DEF_VAR(fidM,"time",NF90_DOUBLE,(/dimID_time/),time_ID)
         call erreur(status,.TRUE.,"def_var_time_ID")
         status = NF90_DEF_VAR(fidM,"lon",NF90_DOUBLE,(/dimID_lon/),lon_ID)
         call erreur(status,.TRUE.,"def_var_lon_ID")
         status = NF90_DEF_VAR(fidM,"lat",NF90_DOUBLE,(/dimID_lat/),lat_ID)
         call erreur(status,.TRUE.,"def_var_lat_ID")
         status = NF90_DEF_VAR(fidM,TRIM(varnam_his),NF90_FLOAT,(/dimID_lon,dimID_lat,dimID_time/),var_his_ID)
         call erreur(status,.TRUE.,"def_var_his_ID")
         status = NF90_DEF_VAR(fidM,TRIM(varnam_rcp),NF90_FLOAT,(/dimID_lon,dimID_lat,dimID_time/),var_rcp_ID)
         call erreur(status,.TRUE.,"def_var_rcp_ID")                 
    
         status = NF90_PUT_ATT(fidM,time_ID,"standard_name","time")
         call erreur(status,.TRUE.,"put_att_time_ID")
         status = NF90_PUT_ATT(fidM,time_ID,"long_name","time")
         call erreur(status,.TRUE.,"put_att_time_ID")
         status = NF90_PUT_ATT(fidM,time_ID,"axis","T")
         call erreur(status,.TRUE.,"put_att_time_ID")
         status = NF90_PUT_ATT(fidM,time_ID,"calendar","proleptic_gregorian")
         call erreur(status,.TRUE.,"put_att_time_ID")
         status = NF90_PUT_ATT(fidM,time_ID,"units","days since 0001-01-01")
         call erreur(status,.TRUE.,"put_att_time_ID")
         status = NF90_PUT_ATT(fidM,time_ID,"bounds","time_bnds")
         call erreur(status,.TRUE.,"put_att_time_ID")
         status = NF90_PUT_ATT(fidM,lon_ID,"standard_name","longitude")
         call erreur(status,.TRUE.,"put_att_lon_ID")
         status = NF90_PUT_ATT(fidM,lon_ID,"long_name","longitude")
         call erreur(status,.TRUE.,"put_att_lon_ID")
         status = NF90_PUT_ATT(fidM,lon_ID,"axis","X")
         call erreur(status,.TRUE.,"put_att_lon_ID")
         status = NF90_PUT_ATT(fidM,lon_ID,"units","degrees_east")
         call erreur(status,.TRUE.,"put_att_lon_ID")
         status = NF90_PUT_ATT(fidM,lat_ID,"standard_name","latitude")
         call erreur(status,.TRUE.,"put_att_lat_ID")
         status = NF90_PUT_ATT(fidM,lat_ID,"long_name","latitude")
         call erreur(status,.TRUE.,"put_att_lat_ID")
         status = NF90_PUT_ATT(fidM,lat_ID,"axis","Y")
         call erreur(status,.TRUE.,"put_att_lat_ID")
         status = NF90_PUT_ATT(fidM,lat_ID,"units","degrees_north")
         call erreur(status,.TRUE.,"put_att_lat_ID")
         status = NF90_PUT_ATT(fidM,var_his_ID,"_FillValue",FillValue)
         call erreur(status,.TRUE.,"put_att_var_his_ID")
         status = NF90_PUT_ATT(fidM,var_his_ID,"missing_value",missing_value)
         call erreur(status,.TRUE.,"put_att_var_his_ID")
         status = NF90_PUT_ATT(fidM,var_his_ID,"long_name",TRIM(long_name_his))
         call erreur(status,.TRUE.,"put_att_var_his_ID")
         status = NF90_PUT_ATT(fidM,var_his_ID,"standard_name",TRIM(standard_name_his))
         call erreur(status,.TRUE.,"put_att_var_his_ID")
         status = NF90_PUT_ATT(fidM,var_rcp_ID,"_FillValue",FillValue)
         call erreur(status,.TRUE.,"put_att_var_rcp_ID")
         status = NF90_PUT_ATT(fidM,var_rcp_ID,"missing_value",missing_value)
         call erreur(status,.TRUE.,"put_att_var_rcp_ID")
         status = NF90_PUT_ATT(fidM,var_rcp_ID,"long_name",TRIM(long_name_rcp))
         call erreur(status,.TRUE.,"put_att_var_rcp_ID")
         status = NF90_PUT_ATT(fidM,var_rcp_ID,"standard_name",TRIM(standard_name_rcp))
         call erreur(status,.TRUE.,"put_att_var_rcp_ID")

         status = NF90_PUT_ATT(fidM,NF90_GLOBAL,"history","interpolated from CMIP5 to ERAI grid using regrid_GENERIC_SRF.f90")
         call erreur(status,.TRUE.,"put_att_GLOBAL_ID")
                                                      
         status = NF90_ENDDEF(fidM)                   
         call erreur(status,.TRUE.,"fin_definition") 
                                                      
         status = NF90_PUT_VAR(fidM,time_bnds_ID,time_bnds)
         call erreur(status,.TRUE.,"var_time_bnds_ID")
         status = NF90_PUT_VAR(fidM,time_ID,time)
         call erreur(status,.TRUE.,"var_time_ID")
         status = NF90_PUT_VAR(fidM,lon_ID,lon)
         call erreur(status,.TRUE.,"var_lon_ID")
         status = NF90_PUT_VAR(fidM,lat_ID,lat)
         call erreur(status,.TRUE.,"var_lat_ID")
         status = NF90_PUT_VAR(fidM,var_his_ID,his_regrid)
         call erreur(status,.TRUE.,"var_var_his_ID")
         status = NF90_PUT_VAR(fidM,var_rcp_ID,rcp_regrid)
         call erreur(status,.TRUE.,"var_var_rcp_ID")
                                                      
    status = NF90_CLOSE(fidM)                    
    call erreur(status,.TRUE.,"final")         

    !----------

    DEALLOCATE( time_bnds, time, lon_mod, lat_mod )
    DEALLOCATE( var_his, var_rcp, his_regrid, rcp_regrid )
    DEALLOCATE( iminf, imsup, jminf, jmsup )

ENDDO


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
