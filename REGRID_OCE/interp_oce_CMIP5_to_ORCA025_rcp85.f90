program modif
!=================================================================================================
! N. JOURDAIN, LGGE-CNRS, JULY 2015
!
! Used to interpolate individual CMIP5 models (12-month climatology) onto the ORCA025 grid
!
!=================================================================================================
USE netcdf                                            

IMPLICIT NONE                                         

CHARACTER(LEN=6), ALLOCATABLE, DIMENSION(:) :: varnam

INTEGER :: fidCMIP5, dimID_time, dimID_bnds, dimID_lev, dimID_j, dimID_i, mtime, mbnds, mlev, mj, mi,     &
&          time_ID, lev_bnds_ID, lev_ID, var_CMIP5_ID, lon_ID, lat_ID, j_ID, i_ID, nsigma_ID, depth_c_ID, &
&          zlev_bnds_ID, sigma_ID, sigma_bnds_ID, rs, rz, rsmax, rzmax, irs, jrs, krs, ji, jj, kk, jiout, &
&          jjout, jkout, ivar, status, zzi, zzj, zzip1, zzjp1, zzim1, zzjm1, fidmshORCA025, dimID_t,      &
&          dimID_z, dimID_y, dimID_x, mt, mz, my, mx, vmask_out_ID, umask_out_ID, tmask_out_ID,           &
&          e3v_out_ID, e3u_out_ID, e2v_out_ID, e2u_out_ID, e2t_out_ID, e1v_out_ID, e1u_out_ID, e1t_out_ID,&
&          gphiv_out_ID, gphiu_out_ID, gphit_out_ID, glamv_out_ID, glamu_out_ID, glamt_out_ID, nav_lev_ID,&
&          nav_lat_ID, nav_lon_ID, fidM, fidT, fidU, fidV, depth_t_ID, vosaline_ID, votemper_ID,          &
&          vozocrtx_ID, vomecrty_ID, dimID_lon, dimID_lat, dimID_eta, jioutm1, jjoutm1, jioutp1, jjoutp1, &
&          zlev_ID, depth_ID, eta_ID, nsigma, perio, jm, ktop, kbot, kinc, kt, ziC, ziN, ziS, ziM, ziE,   &
&          ziW, zjC, zjN, zjS, zjM, zjE, zjW, dimID_time_counter, time_counter_ID, nvar

INTEGER*4,ALLOCATABLE,DIMENSION(:) :: j, i      

INTEGER*1,ALLOCATABLE,DIMENSION(:,:,:) :: mask, msk_CMIP5_L75, zmask_out

INTEGER*1,ALLOCATABLE,DIMENSION(:,:,:,:) :: vmask_out, umask_out, tmask_out

REAL*4,ALLOCATABLE,DIMENSION(:) :: nav_lev

REAL*8,ALLOCATABLE,DIMENSION(:) :: time, lev, lev_ref, zlev, sigma

!##1D !##FL REAL*4,ALLOCATABLE,DIMENSION(:) :: tmp_lon, tmp_lat
!##1D !##DB REAL*8,ALLOCATABLE,DIMENSION(:) :: tmp_lon, tmp_lat
!##2D !##FL REAL*4,ALLOCATABLE,DIMENSION(:,:) :: tmp_lon, tmp_lat
!##2D !##DB REAL*8,ALLOCATABLE,DIMENSION(:,:) :: tmp_lon, tmp_lat
!##2D !##FL REAL*4,ALLOCATABLE,DIMENSION(:) :: X, Y
!##2D !##DB REAL*8,ALLOCATABLE,DIMENSION(:) :: X, Y

REAL*4,ALLOCATABLE,DIMENSION(:,:) :: lon, lat, dilon, dilat, djlon, djlat, e1, e2,  &
&                                    angoutXt, angoutXu, angoutXv, angoutXz,        &
&                                    angoutYt, angoutYu, angoutYv, angoutYz,        &
&                                    e2v_out, e2u_out, e2t_out, e1v_out, e1u_out,   &
&                                    e1t_out, gphiv_out, gphiu_out, gphit_out,      &
&                                    glamv_out, glamu_out, glamt_out, glamz_out,    &
&                                    gphiz_out, zglamz_out, zglamt_out, zglamu_out, &
&                                    zglamv_out, e1z_out, e2z_out, nav_lat, nav_lon,&
&                                    depth, zlon, wlon                         

REAL*8,ALLOCATABLE,DIMENSION(:,:) :: lev_bnds, lev_bnds_ref, zlev_bnds, sigma_bnds

REAL*4,ALLOCATABLE,DIMENSION(:,:,:) :: e3, eta

REAL*4, DIMENSION(75) :: e3t_0_out, gdept_0_out

REAL*4, DIMENSION(12) :: time_counter

REAL*4,ALLOCATABLE,DIMENSION(:,:,:,:) :: var_CMIP5, var_CMIP5_L75, var_CMIP5_ORCA025, uo_CMIP5_ORCA025, &
&                                        vo_CMIP5_ORCA025, to_CMIP5_ORCA025, so_CMIP5_ORCA025 

REAL*4 :: tS, aS, tM, aM, tN, aN, aE, aW, aC, angleX, angleY, ra, rad, eps, missval, &
&         uo_missval, vo_missval, to_missval, so_missval, a1, a2, dist, distmin,     &
&         anginX, anginY, wgC, wgN, wgS, wgM, wgE, wgW 

REAL*8 :: depth_c

LOGICAL :: misig, lsigma, ll_bnds, lout, isitdone

CHARACTER(LEN=150) :: file_CMIP5, file_msh_ORCA025, file_out_gridT, file_out_gridU, file_out_gridV

CHARACTER(LEN=50) :: long_name, units, uo_units, vo_units, so_units, to_units

!=================================================================================================

nvar = 4
ALLOCATE( varnam(nvar) )
varnam = (/ 'thetao', 'so    ', 'uo    ', 'vo    ' /)

!- mesh mask of target grid :
file_msh_ORCA025 = 'mesh_mask_ORCA025.L75-MJM91.nc'

!- name of output files :
file_out_gridT = 'MMMM_rcp85_2080-2100_gridT.nc'
file_out_gridU = 'MMMM_rcp85_2080-2100_gridU.nc'
file_out_gridV = 'MMMM_rcp85_2080-2100_gridV.nc'

eps = 1.e-9  !-- to avoid division by zero

!- earth radius (meter), must agree with NEMO file phycst.F90
ra = 6371229.0

!- deg to rad conversion (same as phycst.F90)
rad = 3.141592653589793 / 180.0

gdept_0_out =                                                             & 
 (/ 0.50576, 1.555855, 2.667682, 3.85628, 5.140361, 6.543034, 8.092519,   &
    9.82275, 11.77368, 13.99104, 16.52532, 19.4298, 22.75762, 26.5583,    &
    30.87456, 35.7402, 41.18002, 47.21189, 53.85064, 61.11284, 69.02168,  &
    77.61116, 86.92943, 97.04131, 108.0303, 120, 133.0758, 147.4062,      &
    163.1645, 180.5499, 199.79, 221.1412, 244.8906, 271.3564, 300.8875,   &
    333.8628, 370.6885, 411.7939, 457.6256, 508.6399, 565.2923, 628.026,  &
    697.2587, 773.3683, 856.679, 947.4479, 1045.854, 1151.991, 1265.861,  &
    1387.377, 1516.364, 1652.568, 1795.671, 1945.296, 2101.027, 2262.422, &
    2429.025, 2600.38, 2776.039, 2955.57, 3138.565, 3324.641, 3513.446,   &
    3704.657, 3897.982, 4093.159, 4289.953, 4488.155, 4687.581, 4888.07,  &
    5089.479, 5291.683, 5494.575, 5698.061, 5902.058 /)

e3t_0_out =                                                               &
 (/ 1.023461, 1.078725, 1.147423, 1.23288, 1.339123, 1.470915, 1.633704,  &
    1.833421, 2.076076, 2.367091, 2.710405, 3.107473, 3.556467, 4.052056, &
    4.586102, 5.149281, 5.733272, 6.332855, 6.947313, 7.580885, 8.242412, &
    8.944528, 9.702774, 10.53488, 11.46031, 12.5, 13.67637, 15.01332,     &
    16.53633, 18.27247, 20.25036, 22.49991, 25.05199, 27.93773, 31.1877,  &
    34.83069, 38.89227, 43.39304, 48.34669, 53.75801, 59.62083, 65.91641, &
    72.61217, 79.66129, 87.00313, 94.56478, 102.2636, 110.0107, 117.7149, &
    125.287, 132.6439, 139.7121, 146.4299, 152.7498, 158.6383, 164.0757,  &
    169.0551, 173.5806, 177.6651, 181.3288, 184.5965, 187.4966, 190.0591, &
    192.3144, 194.2924, 196.0221, 197.5306, 198.8431, 199.9829, 200.9709, &
    201.826, 202.5652, 203.2034, 203.7539, 204.2284 /)

!===============================================================================================
!- Read target grid (ORCA025 mesh_mask)

status = NF90_OPEN(TRIM(file_msh_ORCA025),0,fidmshORCA025)          
call erreur(status,.TRUE.,"read_mesh_mask_ORCA025") 

status = NF90_INQ_DIMID(fidmshORCA025,"t",dimID_t)
call erreur(status,.TRUE.,"inq_dimID_t")
status = NF90_INQ_DIMID(fidmshORCA025,"z",dimID_z)
call erreur(status,.TRUE.,"inq_dimID_z")
status = NF90_INQ_DIMID(fidmshORCA025,"y",dimID_y)
call erreur(status,.TRUE.,"inq_dimID_y")
status = NF90_INQ_DIMID(fidmshORCA025,"x",dimID_x)
call erreur(status,.TRUE.,"inq_dimID_x")

status = NF90_INQUIRE_DIMENSION(fidmshORCA025,dimID_t,len=mt)
call erreur(status,.TRUE.,"inq_dim_t")
status = NF90_INQUIRE_DIMENSION(fidmshORCA025,dimID_z,len=mz)
call erreur(status,.TRUE.,"inq_dim_z")
status = NF90_INQUIRE_DIMENSION(fidmshORCA025,dimID_y,len=my)
call erreur(status,.TRUE.,"inq_dim_y")
status = NF90_INQUIRE_DIMENSION(fidmshORCA025,dimID_x,len=mx)
call erreur(status,.TRUE.,"inq_dim_x")

ALLOCATE(  vmask_out(mx,my,mz,mt)  ) 
ALLOCATE(  umask_out(mx,my,mz,mt)  ) 
ALLOCATE(  tmask_out(mx,my,mz,mt)  ) 
ALLOCATE(  zmask_out(mx,my,mz)  ) 
ALLOCATE(  e2v_out(mx,my)  ) 
ALLOCATE(  e2u_out(mx,my)  ) 
ALLOCATE(  e2t_out(mx,my)  ) 
ALLOCATE(  e2z_out(mx,my)  )
ALLOCATE(  e1v_out(mx,my)  ) 
ALLOCATE(  e1u_out(mx,my)  ) 
ALLOCATE(  e1t_out(mx,my)  )
ALLOCATE(  e1z_out(mx,my)  )
ALLOCATE(  gphiv_out(mx,my)  ) 
ALLOCATE(  gphiu_out(mx,my)  ) 
ALLOCATE(  gphit_out(mx,my)  )
ALLOCATE(  gphiz_out(mx,my)  ) 
ALLOCATE(  glamv_out(mx,my)  ) 
ALLOCATE(  glamu_out(mx,my)  ) 
ALLOCATE(  glamt_out(mx,my)  )
ALLOCATE(  glamz_out(mx,my)  )
ALLOCATE(  zglamz_out(mx,my)  )
ALLOCATE(  zglamv_out(mx,my)  )
ALLOCATE(  zglamu_out(mx,my)  )
ALLOCATE(  zglamt_out(mx,my)  ) 
ALLOCATE(  angoutXt(mx,my), angoutXu(mx,my), angoutXv(mx,my), angoutYt(mx,my), angoutYu(mx,my), angoutYv(mx,my) )
ALLOCATE(  angoutXz(mx,my), angoutYz(mx,my) )
ALLOCATE(  nav_lev(mz)  ) 
ALLOCATE(  nav_lat(mx,my)  ) 
ALLOCATE(  nav_lon(mx,my)  ) 

status = NF90_INQ_VARID(fidmshORCA025,"vmask",vmask_out_ID)
call erreur(status,.TRUE.,"inq_vmask_out_ID")
status = NF90_INQ_VARID(fidmshORCA025,"umask",umask_out_ID)
call erreur(status,.TRUE.,"inq_umask_out_ID")
status = NF90_INQ_VARID(fidmshORCA025,"tmask",tmask_out_ID)
call erreur(status,.TRUE.,"inq_tmask_out_ID")
status = NF90_INQ_VARID(fidmshORCA025,"e2v",e2v_out_ID)
call erreur(status,.TRUE.,"inq_e2v_out_ID")
status = NF90_INQ_VARID(fidmshORCA025,"e2u",e2u_out_ID)
call erreur(status,.TRUE.,"inq_e2u_out_ID")
status = NF90_INQ_VARID(fidmshORCA025,"e2t",e2t_out_ID)
call erreur(status,.TRUE.,"inq_e2t_out_ID")
status = NF90_INQ_VARID(fidmshORCA025,"e1v",e1v_out_ID)
call erreur(status,.TRUE.,"inq_e1v_out_ID")
status = NF90_INQ_VARID(fidmshORCA025,"e1u",e1u_out_ID)
call erreur(status,.TRUE.,"inq_e1u_out_ID")
status = NF90_INQ_VARID(fidmshORCA025,"e1t",e1t_out_ID)
call erreur(status,.TRUE.,"inq_e1t_out_ID")
status = NF90_INQ_VARID(fidmshORCA025,"gphiv",gphiv_out_ID)
call erreur(status,.TRUE.,"inq_gphiv_out_ID")
status = NF90_INQ_VARID(fidmshORCA025,"gphiu",gphiu_out_ID)
call erreur(status,.TRUE.,"inq_gphiu_out_ID")
status = NF90_INQ_VARID(fidmshORCA025,"gphit",gphit_out_ID)
call erreur(status,.TRUE.,"inq_gphit_out_ID")
status = NF90_INQ_VARID(fidmshORCA025,"glamv",glamv_out_ID)
call erreur(status,.TRUE.,"inq_glamv_out_ID")
status = NF90_INQ_VARID(fidmshORCA025,"glamu",glamu_out_ID)
call erreur(status,.TRUE.,"inq_glamu_out_ID")
status = NF90_INQ_VARID(fidmshORCA025,"glamt",glamt_out_ID)
call erreur(status,.TRUE.,"inq_glamt_out_ID")
status = NF90_INQ_VARID(fidmshORCA025,"nav_lev",nav_lev_ID)
call erreur(status,.TRUE.,"inq_nav_lev_ID")
status = NF90_INQ_VARID(fidmshORCA025,"nav_lat",nav_lat_ID)
call erreur(status,.TRUE.,"inq_nav_lat_ID")
status = NF90_INQ_VARID(fidmshORCA025,"nav_lon",nav_lon_ID)
call erreur(status,.TRUE.,"inq_nav_lon_ID")

status = NF90_GET_VAR(fidmshORCA025,vmask_out_ID,vmask_out)
call erreur(status,.TRUE.,"getvar_vmask_out")
status = NF90_GET_VAR(fidmshORCA025,umask_out_ID,umask_out)
call erreur(status,.TRUE.,"getvar_umask_out")
status = NF90_GET_VAR(fidmshORCA025,tmask_out_ID,tmask_out)
call erreur(status,.TRUE.,"getvar_tmask_out")
status = NF90_GET_VAR(fidmshORCA025,e2v_out_ID,e2v_out)
call erreur(status,.TRUE.,"getvar_e2v_out")
status = NF90_GET_VAR(fidmshORCA025,e2u_out_ID,e2u_out)
call erreur(status,.TRUE.,"getvar_e2u_out")
status = NF90_GET_VAR(fidmshORCA025,e2t_out_ID,e2t_out)
call erreur(status,.TRUE.,"getvar_e2t_out")
status = NF90_GET_VAR(fidmshORCA025,e1v_out_ID,e1v_out)
call erreur(status,.TRUE.,"getvar_e1v_out")
status = NF90_GET_VAR(fidmshORCA025,e1u_out_ID,e1u_out)
call erreur(status,.TRUE.,"getvar_e1u_out")
status = NF90_GET_VAR(fidmshORCA025,e1t_out_ID,e1t_out)
call erreur(status,.TRUE.,"getvar_e1t_out")
status = NF90_GET_VAR(fidmshORCA025,gphiv_out_ID,gphiv_out)
call erreur(status,.TRUE.,"getvar_gphiv_out")
status = NF90_GET_VAR(fidmshORCA025,gphiu_out_ID,gphiu_out)
call erreur(status,.TRUE.,"getvar_gphiu_out")
status = NF90_GET_VAR(fidmshORCA025,gphit_out_ID,gphit_out)
call erreur(status,.TRUE.,"getvar_gphit_out")
status = NF90_GET_VAR(fidmshORCA025,glamv_out_ID,glamv_out)
call erreur(status,.TRUE.,"getvar_glamv_out")
status = NF90_GET_VAR(fidmshORCA025,glamu_out_ID,glamu_out)
call erreur(status,.TRUE.,"getvar_glamu_out")
status = NF90_GET_VAR(fidmshORCA025,glamt_out_ID,glamt_out)
call erreur(status,.TRUE.,"getvar_glamt_out")
status = NF90_GET_VAR(fidmshORCA025,nav_lev_ID,nav_lev)
call erreur(status,.TRUE.,"getvar_nav_lev")
status = NF90_GET_VAR(fidmshORCA025,nav_lat_ID,nav_lat)
call erreur(status,.TRUE.,"getvar_nav_lat")
status = NF90_GET_VAR(fidmshORCA025,nav_lon_ID,nav_lon)
call erreur(status,.TRUE.,"getvar_nav_lon")

status = NF90_CLOSE(fidmshORCA025)                      
call erreur(status,.TRUE.,"fin_lecture_ORCA025")

!--

where ( glamt_out(:,:) .lt. 0.0 )
    zglamt_out(:,:) = 360.0 + glamt_out(:,:)
elsewhere
    zglamt_out(:,:) = glamt_out(:,:)
endwhere
!-
where ( glamu_out(:,:) .lt. 0.0 )
    zglamu_out(:,:) = 360.0 + glamu_out(:,:)
elsewhere
    zglamu_out(:,:) = glamu_out(:,:)
endwhere
!-
where ( glamv_out(:,:) .lt. 0.0 )
    zglamv_out(:,:) = 360.0 + glamv_out(:,:)
elsewhere
    zglamv_out(:,:) = glamv_out(:,:)
endwhere

do jiout=1,mx
do jjout=1,my

  if ( jiout .eq. 1 ) then
    jioutm1=jiout
  else
    jioutm1=jiout-1
  endif
  !-
  if ( jiout .eq. mx ) then
    jioutp1=jiout
  else
    jioutp1=jiout+1
  endif
  !-
  if ( jjout .eq. 1 ) then
    jjoutm1=jjout
  else
    jjoutm1=jjout-1
  endif
  !-
  if ( jjout .eq. my ) then
    jjoutp1=jjout
  else
    jjoutp1=jjout+1
  endif

  ! local angle between i-direction and the zonal direction and between j-direction and the meridional direction (should be similar)

  if ( glamt_out(jiout,jjout) .gt. 160.0 .or. glamt_out(jiout,jjout) .lt. -160.0 ) then
    angoutXt(jiout,jjout) = ATAN2( gphit_out(jioutp1,jjout) - gphit_out(jioutm1,jjout) , ( zglamt_out(jioutp1,jjout) - zglamt_out(jioutm1,jjout) ) &
    &                       * cos(rad*gphit_out(jiout,jjout)) )
    angoutYt(jiout,jjout) = ATAN2( gphit_out(jiout,jjoutp1) - gphit_out(jiout,jjoutm1) , ( zglamt_out(jiout,jjoutp1) - zglamt_out(jiout,jjoutm1) ) &
    &                       * cos(rad*gphit_out(jiout,jjout)) )
  else
    angoutXt(jiout,jjout) = ATAN2( gphit_out(jioutp1,jjout) - gphit_out(jioutm1,jjout) , (  glamt_out(jioutp1,jjout) -  glamt_out(jioutm1,jjout) ) &
    &                       * cos(rad*gphit_out(jiout,jjout)) )
    angoutYt(jiout,jjout) = ATAN2( gphit_out(jiout,jjoutp1) - gphit_out(jiout,jjoutm1) , (  glamt_out(jiout,jjoutp1) -  glamt_out(jiout,jjoutm1) ) &
    &                       * cos(rad*gphit_out(jiout,jjout)) )
  endif

  if ( glamu_out(jiout,jjout) .gt. 160.0 .or. glamu_out(jiout,jjout) .lt. -160.0 ) then
    angoutXu(jiout,jjout) = ATAN2( gphiu_out(jioutp1,jjout) - gphiu_out(jioutm1,jjout) , ( zglamu_out(jioutp1,jjout) - zglamu_out(jioutm1,jjout) ) &
    &                       * cos(rad*gphiu_out(jiout,jjout)) )
    angoutYu(jiout,jjout) = ATAN2( gphiu_out(jiout,jjoutp1) - gphiu_out(jiout,jjoutm1) , ( zglamu_out(jiout,jjoutp1) - zglamu_out(jiout,jjoutm1) ) &
    &                       * cos(rad*gphiu_out(jiout,jjout)) )
  else
    angoutXu(jiout,jjout) = ATAN2( gphiu_out(jioutp1,jjout) - gphiu_out(jioutm1,jjout) , (  glamu_out(jioutp1,jjout) -  glamu_out(jioutm1,jjout) ) &
    &                       * cos(rad*gphiu_out(jiout,jjout)) )
    angoutYu(jiout,jjout) = ATAN2( gphiu_out(jiout,jjoutp1) - gphiu_out(jiout,jjoutm1) , (  glamu_out(jiout,jjoutp1) -  glamu_out(jiout,jjoutm1) ) &
    &                       * cos(rad*gphiu_out(jiout,jjout)) )
  endif

  if ( glamv_out(jiout,jjout) .gt. 160.0 .or. glamv_out(jiout,jjout) .lt. -160.0 ) then
    angoutXv(jiout,jjout) = ATAN2( gphiv_out(jioutp1,jjout) - gphiv_out(jioutm1,jjout) , ( zglamv_out(jioutp1,jjout) - zglamv_out(jioutm1,jjout) ) &
    &                       * cos(rad*gphiv_out(jiout,jjout)) )
    angoutYv(jiout,jjout) = ATAN2( gphiv_out(jiout,jjoutp1) - gphiv_out(jiout,jjoutm1) , ( zglamv_out(jiout,jjoutp1) - zglamv_out(jiout,jjoutm1) ) &
    &                       * cos(rad*gphiv_out(jiout,jjout)) )
  else
    angoutXv(jiout,jjout) = ATAN2( gphiv_out(jioutp1,jjout) - gphiv_out(jioutm1,jjout) , (  glamv_out(jioutp1,jjout) -  glamv_out(jioutm1,jjout) ) &
    &                       * cos(rad*gphiv_out(jiout,jjout)) )
    angoutYv(jiout,jjout) = ATAN2( gphiv_out(jiout,jjoutp1) - gphiv_out(jiout,jjoutm1) , (  glamv_out(jiout,jjoutp1) -  glamv_out(jiout,jjoutm1) ) &
    &                       * cos(rad*gphiv_out(jiout,jjout)) )
  endif

enddo
enddo

!===============================================================================================

DO ivar=1,nvar

    !===============================================================================================
    !-- Read CMIP5 thetao, so, uo, vo

    write(file_CMIP5,102) TRIM(varnam(ivar))
    102 FORMAT('../CLIMO_OCE/',a,'_MMMM_climo_2080_2100.nc')
    write(*,*) 'Reading ', TRIM(file_CMIP5)
    status = NF90_OPEN(TRIM(file_CMIP5),0,fidCMIP5)
    call erreur(status,.TRUE.,"read CMIP5 thetao") 

       !== dimensions ==
                                                           
       status = NF90_INQ_DIMID(fidCMIP5,"time",dimID_time)    
       call erreur(status,.TRUE.,"inq_dimID_time")
       status = NF90_INQ_DIMID(fidCMIP5,"lon",dimID_lon)
       if ( status.ne.0 ) then
         status = NF90_INQ_DIMID(fidCMIP5,"i",dimID_lon)
         if ( status.ne.0 ) then
           status = NF90_INQ_DIMID(fidCMIP5,"rlon",dimID_lon)
           call erreur(status,.TRUE.,"inq_dimID_lon")
         endif
       endif
       status = NF90_INQ_DIMID(fidCMIP5,"lat",dimID_lat)
       if ( status.ne.0 ) then
         status = NF90_INQ_DIMID(fidCMIP5,"j",dimID_lat)
         if ( status.ne.0 ) then
           status = NF90_INQ_DIMID(fidCMIP5,"rlat",dimID_lat)
           call erreur(status,.TRUE.,"inq_dimID_lat")
         endif
       endif
      status = NF90_INQ_DIMID(fidCMIP5,"lev",dimID_lev)
      if ( status.ne.0 ) status = NF90_INQ_DIMID(fidCMIP5,"depth",dimID_lev)
      call erreur(status,.TRUE.,"inq_dimID_lev")         
                                                      
      status = NF90_INQUIRE_DIMENSION(fidCMIP5,dimID_time,len=mtime) ; call erreur(status,.TRUE.,"inq_dim_time")
      status = NF90_INQUIRE_DIMENSION(fidCMIP5,dimID_lev,len=mlev)   ; call erreur(status,.TRUE.,"inq_dim_lev")
      status = NF90_INQUIRE_DIMENSION(fidCMIP5,dimID_lat,len=mj)     ; call erreur(status,.TRUE.,"inq_dim_j")
      status = NF90_INQUIRE_DIMENSION(fidCMIP5,dimID_lon,len=mi)     ; call erreur(status,.TRUE.,"inq_dim_i")
                       
      write(*,*) '   (mi,mj,mlev) = ', mi, mj, mlev
 
      !== variables (and find depth coordinate system) ==
       
      ALLOCATE(  lev_bnds(2,mlev)  )
      ALLOCATE(  lev(mlev)  )
      ALLOCATE(  e3(mi,mj,mlev) )
      ALLOCATE(  var_CMIP5(mi,mj,mlev,mtime)  )
      ALLOCATE(  lon(mi,mj), e1(mi,mj), dilon(mi,mj), djlon(mi,mj), zlon(mi,mj)  )
      ALLOCATE(  lat(mi,mj), e2(mi,mj), dilat(mi,mj), djlat(mi,mj), wlon(mi,mj)  )
!##1D        ALLOCATE(  tmp_lon(mi   ), tmp_lat(mj   )       )
!##2D        ALLOCATE(  tmp_lon(mi,mj), tmp_lat(mi,mj)       )
      ALLOCATE(  j(mj)  )
      ALLOCATE(  i(mi)  )
      ALLOCATE(  mask(mi,mj,mlev) )
      ALLOCATE(  msk_CMIP5_L75(mi,mj,mz), var_CMIP5_L75(mi,mj,mz,mtime)  )
      ALLOCATE(  var_CMIP5_ORCA025(mx,my,mz,mtime) )
      if ( ivar .eq. 1 ) then
         ALLOCATE(  uo_CMIP5_ORCA025(mx,my,mz,mtime) )
         ALLOCATE(  vo_CMIP5_ORCA025(mx,my,mz,mtime) )
         ALLOCATE(  to_CMIP5_ORCA025(mx,my,mz,mtime) )
         ALLOCATE(  so_CMIP5_ORCA025(mx,my,mz,mtime) )
         uo_CMIP5_ORCA025(:,:,:,:) = 0.0
         vo_CMIP5_ORCA025(:,:,:,:) = 0.0
         to_CMIP5_ORCA025(:,:,:,:) = 0.0
         so_CMIP5_ORCA025(:,:,:,:) = 0.0
      endif

      ll_bnds = .TRUE.               

      status = NF90_INQ_VARID(fidCMIP5,"lon",lon_ID)
      call erreur(status,.TRUE.,"inq_lon_ID")
      status = NF90_INQ_VARID(fidCMIP5,"lat",lat_ID)
      call erreur(status,.TRUE.,"inq_lat_ID")
      status = NF90_INQ_VARID(fidCMIP5,"lev",lev_ID)
      if ( status .ne. 0 ) status = NF90_INQ_VARID(fidCMIP5,"zlev",lev_ID)
      if ( status .ne. 0 ) status = NF90_INQ_VARID(fidCMIP5,"depth",lev_ID)
      call erreur(status,.TRUE.,"inq_lev_ID")
      status = NF90_INQ_VARID(fidCMIP5,"lev_bnds",lev_bnds_ID)
      if ( status .ne. 0 ) status = NF90_INQ_VARID(fidCMIP5,"zlev_bnds",lev_bnds_ID)
      if ( status .ne. 0 ) ll_bnds = .FALSE.
      status = NF90_INQ_VARID(fidCMIP5,"time",time_ID)
      call erreur(status,.TRUE.,"inq_time_ID")

      status = NF90_INQ_VARID(fidCMIP5,"eta",eta_ID)
      if ( status == 0 ) then
        status = NF90_INQ_VARID(fidCMIP5,"zlev",zlev_ID)
        if ( status == 0 ) then
          misig=.TRUE.
          status = NF90_INQ_VARID(fidCMIP5,"nsigma",nsigma_ID)
          call erreur(status,.TRUE.,"inq_nsigma_ID")
          status = NF90_INQ_VARID(fidCMIP5,"depth_c",depth_c_ID)
          call erreur(status,.TRUE.,"inq_depth_c_ID")
          status = NF90_INQ_VARID(fidCMIP5,"zlev_bnds",zlev_bnds_ID)
          call erreur(status,.TRUE.,"inq_zlev_bnds_ID")
          status = NF90_INQ_VARID(fidCMIP5,"sigma",sigma_ID)
          call erreur(status,.TRUE.,"inq_sigma_ID")
          status = NF90_INQ_VARID(fidCMIP5,"sigma_bnds",sigma_bnds_ID)
          call erreur(status,.TRUE.,"inq_sigma_bnds_ID")
          ALLOCATE( zlev(mlev), zlev_bnds(2,mlev), sigma(mlev), sigma_bnds(2,mlev) )
        else
          misig=.FALSE.
        endif
        status = NF90_INQ_VARID(fidCMIP5,"depth",depth_ID)
        call erreur(status,.TRUE.,"inq_depth_ID")
        lsigma=.TRUE.
        ALLOCATE( eta(mi,mj,mtime), depth(mi,mj) )
        ALLOCATE( lev_ref(mlev), lev_bnds_ref(2,mlev)   )
      else
        lsigma=.FALSE.
      endif

      status = NF90_INQ_VARID(fidCMIP5,TRIM(varnam(ivar)),var_CMIP5_ID)
      call erreur(status,.TRUE.,"inq_var_CMIP5_ID")

      !====
 
      status = NF90_GET_VAR(fidCMIP5,lon_ID,tmp_lon)
      call erreur(status,.TRUE.,"getvar_lon")
      status = NF90_GET_VAR(fidCMIP5,lat_ID,tmp_lat)
      call erreur(status,.TRUE.,"getvar_lat")
!##1D       do ji=1,mi    
!##1D         lat(ji,:) = tmp_lat(:)  
!##1D       enddo    
!##1D       do jj=1,mj  
!##1D         lon(:,jj) = tmp_lon(:)  
!##1D       enddo      
!##2D       lon(:,:) = tmp_lon(:,:)    
!##2D       lat(:,:) = tmp_lat(:,:)    
      DEALLOCATE( tmp_lat, tmp_lon )

      status = NF90_GET_VAR(fidCMIP5,lev_ID,lev)
      call erreur(status,.TRUE.,"getvar_lev")
      if ( ll_bnds ) then
        status = NF90_GET_VAR(fidCMIP5,lev_bnds_ID,lev_bnds)
        call erreur(status,.TRUE.,"getvar_lev_bnds")
      elseif ( .NOT. lsigma ) then
        do kk=2,mlev-1
          lev_bnds(1,kk) = lev(kk) - 0.25 * ( lev(kk+1) - lev(kk-1) )
          lev_bnds(2,kk) = lev(kk) + 0.25 * ( lev(kk+1) - lev(kk-1) )
        enddo
        lev_bnds(1,1   ) = 0.0
        lev_bnds(2,1   ) = 2*lev(1   ) - lev_bnds(1,1   ) 
        lev_bnds(1,mlev) = lev(mlev-1) + 0.25*( lev(mlev) - lev(mlev-2) )
        lev_bnds(2,mlev) = 2*lev(mlev) - lev_bnds(1,mlev)
      endif
      status = NF90_GET_VAR(fidCMIP5,time_ID,time)
      call erreur(status,.TRUE.,"getvar_time")

      status = NF90_GET_VAR(fidCMIP5,var_CMIP5_ID,var_CMIP5)
      call erreur(status,.TRUE.,"get_var_CMIP5")

      write(*,*) '   >>> min value for ', TRIM(varnam(ivar)), ' = ', minval(var_CMIP5)
      write(*,*) '   >>> max value for ', TRIM(varnam(ivar)), ' = ', maxval(var_CMIP5)

      if ( lsigma ) then
        status = NF90_GET_VAR(fidCMIP5,eta_ID,eta)
        call erreur(status,.TRUE.,"getvar_eta")
        status = NF90_GET_VAR(fidCMIP5,depth_ID,depth)
        call erreur(status,.TRUE.,"getvar_depth")
        if ( misig ) then
          status = NF90_GET_VAR(fidCMIP5,nsigma_ID,nsigma)
          call erreur(status,.TRUE.,"getvar_nsigma")
          status = NF90_GET_VAR(fidCMIP5,depth_c_ID,depth_c)
          call erreur(status,.TRUE.,"getvar_depth_c")
          status = NF90_GET_VAR(fidCMIP5,zlev_ID,zlev)
          call erreur(status,.TRUE.,"getvar_zlev")
          status = NF90_GET_VAR(fidCMIP5,zlev_bnds_ID,zlev_bnds)
          call erreur(status,.TRUE.,"getvar_zlev_bnds")
          status = NF90_GET_VAR(fidCMIP5,sigma_bnds_ID,sigma_bnds)
          call erreur(status,.TRUE.,"getvar_sigma_bnds")
          status = NF90_GET_VAR(fidCMIP5,sigma_ID,sigma)
          call erreur(status,.TRUE.,"getvar_sigma")
        endif
      endif

      status = NF90_GET_ATT(fidCMIP5,var_CMIP5_ID,"long_name",long_name)
      call erreur(status,.TRUE.,"get_var_long_name")
      write(*,*) '   long_name :', long_name
      !-
      status = NF90_GET_ATT(fidCMIP5,var_CMIP5_ID,"missing_value",missval)
      if ( status .ne. 0 ) status = NF90_GET_ATT(fidCMIP5,var_CMIP5_ID,"_FillValue",missval)
      if ( status .ne. 0 ) missval = 1.e20
      write(*,*) '   missing value = ', missval
      !-
      status = NF90_GET_ATT(fidCMIP5,var_CMIP5_ID,"units",units)
      call erreur(status,.TRUE.,"get_var_units")
      write(*,*) '   UNITS :', units
     
  status = NF90_CLOSE(fidCMIP5)
  call erreur(status,.TRUE.,"End read CMIP5") 

  !== calculate mesh horizontal dimensions (e1,e2) ==

  write(*,*) '   Calculate mesh horizontal dimensions (e1,e2)'

  where ( lon(:,:) .lt. 0.0 )
    zlon(:,:) = 360.0 + lon(:,:)
  elsewhere
    zlon(:,:) = lon(:,:)
  endwhere

  where ( lon(:,:) .gt. 180.0 )
    wlon(:,:) = lon(:,:) - 360.0
  elsewhere
    wlon(:,:) = lon(:,:)
  endwhere

  ! figure out what kind of periodicity we have :
  jm=INT(mj/2)
  if ( lon(1,jm) .eq. lon(mi,jm) ) then        ! one column overlap
    perio = 1
  elseif ( lon(1,jm) .eq. lon(mi-1,jm) ) then  ! ORCA style (2 columns overlap)
    perio = 2
  else                                         ! no overlap
    perio = 0
  endif

  ! i-derivatives :
  do jj=1,mj
    do ji=2,mi-1
      dilat(ji,jj) = 0.5 * ( lat(ji+1,jj) - lat(ji-1,jj) )
      if ( wlon(ji,jj) .gt. 160 .or. wlon(ji,jj) .lt. -160.0 ) then
        dilon(ji,jj) = 0.5 * ( zlon(ji+1,jj) - zlon(ji-1,jj) )
      else
        dilon(ji,jj) = 0.5 * ( wlon(ji+1,jj) - wlon(ji-1,jj) )
      endif
    enddo
    !-
    dilat(1 ,jj) = lat(2 ,jj) - lat(1   ,jj)
    dilat(mi,jj) = lat(mi,jj) - lat(mi-1,jj)
    if ( perio .eq. 0 ) then
      if ( wlon(1,jj) .gt. 160 .or. wlon(1,jj) .lt. -160.0 ) then
        dilon(1,jj) = zlon(2,jj) - zlon(1,jj)
      else
        dilon(1,jj) = wlon(2,jj) - wlon(1,jj)
      endif
      if ( wlon(mi,jj) .gt. 160 .or. wlon(mi,jj) .lt. -160.0 ) then
        dilon(mi,jj) = zlon(mi,jj) - zlon(mi-1,jj) 
      else
        dilon(mi,jj) = wlon(mi,jj) - wlon(mi-1,jj) 
      endif
    elseif ( perio .eq. 1 ) then 
      if ( wlon(1,jj) .gt. 160 .or. wlon(1,jj) .lt. -160.0 ) then
        dilon(1,jj) = 0.5 * ( zlon(2,jj) - zlon(mi-1,jj) )
      else
        dilon(1,jj) = 0.5 * ( wlon(2,jj) - wlon(mi-1,jj) )
      endif
      dilon(mi,jj) = dilon(1,jj)
    elseif ( perio .eq. 2 ) then
      dilon(1 ,jj) = dilon(mi-1,jj)
      dilon(mi,jj) = dilon(2,jj)
    endif
  enddo

  ! j-derivatives :
  do ji=1,mi
    do jj=2,mj-1
      djlat(ji,jj) = 0.5 * ( lat(ji,jj+1) - lat(ji,jj-1) )
      if ( wlon(ji,jj) .gt. 160 .or. wlon(ji,jj) .lt. -160.0 ) then
        djlon(ji,jj) = 0.5 * ( zlon(ji,jj+1) - zlon(ji,jj-1) )
      else
        djlon(ji,jj) = 0.5 * ( wlon(ji,jj+1) - wlon(ji,jj-1) )
      endif
    enddo
    djlat(ji,1 ) = lat(ji, 2) - lat(ji,   1)
    djlat(ji,mj) = lat(ji,mj) - lat(ji,mj-1)
    if ( wlon(ji,1) .gt. 160 .or. wlon(ji,1) .lt. -160.0 ) then
      djlon(ji,1) = zlon(ji,2) - zlon(ji,1) 
    else
      djlon(ji,1) = wlon(ji,2) - wlon(ji,1) 
    endif
    if ( wlon(ji,mj) .gt. 160 .or. wlon(ji,mj) .lt. -160.0 ) then
      djlon(ji,mj) = zlon(ji,mj) - zlon(ji,mj-1)
    else
      djlon(ji,mj) = wlon(ji,mj) - wlon(ji,mj-1)
    endif
  enddo

  !- CMIP5 mesh sizes along X
  e1(:,:) = ra * rad * sqrt( (cos(rad*lat(:,:))*dilon(:,:))**2  +  dilat(:,:)**2  )
  !- CMIP5 mesh sizes along Y
  e2(:,:) = ra * rad * sqrt( (cos(rad*lat(:,:))*djlon(:,:))**2  +  djlat(:,:)**2  )

  write(*,*) '      > Maximum e1 value : ', maxval(e1)
  write(*,*) '      > Maximum e2 value : ', maxval(e2)

  if ( lsigma )  then
    lev_bnds_ref(:,:) = lev_bnds(:,:)
    lev_ref(:)        = lev(:)
  endif

  !-- corresponding grid (t,U, or V) on ORCA025 :
  if ( TRIM(varnam(ivar)) .eq. 'thetao' .or. TRIM(varnam(ivar)) .eq. 'so' ) then
    zglamz_out(:,:)=zglamt_out(:,:)
    glamz_out(:,:)=glamt_out(:,:)
    gphiz_out(:,:)=gphit_out(:,:)
    angoutXz(:,:)=angoutXt(:,:)
    angoutYz(:,:)=angoutYt(:,:)
    e1z_out(:,:)=e1t_out(:,:)
    e2z_out(:,:)=e2t_out(:,:)
    zmask_out(:,:,:)=tmask_out(:,:,:,1)
  elseif ( TRIM(varnam(ivar)) .eq. 'uo' ) then
    zglamz_out(:,:)=zglamu_out(:,:)
    glamz_out(:,:)=glamu_out(:,:)
    gphiz_out(:,:)=gphiu_out(:,:)
    angoutXz(:,:)=angoutXu(:,:)
    angoutYz(:,:)=angoutYu(:,:)
    e1z_out(:,:)=e1u_out(:,:)
    e2z_out(:,:)=e2u_out(:,:)
    zmask_out(:,:,:)=umask_out(:,:,:,1)
  elseif ( TRIM(varnam(ivar)) .eq. 'vo' ) then
    zglamz_out(:,:)=zglamv_out(:,:)
    glamz_out(:,:)=glamv_out(:,:)
    gphiz_out(:,:)=gphiv_out(:,:)
    angoutXz(:,:)=angoutXv(:,:)
    angoutYz(:,:)=angoutYv(:,:)
    e1z_out(:,:)=e1v_out(:,:)
    e2z_out(:,:)=e2v_out(:,:)
    zmask_out(:,:,:)=vmask_out(:,:,:,1)
  endif

  !- some vertical axis are from top to bottom and others are the other way round:
  if ( lsigma .or. lev(1) .lt. lev(2) ) then
    ktop = 1
    kbot = mlev
    kinc = 1
  else
    ktop = mlev
    kbot = 1
    kinc = -1
  endif
  !if ( lsigma .and. .not. misig ) then
  !  ktop = mlev
  !  kbot = 1
  !  kinc = -1
  !endif
  write(*,*) '   k-index for top of the ocean = ', ktop
  write(*,*) '   k-index for bottom of the ocean = ', kbot

  mask(:,:,:) = 0
  msk_CMIP5_L75(:,:,:) = 0

  !=====

  DO kt=1,mtime  !! calculation for the 12 months

    write(*,*) '   Processing time step ', kt

    do ji=1,mi
    do jj=1,mj

      !write(*,*) 'toto1'

      !-- calculate Z discretization in case of sigma levels:
      !   z(n,k,j,i) = eta(n,j,i) + sigma(k)*(depth(j,i)+eta(n,j,i))
      if ( lsigma .and. .not. misig ) then
        lev_bnds(:,:) = ( eta(ji,jj,kt) + lev_bnds_ref(:,:) * ( depth(ji,jj) + eta(ji,jj,kt) ) ) * (-1)
        lev(:)        = ( eta(ji,jj,kt) + lev_ref(:)        * ( depth(ji,jj) + eta(ji,jj,kt) ) ) * (-1)
      elseif ( lsigma) then
        do kk=1,nsigma
          lev_bnds(:,kk) = eta(ji,jj,kt) + sigma_bnds(:,kt) * ( min(depth_c,depth(ji,jj)) + eta(ji,jj,kt) )
          lev(kk)        = eta(ji,jj,kt) + sigma(kt)        * ( min(depth_c,depth(ji,jj)) + eta(ji,jj,kt) )
        enddo
        do kk=nsigma+1,mlev
          lev_bnds(:,kk) = zlev_bnds(:,kk)
          lev(kk)        = zlev(kk)
        enddo
      endif

      !-- original vertical mesh size of CMIP5 model :
      !write(*,*) 'toto2'
      e3(ji,jj,:) = abs( lev_bnds(2,:) - lev_bnds(1,:) ) 

      !-- model mask (=0 if land, =1 if ocean) --
      do kk=1,mlev
        if ( var_CMIP5(ji,jj,kk,1) .ne. missval ) then
          mask(ji,jj,kk) = 1
        else
          mask(ji,jj,kk) = 0
        endif
      enddo

      !write(*,*) 'toto3'

      !== vertical interpolation (everything on ORCA025's e3t_0, i.e. no partial step here) ==

      if ( mask(ji,jj,ktop) .eq. 1 ) then
        var_CMIP5_L75(ji,jj,1,kt) = var_CMIP5(ji,jj,ktop,kt)
        msk_CMIP5_L75(ji,jj,1) = 1
      else
        var_CMIP5_L75(ji,jj,1,kt) = missval
        msk_CMIP5_L75(ji,jj,1) = 0
      endif

      !write(*,*) 'toto4'

      do jkout=2,mz

        !write(*,*) 'toto5', jkout

        isitdone=.false.

        !- look for target vertical meshes entirely enclosed in a CMIP5 vertical mesh:
        do kk=1,mlev
          if (       gdept_0_out(jkout) + 0.5*e3t_0_out(jkout) .le. lev(kk) + 0.5*e3(ji,jj,kk)  &
          &    .and. gdept_0_out(jkout) - 0.5*e3t_0_out(jkout) .ge. lev(kk) - 0.5*e3(ji,jj,kk) ) then
             var_CMIP5_L75(ji,jj,jkout,kt) = var_CMIP5(ji,jj,kk,kt)
             msk_CMIP5_L75(ji,jj,jkout   ) = mask     (ji,jj,kk   )
             isitdone=.true.
             exit
          endif
        enddo

        !write(*,*) 'toto6'

        !- then, among the remaining cells, look for target vertical meshes covering 2 CMIP5 vertical meshes:
        do kk=ktop+MIN(0,kinc),kbot-MAX(0,kinc),kinc
          if ( gdept_0_out(jkout) .gt. lev(kk) .and. gdept_0_out(jkout) .le. lev(kk+kinc) .and. .not. isitdone ) then
             a1 = ( lev(kk) + 0.5*e3(ji,jj,kk) ) - ( gdept_0_out(jkout) - 0.5*e3t_0_out(jkout) )
             a2 = ( gdept_0_out(jkout) + 0.5*e3t_0_out(jkout) ) - ( lev(kk) + 0.5*e3(ji,jj,kk) )
             var_CMIP5_L75(ji,jj,jkout,kt) =   ( a1*mask(ji,jj,kk)*var_CMIP5(ji,jj,kk,kt) + a2*mask(ji,jj,kk+kinc)*var_CMIP5(ji,jj,kk+kinc,kt) ) &
             &                               / ( a1*mask(ji,jj,kk) + a2*mask(ji,jj,kk+kinc) + eps )
             msk_CMIP5_L75(ji,jj,jkout   ) = MIN( 1, mask(ji,jj,kk) + mask(ji,jj,kk+kinc) )
             isitdone=.true.
             exit
           endif
        enddo

        !write(*,*) 'toto7'

        ! Fill bottom meshes if not done yet:
        if ( gdept_0_out(jkout) .gt. lev(kbot)-0.5*e3(ji,jj,kbot) .and. .not. isitdone ) then
           var_CMIP5_L75(ji,jj,jkout,kt) = mask(ji,jj,kbot) * var_CMIP5(ji,jj,kbot,kt)
           msk_CMIP5_L75(ji,jj,jkout   ) = mask(ji,jj,kbot)
        endif

      enddo !! jkout=2,mz

    enddo !! jj=1,mj
    enddo !! ji=1,mi

    !write(*,*) 'toto8'

    !== Find horizontal neighbours for interpolation ==

    write(*,*) ' Check mask            : ', sum(sum(sum(mask*1,3),2),1), sum(sum(sum(mask*0+1,3),2),1)
    write(*,*) ' Check msk_CMIP5_L75   : ', sum(sum(sum(msk_CMIP5_L75*1,3),2),1), sum(sum(sum(msk_CMIP5_L75*0+1,3),2),1)
    write(*,*) ' Check zmask_out       : ', sum(sum(sum(zmask_out*1,3),2),1), sum(sum(sum(zmask_out*0+1,3),2),1)
    write(*,*) ' Min/Max mask          : ', minval(mask), maxval(mask)
    write(*,*) ' Min/Max msk_CMIP5_L75 : ', minval(msk_CMIP5_L75), maxval(msk_CMIP5_L75)
    write(*,*) ' Min/Max zmask_out     : ', minval(zmask_out), maxval(zmask_out)

    write(*,*) '   Looking for closest neighbours horizontally'

    zzi = 0
    zzj = 0

    do jiout=1,mx
    do jjout=1,my

     IF ( zmask_out(jiout,jjout,1) .NE. 0 ) THEN

      distmin = ra * rad * 10.0 ! let's assume that nobody's using grids coarser than 10 degrees...

      !-- loop to find closest point ( we allow 1 bdy cell to cover a maximum of 3 cells from the input grid )
      do ji=1,mi
      do jj=1,mj
           dist = ra * rad * sqrt(   ( cos(rad*gphiz_out(jiout,jjout)) * ( zglamz_out(jiout,jjout) - zlon(ji,jj) ) )**2  &
           &                     + (                                        gphiz_out(jiout,jjout) -  lat(ji,jj)   )**2  )
        if ( dist .lt. distmin ) then
          distmin=dist
          zzi = ji
          zzj = jj
        endif
      enddo
      enddo

      !write(*,*) 'toto9'

      !--

      if ( perio .eq. 2 ) then
        if ( zzi+1 .gt. mi ) then
          zzip1 = 3
          zzim1 = zzi - 1
        elseif ( zzi-1 .lt. 1 ) then
          zzip1 = zzi + 1
          zzim1 = mi-2
        else
          zzip1 = zzi + 1
          zzim1 = zzi - 1
        endif
      elseif ( perio .eq. 1 ) then 
        if ( zzi+1 .gt. mi ) then
          zzip1 = 2
          zzim1 = zzi - 1
        elseif ( zzi-1 .lt. 1 ) then
          zzip1 = zzi + 1
          zzim1 = mi-1
        else
          zzip1 = zzi + 1
          zzim1 = zzi - 1
        endif
      else
        if ( zzi+1 .gt. mi ) then
          zzip1 = zzi
          zzim1 = zzi - 1
        elseif ( zzi-1 .lt. 1 ) then
          zzip1 = zzi + 1
          zzim1 = zzi
        else
          zzip1 = zzi + 1
          zzim1 = zzi - 1
        endif
      endif
      !- upper lower bounds of global input grid
      if     ( zzj+1 .gt. mj ) then
        zzjp1 = zzj
        zzjm1 = zzj - 1
      elseif ( zzj-1 .lt. 1     ) then
        zzjp1 = zzj + 1
        zzjm1 = zzj
      else
        zzjp1 = zzj + 1
        zzjm1 = zzj - 1
      endif

      !write(*,*) 'toto10'

      if ( wlon(zzi,zzj) .gt. 160.0 .or. wlon(zzi,zzj) .lt. -160.0 ) then
        !-- local angle on global input grid between i-direction and the zonal direction and between j-direction and the meridional direction
        anginX = ATAN2(    lat( zzip1 , zzj ) -  lat( zzim1 , zzj) ,                                &
        &               ( zlon( zzip1 , zzj ) - zlon( zzim1 , zzj) ) * cos( lat(zzi,zzj)*rad )  )
        anginY = ATAN2(    lat( zzi , zzjp1 ) -  lat( zzi , zzjm1) ,                                &
        &               ( zlon( zzi , zzjp1 ) - zlon( zzi , zzjm1) ) * cos( lat(zzi,zzj)*rad )  )
        !-- local angle between the two grids :
        angleX = angoutXz(jiout,jjout) - anginX
        angleY = angoutYz(jiout,jjout) - anginY
        !--
        if  ( e1z_out(jiout,jjout) .ge. e1(zzi,zzj) * cos(angleX*rad) ) then
           ziW = zzim1 ! closest point westward
           ziC = zzi   ! closest point ( central )
           ziE = zzip1 ! closest point eastward
           wgC = e1(zzi,zzj) * cos(angleX*rad) !weight for central point
        elseif  ( zglamz_out(jiout,jjout) .ge. zlon(zzi,zzj) )  then
           ziW = zzi
           ziC = 1 ! not used
           ziE = zzip1
           wgC = 0.0
        else
           ziW = zzim1
           ziC = 1 ! not used
           ziE = zzi
           wgC = 0.0
        endif
        zjW = zzj
        zjC = zzj
        zjE = zzj
        !-- weights :
        wgW = MAX( 0.0,   0.5 * e1(ziW,zjW) * cos(angleX*rad)                                                               &
        &               - ra * rad * abs(   ( zglamz_out(jiout,jjout) - zlon(ziW,zjW) ) * cos( gphiz_out(jiout,jjout)*rad ) ) &
        &               + 0.5 * e1z_out(jiout,jjout)                                                                        )
        wgE = MAX( 0.0,   0.5 * e1(ziE,zjE) * cos(angleX*rad)                                                               &
        &               - ra * rad * abs(   ( zglamz_out(jiout,jjout) - zlon(ziE,zjE) ) * cos( gphiz_out(jiout,jjout)*rad ) ) &
        &               + 0.5 * e1z_out(jiout,jjout)                                                                        )
      else
        !-- local angle on global input grid between i-direction and the zonal direction and between j-direction and the meridional direction
        anginX = ATAN2(    lat( zzip1 , zzj ) -  lat( zzim1 , zzj) ,                                &
        &               ( wlon( zzip1 , zzj ) - wlon( zzim1 , zzj) ) * cos( lat(zzi,zzj)*rad )  )
        anginY = ATAN2(    lat( zzi , zzjp1 ) -  lat( zzi , zzjm1) ,                                &
        &               ( wlon( zzi , zzjp1 ) - wlon( zzi , zzjm1) ) * cos( lat(zzi,zzj)*rad )  )
        !-- local angle between the two grids :
        angleX = angoutXz(jiout,jjout) - anginX
        angleY = angoutYz(jiout,jjout) - anginY
        !--
        if  ( e1z_out(jiout,jjout) .ge. e1(zzi,zzj) * cos(angleX*rad) ) then
           ziW = zzim1 ! closest point westward
           ziC = zzi   ! closest point ( central )
           ziE = zzip1 ! closest point eastward
           wgC = e1(zzi,zzj) * cos(angleX*rad) !weight for central point
        elseif  ( glamz_out(jiout,jjout) .ge. wlon(zzi,zzj) )  then
           ziW = zzi
           ziC = 1 ! not used
           ziE = zzip1
           wgC = 0.0
        else
           ziW = zzim1
           ziC = 1 ! not used
           ziE = zzi
           wgC = 0.0
        endif
        zjW = zzj
        zjC = zzj
        zjE = zzj
        !-- weights :
        wgW = MAX( 0.0,   0.5 * e1(ziW,zjW) * cos(angleX*rad)                                                                 &
        &               - ra * rad * abs(   (  glamz_out(jiout,jjout) - wlon(ziW,zjW) ) * cos( gphiz_out(jiout,jjout)*rad ) ) &
        &               + 0.5 * e1z_out(jiout,jjout)                                                                          )
        wgE = MAX( 0.0,   0.5 * e1(ziE,zjE) * cos(angleX*rad)                                                                 &
        &               - ra * rad * abs(   (  glamz_out(jiout,jjout) - wlon(ziE,zjE) ) * cos( gphiz_out(jiout,jjout)*rad ) ) &
        &               + 0.5 * e1z_out(jiout,jjout)                                                                          )
      endif

      !write(*,*) 'toto11'

      !-- North & South neighbours :
      if  (   e2z_out(jiout,jjout) .ge. e2(zzi,zzj) * cos(angleY*rad)  ) then
         zjS = zzjm1  ! closest point southward
         zjM = zzj    ! closest point ( middle )
         zjN = zzjp1  ! closest point northward
         wgM = e2(zzi,zzj) * cos(angleY*rad) !weight for central point
      elseif ( gphiz_out(jiout,jjout) .ge. lat(zzi,zzj) )  then
         zjS = zzj
         zjM = 1      ! not used
         zjN = zzjp1
         wgM = 0.0
      else
         zjS = zzjm1
         zjM = 1      ! not used
         zjN = zzj
         wgM = 0.0
      endif
      ziS = zzi
      ziM = zzi
      ziN = zzi
      !-- weights :
      wgS = MAX( 0.0,   0.5 * e2(ziS,zjS) * cos(angleY*rad)                       &
      &               - ra * rad * abs(  gphiz_out(jiout,jjout) - lat(ziS,zjS)  ) &
      &               + 0.5 * e2z_out(jiout,jjout)                                )
      wgN = MAX( 0.0,   0.5 * e2(ziN,zjN) * cos(angleY*rad)                       &
      &               - ra * rad * abs(  gphiz_out(jiout,jjout) - lat(ziN,zjN)  ) &
      &               + 0.5 * e2z_out(jiout,jjout)                                )

      !write(*,*) 'toto12'

      !== horizontal interpolation ==

      do jkout=1,mz
    
         !-- zonal interpolation at Southern point
         aE  = wgE * msk_CMIP5_L75(ziE,zjS,jkout) !* e3tin( ziEt(ji,jj),zjSt(ji,jj),jk )
         aC  = wgC * msk_CMIP5_L75(ziC,zjS,jkout) !* e3tin( ziCt(ji,jj),zjSt(ji,jj),jk )
         aW  = wgW * msk_CMIP5_L75(ziW,zjS,jkout) !* e3tin( ziWt(ji,jj),zjSt(ji,jj),jk )
     
         tS  = (   var_CMIP5_L75(ziE,zjS,jkout,kt) * aE                             &
         &       + var_CMIP5_L75(ziC,zjS,jkout,kt) * aC                             &
         &       + var_CMIP5_L75(ziW,zjS,jkout,kt) * aW )  / ( aE + aC + aW + eps )
     
         aS  = ( aE + aC + aW ) * wgS
     
         !-- zonal interpolation at Central point
         aE  = wgE * msk_CMIP5_L75(ziE,zjM,jkout) !* e3tin( ziEt(ji,jj),zjMt(ji,jj),jk )
         aC  = wgC * msk_CMIP5_L75(ziC,zjM,jkout) !* e3tin( ziCt(ji,jj),zjMt(ji,jj),jk )
         aW  = wgW * msk_CMIP5_L75(ziW,zjM,jkout) !* e3tin( ziWt(ji,jj),zjMt(ji,jj),jk )
 
         tM  = (   var_CMIP5_L75(ziE,zjM,jkout,kt) * aE                             &
         &       + var_CMIP5_L75(ziC,zjM,jkout,kt) * aC                             &
         &       + var_CMIP5_L75(ziW,zjM,jkout,kt) * aW )  / ( aE + aC + aW + eps )
 
         aM  = ( aE + aC + aW ) * wgM

         !-- zonal interpolation at Northern point
         aE  = wgE * msk_CMIP5_L75(ziE,zjN,jkout) !* e3tin( ziEt(ji,jj),zjNt(ji,jj),jk )
         aC  = wgC * msk_CMIP5_L75(ziC,zjN,jkout) !* e3tin( ziCt(ji,jj),zjNt(ji,jj),jk )
         aW  = wgW * msk_CMIP5_L75(ziW,zjN,jkout) !* e3tin( ziWt(ji,jj),zjNt(ji,jj),jk )
 
         tN  = (   var_CMIP5_L75(ziE,zjN,jkout,kt) * aE                             &
         &       + var_CMIP5_L75(ziC,zjN,jkout,kt) * aC                             &
         &       + var_CMIP5_L75(ziW,zjN,jkout,kt) * aW )  / ( aE + aC + aW + eps )
 
         aN  = ( aE + aC + aW ) * wgN

         !write(*,*) 'toto13'

         !-- Meridional interpolation
         if ( abs(aS+aM+aN) .gt. eps ) then

            var_CMIP5_ORCA025 (jiout,jjout,jkout,kt) = ( tS*aS + tM*aM + tN*aN ) * zmask_out(jiout,jjout,jkout) / ( aS + aM + aN )

         elseif (       zmask_out(jiout,jjout,jkout) .eq. 1  &  !- oceanic point in ORCA025 but not in CMIP5 model
         &        .and. ( ( TRIM(varnam(ivar)) .eq. 'thetao' ) .or. ( TRIM(varnam(ivar)) .eq. 'so' ) ) ) then

            !- try various extensions always turning anti-clockwise (then going upward) 
            !  (to favor more or less the same direction first at 2 neighbouring location) 
            rsmax = 10! maximum number of point for lateral and vertical extrapolation
                      ! -> need to go far near boundaries because bathymetry has been smoothed
            rzmax = 30! maximum upper extension if horizontal extension does not work.
            lout=.FALSE.
            do rz=0,rzmax,1
              do rs=1,rsmax+rzmax,1

                 irs=MIN(ziE+rs,mx) ; jrs=    zjE        ; krs= MAX(jkout-rz, 1) !- E 
                 if ( msk_CMIP5_L75(irs,jrs,krs) .eq. 1 ) then ; lout=.TRUE. ; exit ; endif
                 irs=MIN(ziE+rs,mx) ; jrs=    zjN        ; krs= MAX(jkout-rz, 1) !- NEE 
                 if ( msk_CMIP5_L75(irs,jrs,krs) .eq. 1 ) then ; lout=.TRUE. ; exit ; endif
                 irs=MIN(ziE+rs,mx) ; jrs=MIN(zjN+rs,my) ; krs= MAX(jkout-rz, 1) !- NE
                 if ( msk_CMIP5_L75(irs,jrs,krs) .eq. 1 ) then ; lout=.TRUE. ; exit ; endif
                 irs=    ziE        ; jrs=MIN(zjN+rs,my) ; krs= MAX(jkout-rz, 1) !- NNE
                 if ( msk_CMIP5_L75(irs,jrs,krs) .eq. 1 ) then ; lout=.TRUE. ; exit ; endif
                 irs=    ziN        ; jrs=MIN(zjN+rs,my) ; krs= MAX(jkout-rz, 1) !- N 
                 if ( msk_CMIP5_L75(irs,jrs,krs) .eq. 1 ) then ; lout=.TRUE. ; exit ; endif
                 irs=    ziW        ; jrs=MIN(zjN+rs,my) ; krs= MAX(jkout-rz, 1) !- NNW 
                 if ( msk_CMIP5_L75(irs,jrs,krs) .eq. 1 ) then ; lout=.TRUE. ; exit ; endif
                 irs=MAX(ziW-rs, 1) ; jrs=MIN(zjN+rs,my) ; krs= MAX(jkout-rz, 1) !- NW 
                 if ( msk_CMIP5_L75(irs,jrs,krs) .eq. 1 ) then ; lout=.TRUE. ; exit ; endif
                 irs=MAX(ziW-rs, 1) ; jrs=    zjN        ; krs= MAX(jkout-rz, 1) !- NWW 
                 if ( msk_CMIP5_L75(irs,jrs,krs) .eq. 1 ) then ; lout=.TRUE. ; exit ; endif
                 irs=MAX(ziW-rs, 1) ; jrs=    zjW        ; krs= MAX(jkout-rz, 1) !- W 
                 if ( msk_CMIP5_L75(irs,jrs,krs) .eq. 1 ) then ; lout=.TRUE. ; exit ; endif
                 irs=MAX(ziW-rs, 1) ; jrs=    zjS        ; krs= MAX(jkout-rz, 1) !- SWW
                 if ( msk_CMIP5_L75(irs,jrs,krs) .eq. 1 ) then ; lout=.TRUE. ; exit ; endif
                 irs=MAX(ziW-rs, 1) ; jrs=MAX(zjS-rs, 1) ; krs= MAX(jkout-rz, 1) !- SW
                 if ( msk_CMIP5_L75(irs,jrs,krs) .eq. 1 ) then ; lout=.TRUE. ; exit ; endif
                 irs=    ziW        ; jrs=MAX(zjS-rs, 1) ; krs= MAX(jkout-rz, 1) !- SSW 
                 if ( msk_CMIP5_L75(irs,jrs,krs) .eq. 1 ) then ; lout=.TRUE. ; exit ; endif
                 irs=    ziS        ; jrs=MAX(zjS-rs, 1) ; krs= MAX(jkout-rz, 1) !- S
                 if ( msk_CMIP5_L75(irs,jrs,krs) .eq. 1 ) then ; lout=.TRUE. ; exit ; endif
                 irs=    ziE        ; jrs=MAX(zjS-rs, 1) ; krs= MAX(jkout-rz, 1) !- SSE 
                 if ( msk_CMIP5_L75(irs,jrs,krs) .eq. 1 ) then ; lout=.TRUE. ; exit ; endif
                 irs=MIN(ziE+rs,mx) ; jrs=MAX(zjS-rs, 1) ; krs= MAX(jkout-rz, 1) !- SE 
                 if ( msk_CMIP5_L75(irs,jrs,krs) .eq. 1 ) then ; lout=.TRUE. ; exit ; endif
                 irs=MIN(ziE+rs,mx) ; jrs=    zjS        ; krs= MAX(jkout-rz, 1) !- SEE
                 if ( msk_CMIP5_L75(irs,jrs,krs) .eq. 1 ) then
                   lout=.TRUE.
                   exit
                 elseif ( rs .eq. rsmax+rzmax .and. rz .eq. rzmax ) then
                   write(*,*) '!@#$%^* FATAL PROBLEM !! Argh...'
                   write(*,953) jiout, jjout, jkout
                   953 FORMAT(' >>> you need to develop code to fill T-point (',3I5,')')
                   write(*,*) ' >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> STOP'
                   stop
                 endif

              enddo  !! rs=1,rsmax,1
              if (lout) exit
            enddo !! rz=0,rzmax,1

            !write(*,307) jiout, jjout, jkout, irs,jrs,krs
            !307 FORMAT('Further extrapolation at regional point (',3i4,') -> filled with global (',3i5,')')

            var_CMIP5_ORCA025(jiout,jjout,jkout,kt) = var_CMIP5_L75(irs,jrs,krs,kt)

         elseif ( zmask_out(jiout,jjout,jkout) .eq. 1 ) then  !- oceanic point in ORCA025 but not in CMIP5 model and velocity field :

            var_CMIP5_ORCA025(jiout,jjout,jkout,kt) = 0.0

         else

            var_CMIP5_ORCA025(jiout,jjout,jkout,kt) = missval 

         endif  ! ( abs(aS+aM+aN) .gt. eps )

         !write(*,*) 'toto14'

         if ( TRIM(varnam(ivar)) .eq. 'uo' ) then
           uo_CMIP5_ORCA025(jiout,jjout,jkout,kt) = uo_CMIP5_ORCA025(jiout,jjout,jkout,kt) + var_CMIP5_ORCA025(jiout,jjout,jkout,kt) * cos(angleX) * zmask_out(jiout,jjout,jkout)
           vo_CMIP5_ORCA025(jiout,jjout,jkout,kt) = vo_CMIP5_ORCA025(jiout,jjout,jkout,kt) - var_CMIP5_ORCA025(jiout,jjout,jkout,kt) * sin(angleX) * zmask_out(jiout,jjout,jkout)
         elseif ( TRIM(varnam(ivar)) .eq. 'vo' ) then
           uo_CMIP5_ORCA025(jiout,jjout,jkout,kt) = uo_CMIP5_ORCA025(jiout,jjout,jkout,kt) + var_CMIP5_ORCA025(jiout,jjout,jkout,kt) * sin(angleY) * zmask_out(jiout,jjout,jkout)
           vo_CMIP5_ORCA025(jiout,jjout,jkout,kt) = vo_CMIP5_ORCA025(jiout,jjout,jkout,kt) + var_CMIP5_ORCA025(jiout,jjout,jkout,kt) * cos(angleY) * zmask_out(jiout,jjout,jkout)
         elseif ( TRIM(varnam(ivar)) .eq. 'thetao' ) then
           to_CMIP5_ORCA025(jiout,jjout,jkout,kt) = var_CMIP5_ORCA025(jiout,jjout,jkout,kt) * zmask_out(jiout,jjout,jkout)
         elseif ( TRIM(varnam(ivar)) .eq. 'so' ) then
           so_CMIP5_ORCA025(jiout,jjout,jkout,kt) = var_CMIP5_ORCA025(jiout,jjout,jkout,kt) * zmask_out(jiout,jjout,jkout) 
         endif

      enddo  !! jkout=1,mz

     ENDIF !! IF ( zmask_out(jiout,jjout,1) .NE. 0 ) THEN
 
    enddo !! jjout=1,my
    enddo !! jiout=1,mx

    write(*,*) '      > check min e3 value on CMIP5 grid = ', MINVAL(e3)
    write(*,*) '      > check max e3 value on CMIP5 grid = ', MAXVAL(e3)
    write(*,*) '      > check land proportion in CMIP5 at surface = ', SUM(SUM(mask(:,:,1)*1,2),1)/SUM(SUM(mask(:,:,1)*0+1,2),1)

  ENDDO  !!  kt=1,mtime  

  !write(*,*) 'toto_new_1'

  if ( ivar .lt. nvar ) then
    DEALLOCATE( lev_bnds, lev, e3, var_CMIP5, lon, lat, e1, e2, dilon, djlon, dilat, djlat, i, j )
    DEALLOCATE( mask, msk_CMIP5_L75, var_CMIP5_L75, var_CMIP5_ORCA025, zlon, wlon )
    if ( lsigma ) then
      DEALLOCATE( eta, depth, lev_ref, lev_bnds_ref )
      if ( misig ) DEALLOCATE( zlev, zlev_bnds, sigma, sigma_bnds )
    endif
  endif

  !write(*,*) 'toto_new_2'

  if ( TRIM(varnam(ivar)) .eq. 'uo' ) then
    uo_missval = missval
    uo_units = units
  elseif ( TRIM(varnam(ivar)) .eq. 'vo' ) then
    vo_missval = missval
    vo_units = units
  elseif ( TRIM(varnam(ivar)) .eq. 'thetao' ) then
    to_missval = missval
    to_units = units
  elseif ( TRIM(varnam(ivar)) .eq. 'so' ) then
    so_missval = missval
    so_units = units
  endif

ENDDO !- ivar

!=================================================================================================
!- Write output gridT file :

time_counter = (/ 1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0, 10.0, 11.0, 12.0 /)

write(*,*) 'Writing ', TRIM(file_out_gridT)

status = NF90_CREATE(TRIM(file_out_gridT),NF90_NOCLOBBER,fidT)
call erreur(status,.TRUE.,'create gridT output')
                                                        
 status = NF90_DEF_DIM(fidT,"time_counter",NF90_UNLIMITED,dimID_time_counter)
 call erreur(status,.TRUE.,"def_dimID_time_counter")
 status = NF90_DEF_DIM(fidT,"deptht",mz,dimID_z)
 call erreur(status,.TRUE.,"def_dimID_z")
 status = NF90_DEF_DIM(fidT,"y",my,dimID_y)
 call erreur(status,.TRUE.,"def_dimID_y")
 status = NF90_DEF_DIM(fidT,"x",mx,dimID_x)
 call erreur(status,.TRUE.,"def_dimID_x")
                                                      
 status = NF90_DEF_VAR(fidT,"vosaline",NF90_FLOAT,(/dimID_x,dimID_y,dimID_z,dimID_time_counter/),vosaline_ID)
 call erreur(status,.TRUE.,"def_var_vosaline_ID")
 status = NF90_DEF_VAR(fidT,"votemper",NF90_FLOAT,(/dimID_x,dimID_y,dimID_z,dimID_time_counter/),votemper_ID)
 call erreur(status,.TRUE.,"def_var_votemper_ID")
 status = NF90_DEF_VAR(fidT,"time_counter",NF90_FLOAT,(/dimID_time_counter/),time_counter_ID)
 call erreur(status,.TRUE.,"def_var_time_counter_ID")
 status = NF90_DEF_VAR(fidT,"deptht",NF90_FLOAT,(/dimID_z/),nav_lev_ID)
 call erreur(status,.TRUE.,"def_var_nav_lev_ID")
 status = NF90_DEF_VAR(fidT,"nav_lat",NF90_FLOAT,(/dimID_x,dimID_y/),nav_lat_ID)
 call erreur(status,.TRUE.,"def_var_nav_lat_ID")
 status = NF90_DEF_VAR(fidT,"nav_lon",NF90_FLOAT,(/dimID_x,dimID_y/),nav_lon_ID)
 call erreur(status,.TRUE.,"def_var_nav_lon_ID")

 !write(*,*) 'toto15'                          
 
 status = NF90_PUT_ATT(fidT,vosaline_ID,"associate","time_counter deptht nav_lat nav_lon")
 call erreur(status,.TRUE.,"put_att_vosaline_ID")
 status = NF90_PUT_ATT(fidT,vosaline_ID,"axis","TZYX")
 call erreur(status,.TRUE.,"put_att_vosaline_ID")
 status = NF90_PUT_ATT(fidT,vosaline_ID,"short_name","vosaline")
 call erreur(status,.TRUE.,"put_att_vosaline_ID")
 status = NF90_PUT_ATT(fidT,vosaline_ID,"long_name","Salinity")
 call erreur(status,.TRUE.,"put_att_vosaline_ID")
 status = NF90_PUT_ATT(fidT,vosaline_ID,"missing_value",so_missval)
 call erreur(status,.TRUE.,"put_att_vosaline_ID")
 status = NF90_PUT_ATT(fidT,vosaline_ID,"units",so_units)
 call erreur(status,.TRUE.,"put_att_vosaline_ID")
 !-
 status = NF90_PUT_ATT(fidT,votemper_ID,"associate","time_counter deptht nav_lat nav_lon")
 call erreur(status,.TRUE.,"put_att_votemper_ID")
 status = NF90_PUT_ATT(fidT,votemper_ID,"axis","TZYX")
 call erreur(status,.TRUE.,"put_att_votemper_ID")
 status = NF90_PUT_ATT(fidT,votemper_ID,"short_name","votemper")
 call erreur(status,.TRUE.,"put_att_votemper_ID")
 status = NF90_PUT_ATT(fidT,votemper_ID,"long_name","Temperature")
 call erreur(status,.TRUE.,"put_att_votemper_ID")
 status = NF90_PUT_ATT(fidT,votemper_ID,"missing_value",to_missval)
 call erreur(status,.TRUE.,"put_att_votemper_ID")
 status = NF90_PUT_ATT(fidT,votemper_ID,"units",to_units)
 call erreur(status,.TRUE.,"put_att_votemper_ID")
 !-
 status = NF90_PUT_ATT(fidT,time_counter_ID,"long_name","Time axis")
 call erreur(status,.TRUE.,"put_att_time_counter_ID")
 status = NF90_PUT_ATT(fidT,time_counter_ID,"title","Time")
 call erreur(status,.TRUE.,"put_att_time_counter_ID")
 status = NF90_PUT_ATT(fidT,time_counter_ID,"time_origin","0001-JAN-15 00:00:00")
 call erreur(status,.TRUE.,"put_att_time_counter_ID")
 status = NF90_PUT_ATT(fidT,time_counter_ID,"units","months since 0001-01-15 00:00:00")
 call erreur(status,.TRUE.,"put_att_time_counter_ID")
 !-
 status = NF90_PUT_ATT(fidT,nav_lev_ID,"long_name","Vertical T levels")
 call erreur(status,.TRUE.,"put_att_nav_lev_ID")
 status = NF90_PUT_ATT(fidT,nav_lev_ID,"title","deptht")
 call erreur(status,.TRUE.,"put_att_nav_lev_ID")
 status = NF90_PUT_ATT(fidT,nav_lev_ID,"valid_max",75.)
 call erreur(status,.TRUE.,"put_att_nav_lev_ID")
 status = NF90_PUT_ATT(fidT,nav_lev_ID,"valid_min",0.)
 call erreur(status,.TRUE.,"put_att_nav_lev_ID")
 status = NF90_PUT_ATT(fidT,nav_lev_ID,"positive","unknown")
 call erreur(status,.TRUE.,"put_att_nav_lev_ID")
 status = NF90_PUT_ATT(fidT,nav_lev_ID,"units","m")
 call erreur(status,.TRUE.,"put_att_nav_lev_ID")
 !-
 status = NF90_PUT_ATT(fidT,nav_lat_ID,"nav_model","Default grid")
 call erreur(status,.TRUE.,"put_att_nav_lat_ID")
 status = NF90_PUT_ATT(fidT,nav_lat_ID,"long_name","Latitude")
 call erreur(status,.TRUE.,"put_att_nav_lat_ID")
 status = NF90_PUT_ATT(fidT,nav_lat_ID,"valid_max",89.94787)
 call erreur(status,.TRUE.,"put_att_nav_lat_ID")
 status = NF90_PUT_ATT(fidT,nav_lat_ID,"valid_min",-77.01048)
 call erreur(status,.TRUE.,"put_att_nav_lat_ID")
 status = NF90_PUT_ATT(fidT,nav_lat_ID,"units","degrees_north")
 call erreur(status,.TRUE.,"put_att_nav_lat_ID")
 !-
 status = NF90_PUT_ATT(fidT,nav_lon_ID,"nav_model","Default grid")
 call erreur(status,.TRUE.,"put_att_nav_lon_ID")
 status = NF90_PUT_ATT(fidT,nav_lon_ID,"long_name","Longitude")
 call erreur(status,.TRUE.,"put_att_nav_lon_ID")
 status = NF90_PUT_ATT(fidT,nav_lon_ID,"valid_max",180.)
 call erreur(status,.TRUE.,"put_att_nav_lon_ID")
 status = NF90_PUT_ATT(fidT,nav_lon_ID,"valid_min",-180.)
 call erreur(status,.TRUE.,"put_att_nav_lon_ID")
 status = NF90_PUT_ATT(fidT,nav_lon_ID,"units","degrees_east")
 call erreur(status,.TRUE.,"put_att_nav_lon_ID")

 status = NF90_PUT_ATT(fidT,NF90_GLOBAL,"rcp85ory","Created using interp_oce_CMIP5_to_ORCA025_rcp85.f90")
 call erreur(status,.TRUE.,"put_att_GLOBAL")

 status = NF90_ENDDEF(fidT)                   
 call erreur(status,.TRUE.,"fin_definition") 

 !write(*,*) 'toto16'

 status = NF90_PUT_VAR(fidT,vosaline_ID,so_CMIP5_ORCA025)
 call erreur(status,.TRUE.,"var_vosaline_ID")
 status = NF90_PUT_VAR(fidT,votemper_ID,to_CMIP5_ORCA025)
 call erreur(status,.TRUE.,"var_votemper_ID")
 status = NF90_PUT_VAR(fidT,time_counter_ID,time_counter)
 call erreur(status,.TRUE.,"var_time_counter_ID")
 status = NF90_PUT_VAR(fidT,nav_lev_ID,nav_lev)
 call erreur(status,.TRUE.,"var_nav_lev_ID")
 status = NF90_PUT_VAR(fidT,nav_lat_ID,nav_lat)
 call erreur(status,.TRUE.,"var_nav_lat_ID")
 status = NF90_PUT_VAR(fidT,nav_lon_ID,nav_lon)
 call erreur(status,.TRUE.,"var_nav_lon_ID")

status = NF90_CLOSE(fidT)                    
call erreur(status,.TRUE.,"final")
 
!=================================================================================================
!- Write output gridU file :

write(*,*) 'Writing ', TRIM(file_out_gridU)

status = NF90_CREATE(TRIM(file_out_gridU),NF90_NOCLOBBER,fidU)
call erreur(status,.TRUE.,'create gridU output')

 status = NF90_DEF_DIM(fidU,"time_counter",NF90_UNLIMITED,dimID_time_counter)
 call erreur(status,.TRUE.,"def_dimID_time_counter")
 status = NF90_DEF_DIM(fidU,"depthu",mz,dimID_z)
 call erreur(status,.TRUE.,"def_dimID_z")
 status = NF90_DEF_DIM(fidU,"y",my,dimID_y)
 call erreur(status,.TRUE.,"def_dimID_y")
 status = NF90_DEF_DIM(fidU,"x",mx,dimID_x)
 call erreur(status,.TRUE.,"def_dimID_x")

 status = NF90_DEF_VAR(fidU,"vozocrtx",NF90_FLOAT,(/dimID_x,dimID_y,dimID_z,dimID_time_counter/),vozocrtx_ID)
 call erreur(status,.TRUE.,"def_var_vozocrtx_ID")
 status = NF90_DEF_VAR(fidU,"time_counter",NF90_FLOAT,(/dimID_time_counter/),time_counter_ID)
 call erreur(status,.TRUE.,"def_var_time_counter_ID")
 status = NF90_DEF_VAR(fidU,"depthu",NF90_FLOAT,(/dimID_z/),nav_lev_ID)
 call erreur(status,.TRUE.,"def_var_nav_lev_ID")
 status = NF90_DEF_VAR(fidU,"nav_lat",NF90_FLOAT,(/dimID_x,dimID_y/),nav_lat_ID)
 call erreur(status,.TRUE.,"def_var_nav_lat_ID")
 status = NF90_DEF_VAR(fidU,"nav_lon",NF90_FLOAT,(/dimID_x,dimID_y/),nav_lon_ID)
 call erreur(status,.TRUE.,"def_var_nav_lon_ID")

 !write(*,*) 'toto17'

 status = NF90_PUT_ATT(fidU,vozocrtx_ID,"associate","time_counter depthu nav_lat nav_lon")
 call erreur(status,.TRUE.,"put_att_vozocrtx_ID")
 status = NF90_PUT_ATT(fidU,vozocrtx_ID,"axis","TZYX")
 call erreur(status,.TRUE.,"put_att_vozocrtx_ID")
 status = NF90_PUT_ATT(fidU,vozocrtx_ID,"short_name","vozocrtx")
 call erreur(status,.TRUE.,"put_att_vozocrtx_ID")
 status = NF90_PUT_ATT(fidU,vozocrtx_ID,"long_name","Zonal Velocity")
 call erreur(status,.TRUE.,"put_att_vozocrtx_ID")
 status = NF90_PUT_ATT(fidU,vozocrtx_ID,"missing_value",uo_missval)
 call erreur(status,.TRUE.,"put_att_vozocrtx_ID")
 status = NF90_PUT_ATT(fidU,vozocrtx_ID,"units",uo_units)
 call erreur(status,.TRUE.,"put_att_vozocrtx_ID")
 !
 status = NF90_PUT_ATT(fidU,time_counter_ID,"long_name","Time axis")
 call erreur(status,.TRUE.,"put_att_time_counter_ID")
 status = NF90_PUT_ATT(fidU,time_counter_ID,"title","Time")
 call erreur(status,.TRUE.,"put_att_time_counter_ID")
 status = NF90_PUT_ATT(fidU,time_counter_ID,"time_origin","0001-JAN-15 00:00:00")
 call erreur(status,.TRUE.,"put_att_time_counter_ID")
 status = NF90_PUT_ATT(fidU,time_counter_ID,"units","months since 0001-01-15 00:00:00")
 call erreur(status,.TRUE.,"put_att_time_counter_ID")
 !-
 status = NF90_PUT_ATT(fidU,nav_lev_ID,"long_name","Vertical U levels")
 call erreur(status,.TRUE.,"put_att_nav_lev_ID")
 status = NF90_PUT_ATT(fidU,nav_lev_ID,"title","depthu")
 call erreur(status,.TRUE.,"put_att_nav_lev_ID")
 status = NF90_PUT_ATT(fidU,nav_lev_ID,"valid_max",75.)
 call erreur(status,.TRUE.,"put_att_nav_lev_ID")
 status = NF90_PUT_ATT(fidU,nav_lev_ID,"valid_min",0.)
 call erreur(status,.TRUE.,"put_att_nav_lev_ID")
 status = NF90_PUT_ATT(fidU,nav_lev_ID,"positive","unknown")
 call erreur(status,.TRUE.,"put_att_nav_lev_ID")
 status = NF90_PUT_ATT(fidU,nav_lev_ID,"units","m")
 call erreur(status,.TRUE.,"put_att_nav_lev_ID")
 !-
 status = NF90_PUT_ATT(fidU,nav_lat_ID,"nav_model","Default grid")
 call erreur(status,.TRUE.,"put_att_nav_lat_ID")
 status = NF90_PUT_ATT(fidU,nav_lat_ID,"long_name","Latitude")
 call erreur(status,.TRUE.,"put_att_nav_lat_ID")
 status = NF90_PUT_ATT(fidU,nav_lat_ID,"valid_max",89.95911)
 call erreur(status,.TRUE.,"put_att_nav_lat_ID")
 status = NF90_PUT_ATT(fidU,nav_lat_ID,"valid_min",-77.01048)
 call erreur(status,.TRUE.,"put_att_nav_lat_ID")
 status = NF90_PUT_ATT(fidU,nav_lat_ID,"units","degrees_north")
 call erreur(status,.TRUE.,"put_att_nav_lat_ID")
 !-
 status = NF90_PUT_ATT(fidU,nav_lon_ID,"nav_model","Default grid")
 call erreur(status,.TRUE.,"put_att_nav_lon_ID")
 status = NF90_PUT_ATT(fidU,nav_lon_ID,"long_name","Longitude")
 call erreur(status,.TRUE.,"put_att_nav_lon_ID")
 status = NF90_PUT_ATT(fidU,nav_lon_ID,"valid_max",179.9996)
 call erreur(status,.TRUE.,"put_att_nav_lon_ID")
 status = NF90_PUT_ATT(fidU,nav_lon_ID,"valid_min",-179.9991)
 call erreur(status,.TRUE.,"put_att_nav_lon_ID")
 status = NF90_PUT_ATT(fidU,nav_lon_ID,"units","degrees_east")
 call erreur(status,.TRUE.,"put_att_nav_lon_ID")

 status = NF90_PUT_ATT(fidU,NF90_GLOBAL,"rcp85ory","Created using interp_oce_CMIP5_to_ORCA025_rcp85.f90")
 call erreur(status,.TRUE.,"put_att_GLOBAL_U")

 status = NF90_ENDDEF(fidU)                   
 call erreur(status,.TRUE.,"fin_definition") 

 status = NF90_PUT_VAR(fidU,vozocrtx_ID,uo_CMIP5_ORCA025)
 call erreur(status,.TRUE.,"var_vozocrtx_ID")
 status = NF90_PUT_VAR(fidU,time_counter_ID,time_counter)
 call erreur(status,.TRUE.,"var_time_counter_ID")
 status = NF90_PUT_VAR(fidU,nav_lev_ID,nav_lev)
 call erreur(status,.TRUE.,"var_nav_lev_ID")
 status = NF90_PUT_VAR(fidU,nav_lat_ID,nav_lat)
 call erreur(status,.TRUE.,"var_nav_lat_ID")
 status = NF90_PUT_VAR(fidU,nav_lon_ID,nav_lon)
 call erreur(status,.TRUE.,"var_nav_lon_ID")

status = NF90_CLOSE(fidU)                    
call erreur(status,.TRUE.,"final")         

!=================================================================================================
!- Write output gridV file :

write(*,*) 'Writing ', TRIM(file_out_gridV)

status = NF90_CREATE(TRIM(file_out_gridV),NF90_NOCLOBBER,fidV)
call erreur(status,.TRUE.,'create')                     

 status = NF90_DEF_DIM(fidV,"time_counter",NF90_UNLIMITED,dimID_time_counter)
 call erreur(status,.TRUE.,"def_dimID_time_counter")
 status = NF90_DEF_DIM(fidV,"depthv",mz,dimID_z)
 call erreur(status,.TRUE.,"def_dimID_z")
 status = NF90_DEF_DIM(fidV,"y",my,dimID_y)
 call erreur(status,.TRUE.,"def_dimID_y")
 status = NF90_DEF_DIM(fidV,"x",mx,dimID_x)
 call erreur(status,.TRUE.,"def_dimID_x")

 status = NF90_DEF_VAR(fidV,"vomecrty",NF90_FLOAT,(/dimID_x,dimID_y,dimID_z,dimID_time_counter/),vomecrty_ID)
 call erreur(status,.TRUE.,"def_var_vomecrty_ID")
 status = NF90_DEF_VAR(fidV,"time_counter",NF90_FLOAT,(/dimID_time_counter/),time_counter_ID)
 call erreur(status,.TRUE.,"def_var_time_counter_ID")
 status = NF90_DEF_VAR(fidV,"depthv",NF90_FLOAT,(/dimID_z/),nav_lev_ID)
 call erreur(status,.TRUE.,"def_var_nav_lev_ID")
 status = NF90_DEF_VAR(fidV,"nav_lat",NF90_FLOAT,(/dimID_x,dimID_y/),nav_lat_ID)
 call erreur(status,.TRUE.,"def_var_nav_lat_ID")
 status = NF90_DEF_VAR(fidV,"nav_lon",NF90_FLOAT,(/dimID_x,dimID_y/),nav_lon_ID)
 call erreur(status,.TRUE.,"def_var_nav_lon_ID")

 !write(*,*) 'toto18'

 status = NF90_PUT_ATT(fidV,vomecrty_ID,"associate","time_counter depthv nav_lat nav_lon")
 call erreur(status,.TRUE.,"put_att_vomecrty_ID")
 status = NF90_PUT_ATT(fidV,vomecrty_ID,"axis","TZYX")
 call erreur(status,.TRUE.,"put_att_vomecrty_ID")
 status = NF90_PUT_ATT(fidV,vomecrty_ID,"short_name","vomecrty")
 call erreur(status,.TRUE.,"put_att_vomecrty_ID")
 status = NF90_PUT_ATT(fidV,vomecrty_ID,"long_name","Meridional Velocity")
 call erreur(status,.TRUE.,"put_att_vomecrty_ID")
 status = NF90_PUT_ATT(fidV,vomecrty_ID,"missing_value",vo_missval)
 call erreur(status,.TRUE.,"put_att_vomecrty_ID")
 status = NF90_PUT_ATT(fidV,vomecrty_ID,"units",vo_units)
 call erreur(status,.TRUE.,"put_att_vomecrty_ID")
 !-
 status = NF90_PUT_ATT(fidV,time_counter_ID,"long_name","Time axis")
 call erreur(status,.TRUE.,"put_att_time_counter_ID")
 status = NF90_PUT_ATT(fidV,time_counter_ID,"title","Time")
 call erreur(status,.TRUE.,"put_att_time_counter_ID")
 status = NF90_PUT_ATT(fidV,time_counter_ID,"time_origin","0001-JAN-15 00:00:00")
 call erreur(status,.TRUE.,"put_att_time_counter_ID")
 status = NF90_PUT_ATT(fidV,time_counter_ID,"units","months since 0001-01-15 00:00:00")
 call erreur(status,.TRUE.,"put_att_time_counter_ID")
 !-
 status = NF90_PUT_ATT(fidV,nav_lev_ID,"long_name","Vertical V levels")
 call erreur(status,.TRUE.,"put_att_nav_lev_ID")
 status = NF90_PUT_ATT(fidV,nav_lev_ID,"title","depthv")
 call erreur(status,.TRUE.,"put_att_nav_lev_ID")
 status = NF90_PUT_ATT(fidV,nav_lev_ID,"valid_max",75.)
 call erreur(status,.TRUE.,"put_att_nav_lev_ID")
 status = NF90_PUT_ATT(fidV,nav_lev_ID,"valid_min",0.)
 call erreur(status,.TRUE.,"put_att_nav_lev_ID")
 status = NF90_PUT_ATT(fidV,nav_lev_ID,"positive","unknown")
 call erreur(status,.TRUE.,"put_att_nav_lev_ID")
 status = NF90_PUT_ATT(fidV,nav_lev_ID,"units","m")
 call erreur(status,.TRUE.,"put_att_nav_lev_ID")
 !-
 status = NF90_PUT_ATT(fidV,nav_lat_ID,"nav_model","Default grid")
 call erreur(status,.TRUE.,"put_att_nav_lat_ID")
 status = NF90_PUT_ATT(fidV,nav_lat_ID,"long_name","Latitude")
 call erreur(status,.TRUE.,"put_att_nav_lat_ID")
 status = NF90_PUT_ATT(fidV,nav_lat_ID,"valid_max",89.92738)
 call erreur(status,.TRUE.,"put_att_nav_lat_ID")
 status = NF90_PUT_ATT(fidV,nav_lat_ID,"valid_min",-76.98235)
 call erreur(status,.TRUE.,"put_att_nav_lat_ID")
 status = NF90_PUT_ATT(fidV,nav_lat_ID,"units","degrees_north")
 call erreur(status,.TRUE.,"put_att_nav_lat_ID")
 !-
 status = NF90_PUT_ATT(fidV,nav_lon_ID,"nav_model","Default grid")
 call erreur(status,.TRUE.,"put_att_nav_lon_ID")
 status = NF90_PUT_ATT(fidV,nav_lon_ID,"long_name","Longitude")
 call erreur(status,.TRUE.,"put_att_nav_lon_ID")
 status = NF90_PUT_ATT(fidV,nav_lon_ID,"valid_max",180.)
 call erreur(status,.TRUE.,"put_att_nav_lon_ID")
 status = NF90_PUT_ATT(fidV,nav_lon_ID,"valid_min",-180.)
 call erreur(status,.TRUE.,"put_att_nav_lon_ID")
 status = NF90_PUT_ATT(fidV,nav_lon_ID,"units","degrees_east")
 call erreur(status,.TRUE.,"put_att_nav_lon_ID")

 status = NF90_PUT_ATT(fidV,NF90_GLOBAL,"rcp85ory","Created using interp_oce_CMIP5_to_ORCA025_rcp85.f90")
 call erreur(status,.TRUE.,"put_att_GLOBAL_V")

 status = NF90_ENDDEF(fidV)                   
 call erreur(status,.TRUE.,"fin_definition") 

 status = NF90_PUT_VAR(fidV,vomecrty_ID,vo_CMIP5_ORCA025)
 call erreur(status,.TRUE.,"var_vomecrty_ID")
 status = NF90_PUT_VAR(fidV,time_counter_ID,time_counter)
 call erreur(status,.TRUE.,"var_time_counter_ID")
 status = NF90_PUT_VAR(fidV,nav_lev_ID,nav_lev)
 call erreur(status,.TRUE.,"var_nav_lev_ID")
 status = NF90_PUT_VAR(fidV,nav_lat_ID,nav_lat)
 call erreur(status,.TRUE.,"var_nav_lat_ID")
 status = NF90_PUT_VAR(fidV,nav_lon_ID,nav_lon)
 call erreur(status,.TRUE.,"var_nav_lon_ID")

status = NF90_CLOSE(fidV)                    
call erreur(status,.TRUE.,"final")         

!=================================================================================================
!=================================================================================================

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
    WRITE(*,*) 'CHECK IN ROUTINE: ', TRIM(chaine)
    WRITE(*,*) 'ERROR: ', iret
    message=NF90_STRERROR(iret)
    WRITE(*,*) 'WHICH MEANS:',TRIM(message)
    IF ( lstop ) STOP
  ENDIF
  !
END SUBROUTINE erreur
