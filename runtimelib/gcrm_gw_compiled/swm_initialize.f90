   MODULE swm_initialize
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!  PURPOSE:  
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   USE kinds
   USE numbers

   USE parallel_params
   USE parallel_utilities

   USE grid_params
   USE grid_subdomain
   USE grid_connectivity
   USE grid_metrics
   USE grid_utilities

   USE utilities_advection_horz
   USE utilities_interp
   USE utilities_timer
   USE utilities_io
   USE wrap_data
   USE wrp1D_data
   USE multigrid
   USE multigrid_2D

   USE swm_params
   USE swm_params_time
   USE swm_params_vertical

   USE swm_vars_prognostic
   USE swm_vars_diagnostic

   USE swm_wnd

   IMPLICIT NONE
   SAVE

   CONTAINS
!=======================================================================
!  BEGIN initialize_swm
!=======================================================================
   SUBROUTINE initialize_swm ()
!.......................................................................
!  LOCAL
!.......................................................................
   INTEGER (KIND=int_kind) :: &
      i,j,jj,k,c,e,nsd,iter,m,crn,edg,di(7),dj(7),ierr
   REAL (KIND=dbl_kind) :: &
      lonlat(2),lon,lat,rho,rotation_matrix(3,3), &
      p(3),p1(3),p2(3),mid(3),nrm(3),east(3),north(3),wndwnd(3),u,v, &
      alph,r,r0,lon_c,lat_c,u0,u1,h0,lat0,lon0,beta,thing,thing1,thing2,v0
   REAL (KIND=dbl_kind) :: &
      om,kK,rR,cos2,a0,a1,a2,aA,bB,cC
   REAL (KIND=dbl_kind) :: &
      lat3,lat1,umax,en,sum_sum,h_hat,lon7,lat7,uu,vv
   REAL (KIND=dbl_kind),DIMENSION(  edgm,im,jm,  km,nsdm) :: &
      tmpry01_edg
   REAL (KIND=dbl_kind),DIMENSION(3,edgm,im,jm,  km,nsdm) :: &
      tmpry01_edg3D
   REAL (KIND=dbl_kind),DIMENSION(crnm,im,jm,km,nsdm) :: &
      psi_crn

!.:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:.
!-----------------------------------------------------------------------
!  set subdomains. partition global grid and assign pieces to processes.
!-----------------------------------------------------------------------
   CALL initialize_subdomain ()
!-----------------------------------------------------------------------
!  set tree data structure. each process builds its portion of the global grid.
!-----------------------------------------------------------------------
   CALL timer (event_name="initialize_grid_connectivity",action="start")
   CALL initialize_grid_connectivity ("world")
   CALL timer (event_name="initialize_grid_connectivity",action="stop")
!-----------------------------------------------------------------------
!  set parallel communication for ghost cell updates of 2D-array data structure
!-----------------------------------------------------------------------
   CALL timer (event_name="initialize_wrap",action="start")
   CALL initialize_wrap ("world","swm",level_max, &
                 sbdmn(level_max)%sbdmn_iota,sbdmn(level_max)%lst(:), &
                 l_report=.FALSE.)
   CALL timer (event_name="initialize_wrap",action="stop")
!-----------------------------------------------------------------------
!  set grid metrics 
!-----------------------------------------------------------------------
   CALL parallel_barrier ("world")
   CALL initialize_grid_metrics ("swm",grid_point_select,grid_point_path)
!-----------------------------------------------------------------------
!  set parallel communication for ghost cell updates of 1D-array data structure
!-----------------------------------------------------------------------
   CALL parallel_barrier ("world")
   CALL timer (event_name="initialize_wrp1D",action="start")
   CALL initialize_wrp1D ("world","swm",level_max)
   CALL timer (event_name="initialize_wrp1D",action="stop")
!-----------------------------------------------------------------------
!  initialize multigrid
!-----------------------------------------------------------------------
   CALL timer (event_name="initialize_multigrid",action="start")
   CALL mltgrd_init ("world")
   CALL timer (event_name="initialize_multigrid",action="stop")

   CALL initialize_params_time ()

   CALL initialize_vars_diagnostic ()
   CALL initialize_vars_prognostic ()

   CALL initialize_advection_horz ("swm")

   CALL initialize_interp ("swm")
!-----------------------------------------------------------------------
!  write a report
!-----------------------------------------------------------------------
   CALL write_report_swm ()
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!  SET THE INITIAL CONDITIONS
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   IF (l_restart) THEN
!    << add restart code here >>
   ELSE
      SELECT CASE (initial_condition_select)
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!  BAROTROPIC VORTICITY TEST
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      CASE (00)
!-----------------------------------------------------------------------
!  set geopot_surf
!-----------------------------------------------------------------------
         geopot_surf = zero
!-----------------------------------------------------------------------
!  set mass and divergence
!-----------------------------------------------------------------------
         mss = one
         div = zero
!-----------------------------------------------------------------------
!  set relative vorticity
!-----------------------------------------------------------------------
         beta = pi/12._dbl_kind
         rotation_matrix(:,:)=RESHAPE ( (/  COS (beta), zero,-SIN (beta), &
                                                 zero,  one,      zero, &
                                            SIN (beta), zero, COS (beta) /),(/3,3/))
         u0   =  20.0_dbl_kind
         u1   = 200.0_dbl_kind
         v0   =   0.0_dbl_kind 
         alph =  16.0_dbl_kind
         lat0 =  pi/three

            DO nsd = 1,nsdm
               DO j = 1,jm
                  DO i = 1,im
                     p(:) = unit_vector (MATMUL (rotation_matrix,point(:,i,j,nsd)))

                     lonlat(:) = xyz_to_lonlat (p(:)); lon=lonlat(1); lat=lonlat(2);

                     thing1 = EXP (two*alph*(lat-lat0))
                     relative(i,j,:,nsd) = (two*alph*thing1*(u0-u1)*COS (lat)+ &
                        (one+thing1)*(u0+thing1*u1)*SIN (lat))/(a*(one+thing1)**2)

                     eta(i,j,:,np0,nsd) = relative(i,j,:,nsd)+two*omega*SIN (lat)
                     f  (i,j,      nsd) = two*omega*SIN (lat)

                  ENDDO
               ENDDO
            ENDDO
            CALL wrap ("swm",face=mss(:,:,:,np0,:))
            CALL wrap ("swm",face=eta(:,:,:,np0,:))
            CALL wrap ("swm",face=div(:,:,:,np0,:))
            CALL wrap ("swm",face_1lyr=f)
!-----------------------------------------------------------------------
!  solve for stream function and velocity potential. 
!  set normal and tangent wind
!-----------------------------------------------------------------------
         DO k = 1,km
            relative(:,:,k,:) = eta(:,:,k,np0,:) - f(:,:,:)
         ENDDO
         DO iter = 1,12
            CALL mltgrd2D ("world",km,relative,psi)
         ENDDO
         CALL wrap ("swm",face=psi)
         DO iter = 1,12
            CALL mltgrd2D ("world",km,div(:,:,:,np0,:),chi)
         ENDDO
         CALL wrap ("swm",face=chi)
         CALL set_wnd (eta(:,:,:,np0,:),div(:,:,:,np0,:),f,relative,psi,chi,ke, &
                                                     wnd_nrm,wnd_tng,wnd_edg)

         CALL wrt_fld (path_output,"mss","ascii",proc=rnk_wrld, &
                       time=time_swm,time_unit="h",face=mss     (:,:,1:1,np0,:))
         CALL wrt_fld (path_output,"eta","ascii",proc=rnk_wrld, &
                       time=time_swm,time_unit="h",face=eta     (:,:,1:1,np0,:))
         CALL wrt_fld (path_output,"div","ascii",proc=rnk_wrld, &
                       time=time_swm,time_unit="h",face=div     (:,:,1:1,np0,:))
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!  WILLIAMSON test case 1 (JOURNAL OF COMPUTATIONAL PHYSICS 102, 211-224 (1992) )
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      CASE (01)
!-----------------------------------------------------------------------
!  set geopot_surf
!-----------------------------------------------------------------------
         geopot_surf = zero
!-----------------------------------------------------------------------
!  set mss, eta and div
!-----------------------------------------------------------------------
            u0 = 38.6107373_dbl_kind; h0 = 1000.0_dbl_kind; rR = a/three; ! TEST 1
            lon_c =-1.5707963267948966192313216916398_dbl_kind
            lat_c = zero

            eta      = zero
            relative = zero
            f        = zero
            div      = zero
            chi      = zero

            DO nsd = 1,nsdm
               DO j = 1,jm
                  DO i = 1,im
                     lonlat(:) = xyz_to_lonlat (point(:,i,j,nsd))
                     lon = lonlat(1); lat = lonlat(2);

                     mss(i,j,:,np0,nsd) = mss_TRUE_IC01 (advctn_alph_IC01,zero,point(:,i,j,nsd))

                     psi(i,j,:,    nsd) = -a*u0*(SIN (lat)*COS (advctn_alph_IC01)-COS (lon)*COS (lat)*SIN (advctn_alph_IC01))
                  ENDDO
               ENDDO
            ENDDO

            IF (.FALSE.) THEN ! wnd_nrm is set using the stream function

               CALL set_wnd_nrm (psi,chi,wnd_nrm)
               CALL set_wnd_tng (psi,chi,wnd_tng)

            ELSE              ! wnd_nrm is set using the analytic zonal wind

               tmpry01_edg = zero

               DO nsd = 1,nsdm
                  DO j = 1,jm
                     DO i = 1,im
                        DO e = 1,3
                           lonlat(:) = xyz_to_lonlat (point_edg(:,e,i,j,nsd))
                           lon = lonlat(1); lat = lonlat(2);

                           east  = local_east  (point_edg(:,e,i,j,nsd))
                           north = local_north (point_edg(:,e,i,j,nsd))

                           uu =  u0*(COS (lat)*COS (advctn_alph_IC01)+SIN (lat)*COS (lon)*SIN (advctn_alph_IC01))
                           vv = -u0* SIN (lon)*SIN (advctn_alph_IC01)

                           wndwnd = uu*east + vv*north

                           wnd_nrm(e,i,j,:,nsd) = DOT_PRODUCT (wndwnd,nrm_edg(:,e,i,j,nsd))
                           wnd_tng(e,i,j,:,nsd) = DOT_PRODUCT (wndwnd,tng_edg(:,e,i,j,nsd))

                        ENDDO
                     ENDDO
                  ENDDO
               ENDDO
            ENDIF

            CALL wrt_fld (path_output,"mss","ascii",proc=rnk_wrld, &
                          time=time_swm,time_unit=time_unit_strng,face=mss(:,:,1:1,np0,:))
            CALL wrt_fld (path_output,"wnd_nrm","parallel",proc=rnk_wrld, &
                          time=time_swm,time_unit=time_unit_strng,edge=wnd_nrm)
            CALL wrt_fld (path_output,"wnd_tng","parallel",proc=rnk_wrld, &
                          time=time_swm,time_unit=time_unit_strng,edge=wnd_tng)
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!  WILLIAMSON test case 2 (JOURNAL OF COMPUTATIONAL PHYSICS 102, 211-224 (1992) )
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      CASE (02)
!-----------------------------------------------------------------------
!  set geopot_surf
!-----------------------------------------------------------------------
         geopot_surf = zero
!-----------------------------------------------------------------------
!  set mss, eta and div
!-----------------------------------------------------------------------
            u0 = 38.6107_dbl_kind; h0 = 2998.0_dbl_kind; ! TEST 2
            DO nsd = 1,nsdm
               DO j = 1,jm
                  DO i = 1,im
                     lonlat(:) = xyz_to_lonlat (point(:,i,j,nsd))
                     lon = lonlat(1); lat = lonlat(2);
                     mss(i,j,:,np0,nsd) = &
                          h0 - (one/grav)*(a*omega*u0+half*u0**2)*SIN (lat)**2
                     eta     (i,j,:,np0,nsd) = two*((u0/a)+omega)*SIN (lat)
                     relative(i,j,:,    nsd) = two*((u0/a)      )*SIN (lat)
                     f       (i,j,      nsd) = two*omega*SIN (lat)
                     div     (i,j,:,np0,nsd) = zero
                  ENDDO
               ENDDO
            ENDDO
            CALL wrap ("swm",face=mss(:,:,:,np0,:))
            CALL wrap ("swm",face=eta(:,:,:,np0,:))
            CALL wrap ("swm",face=div(:,:,:,np0,:))
            CALL wrap ("swm",face_1lyr=f)
!-----------------------------------------------------------------------
!  solve for stream function and velocity potential. 
!  set normal and tangent wind
!-----------------------------------------------------------------------
         DO k = 1,km
            relative(:,:,k,:) = eta(:,:,k,np0,:) - f(:,:,:)
         ENDDO
         DO iter = 1,12
            CALL mltgrd2D ("world",km,relative,psi)
         ENDDO
         CALL wrap ("swm",face=psi)
         DO iter = 1,12
            CALL mltgrd2D ("world",km,div(:,:,:,np0,:),chi)
         ENDDO
         CALL wrap ("swm",face=chi)
         CALL set_wnd (eta(:,:,:,np0,:),div(:,:,:,np0,:),f,relative,psi,chi,ke, &
                                                     wnd_nrm,wnd_tng,wnd_edg)

            CALL wrt_fld (path_output,"mss","ascii",proc=rnk_wrld, &
                          time=time_swm,time_unit="h",face=mss     (:,:,1:1,np0,:))
            CALL wrt_fld (path_output,"eta","ascii",proc=rnk_wrld, &
                          time=time_swm,time_unit="h",face=eta     (:,:,1:1,np0,:))
            CALL wrt_fld (path_output,"div","ascii",proc=rnk_wrld, &
                          time=time_swm,time_unit="h",face=div     (:,:,1:1,np0,:))
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!  WILLIAMSON test case 5 (JOURNAL OF COMPUTATIONAL PHYSICS 102, 211-224 (1992) )
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      CASE (05)
!-----------------------------------------------------------------------
!  set geopot_surf
!-----------------------------------------------------------------------
            lon_c =-1.5707963267948966192313216916398_dbl_kind
            lat_c = 0.5235987755982988730771072305466_dbl_kind
            r0 = 0.34906585039886591538473815369772_dbl_kind
            DO nsd = 1,nsdm
               DO j = 1,jm
                  DO i = 1,im
                     lonlat(:) = xyz_to_lonlat (point(:,i,j,nsd))
                     lon = lonlat(1); lat = lonlat(2);
                     r = MIN (r0,SQRT ((lon-lon_c)**2+(lat-lat_c)**2))
                     geopot_surf(i,j,nsd) = grav*(2000.0_dbl_kind*(one-r/r0))
                  ENDDO
               ENDDO
            ENDDO
            CALL wrap ("swm",face_1lyr=geopot_surf(:,:,:))
!-----------------------------------------------------------------------
!  set mss, eta and div
!-----------------------------------------------------------------------
            u0 = 20.0000_dbl_kind; h0 = 5960.0_dbl_kind; ! TEST 5
            DO nsd = 1,nsdm
               DO j = 1,jm
                  DO i = 1,im
                     lonlat(:) = xyz_to_lonlat (point(:,i,j,nsd))
                     lon = lonlat(1); lat = lonlat(2);
                     mss(i,j,:,np0,nsd) = &
                          h0 - (one/grav)*(a*omega*u0+half*u0**2)*SIN (lat)**2
                     mss(i,j,:,np0,nsd) = &
                                mss(i,j,:,np0,nsd) - geopot_surf(i,j,nsd)/grav
                     eta     (i,j,:,np0,nsd) = two*((u0/a)+omega)*SIN (lat)
                     relative(i,j,:,    nsd) = two*((u0/a)      )*SIN (lat)
                     f       (i,j,      nsd) = two*omega*SIN (lat)

                     div     (i,j,:,np0,nsd) = zero
                  ENDDO
               ENDDO
            ENDDO
            CALL wrap ("swm",face=mss(:,:,:,np0,:))
            CALL wrap ("swm",face=eta(:,:,:,np0,:))
            CALL wrap ("swm",face=div(:,:,:,np0,:))
            CALL wrap ("swm",face_1lyr=f)
!-----------------------------------------------------------------------
!  solve for stream function and velocity potential. 
!  set normal and tangent wind
!-----------------------------------------------------------------------
         DO k = 1,km
            relative(:,:,k,:) = eta(:,:,k,np0,:) - f(:,:,:)
         ENDDO
         DO iter = 1,12
            CALL mltgrd2D ("world",km,relative,psi)
         ENDDO
         CALL wrap ("swm",face=psi)
         DO iter = 1,12
            CALL mltgrd2D ("world",km,div(:,:,:,np0,:),chi)
         ENDDO
         CALL wrap ("swm",face=chi)
         CALL set_wnd (eta(:,:,:,np0,:),div(:,:,:,np0,:),f,relative,psi,chi,ke, &
                                                     wnd_nrm,wnd_tng,wnd_edg)

         CALL wrt_fld (path_output,"mss","ascii",proc=rnk_wrld, &
                          time=time_swm,time_unit="h",face=mss     (:,:,1:1,np0,:))
         CALL wrt_fld (path_output,"eta","ascii",proc=rnk_wrld, &
                          time=time_swm,time_unit="h",face=eta     (:,:,1:1,np0,:))
         CALL wrt_fld (path_output,"div","ascii",proc=rnk_wrld, &
                          time=time_swm,time_unit="h",face=div     (:,:,1:1,np0,:))
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!  ROSSBY-HAURWITZ WAVE. WILLIAMSON test case 6 (JOURNAL OF COMPUTATIONAL PHYSICS 102, 211-224 (1992) )
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      CASE (06)
!-----------------------------------------------------------------------
!  set geopot_surf
!-----------------------------------------------------------------------
         geopot_surf = zero
!-----------------------------------------------------------------------
!  set mss, eta and div
!-----------------------------------------------------------------------

   om = 7.848E-06_dbl_kind
   kK = 7.848E-06_dbl_kind
   h0 = 8000.0_dbl_kind
   rR = 4.0_dbl_kind

            DO nsd = 1,nsdm
               DO j = 1,jm
                  DO i = 1,im
                     lonlat(:) = xyz_to_lonlat (point(:,i,j,nsd))
                     lon = lonlat(1); lat = lonlat(2);
!~~~~
   cos2 = (COS (lat))**two
!~~~~
   a0 = om*(two*omega+om)*cos2
   a1 = (kK**two)*(cos2**rR)
   a2 = (rR+one)*cos2 + (two*rR**two-rR-two) - two*(rR**two)/cos2

   aA = a0/two + (a1*a2)/four
!~~~~
   a0 = (two*(omega+om)*kK)/((rR+one)*(rR+two))
   a1 = (COS (lat))**rR
   a2 = (rR**two+two*rR+two) - ((rR+one)**two)*cos2

   bB = a0*a1*a2
!~~~~
   a0 = (kK**two)*(cos2**rR)
   a1 = (rR+one)*cos2 - (rR+two)

   cC = (a0*a1)/four
!~~~~
   mss     (i,j,:,np0,nsd) = h0 + ((a**two)*(aA+bB*COS (rR*lon)+cC*COS (two*rR*lon)))/grav

   relative(i,j,:,    nsd) = two*om*SIN (lat) - kK*SIN (lat)*((COS (lat))**rR)*(rR**two+three*rR+two)*COS (rR*lon)
   f       (i,j,      nsd) = two*omega*SIN (lat)
   eta     (i,j,:,np0,nsd) = relative(i,j,:,nsd) + f(i,j,nsd)

   div     (i,j,:,np0,nsd) = zero

!  psi     (i,j,:,    nsd) = -(a**two)*om*SIN (lat) + (a**two)*kK*((COS (lat))**(rR))*SIN (lat)*COS (rR*lon)

                  ENDDO
               ENDDO
            ENDDO
            CALL wrap ("swm",face=mss(:,:,:,np0,:))
            CALL wrap ("swm",face=relative)
            CALL wrap ("swm",face_1lyr=f)
            CALL wrap ("swm",face=eta(:,:,:,np0,:))
            CALL wrap ("swm",face=div(:,:,:,np0,:))
!-----------------------------------------------------------------------
!  solve for stream function and velocity potential. 
!  set normal and tangent wind
!-----------------------------------------------------------------------
         DO k = 1,km
            relative(:,:,k,:) = eta(:,:,k,np0,:) - f(:,:,:)
         ENDDO
         DO iter = 1,12
            CALL mltgrd2D ("world",km,relative,psi)
         ENDDO
         CALL wrap ("swm",face=psi)
         DO iter = 1,12
            CALL mltgrd2D ("world",km,div(:,:,:,np0,:),chi)
         ENDDO
         CALL wrap ("swm",face=chi)
         CALL set_wnd (eta(:,:,:,np0,:),div(:,:,:,np0,:),f,relative,psi,chi,ke, &
                                                     wnd_nrm,wnd_tng,wnd_edg)

            CALL wrt_fld (path_output,"mss","parallel",proc=rnk_wrld, &
                          time=time_swm,time_unit="h",face=mss     (:,:,1:1,np0,:))
            CALL wrt_fld (path_output,"eta","parallel",proc=rnk_wrld, &
                          time=time_swm,time_unit="h",face=eta     (:,:,1:1,np0,:))
            CALL wrt_fld (path_output,"div","parallel",proc=rnk_wrld, &
                          time=time_swm,time_unit="h",face=div     (:,:,1:1,np0,:))

!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!  GALEWSKY.  TELLUS (2004), 56A, 429-440
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      CASE (11)
!-----------------------------------------------------------------------
!  set geopot_surf
!-----------------------------------------------------------------------
            geopot_surf = zero
!-----------------------------------------------------------------------
!  set mss, eta and div
!-----------------------------------------------------------------------

            lat0 = 0.4487989505128276054946633404685004120281670570535865458535635132_dbl_kind
            lat1 = 1.121997376282069013736658351171251030070417642633966364633908783_dbl_kind
            
            umax = 80._dbl_kind

            en = EXP (-four/((lat1-lat0)**two))

            h0 = 10158.18856585231774_dbl_kind

            h_hat = 120._dbl_kind
            lon_c = zero;
            lat_c = pi/four
            lon7  = 1._dbl_kind/03._dbl_kind
            lat7  = 1._dbl_kind/15._dbl_kind

            DO nsd = 1,nsdm
               DO j = 1,jm
                  DO i = 1,im
                     lonlat(:) = xyz_to_lonlat (point(:,i,j,nsd))
                     lon = lonlat(1); lat = lonlat(2);

                     IF ((lat0<lat).AND.(lat<lat1)) THEN
                        relative(i,j,:,nsd) = (one/a)*(umax/en)* &
                            (EXP (one/((lat-lat0)*(lat-lat1))))* &
                            ((two*lat-lat0-lat1)/(((lat-lat0)**two)* &
                            ((lat-lat1)**two)))
                     ELSE
                        relative(i,j,:,nsd) = zero
                     ENDIF

                     f  (i,j,      nsd) = two*omega*SIN (lat)
                     eta(i,j,:,np0,nsd) = relative(i,j,:,nsd) + f(i,j,nsd)

                     div(i,j,:,np0,nsd) = zero

                     IF (lat <= lat0) THEN
                        sum_sum = zero
                     ELSE IF ((lat0<lat).AND.(lat<lat1)) THEN
                        sum_sum = zero
                     ELSE
                        sum_sum = zero
                     ENDIF

                     mss(i,j,:,np0,nsd) = h0 - sum_sum/grav

                     IF (.TRUE.) THEN
                        mss(i,j,:,np0,nsd) = mss(i,j,:,np0,nsd) + h_hat* &
                        COS (lat)*EXP (-(((lon-lon_c)/lon7)**two))* &
                        EXP (-(((lat-lat_c)/lat7)**two))
                     ENDIF

                  ENDDO
               ENDDO
            ENDDO
            CALL wrap ("swm",face=mss(:,:,:,np0,:))
            CALL wrap ("swm",face=relative)
            CALL wrap ("swm",face_1lyr=f)
            CALL wrap ("swm",face=eta(:,:,:,np0,:))
            CALL wrap ("swm",face=div(:,:,:,np0,:))
!-----------------------------------------------------------------------
!  solve for stream function and velocity potential. 
!  set normal and tangent wind
!-----------------------------------------------------------------------
         DO k = 1,km
            relative(:,:,k,:) = eta(:,:,k,np0,:) - f(:,:,:)
         ENDDO
         DO iter = 1,12
            CALL mltgrd2D ("world",km,relative,psi)
         ENDDO
         CALL wrap ("swm",face=psi)
         DO iter = 1,12
            CALL mltgrd2D ("world",km,div(:,:,:,np0,:),chi)
         ENDDO
         CALL wrap ("swm",face=chi)
         CALL set_wnd (eta(:,:,:,np0,:),div(:,:,:,np0,:),f,relative,psi,chi,ke, &
                                                     wnd_nrm,wnd_tng,wnd_edg)


            CALL wrt_fld (path_output,"mss","parallel",proc=rnk_wrld, &
                          time=time_swm,time_unit="h",face=mss     (:,:,1:1,np0,:))
            CALL wrt_fld (path_output,"eta","parallel",proc=rnk_wrld, &
                          time=time_swm,time_unit="h",face=eta     (:,:,1:1,np0,:))
            CALL wrt_fld (path_output,"div","parallel",proc=rnk_wrld, &
                          time=time_swm,time_unit="h",face=div     (:,:,1:1,np0,:))

            CALL wrt_fld (path_output,"rel","parallel",proc=rnk_wrld, &
                          time=time_swm,time_unit="h",face=relative(:,:,1:1,    :))

            CALL wrt_fld (path_output,"psi","parallel",proc=rnk_wrld, &
                          time=time_swm,time_unit="h",face=psi)
            CALL wrt_fld (path_output,"chi","parallel",proc=rnk_wrld, &
                          time=time_swm,time_unit="h",face=chi)

            CALL wrt_fld (path_output,"wnd_nrm","parallel",proc=rnk_wrld, &
                          time=time_swm,time_unit="h",edge=wnd_nrm)
            CALL wrt_fld (path_output,"wnd_tng","parallel",proc=rnk_wrld, &
                          time=time_swm,time_unit="h",edge=wnd_tng)
           
            DO nsd = 1,nsdm
               DO k = 1,km
                  DO i = 2,im-1
                     DO j = 2,jm-1
                        DO e = 1,3
                           tmpry01_edg3D(:,e,i,j,k,nsd) = nrm_edg(:,e,i,j,nsd)*wnd_nrm(e,i,j,k,nsd) + &
                                                          tng_edg(:,e,i,j,nsd)*wnd_tng(e,i,j,k,nsd)
                        ENDDO
                     ENDDO
                  ENDDO
               ENDDO
            ENDDO

            CALL wrt_fld (path_output,"wnd","parallel",proc=rnk_wrld, &
                          time=time_swm,time_unit="h",edge_3D=tmpry01_edg3D)

!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
            tmpry01_edg = zero

            DO nsd = 1,nsdm
               DO j = 1,jm
                  DO i = 1,im
                     DO e = 1,3
                        lonlat(:) = xyz_to_lonlat (point_edg(:,e,i,j,nsd))
                        lon = lonlat(1); lat = lonlat(2);

                        east = local_east (point_edg(:,e,i,j,nsd))

                        IF ((lat0<lat).AND.(lat<lat1)) THEN
                           uu = (umax/en)*(EXP (one/((lat-lat0)*(lat-lat1))))
                        ELSE
                           uu = zero
                        ENDIF

                        tmpry01_edg(e,i,j,:,nsd) = DOT_PRODUCT (east,nrm_edg(:,e,i,j,nsd))*uu

                     ENDDO
                  ENDDO
               ENDDO
            ENDDO

            CALL wrt_fld (path_output,"wnd_nrm_TRUE","parallel",proc=rnk_wrld, &
                          time=time_swm,time_unit="h",edge=tmpry01_edg)

            tmpry01_edg = wnd_nrm - tmpry01_edg

            CALL wrt_fld (path_output,"wnd_nrm_DIFF","parallel",proc=rnk_wrld, &
                          time=time_swm,time_unit="h",edge=tmpry01_edg)

!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!  TEST QUADRATIC INTERPOLATION
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      CASE (99)

        u0 = 40._dbl_kind

         lat0 = pi/four
         alph = 4._dbl_kind

         DO nsd = 1,nsdm
            DO j = 1,jm
               DO i = 1,im
                  p(:) = point(:,i,j,nsd)
                  lonlat(:) = xyz_to_lonlat (p(:)); lon=lonlat(1); lat=lonlat(2);

                  thing = EXP (two*alph*(lat-lat0))

                  psi     (i,j,:,nsd) = -a*(u0/alph)*((thing-one)/(thing+one))

                !  psi     (i,j,:,nsd) = a*(u0/alph)*(four*alph*thing/(thing**two))

                  relative(i,j,:,nsd) = (one/a)*(u0/alph)*(eight*(alph**2)*thing*(thing-one)/((thing+one)**three))


!                 psi(i,j,:,nsd) = -a*40._dbl_kind*(SIN (lat))
               ENDDO
            ENDDO
         ENDDO

         DO nsd = 1,nsdm
            DO j = 1,jm-1
               DO i = 1,im-1
                  DO c = 1,2
                     p(:) = point_crn(:,c,i,j,nsd)
                     lonlat(:) = xyz_to_lonlat (p(:)); lon=lonlat(1); lat=lonlat(2);

                     thing = EXP (two*alph*(lat-lat0))

                     psi_crn(c,i,j,:,nsd) = -a*(u0/alph)*((thing-one)/(thing+one))

                  ENDDO
               ENDDO
            ENDDO
         ENDDO

   chi = zero

!  psi_crn = zero

!  CALL interp_crn_quad ("swm",i1i,km,psi,psi_crn)

   IF (.TRUE.) THEN

   DO nsd = 1,nsdm
      DO k = 1,km
         DO j = 2,jm-1
            DO i = 2,im-1
               wnd_nrm(1,i,j,k,nsd) = &
              d_point_inv(1,i,j,nsd)*(chi(i+1,j  ,k,nsd)-chi(i  ,j  ,k,nsd))- &
        third*d_edge_inv (1,i,j,nsd)*(psi(i+1,j+1,k,nsd)-psi(i  ,j-1,k,nsd))

               wnd_nrm(2,i,j,k,nsd) = &
              d_point_inv(2,i,j,nsd)*(chi(i+1,j+1,k,nsd)-chi(i  ,j  ,k,nsd))- &
        third*d_edge_inv (2,i,j,nsd)*(psi(i  ,j+1,k,nsd)-psi(i+1,j  ,k,nsd))

               wnd_nrm(3,i,j,k,nsd) = &
              d_point_inv(3,i,j,nsd)*(chi(i  ,j+1,k,nsd)-chi(i  ,j  ,k,nsd))- &
        third*d_edge_inv (3,i,j,nsd)*(psi(i-1,j  ,k,nsd)-psi(i+1,j+1,k,nsd))

            ENDDO
         ENDDO
      ENDDO
   ENDDO

   ELSE

   DO nsd = 1,nsdm
      DO k = 1,km
         DO j = 2,jm-1
            DO i = 2,im-1

               wnd_nrm(1,i,j,k,nsd) = &
              d_point_inv(1,i,j,nsd)*(chi    (  i+1,j  ,k,nsd)-chi    (  i  ,j  ,k,nsd))- &
              d_edge_inv (1,i,j,nsd)*(psi_crn(1,i  ,j  ,k,nsd)-psi_crn(2,i  ,j-1,k,nsd))

               wnd_nrm(2,i,j,k,nsd) = &
              d_point_inv(2,i,j,nsd)*(chi    (  i+1,j+1,k,nsd)-chi    (  i  ,j  ,k,nsd))- &
              d_edge_inv (2,i,j,nsd)*(psi_crn(2,i  ,j  ,k,nsd)-psi_crn(1,i  ,j  ,k,nsd))

               wnd_nrm(3,i,j,k,nsd) = &
              d_point_inv(3,i,j,nsd)*(chi    (  i  ,j+1,k,nsd)-chi    (  i  ,j  ,k,nsd))- &
              d_edge_inv (3,i,j,nsd)*(psi_crn(1,i-1,j  ,k,nsd)-psi_crn(2,i  ,j  ,k,nsd))

            ENDDO
         ENDDO
      ENDDO
   ENDDO

   ENDIF
            CALL wrt_fld (path_output,"wnd_nrm","parallel",proc=rnk_wrld, &
                          time=time_swm,time_unit="h",edge=wnd_nrm)
           
            tmpry01_edg = zero

            DO nsd = 1,nsdm
               DO j = 1,jm
                  DO i = 1,im
                     DO e = 1,3
                        lonlat(:) = xyz_to_lonlat (point_edg(:,e,i,j,nsd))
                        lon = lonlat(1); lat = lonlat(2);

                        thing = EXP (two*alph*(lat-lat0))

                        uu = (u0/alph)*(four*alph*thing/((thing+1)**two))

                        east = local_east (point_edg(:,e,i,j,nsd))

!                       uu = 40._dbl_kind*(COS (lat))

                        tmpry01_edg(e,i,j,:,nsd) = DOT_PRODUCT (east,nrm_edg(:,e,i,j,nsd))*uu

                     ENDDO
                  ENDDO
               ENDDO
            ENDDO

            CALL wrt_fld (path_output,"wnd_nrm_TRUE","parallel",proc=rnk_wrld, &
                          time=time_swm,time_unit="h",edge=tmpry01_edg)

            tmpry01_edg = wnd_nrm - tmpry01_edg

            CALL wrt_fld (path_output,"wnd_nrm_DIFF","parallel",proc=rnk_wrld, &
                          time=time_swm,time_unit="h",edge=tmpry01_edg)

!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


  stop
      END SELECT
   ENDIF ! IF (l_restart) THEN

   CALL parallel_barrier ("world")

   END SUBROUTINE initialize_swm
!=======================================================================
!  END initialize_swm
!=======================================================================

!=======================================================================
!  BEGIN mss_TRUE_IC01
!=======================================================================
   FUNCTION mss_TRUE_IC01 (advctn_alph_IC01,time,p) RESULT (tru)
!.......................................................................
!  INTENT IN
!.......................................................................
   REAL (KIND=dbl_kind) :: &
      advctn_alph_IC01,time,p(3)
!.......................................................................
!  INTENT OUT
!.......................................................................
   REAL (KIND=dbl_kind) :: &
      tru
!.......................................................................
!  LOCAL
!.......................................................................
   REAL (KIND=dbl_kind) :: &
      p0(3),p1(3),rotation_matrix(3,3),lonlat(2)
   REAL (KIND=dbl_kind) :: &
      lon_c,lat_c,lon,lat,thing1,h0,r,rrr
   REAL (KIND=dbl_kind) :: &
      pip1 = 4.7123889803846898576939650749193_dbl_kind, &!
      pip2 = 6.2831853071795864769252867665590_dbl_kind   !
!.:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:.

   thing1 = time/1036800.0_dbl_kind

   p0(:) = (/ COS (pip2*thing1+pip1),SIN (pip2*thing1+pip1),zero /)

!   rotation_matrix(:,:) = &
!                     RESHAPE ( (/one ,  zero     ,   zero      , &
!                                 zero, COS (advctn_alph_IC01),-SIN (advctn_alph_IC01)  , &
!                                 zero, SIN (advctn_alph_IC01), COS (advctn_alph_IC01)/),(/3,3/))

   rotation_matrix(:,:) = &
                     RESHAPE ( (/  COS (advctn_alph_IC01),zero,SIN (advctn_alph_IC01)  , &
                                     zero    , one,    zero    , &
                                  -SIN (advctn_alph_IC01),zero,COS (advctn_alph_IC01)/),(/3,3/))

   p1(:) = MATMUL (rotation_matrix(:,:),p0(:))

   lonlat(:) = xyz_to_lonlat (p1(:)); lon_c=lonlat(1); lat_c=lonlat(2)

   h0 = 1000.0_dbl_kind ! 1000.0_dbl_kind

   rrr = one/three

   lonlat(:) = xyz_to_lonlat (p(:))
   lon = lonlat(1); lat = lonlat(2);

   r = ACOS (SIN (lat_c)*SIN (lat)+ &
             COS (lat_c)*COS (lat)*COS (lon-lon_c))

   IF (r < rrr) THEN
      tru = h0 ! (h0/two)*(one+COS (pi*r/rrr))
   ELSE
      tru = zero
   ENDIF

   END FUNCTION mss_TRUE_IC01
!=======================================================================
!  END mss_TRUE_IC01
!=======================================================================

   END MODULE swm_initialize
!cccccccc1ccccccccc2ccccccccc3ccccccccc4ccccccccc5cccgtgccc6ccccccccc7cc

