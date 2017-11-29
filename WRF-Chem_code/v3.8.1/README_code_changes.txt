
=========================================================================


Coding work to add "clean sky" diagnostic outputs to WRF-Chem

1) diagnostic variables are defined in "Registry/Registry.EM_COMMON"
--- Add new variables for output: SWUPTN; SWUPBN; etc (using "CLN" for clean sky variables)

2) diagnostic variables are passed to the "grid" array during the call to the "radiation_driver"
subroutine in "dyn_em/module_first_rk_step_part1.F". So need to add the new diagnostic variables
to the subroutine interface here.

3) "radiation_driver" is in "phys/module_radiation_driver.F" - this calls the subroutines
"RRTMG_SWRAD" and "RRTMG_LWRAD", which calculate the short and long wave fluxes for our
radiative scheme of interest. The new diagnostic variables will need to be added to these
call interfaces here too.

4) Short wave diagnostics ("phys/module_ra_rrtmg_sw.F"):
"RRTMG_SWRAD" calls "rrtmg_sw": this call interface will need the new diagnostic
variables adding. -> also need new local variables swuflxcln etc.

5) "rrtmg_sw" calls "spcvmc_sw", passing it the bulk aerosol properties (ztaua, zasya, zamga).
The call to "spcvmc_sw" should be duplicated, with the aerosol bulk properties set to zero,
so that we can take the output "clear sky" variables and save them as our "clean sky" variables.

6) Long wave diagnostics ("phys/module_ra_rrtmg_lw.F"):
"RRTMG_LWRAD" calls "rrtmg_lw": this call interface will need the new diagnostic
variables adding (and flx variables).
"rrtmg_lw" calls "rtrnmc", passing it a combined gaseous and aerosol optical depth variable.
The call to "rtrnmc" should be duplicated, with only gaseous optical depth information passed,
so that we can take the output "clear sky" variables and save them as our "clean sky" variables.

=========================================================================

edited modules:
- Registry.EM_COMMON           
- module_ra_rrtmg_sw.F
- module_first_rk_step_part1.F 
- module_radiation_driver.F
- module_ra_rrtmg_lw.F

(line numbers given below refer to the modified files for v3.6.1)
=========================================================================

1. Registry.EM_COMMON


1) diagnostic variables are defined in "Registry/Registry.EM_COMMON"
--- Add new variables for output: SWUPTN; SWUPBN; etc (using "CLN" for clear sky variables)

New clean sky variables, around lines 1458--1480:

SWUPTCLN
SWDNTCLN
SWUPBCLN
SWDNBCLN
LWUPTCLN
LWDNTCLN
LWUPBCLN
LWDNBCLN




2) diagnostic variables are passed to the "grid" array during the call to the "radiation_driver"
subroutine in "dyn_em/module_first_rk_step_part1.F". So need to add the new diagnostic variables
to the subroutine interface here.

module_first_rk_step_part1.F

line 364 -> 371 - added calls to new variables in argument
SWUPTCLN=grid%swuptcln etc.

     &        ,SWUPT=grid%swupt,SWUPTC=grid%swuptc,SWUPTCLN=grid%swuptcln &
     &        ,SWDNT=grid%swdnt,SWDNTC=grid%swdntc,SWDNTCLN=grid%swdntcln &
     &        ,SWUPB=grid%swupb,SWUPBC=grid%swupbc,SWUPBCLN=grid%swupbcln &
     &        ,SWDNB=grid%swdnb,SWDNBC=grid%swdnbc,SWDNBCLN=grid%swdnbcln &
     &        ,LWUPT=grid%lwupt,LWUPTC=grid%lwuptc,LWUPTCLN=grid%lwuptcln &
     &        ,LWDNT=grid%lwdnt,LWDNTC=grid%lwdntc,LWDNTCLN=grid%lwdntcln &
     &        ,LWUPB=grid%lwupb,LWUPBC=grid%lwupbc,LWUPBCLN=grid%lwupbcln &
     &        ,LWDNB=grid%lwdnb,LWDNBC=grid%lwdnbc,LWDNBCLN=grid%lwdnbcln &

=========================================================================


3) "radiation_driver" is in "phys/module_radiation_driver.F" - this calls the subroutines
"RRTMG_SWRAD" and "RRTMG_LWRAD", which calculate the short and long wave fluxes for our
radiative scheme of interest. The new diagnostic variables will need to be added to these
call interfaces here too.

module_radiation_driver.F

Add new variables in SUBROUTINE radiation_driver definition:

line 90 -> 97:
             ,SWUPT ,SWUPTC, SWUPTCLN        &
              ,SWDNT ,SWDNTC, SWDNTCLN        &
              ,SWUPB ,SWUPBC, SWUPBCLN        &
              ,SWDNB ,SWDNBC, SWDNBCLN        &
              ,LWUPT ,LWUPTC, LWUPTCLN        &
              ,LWDNT ,LWDNTC, LWDNTCLN        &
              ,LWUPB ,LWUPBC, LWUPBCLN        &
              ,LWDNB ,LWDNBC, LWDNBCLN        &

line 501->505: declare new variables dimensions etc.
   REAL, DIMENSION( ims:ime, jms:jme ), OPTIONAL, INTENT(INOUT) ::&
              SWUPT,  SWUPTC, SWUPTCLN,  SWDNT,  SWDNTC, SWDNTCLN,&
              SWUPB,  SWUPBC, SWUPBCLN,  SWDNB,  SWDNBC, SWDNBCLN,&
              LWUPT,  LWUPTC, LWUPTCLN,  LWDNT,  LWDNTC, LWDNTCLN,&
              LWUPB,  LWUPBC, LWUPBCLN,  LWDNB,  LWDNBC, LWDNBCLN

line 517-523: declare new FLX variables (these we're not passing in/out of the subroutine at the moment, can do this later if needed)
   REAL, DIMENSION( ims:ime, kms:kme+2, jms:jme ) ::              &
          SWUPFLXCLN,SWDNFLXCLN,                                  &
          LWUPFLXCLN,LWDNFLXCLN

Added new variables to call to RRTMG_LWRAD
line 1540->1543
                  LWUPT=LWUPT,LWUPTC=LWUPTC,LWUPTCLN=LWUPTCLN,      & ! SAN new clean sky variables added (07/01/2016)
                  LWDNT=LWDNT,LWDNTC=LWDNTC,LWDNTCLN=LWDNTCLN,      &
                  LWUPB=LWUPB,LWUPBC=LWUPBC,LWUPBCLN=LWUPBCLN,      &
                  LWDNB=LWDNB,LWDNBC=LWDNBC,LWDNBCLN=LWDNBCLN,      &

line 1585->1586
                  LWUPFLX=LWUPFLX,LWUPFLXC=LWUPFLXC,LWUPFLXCLN=LWUPFLXCLN, & ! SAN new clean sky fluxes added (07/01/2016)
                  LWDNFLX=LWDNFLX,LWDNFLXC=LWDNFLXC,LWDNFLXCLN=LWDNFLXCLN  &

Added new variables to call to RRTMG_SWRAD
line 1957->1960
                     SWUPT=SWUPT,SWUPTC=SWUPTC,SWUPTCLN=SWUPTCLN,      & !SAN new clean sky variables (07/01/2016)
                     SWDNT=SWDNT,SWDNTC=SWDNTC,SWDNTCLN=SWDNTCLN,      &
                     SWUPB=SWUPB,SWUPBC=SWUPBC,SWUPBCLN=SWUPBCLN,      &
                     SWDNB=SWDNB,SWDNBC=SWDNBC,SWDNBCLN=SWDNBCLN,      &

line 2006->2007
                     SWUPFLX=SWUPFLX,SWUPFLXC=SWUPFLXC,SWUPFLXCLN=SWUPFLXCLN, & ! SAN new clean sky variables (07/01/2016)
                     SWDNFLX=SWDNFLX,SWDNFLXC=SWDNFLXC,SWDNFLXCLN=SWDNFLXCLN  &

=========================================================================


4) Short wave diagnostics ("phys/module_ra_rrtmg_sw.F"):
"RRTMG_SWRAD" calls "rrtmg_sw": this call interface will need the new diagnostic
variables adding. -> also need new local variables swuflxcln etc.

module_ra_rrtmg_sw.F

- Added new clean variables to SUBROUTINE RRTMG_SWRAD(                                        &

line 9898-9899
                       swupt, swuptc, swuptcln, swdnt, swdntc, swdntcln, &
                       swupb, swupbc, swupbcln, swdnb, swdnbc, swdnbcln, &

line 9935-9936

                       swupflx, swupflxc, swupflxcln,             & !SAN - new cln flx vars (07/01/2016)
                       swdnflx, swdnflxc, swdnflxcln              &

And declared variables in subroutine

line 10087-10088
                    SWUPT,SWUPTC,SWUPTCLN,SWDNT,SWDNTC,SWDNTCLN,  & !SAN - new cln vars (2016-01-16)
                    SWUPB,SWUPBC,SWUPBCLN,SWDNB,SWDNBC,SWDNBCLN

line 10094-10095
! Vertical ordering is from bottom to top (W m-2)
   REAL, DIMENSION( ims:ime, kms:kme+2, jms:jme ),                &
         OPTIONAL, INTENT(OUT) ::                                 &
                               SWUPFLX,SWUPFLXC,SWUPFLXCLN,       & !SAN (2016-01-07)
                               SWDNFLX,SWDNFLXC,SWDNFLXCLN

! new internal flux variables declared:
line 10180-10181
                                                       swuflxcln, &  ! SAN 2016-01-07
                                                       swdflxcln, &  ! SAN 2016-01-07



- Added new flx variables to rrtmg_sw call

line 11086:
             swuflx  ,swdflx  ,swhr,swuflxc ,swdflxc ,swhrc, swuflxcln, swdflxcln, & ! added flncln SAN (2015-01-07)




- Passing new data to the output variables

lines 11099--11120

        if (present(swupt)) then
! Output up and down toa fluxes for total and clear sky
            swupt(i,j)     = swuflx(1,kte+2)
            swuptc(i,j)    = swuflxc(1,kte+2)
            swuptcln(i,j)  = swuflxcln(1,kte+2) ! SAN (2015-01-07) 
            swdnt(i,j)     = swdflx(1,kte+2)
            swdntc(i,j)    = swdflxc(1,kte+2)
            swdntcln(i,j)  = swdflxcln(1,kte+2) ! SAN (2015-01-07) 
! Output up and down surface fluxes for total and clear sky
            swupb(i,j)     = swuflx(1,1)
            swupbc(i,j)    = swuflxc(1,1)
            swupbcln(i,j)  = swuflxcln(1,1) ! SAN (2015-01-07)
            swdnb(i,j)     = swdflx(1,1)
! Added by Zhenxin for 4 compenants of swdown radiation
            swvisdir(i,j)  = sibvisdir(1,1)
            swvisdif(i,j)  = sibvisdif(1,1)
            swnirdir(i,j)  = sibnirdir(1,1)
            swnirdif(i,j)  = sibnirdif(1,1)
!  Ended, Zhenxin (2011/06/20)
            swdnbc(i,j)    = swdflxc(1,1)
            swdnbcln(i,j)  = swdflxcln(1,1) ! SAN (2015-01-07)
         endif


lines 11133--11136

            swupflx(i,k,j)  = swuflx(1,k)
            swupflxc(i,k,j) = swuflxc(1,k)
            swupflxcln(i,k,j) = swuflxcln(1,k) ! SAN (2015-01-07) 
            swdnflx(i,k,j)  = swdflx(1,k)
            swdnflxc(i,k,j) = swdflxc(1,k)
            swdnflxcln(i,k,j) = swdflxcln(1,k)  ! SAN (2015-01-07) 


lines 11151--11161 - Set all outputs to 0 when aer_rad_feedback is off

        if (present(swupt)) then
! Output up and down toa fluxes for total and clear sky
            swupt(i,j)     = 0.
            swuptc(i,j)    = 0.
            swuptcln(i,j)  = 0. ! SAN (2015-01-07)
            swdnt(i,j)     = 0.
            swdntc(i,j)    = 0.
            swdntcln(i,j)  = 0. ! SAN (2015-01-07)
! Output up and down surface fluxes for total and clear sky
            swupb(i,j)     = 0.
            swupbc(i,j)    = 0.
            swupbcln(i,j)  = 0. ! SAN (2015-01-07)
            swdnb(i,j)     = 0.
            swdnbc(i,j)    = 0.
            swdnbcln(i,j)  = 0. ! SAN (2015-01-07)
            swvisdir(i,j)  = 0.  ! Add by Zhenxin (2011/06/20)
            swvisdif(i,j)  = 0.
            swnirdir(i,j)  = 0.
            swnirdif(i,j)  = 0.  ! Add by Zhenxin (2011/06/20)
         endif

=========================================================================



5) Short wave diagnostics ("phys/module_ra_rrtmg_sw.F"), Part 2:
"rrtmg_sw" calls "spcvmc_sw", passing it the bulk aerosol properties (ztaua, zasya, zamga).
The call to "spcvmc_sw" should be duplicated, with the aerosol bulk properties set to zero,
so that we can take the output "clear sky" variables and save them as our "clean sky" variables.



module_ra_rrtmg_sw.F



Addition of swuflxcln and swdflxcln to the subroutine interface

line 8751


             swuflx  ,swdflx  ,swhr    ,swuflxc ,swdflxc ,swhrc, swuflxcln ,swdflxcln , & ! SAN (2016-01-07)

lines 8977 -- 8981

!++ SAN (2016-01-07) - New variables for calculating "clean" sky (no aerosol) fluxs
      real(kind=rb), intent(out) :: swuflxcln(:,:)    ! Clean sky shortwave upward flux (W/m2)
                                                      !    Dimensions: (ncol,nlay+1)
      real(kind=rb), intent(out) :: swdflxcln(:,:)    ! Clean sky shortwave downward flux (W/m2)
                                                      !    Dimensions: (ncol,nlay+1)



New internal variables:

line 9106

      real(kind=rb) :: ztauacln(nlay+1,nbndsw)  ! DL (10/1/16) dummy total aerosol optical depth for clean case (=zero)


line 9130 -- 9138

!++ SAN (2016-01-07) - New temporary clean sky (no aerosol) fluxes
      real(kind=rb) :: zbbclnu(nlay+2)        ! temporary clean sky upward shortwave flux (w/m2)
      real(kind=rb) :: zbbclnd(nlay+2)        ! temporary clean sky downward shortwave flux (w/m2)
      real(kind=rb) :: zbbclnddir(nlay+2)     ! temporary clean sky downward direct shortwave flux (w/m2)
      real(kind=rb) :: zuvclnd(nlay+2)        ! temporary clean sky UV downward shortwave flux (w/m2)
      real(kind=rb) :: zuvclnddir(nlay+2)     ! temporary clean sky UV downward direct shortwave flux (w/m2)
      real(kind=rb) :: zniclnd(nlay+2)        ! temporary clean sky near-IR downward shortwave flux (w/m2)
      real(kind=rb) :: zniclnddir(nlay+2)     ! temporary clean sky near-IR downward direct shortwave flux (w/m2)
!-- SAN


Initialising variables

line 9350
                  ztauacln(i,ib) = 0. ! SAN (2016-01-07) - clean sky Tau no aerosol


line 9440 -- 9455

!++ SAN (2016-01-07) - Repeat call to 2-stream radiation model using "clean sky"
!                      variables and aerosol tau set to 0
         do i=1,nlayers+1
            zbbcu(i) = 0._rb
            zbbcd(i) = 0._rb
            zbbclnu(i) = 0._rb
            zbbclnd(i) = 0._rb
            zbbcddir(i) = 0._rb
            zbbclnddir(i) = 0._rb
            zuvcd(i) = 0._rb
            zuvclnd(i) = 0._rb
            zuvcddir(i) = 0._rb
            zuvclnddir(i) = 0._rb
            znicd(i) = 0._rb
            zniclnd(i) = 0._rb
            znicddir(i) = 0._rb
            zniclnddir(i) = 0._rb
         enddo         

Extra call to spcvmc_sw (with ztauacln instead of ztaua)

line 9457 -- 9468

!++ SAN (2016-01-07) - calling spcvmc_sw using ztauacln(:,:) = 0. and cln flux variables
         call spcvmc_sw &
             (nlayers, istart, iend, icpr, iout, &
              pavel, tavel, pz, tz, tbound, albdif, albdir, &
              zcldfmc, ztaucmc, zasycmc, zomgcmc, ztaormc, &
              ztauacln, zasya, zomga, cossza, coldry, wkl, adjflux, &     
              laytrop, layswtch, laylow, jp, jt, jt1, &
              co2mult, colch4, colco2, colh2o, colmol, coln2o, colo2, colo3, &
              fac00, fac01, fac10, fac11, &
              selffac, selffrac, indself, forfac, forfrac, indfor, &
              zbbclnd, zbbclnu, zbbcd, zbbcu, zuvclnd, zuvcd, zniclnd, znicd, &
              zbbclnddir, zbbcddir, zuvclnddir, zuvcddir, zniclnddir, znicddir)

! Transfer up and down, clear and total sky fluxes to output arrays.
! Vertical indexing goes from bottom to top; reverse here for GCM if necessary.

Recording the clean atmosphere fluxes

line 9473 -- 99477

         do i = 1, nlayers+1
!++ SAN (2016-01-07) - just outputting basic clean sky fluxes
            swuflxcln(iplon,i) = zbbclnu(i)
            swdflxcln(iplon,i) = zbbclnd(i)
         enddo

!++ SAN (2016-01-07) - not calculating clean sky heating rates
!-- SAN


================================================================================

5a) Added some checks to avoid numerical errors (not related to this code - 
a bonus extra...)

module_ra_rrtmg_sw.F

- Added some checks to avoid divide by zero errors

line 8286
      real(kind=rb) :: denom
      
lines 8497 - 8502  and 8531 - 8534
		expanded calculations of zomcc and zgcc




=========================================================================


6) Long wave diagnostics ("phys/module_ra_rrtmg_lw.F"):
"RRTMG_LWRAD" calls "rrtmg_lw": this call interface will need the new diagnostic
variables adding (and flx variables).
"rrtmg_lw" calls "rtrnmc", passing it a combined gaseous and aerosol optical depth variable.
The call to "rtrnmc" should be duplicated, with only gaseous optical depth information passed,
so that we can take the output "clear sky" variables and save them as our "clean sky" variables.


module_ra_rrtmg_lw.F


Added to rrtmg_lw

line 10587
             uflxcln ,dflxcln )    ! DL (10/1/16) add the clean atmosphere fluxes

lines 10784 -- 10787

      real(kind=rb), intent(out) :: uflxcln(:,:)      ! Clean sky longwave upward flux (W/m2)
                                                      !    Dimensions: (ncol,nlay+1)
      real(kind=rb), intent(out) :: dflxcln(:,:)      ! Clean sky longwave downward flux (W/m2)
                                                      !    Dimensions: (ncol,nlay+1)

lines 10900 -- 10905

!!! DL (10/1/16) clean sky variables
      real(kind=rb) :: totuclnlfl(0:nlay+1)   ! clean sky upward longwave flux (w/m2)
      real(kind=rb) :: totdclnlfl(0:nlay+1)   ! clean sky downward longwave flux (w/m2)
      real(kind=rb) :: fnetcln(0:nlay+1)      ! clean sky net longwave flux (w/m2)
      real(kind=rb) :: htrcln(0:nlay+1)       ! clean sky longwave heating rate (k/day)
!!! DL (10/1/16)


lines 11020 -- 11029

!!! DL (10/1/16) call the radiative transfer routine for "clean" sky first,
!!!              passing taug rather than taut so we have no aerosol influence.
!!!              We will keep totuclnlfl, totdclnlfl fnetcln, and htrcln, 
!!!	             and then overwrite the rest with the second call to rtrnmc.
         call rtrnmc(nlayers, istart, iend, iout, pz, semiss, ncbands, &
                     cldfmc, taucmc, planklay, planklev, plankbnd, &
                     pwvcm, fracs, taug, &
                     totuclnlfl, totdclnlfl, fnetcln, htrcln, &
                     totuclfl, totdclfl, fnetc, htrc )
!!! DL (10/1/16)


lines 11046 -- 11049

!!! DL (10/1/16) clean sky output
            uflxcln(iplon,k+1) = totuclnlfl(k)
            dflxcln(iplon,k+1) = totdclnlfl(k)
!!! DL (10/1/16)




Added  to RRTMG_LWRAD

lines 11448-11449
                       lwupt, lwuptc, lwuptcln, lwdnt, lwdntc, lwdntcln, & ! SAN (2016-01-07)
                       lwupb, lwupbc, lwupbcln, lwdnb, lwdnbc, lwdnbcln, & ! SAN (2016-01-07)

line 11480
                       lwupflx, lwupflxc, lwupflxcln, lwdnflx, lwdnflxc, lwdnflxcln &  ! SAN (2016-01-07)



Added new output flux variables

lines 11589 & 11590  &  11596 & 11597

! Top of atmosphere and surface longwave fluxes (W m-2)
   REAL, DIMENSION( ims:ime, jms:jme ),                           &
         OPTIONAL, INTENT(INOUT) ::                               &
                      LWUPT,LWUPTC,LWUPTCLN,LWDNT,LWDNTC,LWDNTCLN,& ! SAN (2016-01-07)
                      LWUPB,LWUPBC,LWUPBCLN,LWDNB,LWDNBC,LWDNBCLN 


! Layer longwave fluxes (including extra layer above model top)
! Vertical ordering is from bottom to top (W m-2)
   REAL, DIMENSION( ims:ime, kms:kme+2, jms:jme ),                &
         OPTIONAL, INTENT(OUT) ::                                 &
                               LWUPFLX,LWUPFLXC,LWDNFLX,LWDNFLXC, &
                               LWUPFLXCLN, LWDNFLXCLN ! SAN (2016-01-07)



Added internal flux variables
lines 11679 & 11680
                                                         uflxcln, & ! SAN (2016-01-07)
                                                         dflxcln

Added clean atmosphere fluxes to rrtmg_lw call
line 12627
             uflx    ,dflx    ,hr      ,uflxc   ,dflxc,  hrc, &
             uflxcln ,dflxcln )    ! DL (10/1/16) add the clean atmosphere fluxes


Saving data in the output variables
line 12652 - 12657

! Output up and down toa fluxes for clean sky
            lwuptcln(i,j)  = uflxcln(1,kte+2)
            lwdntcln(i,j)  = dflxcln(1,kte+2)
! Output up and down surface fluxes for clean sky
            lwupbcln(i,j)  = uflxcln(1,1)
            lwdnbcln(i,j)  = dflxcln(1,1)

line 12670 & 12671

            lwupflxcln(i,k,j) = uflxcln(1,k)
            lwdnflxcln(i,k,j) = dflxcln(1,k)


