
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

New clean sky variables, around lines 1329--1352:

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

line 349 -> 356 - added calls to new variables in argument
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

line 83 -> 90:
             ,SWUPT ,SWUPTC, SWUPTCLN        &
              ,SWDNT ,SWDNTC, SWDNTCLN        &
              ,SWUPB ,SWUPBC, SWUPBCLN        &
              ,SWDNB ,SWDNBC, SWDNBCLN        &
              ,LWUPT ,LWUPTC, LWUPTCLN        &
              ,LWDNT ,LWDNTC, LWDNTCLN        &
              ,LWUPB ,LWUPBC, LWUPBCLN        &
              ,LWDNB ,LWDNBC, LWDNBCLN        &

line 470->474: declare new variables dimensions etc.
   REAL, DIMENSION( ims:ime, jms:jme ), OPTIONAL, INTENT(INOUT) ::&
              SWUPT,  SWUPTC, SWUPTCLN,  SWDNT,  SWDNTC, SWDNTCLN,&
              SWUPB,  SWUPBC, SWUPBCLN,  SWDNB,  SWDNBC, SWDNBCLN,&
              LWUPT,  LWUPTC, LWUPTCLN,  LWDNT,  LWDNTC, LWDNTCLN,&
              LWUPB,  LWUPBC, LWUPBCLN,  LWDNB,  LWDNBC, LWDNBCLN

line 485-487: declare new FLX variables (these we're not passing in/out of the subroutine at the moment, can do this later if needed)
   REAL, DIMENSION( ims:ime, kms:kme+2, jms:jme ) ::              &
          SWUPFLXCLN,SWDNFLXCLN,                                  &
          LWUPFLXCLN,LWDNFLXCLN

Added new variables to call to RRTMG_LWRAD
line 1301->1304
                  LWUPT=LWUPT,LWUPTC=LWUPTC,LWUPTCLN=LWUPTCLN,      & ! SAN new clean sky variables added (07/01/2016)
                  LWDNT=LWDNT,LWDNTC=LWDNTC,LWDNTCLN=LWDNTCLN,      &
                  LWUPB=LWUPB,LWUPBC=LWUPBC,LWUPBCLN=LWUPBCLN,      &
                  LWDNB=LWDNB,LWDNBC=LWDNBC,LWDNBCLN=LWDNBCLN,      &

line 1346->1347
                  LWUPFLX=LWUPFLX,LWUPFLXC=LWUPFLXC,LWUPFLXCLN=LWUPFLXCLN, & ! SAN new clean sky fluxes added (07/01/2016)
                  LWDNFLX=LWDNFLX,LWDNFLXC=LWDNFLXC,LWDNFLXCLN=LWDNFLXCLN  &

Added new variables to call to RRTMG_SWRAD
line 1647->1650
                     SWUPT=SWUPT,SWUPTC=SWUPTC,SWUPTCLN=SWUPTCLN,      & !SAN new clean sky variables (07/01/2016)
                     SWDNT=SWDNT,SWDNTC=SWDNTC,SWDNTCLN=SWDNTCLN,      &
                     SWUPB=SWUPB,SWUPBC=SWUPBC,SWUPBCLN=SWUPBCLN,      &
                     SWDNB=SWDNB,SWDNBC=SWDNBC,SWDNBCLN=SWDNBCLN,      &

line 1696->1697
                     SWUPFLX=SWUPFLX,SWUPFLXC=SWUPFLXC,SWUPFLXCLN=SWUPFLXCLN, & ! SAN new clean sky variables (07/01/2016)
                     SWDNFLX=SWDNFLX,SWDNFLXC=SWDNFLXC,SWDNFLXCLN=SWDNFLXCLN  &

=========================================================================


4) Short wave diagnostics ("phys/module_ra_rrtmg_sw.F"):
"RRTMG_SWRAD" calls "rrtmg_sw": this call interface will need the new diagnostic
variables adding. -> also need new local variables swuflxcln etc.

module_ra_rrtmg_sw.F


- Added new clean variables to SUBROUTINE RRTMG_SWRAD(                                        &

line 9876-9877 
                       swupt, swuptc, swuptcln, swdnt, swdntc, swdntcln, &
                       swupb, swupbc, swupbcln, swdnb, swdnbc, swdnbcln, &

line 9912-9914

                       swupflx, swupflxc, swupflxcln,             & !SAN - new cln flx vars (07/01/2016)
                       swdnflx, swdnflxc, swdnflxcln              &

And declared variables in subroutine

line 10067-10068
                    SWUPT,SWUPTC,SWUPTCLN,SWDNT,SWDNTC,SWDNTCLN,  & !SAN - new cln vars (2016-01-16)
                    SWUPB,SWUPBC,SWUPBCLN,SWDNB,SWDNBC,SWDNBCLN

line 10074-10075
! Vertical ordering is from bottom to top (W m-2)
   REAL, DIMENSION( ims:ime, kms:kme+2, jms:jme ),                &
         OPTIONAL, INTENT(OUT) ::                                 &
                               SWUPFLX,SWUPFLXC,SWUPFLXCLN,       & !SAN (2016-01-07)
                               SWDNFLX,SWDNFLXC,SWDNFLXCLN

! new internal flux variables declared:
line 10155-10156
                                                       swuflxcln, &  ! SAN 2016-01-07
                                                       swdflxcln, &  ! SAN 2016-01-07



- Added new flx variables to rrtmg_sw call

line 11023:
             swuflx  ,swdflx  ,swhr,swuflxc ,swdflxc ,swhrc, swuflxcln, swdflxcln, & ! added flncln SAN (2015-01-07)




- Passing new data to the output variables

lines 11038--11059

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


lines 11067--11076

            swupflx(i,k,j)  = swuflx(1,k)
            swupflxc(i,k,j) = swuflxc(1,k)
            swupflxcln(i,k,j) = swuflxcln(1,k) ! SAN (2015-01-07) 
            swdnflx(i,k,j)  = swdflx(1,k)
            swdnflxc(i,k,j) = swdflxc(1,k)
            swdnflxcln(i,k,j) = swdflxcln(1,k)  ! SAN (2015-01-07) 


lines 11085--11104 - Set all outputs to 0 when aer_rad_feedback is off

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

line 8734


             swuflx  ,swdflx  ,swhr    ,swuflxc ,swdflxc ,swhrc, swuflxcln ,swdflxcln , & ! SAN (2016-01-07)

lines 8966 -- 8970

!++ SAN (2016-01-07) - New variables for calculating "clean" sky (no aerosol) fluxs
      real(kind=rb), intent(out) :: swuflxcln(:,:)    ! Clean sky shortwave upward flux (W/m2)
                                                      !    Dimensions: (ncol,nlay+1)
      real(kind=rb), intent(out) :: swdflxcln(:,:)    ! Clean sky shortwave downward flux (W/m2)
                                                      !    Dimensions: (ncol,nlay+1)



New internal variables:

line 9088

      real(kind=rb) :: ztauacln(nlay+1,nbndsw)  ! DL (10/1/16) dummy total aerosol optical depth for clean case (=zero)


line 9112 -- 9119

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

line 9422 -- 9437

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

line 9440 -- 9450

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

line 9455 -- 99459

         do i = 1, nlayers+1
!++ SAN (2016-01-07) - just outputting basic clean sky fluxes
            swuflxcln(iplon,i) = zbbclnu(i)
            swdflxcln(iplon,i) = zbbclnd(i)
         enddo

!++ SAN (2016-01-07) - not calculating clean sky heating rates
!-- SAN

=========================================================================


6) Long wave diagnostics ("phys/module_ra_rrtmg_lw.F"):
"RRTMG_LWRAD" calls "rrtmg_lw": this call interface will need the new diagnostic
variables adding (and flx variables).
"rrtmg_lw" calls "rtrnmc", passing it a combined gaseous and aerosol optical depth variable.
The call to "rtrnmc" should be duplicated, with only gaseous optical depth information passed,
so that we can take the output "clear sky" variables and save them as our "clean sky" variables.


module_ra_rrtmg_lw.F


Added  to RRTMG_LWRAD

lines 11437-11438
                       lwupt, lwuptc, lwuptcln, lwdnt, lwdntc, lwdntcln, & ! SAN (2016-01-07)
                       lwupb, lwupbc, lwupbcln, lwdnb, lwdnbc, lwdnbcln, & ! SAN (2016-01-07)

line 11468
                       lwupflx, lwupflxc, lwupflxcln, lwdnflx, lwdnflxc, lwdnflxcln &  ! SAN (2016-01-07)



Added new output flux variables

lines 11576 & 11577  &  11583 & 11584

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
lines 11661 & 11662
                                                         uflxcln, & ! SAN (2016-01-07)
                                                         dflxcln

Added clean atmosphere fluxes to rrtmg_lw call
line 12540
             uflx    ,dflx    ,hr      ,uflxc   ,dflxc,  hrc, &
             uflxcln ,dflxcln )    ! DL (10/1/16) add the clean atmosphere fluxes


Saving data in the output variables
line 12574 - 12579

! Output up and down toa fluxes for clean sky
            lwuptcln(i,j)  = uflxcln(1,kte+2)
            lwdntcln(i,j)  = dflxcln(1,kte+2)
! Output up and down surface fluxes for clean sky
            lwupbcln(i,j)  = uflxcln(1,1)
            lwdnbcln(i,j)  = dflxcln(1,1)

line 12592 & 12593

            lwupflxcln(i,k,j) = uflxcln(1,k)
            lwdnflxcln(i,k,j) = dflxcln(1,k)


