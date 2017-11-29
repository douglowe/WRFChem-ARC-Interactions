
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
- registry.chem
- Registry.EM_COMMON           
- module_first_rk_step_part1.F 
- module_radiation_driver.F
- module_ra_rrtmg_sw.F
- module_ra_rrtmg_lw.F
- chemics_init.F

(line numbers given below refer to the modified files for v3.9.1)
=========================================================================

1. registry.chem

1) control flag "clean_atm_diag" for switching on/off diagnostic code is added at line 3763 
--- default value 0 switches off calculation, values of 1+ switch it on


2. Registry.EM_COMMON


1) diagnostic variables are defined in "Registry/Registry.EM_COMMON"
--- Add new variables for output: SWUPTN; SWUPBN; etc (using "CLN" for clear sky variables)
around lines 1465--1486


3. module_first_rk_step_part1.F

1) new diagnostic variables are passed to the "grid" array during the call to the "radiation_driver"
subroutine in "dyn_em/module_first_rk_step_part1.F" (lines 350--357)

2) control variable "clean_atm_diag" is passed from config_flags within the WRF_CHEM only
section of the code (line 370).


4. module_radiation_driver.F

1) new diagnostic variables, and the control variable, are added to the subroutine interface
(lines 90--97 and 107)

2) new control variable is declared as an optional input (line 469)

3) new diagnostic variables are declared as optional, intent inout, variables (lines 502--507)

4) extra diagnostic variables, for the clean fluxes, are declared as local variables (we
don't use these for the analysis, so aren't outputting them at the moment) (lines 515--518)

5) new diagnostic variables are passed to rrtmg_lwrad and rrtmg_swrad
subroutines (lines 1530--1533 & 1575--1576, and 1946--1948 & 1995--1996)

6) new control variable is passed (within the wrfchem only code) to rrtmg_lwrad and rrtmg_sward
too (lines 1566 and 1989).


5. module_ra_rrtmg_sw.F

1) new diagnostic variables, and control variable, added to rrtmg_swrad interface
(lines 9903--9904, 9934, 9940--9941)

2) control variable declared as optional (intent in), and a local control variable (clean_atm_diag_local)
also declared (lines 10065--10066)

3) new diagnostic variables declared as intent out (lines 10094--10095, 10101--10102)

4) local temporary variables added for fluxes (lines 10187--10188)

5) if statement checking for presence of control variable added - copies value to local
control variable (or sets to 0 if not present) (lines 10308--10312)

6) local temporary variables and control variable passed added to rrtmg_sw call (lines 11114 and 11119)

7) flux values passed from local variables to output variables after this call (lines 11132 -- 11163)

8) flux variables and control variable added to rrtmg_sw interface (lines 8750 and 8756)

9) declaration of these variables (lines 8954 & 8978--8981)

10) declaration of new internal "clean sky" variables for optical depth (line 9106) and 
fluxes (lines 9130--9136)

11) "clean sky" aerosol optical depth is set to 0.0 (line 9348)

12) a new call to the spcvmc_sw subroutine is added (passing the clean sky aerosol optical depth
this time) - with a preprocessing statement so that it is only added for WRF-Chem. This call is
also within an if statement, so can be switched off using the control variable when running WRF-Chem
if desired.

13) within spcvmc_sw I have added code to avoid an occasional "divide by zero" error. This follows
the style of other code within WRF that is used to avoid this error elsewhere, and is a
general fix, not one which is specific to the rest of the submitted code (lines 8286, 8495--8501, 
and 8527--8533).



6. module_ra_rrtmg_lw.F

1) new diagnostic variables, and control variable, added to rrtmg_lwrad interface
(lines 11453--11454, 11476, 11485)

2) control variable declared as optional (intent in), and a local control variable (clean_atm_diag_local)
also declared (lines 11580--11581)

3) new diagnostic variables declared as intent inout (lines 11596--11597, 11603--11604)

4) local temporary variables added for fluxes (lines 11686--11688)

5) if statement checking for presence of control variable added - copies value to local
control variable (or sets to 0 if not present) (lines 11848--11852)

6) local temporary variables and control variable passed added to rrtmg_lw call (lines 12642-12643)

7) flux values passed from local variables to output variables after this call 
(lines 12666--12671 and 12682--12683)

8) flux variables and control variable added to rrtmg_sw interface (lines 10586--10587)

9) declaration of these variables (lines 10768 & 10784--10787)

10) declaration of new internal "clean sky" variables for fluxes (lines 10899--10902)

11) a new call to the rtrnmc subroutine is added (passing only the gaseous optical depth, taug,
instead of the combined gas and aerosol optical depth, taut) - with a preprocessing statement 
so that it is only added for WRF-Chem. This call is also within an if statement, so can be 
switched off using the control variable when running WRF-Chem if desired. (lines 11017--11039)

12) new fluxes are copied to the clean sky output variables (lines 11054--11055)


7. chemics_init.F

1) added check to ensure that aer_ra_feedback is on when clean_atm_diag is used
(lines 406--408)

