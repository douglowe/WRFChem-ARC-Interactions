# -*- coding: utf-8 -*-
"""
Created on Sun Mar 20 14:03:42 2016

Radiation plotting scripts reworked from Matlab to Python.

Script for loading and plotting Radiation statistical data from
WRFCHEM. For these scripts to work, WRF-Chem must have been run 
with changes to include 'clean-sky' double radiation calls.
All SW and LW radiative fluxes at the top of the atmosphere
must also have been output. 

The figures produced in this example are averaged diurnal radiative
effects. However, it can easily be modified for other purposes - 
the main 

The data will come from domain (and sub-domain) analysis
performed by the domain_2D_nest_avg.ncl script. This script
works by averaging all radiative fluxes over either the whole
domain (d01, d02...), or a subdomain defined by a lon-lat box 
(SD1, SD2...). 

The following statistical values are calculated and saved
in an array format:

# Data format (columns):
#   0. Time   
#   1. avg      -> average value over the defined domain
#   2. max      -> maximum value
#   3. median   -> median value
#   4. min      -> minimum value
#   5. stddev   -> standard deviation
#   6. x25      -> 25 percentile
#   7. x75      -> 75 percentile
#   8. x05      -> 5th percentile
#   9. x95      -> 95th percentile
#   10.SE       -> Standard error
#   11.morans_i -> Moran's I estimate of autocorrelation
#   12.SE_corr  -> corrected SE
#   13.N        -> Number of grid points

For these figures, avg is used, with the SE and
SE_corr used for error estimates. 

The median, x25/x75 and x5/x95 values can easily be used 
to make box-whisker plots.

To decompose the radiative effects of a particular emission source into 
direct, semi and indirect effects, a minimum of four scenarios are needed.
The following convensions have been used to name scenarios:
* B:     BASE scenario, with all emissions and aerosol-radiative interactions on
* B_nA:  BASE scenario, but with aerosol-radiative interactions turned off
* nE:    The NOEMISS scenario, with the emission source of interest removed
* nE_nA: The NOEMISS scenario, but with aerosol-radiative interactions turned off 

For details of the calculations, please see:
  Archer-Nicholls et al., ACP, 2016. doi:10.5194/acp-16-5573-2016
  Ghan et al., J. Climate, 2012. doi:10.1175/JCLI-D-11-00650.1

This framework can easily be adaped for different questions. e.g., to compare
present day with pre-industrial emissions, the BASE run would be present day, 
and the nE run would be preindustrial emissions.

08/06/2017
Scott Archer-Nicholls
sa847@cam.ac.uk

@author: nicholls
"""

import pandas as pd
import matplotlib.pyplot as plt

import RadDecomp_functions as radfn

#=============================================================================#
#==     1.       Define global variables                                    ==#
#=============================================================================#

#-- Define directories to find the data
COMPDir = '/Users/lowe/work/manchester/CCN-VOL/'
DATADir = COMPDir + 'model_data/rad_force_data/'
OUTDir  = COMPDir + 'plots/rad_force/'

#-- List emission scenarios for comparison
scen_list = [ 'NDOWN_VBS_equilib_run_dry_v3.8.1', 'NDOWN_VBS_equilib_run_wet_v3.8.1' ] # scenario names for directory
# Link name to full name for titles on plots:
scen_names = [ 'DRY' , 'WET' ]

#-- List subdomains
dom_list = [ 'domain' ]
dom_name = { 'domain':'Full Domain' }            

#-- Limits on x- and y- axis: change a appropriate
x_lim   = [ 0, 72] # hours in day
x_ticks = [ 6, 12, 18, 24, 30, 36, 42, 48, 54, 60, 66 ]

#-- Min and max radiative effect (in W/m2), for SW and LW plots
sw_ylim = [-6, 6]
lw_ylim = [-2, 2]

#-- Method for calculating uncertainty:
error_type = 'SE_corr'  # standard error corrected
# error_type = 'SE'     # standard error (uncorrected)


#-- Declare figure counter
k = 0

# loop through the model domains to plot
for dom in dom_list:
        
    #=============================================================================#
    #==     2.       Load the data                                              ==#
    #=============================================================================#

    BASE_dict = radfn.load_Files(DATADir,scen_list[0],'_nA',dom)
    NOEMISS_dict = radfn.load_Files(DATADir,scen_list[1],'_nA',dom)
                

    # Get an array of the time index
    Time = BASE_dict['SWUPT'].index.values



    #=====================================================================#
    #==     3.       Calculate the variables to plot                    ==#
    #=====================================================================#
    
    #-- Use RadDecomp_functions to calculate radiative effects:
    Delta_S, Delta_S_err = radfn.calc_Delta_S( BASE_dict, NOEMISS_dict, error_type)
    Delta_L, Delta_L_err = radfn.calc_Delta_L( BASE_dict, NOEMISS_dict, error_type)        

    SW_DIRECT, SW_DIRECT_err     = radfn.calc_SW_DIRECT( BASE_dict, NOEMISS_dict, error_type)
    SW_INDIRECT, SW_INDIRECT_err = radfn.calc_SW_INDIRECT( BASE_dict, NOEMISS_dict, error_type)
    SW_SEMIDIRECT, SW_SEMIDIRECT_err = radfn.calc_SW_SEMIDIRECT( BASE_dict, NOEMISS_dict, error_type)
                               
    LW_INDIRECT, LW_INDIRECT_err = radfn.calc_LW_INDIRECT( BASE_dict, NOEMISS_dict, error_type)
    LW_SEMIDIRECT, LW_SEMIDIRECT_err = radfn.calc_LW_SEMIDIRECT( BASE_dict, NOEMISS_dict, error_type)
                               

    #=====================================================================#
    #==     4.       Make Figures                                       ==#
    #=====================================================================#
    


    #-- Make SW effect plots:
    fig, ax = radfn.setup_figure(k)

    
    a = ax.errorbar(Time-0.15, SW_DIRECT, fmt="r-", yerr=SW_DIRECT_err, 
                lw=3.5,label="SW$_{\mathrm{DIRECT}}$", zorder=2, 
                capsize=5, capthick=2.5)
    b = ax.errorbar(Time, SW_INDIRECT,  fmt="b--",   yerr=SW_INDIRECT_err,
                lw=4.5,label="SW$_{\mathrm{INDIRECT}}$", zorder=1, 
                capsize=5, capthick=2.5)
    c = ax.errorbar(Time+0.15, SW_SEMIDIRECT, fmt="g-.", yerr=SW_SEMIDIRECT_err, 
                lw=3,label="SW$_{\mathrm{SEMIDIRECT}}$", zorder=3, 
                capsize=5, capthick=2.5)

    radfn.finalise_and_print_figure(fig,ax,[a,b,c],'SW Forcings','SW_forcings_',OUTDir,scen_names,dom_name,dom,x_lim,sw_ylim,x_ticks)


    #-- Add to figure counter
    k = k+1


    #-- LW radiation make plot
    fig, ax = radfn.setup_figure(k)

    a = ax.errorbar(Time-0.1, LW_INDIRECT,   yerr=LW_INDIRECT_err, fmt="b--", 
                    lw=4, label="LW$_{\mathrm{INDIRECT}}$", zorder=1,
                    capsize=5, capthick=2.5)

    b = ax.errorbar(Time+0.1, LW_SEMIDIRECT, yerr=LW_SEMIDIRECT_err, fmt="g-.", 
                    lw=3.5, label="LW$_{\mathrm{SEMIDIRECT}}$", zorder=2, 
                    capsize=5, capthick=2.5)

    radfn.finalise_and_print_figure(fig,ax,[a,b],'LW Forcings','LW_forcings_',OUTDir,scen_names,dom_name,dom,x_lim,lw_ylim,x_ticks)

   
    #-- Add to figure counter
    k = k+1
    
    #-- Make net forcing plots:
    fig, ax = radfn.setup_figure(k)

    a = ax.errorbar(Time-0.1, Delta_S, fmt="k-", yerr=Delta_S_err, 
                lw=3,label="$\Delta \mathrm{SW}_{TOA}$", zorder=1,
                capsize=5, capthick=2.5)
                
    b = ax.errorbar(Time+0.1, Delta_L, fmt="k-.", yerr=Delta_L_err, 
                lw=4,label="$\Delta \mathrm{LW}_{TOA}$", zorder=2,
                capsize=5, capthick=2.5)

    radfn.finalise_and_print_figure(fig,ax,[a,b],'Net Forcings','Net_forcings_',OUTDir,scen_names,dom_name,dom,x_lim,sw_ylim,x_ticks)


#=============================================================================#
