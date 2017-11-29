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
COMPDir = '/Users/XXXX/'
DATADir = COMPDir + 'XXXX/DATA/domain_avg_data/'
OUTDir  = COMPDir + 'XXXX/plots/RadDecomp_plots/'

#-- List emission scenarios
scen_list = [ 'BASE' ] # scenario names for directory
# Link name to full name for titles on plots:
scenario_name = { 'BASE':'BASE' }

#-- Will assume we are finding difference to a scenario with a portion of 
#     all emissions, or all emissions, removed (NOEMISS)
scen_nE    = 'nE'
scen_nE_nA = 'nE_nA'

#-- List subdomains
dom_list = [ 'd01', 'SD1', 'SD2' ]
dom_list = [ 'EC' ] #, 'SB', 'NCP' ]
dom_name = { 'd01':'FullDomain', 'SD1':'Subdomain1', 'SD2':'Subdomain2' }            

#-- Limits on x- and y- axis: change a appropriate
x_lim   = [ 0, 24] # hours in day
x_ticks = [ 3, 6, 9, 12, 15, 18, 21, 24 ]

#-- Min and max radiative effect (in W/m2), for SW and LW plots
sw_ylim = [-6, 6]
lw_ylim = [-2, 2]

#-- Method for calculating uncertainty:
error_type = 'corrected_standard_error'
# error_type = 'standard_error'

#=============================================================================#
#==     2.       Load the data                                              ==#
#=============================================================================#

#-- Declare figure counter
k = 0

for scen in scen_list:
    for dom in dom_list:
        
        #-- Get name for scenario with no aero-rad interactions
        scen_nA = scen+'_nA'

        #-- Load variables from BASE scenario
        SWUPT_BASE    = pd.read_csv( DATADir + '/' + scen + '/' +
            '/SWUPT_' + dom + '.txt',  index_col='Hour')
            
        LWUPT_BASE    = pd.read_csv( DATADir + '/' + scen + '/' +
            '/LWUPT_' + dom + '.txt',  index_col='Hour')
            
        LWUPTC_BASE   = pd.read_csv( DATADir + '/' + scen + '/' +
            '/LWUPTC_'+dom+'.txt',  index_col='Hour')
            
        SWUPTCLN_BASE = pd.read_csv( DATADir + '/' + scen + '/' +
            '/SWUPTCLN_'+dom+'.txt',  index_col='Hour')
        			
        #-- Load variables from NORES scenario
        SWUPT_nE      = pd.read_csv( DATADir + '/'+ scen_nE + '/' +
            '/SWUPT_'+dom+'.txt',  index_col='Hour')
            
        LWUPT_nE      = pd.read_csv( DATADir + '/'+ scen_nE + '/' +
            '/LWUPT_'+dom+'.txt',  index_col='Hour')
            
        LWUPTC_nE     = pd.read_csv( DATADir + '/'+ scen_nE + '/' +
            '/LWUPTC_'+dom+'.txt',  index_col='Hour')
            
        SWUPTCLN_nE   = pd.read_csv( DATADir + '/'+ scen_nE + '/' +
            '/SWUPTCLN_'+dom+'.txt',  index_col='Hour')
                    
        #-- Load variables from BASE with no aerosol-rad interactions scenario
        SWUPT_B_nA    = pd.read_csv( DATADir + '/'+ scen_nA + '/' +
            '/SWUPT_'+dom+'.txt',  index_col='Hour')
            
        LWUPT_B_nA    = pd.read_csv( DATADir + '/'+ scen_nA + '/' +
            '/LWUPT_'+dom+'.txt',  index_col='Hour')
            
        LWUPTC_B_nA   = pd.read_csv( DATADir + '/'+ scen_nA + '/' +
            '/LWUPTC_'+dom+'.txt',  index_col='Hour')

        SWUPTCLN_B_nA = pd.read_csv( DATADir + '/'+ scen_nA + '/' +
            '/SWUPTCLN_'+dom+'.txt',  index_col='Hour')


        #-- Load variables from NORES with no aerosol-rad interactions scenario
        SWUPT_nE_nA   = pd.read_csv( DATADir + '/'+ scen_nE_nA + '/' +
            '/SWUPT_'+dom+'.txt',  index_col='Hour')
            
        LWUPT_nE_nA   = pd.read_csv( DATADir + '/'+ scen_nE_nA + '/' +
            '/LWUPT_'+dom+'.txt',  index_col='Hour')
            
        LWUPTC_nE_nA  = pd.read_csv( DATADir + '/'+ scen_nE_nA + '/' +
            '/LWUPTC_'+dom+'.txt',  index_col='Hour')  
            
        SWUPTCLN_nE_nA= pd.read_csv( DATADir + '/'+ scen_nE_nA + '/' +
            '/SWUPTCLN_'+dom+'.txt',  index_col='Hour')  

        #-- Combine the arrays in lookup tables of all the information
        BASE_dict = { 'SWUPT_BASE':SWUPT_BASE,   'LWUPT_BASE':LWUPT_BASE, 
                      'LWUPTC_BASE':LWUPTC_BASE, 'SWUPTCLN_BASE':SWUPTCLN_BASE,
                      'SWUPT_B_nA':SWUPT_B_nA,   'LWUPT_B_nA':LWUPT_B_nA,
                      'LWUPTC_B_nA':LWUPTC_B_nA, 'SWUPTCLN_B_nA':SWUPTCLN_B_nA
                    }

        NOEMISS_dict = { 'SWUPT_nE':SWUPT_nE,         'LWUPT_nE':LWUPT_nE, 
                         'LWUPTC_nE':LWUPTC_nE,       'SWUPTCLN_nE':SWUPTCLN_nE, 
                         'SWUPT_nE_nA':SWUPT_nE_nA,   'LWUPT_nE_nA':LWUPT_nE_nA, 
                         'LWUPTC_nE_nA':LWUPTC_nE_nA, 'SWUPTCLN_nE_nA':SWUPTCLN_nE_nA
                       }

        # Get an array of the time index
        Time = SWUPT_BASE.index.values



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
        fig = plt.figure(k, figsize=[10,6])
        #ax = fig.add_subplot(121)
        # make space for legend:
        ax = fig.add_axes([0.15, 0.15, 0.6, 0.75])
        
        a = ax.errorbar(Time-0.15, SW_DIRECT, fmt="r-", yerr=SW_DIRECT_err, 
                    lw=3.5,label="SW$_{\mathrm{DIRECT}}$", zorder=2, 
                    capsize=5, capthick=2.5)
        b = ax.errorbar(Time, SW_INDIRECT,  fmt="b--",   yerr=SW_INDIRECT_err,
                    lw=4.5,label="SW$_{\mathrm{INDIRECT}}$", zorder=1, 
                    capsize=5, capthick=2.5)
        c = ax.errorbar(Time+0.15, SW_SEMIDIRECT, fmt="g-.", yerr=SW_SEMIDIRECT_err, 
                    lw=3,label="SW$_{\mathrm{SEMIDIRECT}}$", zorder=3, 
                    capsize=5, capthick=2.5)

        ax.legend(handles = [a, b, c], bbox_to_anchor=(1.05, 0.75), loc=2, 
                  borderaxespad=0, numpoints=1, fontsize = 16)
        plt.title('SW Forcings, '+ scen + ',' + dom_name[dom], fontsize=21, y=1.03)
#        plt.ylim(ylim[0], ylim[1])
        ax.set_ylabel( 'SW Forcings Wm$^{-2}$', fontsize = 18)
        ax.set_xlabel( 'Local Time ', fontsize = 18)
        plt.xlim(x_lim[0], x_lim[1])
        plt.ylim(sw_ylim[0], sw_ylim[1])
        plt.yticks(fontsize = 16)
        plt.xticks(x_ticks, fontsize = 16)
        
        #-- Save figure to pdf
        outname = OUTDir + 'SW_forcings_' + scen + '_' + dom + '.pdf'
        print('Saving file '+outname)
        plt.savefig(outname , format='pdf')

        #-- Add to figure counter
        k = k+1

        #-- Make LW effect plots:
        fig = plt.figure(k, figsize=[10,6])
        #ax = fig.add_subplot(121)
        # make space for legend:
        ax = fig.add_axes([0.15, 0.15, 0.6, 0.75])

        #-- LW radiation make plot
        a = ax.errorbar(Time-0.1, LW_INDIRECT,   yerr=LW_INDIRECT_err, fmt="b--", 
                        lw=4, label="LW$_{\mathrm{INDIRECT}}$", zorder=1,
                        capsize=5, capthick=2.5)

        b = ax.errorbar(Time+0.1, LW_SEMIDIRECT, yerr=LW_SEMIDIRECT_err, fmt="g-.", 
                        lw=3.5, label="LW$_{\mathrm{SEMIDIRECT}}$", zorder=2, 
                        capsize=5, capthick=2.5)
        ax.legend(handles = [a, b], bbox_to_anchor=(1.05, 0.75), loc=2, 
                  borderaxespad=0,  numpoints=1, fontsize = 16)
        plt.title('LW Forcings, '+ scen + ',' + dom_name[dom], fontsize=21, y=1.03)
        ax.set_ylabel( 'LW Forcings Wm$^{-2}$', fontsize = 18)
        ax.set_xlabel( 'Local time.', fontsize = 18)
        plt.xlim(x_lim[0], x_lim[1])
        plt.ylim(lw_ylim[0], lw_ylim[1])
        plt.yticks(fontsize = 16)        
        plt.xticks(x_ticks, fontsize = 16)
        
        #-- Save figure to pdf
        outname = OUTDir + 'LW_forcings_' + scen + '_' + dom + '.pdf'        
        print('Saving file '+outname)
        plt.savefig(outname , format='pdf')
       
        #-- Add to figure counter
        k = k+1
        
        #-- Make net forcing plots:
        fig = plt.figure(k, figsize=[10, 6])
        #ax = fig.add_subplot(121)
        # make space for legend:
        ax = fig.add_axes([0.15, 0.15, 0.6, 0.75])

        a = ax.errorbar(Time-0.1, Delta_S, fmt="k-", yerr=Delta_S_err, 
                    lw=3,label="$\Delta \mathrm{SW}_{TOA}$", zorder=1,
                    capsize=5, capthick=2.5)
                    
        b = ax.errorbar(Time+0.1, Delta_L, fmt="k-.", yerr=Delta_L_err, 
                    lw=4,label="$\Delta \mathrm{LW}_{TOA}$", zorder=2,
                    capsize=5, capthick=2.5)

        ax.legend(handles = [a, b], bbox_to_anchor=(1.05, 0.75), loc=2, 
                  borderaxespad=0,  numpoints=1, fontsize = 16)
        plt.title('Net Forcings, ' + scen + ',' + dom_name[dom], fontsize=21, y=1.03)
        ax.set_ylabel( 'Net Forcings Wm$^{-2}$', fontsize = 18)
        ax.set_xlabel( 'Local time', fontsize = 18)
        plt.xlim(x_lim[0], x_lim[1])
        plt.ylim(sw_ylim[0], sw_ylim[1])
        plt.yticks(fontsize = 16)        
        plt.xticks(x_ticks, fontsize = 16)

        #-- Save figure to pdf
        outname = OUTDir + 'Net_forcings_' + scen + '_' + dom + '.pdf'        
        print('Saving file '+outname)
        plt.savefig(outname , format='pdf')   
        
        #-- Add to counter
        k = k+1

#=============================================================================#
