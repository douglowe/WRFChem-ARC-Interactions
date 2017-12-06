# -*- coding: utf-8 -*-
"""
Created on Sun Mar 20 14:03:42 2016

Radiation plotting scripts reworked from Matlab to Python.

Script containing functions for decomposising radiative effects
into DIRECT, SEMI- and INDIRECT effects. 

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

To decompose the radiative effects of a particular emission source into 
direct, semi and indirect effects, a minimum of four scenarios are needed.
The following convensions have been used to name scenarios:
* BASE:  BASE scenario, with all emissions and aerosol-radiative interactions on
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

import numpy as np
import pandas as pd
import matplotlib.pyplot as plt


#=====================================================================#
#==     1.       FUNCTION DEFINITIONS                               ==#
#=====================================================================#

'''
All functions assume the following inputs:
* BASE_dict: a dict of domain averaged radiative flux arrays at TOA, 

        BASE_dict = { 'SWUPT_BASE':SWUPT_BASE,   'LWUPT_BASE':LWUPT_BASE, 
                      'LWUPTC_BASE':LWUPTC_BASE, 'SWUPTCLN_BASE':SWUPTCLN_BASE,
                      'SWUPT_B_nA':SWUPT_B_nA,   'LWUPT_B_nA':LWUPT_B_nA,
                      'LWUPTC_B_nA':LWUPTC_B_nA, 'SWUPTCLN_B_nA':SWUPTCLN_B_nA
                    }

* NOEMISS_dict: a dict of domain averaged radiative fluxes from TOA
        NOEMISS_dict = { 'SWUPT_nE':SWUPT_nE,         'LWUPT_nE':LWUPT_nE, 
                         'LWUPTC_nE':LWUPTC_nE,       'SWUPTCLN_nE':SWUPTCLN_nE, 
                         'SWUPT_nE_nA':SWUPT_nE_nA,   'LWUPT_nE_nA':LWUPT_nE_nA, 
                         'LWUPTC_nE_nA':LWUPTC_nE_nA, 'SWUPTCLN_nE_nA':SWUPTCLN_nE_nA
                       }
* error_type: String of type oferror to calculate. Currently two options:
    * 'standard_error', where SE = stddev/sqrt(N); N = no. data points
    * 'corrected_standard_error', where the SE has been corrected for 
         autocorrelation (rho); -1 < rho < 1 where 1 = perfect correlation
         => SE_corr = SE * sqrt(1+rho)/sqrt(1-rho)

Each function will return two arrays - average radiative effect, and error.
Errors are calculated by summing the corrected standard error of all 
constituent errors in quadrature
'''

#-- Function for loading the scenario data
def load_Files(DATADir,scen,alt_end,dom):

	# initialise the output dictionary
    out_dict = {}

    # list of the input variables we're opening
    var_list = [ 'SWUPT', 'LWUPT', 'LWUPTC', 'SWUPTCLN' ]
    
    # loop through variables, loading them for both scenarios (main, and supplementary), and add to dictionary
    for var in var_list:
        temp_array_a = pd.read_csv( DATADir + '/' + scen + '/' +
            '/' + var + '_' + dom + '_stats.txt',  index_col='Hour', sep='\s*,\s*', engine='python')
        temp_array_b = pd.read_csv( DATADir + '/' + scen + alt_end + '/' +
            '/' + var + '_' + dom + '_stats.txt',  index_col='Hour', sep='\s*,\s*', engine='python')
        out_dict.update({var:temp_array_a, var+alt_end:temp_array_b })
    

    return out_dict





#-- Functions to calculate net radiative effects at TOA
def calc_Delta_S( BASE_dict, ALT_dict, error_type):
    
    SWUPT_BASE = BASE_dict['SWUPT']
    SWUPT_ALT  = ALT_dict['SWUPT']
    
    Delta_S     = SWUPT_ALT['avg'] - SWUPT_BASE['avg']
    Delta_S_err = np.sqrt(
        np.power( SWUPT_ALT[error_type],   2) +
        np.power( SWUPT_BASE[error_type], 2) )
        
    return Delta_S, Delta_S_err

def calc_Delta_L( BASE_dict, ALT_dict, error_type):
    
    LWUPT_BASE = BASE_dict['LWUPT']
    LWUPT_ALT  = ALT_dict['LWUPT']
    
    Delta_L     = LWUPT_ALT['avg'] - LWUPT_BASE['avg']
    Delta_L_err = np.sqrt(
        np.power( LWUPT_ALT[error_type],   2) +
        np.power( LWUPT_BASE[error_type], 2) )
        
    return Delta_L, Delta_L_err


#=====================================================================#
#-- Functions to calculate SW radiative effects

#-- Calculate SW direct effect:
# SW_Direct   =    (SWUPTCLN - SWUPT )_BASE - (SWUPTCLN - SWUPT)_ALT
def calc_SW_DIRECT( BASE_dict, ALT_dict, error_type):

    SWUPTCLN_BASE = BASE_dict['SWUPTCLN']
    SWUPT_BASE    = BASE_dict['SWUPT']
    SWUPTCLN_ALT  = ALT_dict['SWUPTCLN']
    SWUPT_ALT     = ALT_dict['SWUPT']
    
    
    SW_DIRECT     = SWUPTCLN_BASE['avg'] - SWUPT_BASE['avg'] - \
                    SWUPTCLN_ALT['avg']   + SWUPT_ALT['avg']        
    SW_DIRECT_err = np.sqrt(
            np.power( SWUPTCLN_BASE[error_type], 2) +
            np.power( SWUPT_BASE[error_type],    2) +
            np.power( SWUPTCLN_ALT[error_type],   2) +
            np.power( SWUPT_ALT[error_type],      2) )
            
    return SW_DIRECT, SW_DIRECT_err

#-- Calculate SW indirect effect:        
# SW_INDIRECT   = (SWDNTCLN - SWUPTCLN)_BASE_nA - (SWDNTCLN - SWUPTCLN)_ALT_nA;
def calc_SW_INDIRECT( BASE_dict, ALT_dict, error_type):

    SWUPTCLN_BASE_nA   = BASE_dict['SWUPTCLN_nA']
    SWUPTCLN_ALT_nA  = ALT_dict['SWUPTCLN_nA']

    SW_INDIRECT     = SWUPTCLN_ALT_nA['avg'] - SWUPTCLN_BASE_nA['avg']
    SW_INDIRECT_err = np.sqrt(
        np.power( SWUPTCLN_ALT_nA[error_type], 2) +
        np.power( SWUPTCLN_BASE_nA[error_type],  2) )
        
    return SW_INDIRECT, SW_INDIRECT_err
    
#-- Calculate SW semidirect effect:
# SW_SEMIDIRECT = Delta_S - SW_DIRECT - SW_INDIRECT
#               = SWUPT_ALT - SWUPT_BASE - 
#                 (SWUPTCLN_BASE - SWUPT_BASE - SWUPTCLN_ALT + SWUPT_ALT ) -
#                 (SWUPT_ALT_nA - SWUPT_BASE_nA)
#               = SWUPT_ALT - SWUPT_BASE - SWUPTCLN_BASE + SWUPT_BASE + SWUPTCLN_ALT - SWUPT_ALT 
#                 - SWUPT_ALT_nA + SWUPT_BASE_nA                 
#               = SWUPTCLN_ALT - SWUPTCLN_BASE - SWUPT_ALT_nA + SWUPT_BASE_nA
def calc_SW_SEMIDIRECT( BASE_dict, ALT_dict, error_type):

    SWUPTCLN_BASE = BASE_dict['SWUPTCLN']
    SWUPT_BASE_nA = BASE_dict['SWUPT_nA']
    SWUPTCLN_ALT  = ALT_dict['SWUPTCLN']
    SWUPT_ALT_nA  = ALT_dict['SWUPT_nA']

    SW_SEMIDIRECT = SWUPTCLN_ALT['avg'] - SWUPTCLN_BASE['avg'] - \
                    SWUPT_ALT_nA['avg'] + SWUPT_BASE_nA['avg']
    SW_SEMIDIRECT_err = np.sqrt( 
        np.power( SWUPTCLN_ALT[error_type],   2) +
        np.power( SWUPTCLN_BASE[error_type], 2) + 
        np.power( SWUPT_ALT_nA[error_type],   2) +
        np.power( SWUPT_BASE_nA[error_type],    2) )
        
    return SW_SEMIDIRECT, SW_SEMIDIRECT_err


#=====================================================================#
#-- Functions to calculate LW radiative effects

#!!! Note LW direct effect of aerosol is generally negligible, so is ignored !!

#-- Calculate longwave indirect effect:
# LW INDIRECT   = (LWDNT - LWUPT - LWDNTC + LWUPTC)_BASE_nA - 
#   (LWDNT - LWUPT - LWDNTC + LWUPTC)_ALT_nA
#               =  (LWUPTC - LWUPT)_BASE_nA - (LWUPTC - LWUPT)_ALT_nA
def calc_LW_INDIRECT( BASE_dict, ALT_dict, error_type):
    
    LWUPTC_BASE_nA = BASE_dict['LWUPTC_nA']
    LWUPT_BASE_nA  = BASE_dict['LWUPTC_nA']
    LWUPTC_ALT_nA  = ALT_dict['LWUPTC_nA']
    LWUPT_ALT_nA   = ALT_dict['LWUPTC_nA']
    
    LW_INDIRECT     = LWUPTC_BASE_nA['avg']  - LWUPT_BASE_nA['avg'] - \
                      LWUPTC_ALT_nA['avg'] + LWUPT_ALT_nA['avg']
    
    LW_INDIRECT_err = np.sqrt( 
        np.power(LWUPTC_BASE_nA[error_type],  2) +
        np.power(LWUPT_BASE_nA[error_type],   2) + 
        np.power(LWUPTC_ALT_nA[error_type], 2) +
        np.power(LWUPT_ALT_nA[error_type],  2) )
    
    return LW_INDIRECT, LW_INDIRECT_err

#-- Calculate longwave semidirect effect:
# LW SEMIDIRECT = (LWUPTC - LWUPT)_BASE - (LWUPTC - LWUPT)_ALT - LW_INDIRECT
#               = LWUPTC_BASE - LWUPT_BASE - LWUPTC_ALT + LWUPT_ALT
#                  - LWUPTC_BASE_nA + LWUPT_BASE_nA + LWUPTC_ALT_nA - LWUPT_ALT_nA
def calc_LW_SEMIDIRECT( BASE_dict, ALT_dict, error_type):

    LWUPTC_BASE    = BASE_dict['LWUPTC']
    LWUPT_BASE     = BASE_dict['LWUPT']
    LWUPTC_BASE_nA = BASE_dict['LWUPTC_nA']
    LWUPT_BASE_nA  = BASE_dict['LWUPT_nA']
                        
    LWUPTC_ALT    = ALT_dict['LWUPTC']
    LWUPT_ALT     = ALT_dict['LWUPT']
    LWUPTC_ALT_nA = ALT_dict['LWUPTC_nA']
    LWUPT_ALT_nA  = ALT_dict['LWUPT_nA']


    LW_SEMIDIRECT     = LWUPTC_BASE['avg']  - LWUPT_BASE['avg'] - \
                        LWUPTC_ALT['avg']    + LWUPT_ALT['avg']   - \
                        LWUPTC_BASE_nA['avg']  + LWUPT_BASE_nA['avg'] + \
                        LWUPTC_ALT_nA['avg'] - LWUPT_ALT_nA['avg']
                        
    LW_SEMIDIRECT_err = np.sqrt( 
        np.power(LWUPTC_BASE[error_type], 2) +
        np.power(LWUPT_BASE[error_type],  2) + 
        np.power(LWUPTC_ALT[error_type], 2) +
        np.power(LWUPT_ALT[error_type],  2) +
        np.power(LWUPTC_BASE_nA[error_type],  2) +
        np.power(LWUPT_BASE_nA[error_type],  2) +
        np.power(LWUPTC_ALT_nA[error_type],  2) +
        np.power(LWUPT_ALT_nA[error_type],  2) )
        
    return LW_SEMIDIRECT, LW_SEMIDIRECT_err


##### plotting functions

def setup_figure(k):

    fig = plt.figure(k, figsize=[10,6])
    # make space for legend:
    ax = fig.add_axes([0.15, 0.15, 0.6, 0.75])

    return fig, ax


def finalise_and_print_figure(fig,ax,handles,title_string,file_string,OUTDir,scen_names,dom_name,dom,x_lim,y_lim,x_ticks):

    ax.legend(handles = handles, bbox_to_anchor=(1.05, 0.75), loc=2, 
              borderaxespad=0, numpoints=1, fontsize = 16)
    plt.title(title_string+', ' + scen_names[0] + ' - ' + scen_names[1] + ', ' + dom_name[dom], fontsize=21, y=1.03)
    ax.set_ylabel( title_string+' Wm$^{-2}$', fontsize = 18)
    ax.set_xlabel( 'Local Time ', fontsize = 18)
    plt.xlim(x_lim[0], x_lim[1])
    plt.ylim(y_lim[0], y_lim[1])
    plt.yticks(fontsize = 16)
    plt.xticks(x_ticks, fontsize = 16)
    
    #-- Save figure to pdf
    outname = OUTDir + file_string + scen_names[0] + '-' + scen_names[1] + '_' + dom + '.pdf'
    print('Saving file '+outname)
    plt.savefig(outname , format='pdf')

