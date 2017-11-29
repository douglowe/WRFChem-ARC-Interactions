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

#-- Functions to calculate net radiative effects at TOA
def calc_Delta_S( BASE_dict, NOEMISS_dict, error_type):
    
    SWUPT_BASE = BASE_dict['SWUPT_BASE']
    SWUPT_nE   = NOEMISS_dict['SWUPT_nE']
    
    Delta_S     = SWUPT_nE['avg'] - SWUPT_BASE['avg']
    Delta_S_err = np.sqrt(
        np.power( SWUPT_nE[error_type],   2) +
        np.power( SWUPT_BASE[error_type], 2) )
        
    return Delta_S, Delta_S_err

def calc_Delta_L( BASE_dict, NOEMISS_dict, error_type):
    
    LWUPT_BASE = BASE_dict['LWUPT_BASE']
    LWUPT_NE   = NOEMISS_dict['LWUPT_nE']
    
    Delta_L     = LWUPT_NE['avg'] - LWUPT_BASE['avg']
    Delta_L_err = np.sqrt(
        np.power( LWUPT_NE[error_type],   2) +
        np.power( LWUPT_BASE[error_type], 2) )
        
    return Delta_L, Delta_L_err


#=====================================================================#
#-- Functions to calculate SW radiative effects

#-- Calculate SW direct effect:
# SW_Direct   =    (SWUPTCLN - SWUPT )_BASE - (SWUPTCLN - SWUPT)_nE
def calc_SW_DIRECT( BASE_dict, NOEMISS_dict, error_type):

    SWUPTCLN_BASE = BASE_dict['SWUPTCLN_BASE']
    SWUPT_BASE    = BASE_dict['SWUPT_BASE']
    SWUPTCLN_nE   = NOEMISS_dict['SWUPTCLN_nE']
    SWUPT_nE      = NOEMISS_dict['SWUPT_nE']
    
    
    SW_DIRECT     = SWUPTCLN_BASE['avg'] - SWUPT_BASE['avg'] - \
                    SWUPTCLN_nE['avg']   + SWUPT_nE['avg']        
    SW_DIRECT_err = np.sqrt(
            np.power( SWUPTCLN_BASE[error_type], 2) +
            np.power( SWUPT_BASE[error_type],    2) +
            np.power( SWUPTCLN_nE[error_type],   2) +
            np.power( SWUPT_nE[error_type],      2) )
            
    return SW_DIRECT, SW_DIRECT_err

#-- Calculate SW indirect effect:        
# SW_INDIRECT   = (SWDNTCLN - SWUPTCLN)_B_nA - (SWDNTCLN - SWUPTCLN)_nE_nA;
def calc_SW_INDIRECT( BASE_dict, NOEMISS_dict, error_type):

    SWUPTCLN_B_nA   = BASE_dict['SWUPTCLN_B_nA']
    SWUPTCLN_nE_nA  = NOEMISS_dict['SWUPTCLN_nE_nA']

    SW_INDIRECT     = SWUPTCLN_nE_nA['avg'] - SWUPTCLN_B_nA['avg']
    SW_INDIRECT_err = np.sqrt(
        np.power( SWUPTCLN_nE_nA[error_type], 2) +
        np.power( SWUPTCLN_B_nA[error_type],  2) )
        
    return SW_INDIRECT, SW_INDIRECT_err
    
#-- Calculate SW semidirect effect:
# SW_SEMIDIRECT = Delta_S - SW_DIRECT - SW_INDIRECT
#               = SWUPT_nE - SWUPT_FE - 
#                 (SWUPTCLN_BASE - SWUPT_BASE - SWUPTCLN_nE + SWUPT_nE ) -
#                 (SWUPT_nE_nA - SWUPT_B_nA)
#               = SWUPT_nE - SWUPT_FE - SWUPTCLN_FE + SWUPT_FE + SWUPTCLN_nE - SWUPT_nE 
#                 - SWUPT_nE_nA + SWUPT_B_nA                 
#               = SWUPTCLN_nE - SWUPTCLN_BASE - SWUPT_nE_nA + SWUPT_B_nA
def calc_SW_SEMIDIRECT( BASE_dict, NOEMISS_dict, error_type):

    SWUPTCLN_BASE = BASE_dict['SWUPTCLN_BASE']
    SWUPT_B_nA    = BASE_dict['SWUPT_B_nA']
    SWUPTCLN_nE   = NOEMISS_dict['SWUPTCLN_nE']
    SWUPT_nE_nA   = NOEMISS_dict['SWUPT_nE_nA']

    SWUPTCLN_nE - SWUPTCLN_BASE - SWUPT_nE_nA + SWUPT_B_nA

    SW_SEMIDIRECT = SWUPTCLN_nE['avg'] - SWUPTCLN_BASE['avg'] - \
                    SWUPT_nE_nA['avg'] + SWUPT_B_nA['avg']
    SW_SEMIDIRECT_err = np.sqrt( 
        np.power( SWUPTCLN_nE[error_type],   2) +
        np.power( SWUPTCLN_BASE[error_type], 2) + 
        np.power( SWUPT_nE_nA[error_type],   2) +
        np.power( SWUPT_B_nA[error_type],    2) )
        
    return SW_SEMIDIRECT, SW_SEMIDIRECT_err


#=====================================================================#
#-- Functions to calculate LW radiative effects

#!!! Note LW direct effect of aerosol is generally negligible, so is ignored !!

#-- Calculate longwave indirect effect:
# LW INDIRECT   = (LWDNT - LWUPT - LWDNTC + LWUPTC)_B_nA - 
#   (LWDNT - LWUPT - LWDNTC + LWUPTC)_nE_nA
#               =  (LWUPTC - LWUPT)_B_nA - (LWUPTC - LWUPT)_nE_nA
def calc_LW_INDIRECT( BASE_dict, NOEMISS_dict, error_type):
    
    LWUPTC_B_nA  = BASE_dict['LWUPTC_B_nA']
    LWUPT_B_nA   = BASE_dict['LWUPTC_B_nA']
    LWUPTC_nE_nA = NOEMISS_dict['LWUPTC_nE_nA']
    LWUPT_nE_nA  = NOEMISS_dict['LWUPTC_nE_nA']
    
    LW_INDIRECT     = LWUPTC_B_nA['avg']  - LWUPT_B_nA['avg'] - \
                      LWUPTC_nE_nA['avg'] + LWUPT_nE_nA['avg']
    
    LW_INDIRECT_err = np.sqrt( 
        np.power(LWUPTC_B_nA[error_type],  2) +
        np.power(LWUPT_B_nA[error_type],   2) + 
        np.power(LWUPTC_nE_nA[error_type], 2) +
        np.power(LWUPT_nE_nA[error_type],  2) )
    
    return LW_INDIRECT, LW_INDIRECT_err

#-- Calculate longwave semidirect effect:
# LW SEMIDIRECT = (LWUPTC - LWUPT)_B - (LWUPTC - LWUPT)_nE - LW_INDIRECT
#               = LWUPTC_BASE - LWUPT_B - LWUPTC_nE + LWUPT_nE
#                  - LWUPTC_B_nA + LWUPT_B_nA + LWUPTC_nE_nA - LWUPT_nE_nA
def calc_LW_SEMIDIRECT( BASE_dict, NOEMISS_dict, error_type):

    LWUPTC_BASE  = BASE_dict['LWUPTC_BASE']
    LWUPT_BASE   = BASE_dict['LWUPT_BASE']
    LWUPTC_B_nA  = BASE_dict['LWUPTC_B_nA']
    LWUPT_B_nA   = BASE_dict['LWUPT_B_nA']
                        
    LWUPTC_nE    = NOEMISS_dict['LWUPTC_nE']
    LWUPT_nE     = NOEMISS_dict['LWUPT_nE']
    LWUPTC_nE_nA = NOEMISS_dict['LWUPTC_nE_nA']
    LWUPT_nE_nA  = NOEMISS_dict['LWUPT_nE_nA']


    LW_SEMIDIRECT     = LWUPTC_BASE['avg']  - LWUPT_BASE['avg'] - \
                        LWUPTC_nE['avg']    + LWUPT_nE['avg']   - \
                        LWUPTC_B_nA['avg']  + LWUPT_B_nA['avg'] + \
                        LWUPTC_nE_nA['avg'] - LWUPT_nE_nA['avg']
                        
    LW_SEMIDIRECT_err = np.sqrt( 
        np.power(LWUPTC_BASE[error_type], 2) +
        np.power(LWUPT_BASE[error_type],  2) + 
        np.power(LWUPTC_nE[error_type], 2) +
        np.power(LWUPT_nE[error_type],  2) +
        np.power(LWUPTC_B_nA[error_type],  2) +
        np.power(LWUPT_B_nA[error_type],  2) +
        np.power(LWUPTC_nE_nA[error_type],  2) +
        np.power(LWUPT_nE_nA[error_type],  2) )
        
    return LW_SEMIDIRECT, LW_SEMIDIRECT_err





