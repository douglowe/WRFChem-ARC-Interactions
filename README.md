# WRFChem-ARC-Interactions

Modified Radiative Code for WRF-Chem to calculate Aerosol Radiative Effects.

Contents of this repository are:

WRF-Chem_code: modified files for a range of versions of WRF-Chem. Replace the original files with these in order to add the clean atmosphere radiative calls. From 4.0 onwards the code should be included in the release version, and so these will not be needed.

analysis_scripts: NCL scripts for extracting the necessary data, and python scripts for using this data for plotting the radiative forcings.

For information on how to run the model, and use these scripts, to fully determine the aerosol-radiation-cloud interactions, see the wiki page here: https://github.com/douglowe/WRFChem-ARC-Interactions/wiki

Acknowledgements: If you use this analysis method please reference these papers:

Archer-Nicholls, S. et al., 2016. Aerosol–radiation–cloud interactions in a regional coupled model: the effects of convective parameterisation and resolution. Atmospheric Chemistry and Physics, 16(9), pp.5573–5594. Available at: http://www.atmos-chem-phys.net/16/5573/2016/.

Ghan, S. J., Liu, X., Easter, R. C., Zaveri, R., Rasch, P. J., Yoon, J.-H., and Eaton, B.: Toward a Minimal Representation of Aerosols in Climate Models: Comparative Decomposition of Aerosol Direct, Semidirect, and Indirect Radiative Forcing, J. Climate, 25, 6461–6476, doi:10.1175/JCLI-D-11-00650.1, 2012.

If you use the analysis code provided here please also acknowledge this, and provide a link to this repository in your publications.
