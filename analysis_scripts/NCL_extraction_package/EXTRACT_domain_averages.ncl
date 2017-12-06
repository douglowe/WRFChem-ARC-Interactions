;
;  Plot domain statistical data as box and whisker plots 
;
;  The box middle point is the "median" value.
;  The box upper and lower edges are the 75th and 25th percentile values
;  The upper and lower whiskers are the 95th and 5th percentile values
;


load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/wrf/WRFUserARW.ncl"
load "./data_extraction_library.ncl"

begin

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;; script settings ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
root_directory_a = "/nerc/n02/n02/davet80/Data_analysis/"
root_directory = "/nerc/n02/n02/lowe/CCN-Vol_data_analysis/"

input_root_directory  = root_directory_a+"Data_store/"
output_root_directory = root_directory+"plots/domain_averages_rad_balance/"

;;; list of scenarios to process
scenarios_list = (/"NDOWN_VBS_equilib_run_wet_v3.8.1","NDOWN_VBS_equilib_run_dry_v3.8.1"/)


;;; variables for extraction of statistics
plot_variables = (/ "SWUPT","SWUPTC","SWUPTCLN","SWDNT","SWDNTC","SWDNTCLN", \
					"SWUPB","SWUPBC","SWUPBCLN","SWDNB","SWDNBC","SWDNBCLN", \
					"LWUPT","LWUPTC","LWUPTCLN","LWDNT","LWDNTC","LWDNTCLN", \
					"LWUPB","LWUPBC","LWUPBCLN","LWDNB","LWDNBC","LWDNBCLN" /)


; This sets the altitude levels at which you wish to extract the model data
;   Note 1: if you want the data at ground level, then use an altitude of 0.0
;   Note 2: these heights are in "metres above sea level"
;altitude_levels = (/ 0.0, 50.0, 100.0, 300.0, 500.0, 1000.0, 2000.0 /)
;altitude_levels = (/ 0.0 /)
altitude_levels = (/ 0.0 /)

; Set this to True if you want to "trim" the edge of the domain before calculating 
;    the domain stats.
;  "trim" is the number of grid cells on each side of the domain to remove.
domain_trim = True
domain_trim@trim = 5

; Set this to True if you want to process sub-regions of the domain, and
; add a list of the region names you wish to process
;   Note: This will override the "domain_trim" option!
region_select = False
region_select@regions = (/  "NATL", "SATL", "NSEA", "ENG", "FR", "DE" /)



; Set this to True if you want to label plots with local time, not UTC
;    You will need to set the offset to do this. 
use_local_time = False
use_local_time@offset = 8



; This flag determines if we want to plot a set of defined time points, or the whole campaign.
;    True -> plot the chosen times
;    False -> plot *all* the model data (Warning: it will take 30-50 minutes to load the data)
;    
;    Note: If you set day = "*" then you will plot the selected hour on each day.
;          If you set hour = "*" then you will plot all hours in the selected day.
;    Note: ***These times are all UTC*** 
plot_selected_times = True
day = (/"*"/)
hour = (/"00","03","06","09","12","15","18","21"/)
month = (/"*"/)  
year  = (/"*"/)  


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;; variable settings ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




;;; set region corners here
minlon@NATL = -16
minlat@NATL = 56
maxlon@NATL = -10
maxlat@NATL = 62

minlon@SATL = -14
minlat@SATL = 45
maxlon@SATL = -5
maxlat@SATL = 49

minlon@NSEA = 1
minlat@NSEA = 53
maxlon@NSEA = 5
maxlat@NSEA = 57

minlon@ENG = -4
minlat@ENG = 51
maxlon@ENG = 1
maxlat@ENG = 55

minlon@FR = 0
minlat@FR = 45
maxlon@FR = 5
maxlat@FR = 50

minlon@DE = 6
minlat@DE = 48
maxlon@DE = 10.5
maxlat@DE = 54





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;; end of settings ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; loop through the list of scenarios
do iscen=0,dimsizes(scenarios_list)-1

	scenario = scenarios_list(iscen)

	;;; check that the output directory exists, if not then create it
	system("if ! test -d  "+output_root_directory+" ; then mkdir "+output_root_directory+"; fi")
	system("if ! test -d  "+output_root_directory+scenario+" ; then mkdir "+output_root_directory+scenario+"; fi")



	;;; open the data files
	Dfiles = create_input_file_list(plot_selected_times,"d01",year,month,day,hour,input_root_directory+scenario)
	check_file_status(Dfiles)  
	DATA_FILES = addfiles(Dfiles+".nc","r")


	;;; loop through the list of variables that we want to plot
	do ivar=0,dimsizes(plot_variables)-1
		print("== working on variable "+plot_variables(ivar))

		print("  == loading variable")
		;;; load variable to plot (extracting horizontal slices while we're at it)
		plot_var = load_2D_3D_variable_and_sample_at_given_altitudes(DATA_FILES,plot_variables(ivar),altitude_levels)

		print("  == calculate stats")
		;;; calculate the domain stats
		plot_stats = calculate_domain_stats(DATA_FILES,plot_var,domain_trim,region_select)
				
		print("  == write out data")
		;;; write out the plotted data as a text file
		write_stats_data(DATA_FILES,plot_stats,\
					plot_variables(ivar),output_root_directory+scenario,scenario,\
					altitude_levels,use_local_time,region_select)

		;;; tidy up
		delete([/plot_var,plot_stats/])

	end do


end do

end
