;
; library file for the time series plotting scripts
;

load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/wrf/WRFUserARW.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/contrib/time_axis_labels.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/wrf/WRF_contributed.ncl"

load "./misc_stats_library.ncl"

begin

;;;;;;; Setting up the logical variables which we will use to carry 
;;;;;;;   attributes controlling the variable setups

;; used for carrying information about changing variable units, e.g.:
;;     variable_units@T2  = (/"K","C"/)
variable_units = True

;; used for carrying the variable descriptions
variable_description = True


;;; carrier for lat / lon settings
minlon = True
maxlon = True
minlat = True
maxlat = True



;;;;;; file check routines
undef("check_file_status")
procedure check_file_status(Dfiles:string)
local itest, status, fail
begin

	;;; set fail flag to False
	fail = False

	;;; loop through the file list, making sure that they are all readable
	do itest = 0,dimsizes(Dfiles)-1
		status = systemfunc(" [ -r "+Dfiles(itest)+" ] && echo 'True' || echo 'False' ")
		;; if not readable then tell us, and set fail to True
		if(status .eq. "False")
			print(Dfiles(itest)+" is not readable")
			fail = True
		end if
	end do

	;; stop the program if the files aren't readable
	if(fail)
		print("Not all files are readable (see above messages) --- not continuing with script.")
		exit
	end if

end


;
;  Create a list of strings containing the paths to all the input files we wish to process.
;  This function assumes that the "domain" input lists only one domain - because processing 
;  more domains than this would break all our scripts. However the year, month, day, and hour
;  variables can contain multiple strings to process.
;
undef("create_input_file_list")
function create_input_file_list(plot_selected_times:logical,domain:string,\
				year_array:string,month_array:string,day_array:string,hour_array:string,\
				input_directory:string)
local File_List, ny, nm, nd, nh, iy, im, id, ih, name_string, Start_Array
begin
	

	; check if we need to filter the input files
	if(plot_selected_times)
		ny = dimsizes(year_array)
		nm = dimsizes(month_array)
		nd = dimsizes(day_array)
		nh = dimsizes(hour_array)
		
		if(ny.eq.1 .and. nm.eq.1 .and. nd.eq.1 .and. nh.eq.1)
			name_string = "wrfout_"+domain+"_"+year_array+"-"+month_array+"-"+day_array+"_"+hour_array+"*"
			File_List = systemfunc("ls -1 "+input_directory+"/"+name_string)	
		else
			Start_Array = True
			do iy = 0,ny-1
			do im = 0,nm-1
			do id = 0,nd-1
			do ih = 0,nh-1
				name_string = "wrfout_"+domain+"_"+year_array(iy)+"-"+month_array(im)+"-"+day_array(id)+"_"+hour_array(ih)+"*"
				print(" "+name_string)
				Temp_List = systemfunc("ls -1 "+input_directory+"/"+name_string)

				; check to see if we have only one file in the list
				if(dimsizes(Temp_List).eq.1)
					; check to make sure we have listed *some* files, and to jump to next iteration of 
					;   the loop if not
					if(ismissing(Temp_List))
						delete(Temp_List)
						continue
					end if
				end if
				
				
				
				if(Start_Array)
					File_List = Temp_List
					Start_Array = False
				else
					Old_List = File_List
					delete(File_List)
					File_List = array_append_record (Old_List, Temp_List, 0)
					delete(Old_List)
				end if
				delete(Temp_List)
			end do
			end do
			end do
			end do
			
			; check to see if we have failed to list any files - if stop program
			if(Start_Array)
				print("No input files for specified dates & times")
				exit
			end if
			
		end if
	else ; otherwise we just open everything for that domain
		File_List = systemfunc("ls -1 "+input_directory+"/wrfout_"+domain+"*")
	end if
	
	
	; sort the files into time order
	sqsort(File_List)
	
	print("Input Files:")
	do iy = 0,dimsizes(File_List)-1
		print(""+File_List(iy))
	end do

	return(File_List)

end 





undef("load_2D_3D_variable_and_sample_at_given_altitudes")
function load_2D_3D_variable_and_sample_at_given_altitudes(FILES:list,var:string,altitude_levels:numeric)
local var_temp, dimensions, dimnum, variable, alt_km
begin

		
	; extract variable from FILES, assuming we want all times (and that it's a standard variable!!!)
	var_temp = wrf_user_getvar(FILES,var,-1)


	;;;; sampling through the altitude levels *if* it's needed!

	;; first determine the dimensions of the model data
	dimensions = dimsizes(var_temp)
	; (time, height, x, y)  if data is 3D (plus time)
	;   or
	; (time, x, y)          if data is 2D (plus time)
	
	;; determine how many dimensions
	dimnum = dimsizes(dimensions)
	

	;;; if we have loaded a 3D variable then sample at given altitudes
	if ( dimnum .eq. 4 ) ; 3D data

		;;; convert from metres to km for the altitude
		alt_km = altitude_levels / 1000.0


		;;; extract horizontal slices from the domain volume
		variable = wrf_user_vert_interp(FILES,var_temp,"ght_msl",alt_km,True)

		;;; replace any altitudes of zero with the data from the ground level
		do in = 0,dimsizes(alt_km)-1
			if(alt_km(in) .eq. 0.0)
				variable(:,in,:,:) = (/var_temp(:,0,:,:)/)
			end if
		end do

	;;; otherwise just return the single level of data
	else if ( dimnum .eq. 3 ) ; 2D data

		variable = var_temp
		
	else
		print("don't know how to deal with data with "+dimsizes(dimensions)+" dimensions")
		exit
	end if
	end if
	


	return(variable)	
	
end




; calculates, standard stats for domain averages5th, 25th , median, 75th and 95th percentiles. 
; Returns them an array of size ( #time, #levels, 9):
;	stats(0) = mean(avg)
;	stats(1) = stdev
; 	stats(2) = min
;	stats(3) = max 
;	stats(4) = median
; 	stats(5) = lower quartile
;	stats(6) = upper quartile
;	stats(7) = 5th percentile
;	stats(8) = 95th percentile
;   stats(9) = standard error
;   stats(10)= Moran's I estimate of autocorrelation
;   stats(11)= corrected standard error
;   stats(12)= number of grid points
;
undef("calculate_domain_stats")
function calculate_domain_stats(FILES:list,var_temp:numeric,domain_trim:logical,region_select:logical)
local dimensions, dimnum, dimlat, dimlon, stat_data, \
		lon_start, lon_end, lat_start, lat_end, itime, ilev, \
		nreg, ireg, region, locmin, locmax
begin
	;; first determine the dimensions of the model data
	dimensions = dimsizes(var_temp)
	; (time, height, x, y)  if data is 3D (plus time)
	;   or
	; (time, x, y)          if data is 2D (plus time)
	
	;; determine how many dimensions
	dimnum = dimsizes(dimensions)
	
	
	;; prepare for processing a list of regions
	if ( region_select )
		nreg = dimsizes(region_select@regions)
	else
		nreg = 1
	end if

	;; create index arrays
	lat_start = new(nreg,integer)
	lat_end   = new(nreg,integer)
	lon_start = new(nreg,integer)
	lon_end   = new(nreg,integer)

	;; create stats storage array
	if ( dimnum .eq. 4 ) ; 3D data
		stat_data = new((/nreg,dimensions(0),dimensions(1),13/),float)
		dimlat = dimensions(2)
		dimlon = dimensions(3)
	else if ( dimnum .eq. 3 ) ; 2D data
		stat_data = new((/nreg,dimensions(0),13/),float)
		dimlat = dimensions(1)
		dimlon = dimensions(2)
	else
		print("don't know how to calculate stats for data with "+dimsizes(dimensions)+" dimensions")
		break
	end if
	end if

	
	
	;; process a list of regions
	if ( region_select )
		
		
		do ireg = 0, nreg - 1
		
			; set region name
			region = region_select@regions(ireg)
	
			; find xy model coordinates corresponding to lon/lat
			if ( isatt(minlon,region) .and. isatt(maxlon,region) \
					.and. isatt(minlat,region) .and. isatt(maxlat,region) )
				locmin = wrf_user_ll_to_ij(FILES[0], minlon@$region$,minlat@$region$, True) 
				locmax = wrf_user_ll_to_ij(FILES[0], maxlon@$region$,maxlat@$region$, True) 
			else
				print("unknown region: "+region+" --- check minlon, minlat, maxlon, maxlat")
			end if

			; store the location information for region corners
			lon_end(ireg) = locmax(0) -1	; xmax
			lon_start(ireg) = locmin(0) -1	; xmin
			lat_end(ireg) = locmax(1) -1	; ymax
			lat_start(ireg) = locmin(1) -1	; ymin

		end do

			
	;; check if we're trimming cells from the edge of the domain - and set the limits for the domain if we are
	else if ( domain_trim )
		lat_start(0) = 0 + domain_trim@trim 
		lat_end(0)   = dimlat - 1 - domain_trim@trim
		lon_start(0) = 0 + domain_trim@trim 
		lon_end(0)   = dimlon - 1 - domain_trim@trim
	;; use all the model data
	else
		lat_start(0) = 0 
		lat_end(0)   = dimlat - 1
		lon_start(0) = 0
		lon_end(0)   = dimlon - 1
	end if
	end if

	
	;; loop through the time dimension, calculating stats for each time slice
	do ireg = 0, nreg-1
	do itime = 0,dimensions(0)-1
	
		if ( dimnum .eq. 4 )
			do ilev = 0,dimensions(1)-1
				stat_data(ireg,itime,ilev,:) = \
						calc_standard_stats(var_temp(itime,ilev,lat_start(ireg):lat_end(ireg),lon_start(ireg):lon_end(ireg)))
			end do
		else if ( dimnum .eq. 3 )
			stat_data(ireg,itime,:) = calc_standard_stats(var_temp(itime,lat_start(ireg):lat_end(ireg),lon_start(ireg):lon_end(ireg)))
		end if
		end if
	
	end do
	end do
	
	;;; copy the units information
	if(isatt(var_temp,"units"))
		stat_data@units = var_temp@units
	end if
	
	;;; copy the description
	if(isatt(var_temp,"description"))
		stat_data@description = var_temp@description
	end if
	

	return(stat_data)
	

end


;
;  Function for creating times strings to use in our stats output files.
;   This uses a function which has been created for setting up plot labels.
;
undef("create_local_time_strings")
function create_local_time_strings(FILES:list,use_local_time:logical,istart:integer,iend:integer)
local time_strings, tempres, times_temp, times, times_a, restick
begin

	;;; temporary resources variable to attach the attribute to
	tempres = True

	;;; get characters of the time information
	times_temp = wrf_user_getvar(FILES,"Times",-1)
	times = times_temp(istart:iend,:)
	;;; convert to integer string
	times_a = wrf_times_c(times,1)
	times_a@units = "hours since 1901-01-01 00:00:00"

	;;; check if we are using local time, and if so change times (and label)
	if(use_local_time)
		times_a = times_a + use_local_time@offset
	end if

	;;; create the time axis labels
	restick = True
	restick@ttmFormat = "(%c-%D) %H"
	restick@ttmNumTicks = dimsizes(times_a)
	time_axis_labels(times_a,tempres,restick)	

	;;; copy the required strings to a new data array
	time_strings = tempres@tmXBLabels

	return(time_strings)

end




;
;  writing the stats data to a text file
;
undef("write_stats_data")
procedure write_stats_data(FILES:list,plot_stats:numeric,var_name:string,\
							output_directory:string,scenario:string,\
							altitude_levels:numeric,use_local_time:logical,\
							region_select:logical)
local nreg, dimensions, dimscount, time_strings, Times, ireg, output_file_name, output_file, \
		file_names, ilev, header_strings, level_limit, col_head_string, columns, cf, stat_order, \
		itim, out_string_array, \
		 OUTFILE, \
		dimNames, dimSizes, dimUnlim, Temp_Var, globalAtt, Alt_Var
begin


	;;; check how many regions we are plotting for
	if(region_select)
		nreg = dimsizes(region_select@regions)
	else ;; just processing a single set of data
		nreg = 1
	end if

	;; determine how many dimensions to input data
	dimensions = dimsizes(plot_stats)
	dimscount = dimsizes(dimensions)

	; get the time strings for our data sets
	time_strings = create_local_time_strings(FILES,use_local_time,0,dimensions(1)-1)
	Times = wrf_user_getvar(FILES,"Times",-1)



	do ireg = 0,nreg-1

		;;; name the output file
		if(region_select)
			output_file_name = var_name+"_"+scenario+"_"+region_select@regions(ireg)+"_domain_averages"
		else
			output_file_name = var_name+"_"+scenario+"_domain_averages"
		end if
		output_file = output_directory+"/"+output_file_name



		if(dimscount .eq. 4) ; dealing with data that has altitude information				

			; create the filenames and header strings
			file_names = new(dimsizes(altitude_levels),string)
			header_strings = new(dimsizes(altitude_levels),string)
			do ilev = 0,dimsizes(altitude_levels)-1
				if(altitude_levels(ilev).eq.0)
					if(region_select)
						file_names(ilev) = var_name+"_ground_level_"+region_select@regions(ireg)+"_domain_stats.txt"
					else
						file_names(ilev) = var_name+"_ground_level_domain_stats.txt"
					end if
					if(isatt(plot_stats,"units"))
						header_strings(ilev) = var_name+" ("+plot_stats@units+") at ground level"
					else
						header_strings(ilev) = var_name+" at ground level"				
					end if
				else
					if(region_select)
						file_names(ilev) = var_name+"_"+flt2string(altitude_levels(ilev))+"m_ASL_"+region_select@regions(ireg)+"_domain_stats.txt"
					else
						file_names(ilev) = var_name+"_"+flt2string(altitude_levels(ilev))+"m_ASL_domain_stats.txt"
					end if
					if(isatt(plot_stats,"units"))
						header_strings(ilev) = var_name+" ("+plot_stats@units+") at "+flt2string(altitude_levels(ilev))+"m altitude (above sea level)"
					else
						header_strings(ilev) = var_name+" at "+flt2string(altitude_levels(ilev))+"m altitude (above sea level)"
					end if
				end if
			end do

			; set the number of levels for the rest of this subroutine
			level_limit = dimsizes(altitude_levels)


		else if(dimscount .eq. 3) ; dealing with single level data

			; create the filenames and header strings
			file_names = new(1,string)
			header_strings = new(1,string)

			if(region_select)
				file_names(0) = var_name+"_"+region_select@regions(ireg)+"_domain_stats.txt"
			else
				file_names(0) = var_name+"_domain_stats.txt"
			end if
			if(isatt(plot_stats,"units"))
				header_strings(0) = var_name+" ("+plot_stats@units+")"
			else
				header_strings(0) = var_name
			end if

			; set the number of levels for the rest of this subroutine
			level_limit = 1

		end if
		end if


		; create the column header string
		col_head_string = "Time, min, x05, x25, median, x75, x95, max, avg, stddev, SE, morans_i, SE_corr, N"
		; set the statistical data order
		stat_order = (/2, 7, 5, 4, 6, 8, 3, 0, 1, 9, 10, 11, 12/)

		; create the column data
		columns := new((/dimsizes(time_strings),level_limit/),string)


		; set the delimiter for the columns
		cf = ", "


		; loop through time and levels, to create strings
		do itim = 0, dimsizes(time_strings)-1
		do ilev = 0, level_limit-1
			columns(itim,ilev) = time_strings(itim)
			; loop through the stats, getting them into the order defined above
			do istat = 0,dimsizes(stat_order)-1
				if(dimscount .eq. 4) ; dealing with data that has altitude information				
					columns(itim,ilev) = columns(itim,ilev) + cf+sprintf("%7.4f",plot_stats(ireg,itim,ilev,stat_order(istat)))
				else if(dimscount .eq. 3) ; dealing with single level data
					columns(itim,ilev) = columns(itim,ilev) + cf+sprintf("%7.4f",plot_stats(ireg,itim,stat_order(istat)))
				end if
				end if
			end do
		end do
		end do


		; loop through the model levels, organise the data strings, and write them out
		do ilev = 0, level_limit-1
			out_string_array = new(2+dimsizes(time_strings),string)
			out_string_array(0) = header_strings(ilev)
			out_string_array(1) = col_head_string
			do istat = 2,dimsizes(out_string_array)-1	    
				out_string_array(istat) = columns(istat-2,ilev)
			end do
			asciiwrite(output_directory+"/"+file_names(ilev),out_string_array)
			delete(out_string_array)
		end do


	end do ; loop through regions to output

end ; write_stats_data






end ;; end of script
