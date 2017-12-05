;
;  Statistical Functions  (from WRFCHEM_misc.ncl)
;



load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"     ; include standard resources
load "$NCARG_ROOT/lib/ncarg/nclscripts/wrf/WRFUserARW.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/contributed.ncl"

begin





;--------------------------------------------------------------------------------

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Functions for calculating statistics from data							;;;;;
;--------------------------------------------------------------------------------

; rm_missing_1d - remove missing values from a 1D array and resize array
undef("rm_missing_1d") 
function rm_missing_1d(data[*]:numeric)
local ii
begin
	ii = ind(.not.ismissing(data))
	oneD_data_processed = data(ii)
	return(oneD_data_processed)
end

;--------------------------------------------------------------------------------

; Calculates weighted mean.
; Requires two input arrays - must be of the same size (or will crash)
; Based on function:
; weighted_mean = sum(x_i*w_i)/sum(w_i)

undef("calc_wght_mean")
function calc_wght_mean(data:numeric, weight:numeric)
local Sigma_data, Sigma_wght, weighted_data
begin
	; WARNING: input array data must be of same size as Weight array

	Sigma_wght = sum(weight)

	weighted_data = data * weight
	Sigma_data = sum(weighted_data)

	wght_mean = (Sigma_data)/Sigma_wght

	return(wght_mean)

end

;--------------------------------------------------------------------------------

; Calculates median by sorting data into 1D array
undef("calc_median")
function calc_median(data:numeric)
local oneD_data 
begin
	oneD_data = ndtooned(data)		; put data into 1 dimension
	qsort(oneD_data)	; sort data - ascending order
	median = dim_median(oneD_data)

	return(median)
end

;--------------------------------------------------------------------------------
; Calculates lower quartile by sorting data into 1D array
undef("calc_lower_quartile")
function calc_lower_quartile(data:numeric)
local oneD_data, numel, pt_x25, oneD_data_in, ii
begin
	oneD_data_in = ndtooned(data)		; put data into 1 dimension

	ii = ind(.not.ismissing(oneD_data_in))  ; find locations of non-missing values
	oneD_data = oneD_data_in(ii)            ; new array containing no missing values


	qsort(oneD_data)	; sort data - ascending order
	numel = dimsizes(oneD_data)	; find number of elements
		
	; find point of 25th percentile
	pt_x25 = round(.25*(numel-1),3)
	lower_quart = oneD_data(pt_x25)
	
	return(lower_quart)
end
	
;--------------------------------------------------------------------------------
; similar for upper quartile
undef("calc_upper_quartile")
function calc_upper_quartile(data:numeric)
local oneD_data, sorted_data, numel, pt_x75, oneD_data_in, ii
begin
	oneD_data_in = ndtooned(data)		; put data into 1 dimension

	ii = ind(.not.ismissing(oneD_data_in))  ; find locations of non-missing values
	oneD_data = oneD_data_in(ii)            ; new array containing no missing values

	qsort(oneD_data)	; sort data - ascending order
	numel = dimsizes(oneD_data)	; find number of elements
		
	; find point of 75th percentile
	pt_x75 = round(.75*(numel-1),3)
	upper_quart = oneD_data(pt_x75)
	
	return(upper_quart)
end

;--------------------------------------------------------------------------------
; calculation for given percentile
undef("calc_percentile")
function calc_percentile(data:numeric, perc_point:numeric)
local oneD_data, sorted_data, numel, pt_x, oneD_data_in, ii
begin
	oneD_data_in = ndtooned(data)		; put data into 1 dimension

	ii = ind(.not.ismissing(oneD_data_in))  ; find locations of non-missing values
	oneD_data = oneD_data_in(ii)            ; new array containing no missing values

	qsort(oneD_data)				; sort data - ascending order
	numel = dimsizes(oneD_data)		; find number of elements
	
	; find point of percentile - perc_point must be between 0 and 100
	pt_x = round(.01*perc_point*(numel-1),3)
	percentile = oneD_data(pt_x)
	
	return(percentile)
end

;--------------------------------------------------------------------------------
; calc_boxplot_stats: combination of above functions
; calculates, 5th, 25th , median, 75th and 95th percentiles. 
; Returns them an array of size 5:
; 	stats(0) = 5th percentile
; 	stats(1) = lower quartile
;	stats(2) = median
;	stats(3) = upper quartile
;	stats(4) = 95th percentile

undef("calc_boxplot_stats")
function calc_boxplot_stats(data:numeric)
local oneD_data, numel, perc_point, pt_x, oneD_data_in, ii, j
begin

	ndims = dimsizes(dimsizes(data))
	if (ndims .gt. 1)
		oneD_data_in = ndtooned(data) 
	else
		oneD_data_in = data 
	end if
	
	; remove missing values
	if (any(ismissing(oneD_data_in)))
		ii = ind(.not. ismissing(oneD_data_in))  ; find locations of non-missing values
		oneD_data = oneD_data_in(ii)            ; new array containing no missing values
	else
		oneD_data = oneD_data_in 
	end if

	qsort(oneD_data)				; sort data - ascending order
	numel = dimsizes(oneD_data)		; find number of elements
	
	; declare stats array, of type data, to hold values
	stats = new(5, typeof(data))
	
	perc_point = (/ 5., 25., 50., 75., 95. /)
	
	do j = 0, 4
	
		; find point of percentile - perc_point must be between 0 and 100
		pt_x = round(.01*perc_point(j)*(numel-1),3)
		
		; quick correction, in case of negative pt_x
		if (pt_x .lt. 0)
			print("Catching error in stats calc due to small sample size")
			pt_x = 0
		end if
		
		stats(j) = oneD_data(pt_x)

	end do
	
	return(stats)
end



; https://en.wikipedia.org/wiki/Moran%27s_I
; WARNING: this function can run very slowly if input matrix is large.
undef("calc_morans_i_2D")
function calc_morans_i_2D(data[*][*]:numeric, wt_opts:logical, max_loop:integer) 
local X_mean, X_sttdev, X_diff, dims_sub, n_j, n_i, i, j, i_ind, j_ind, i_ind_1D, \
	m, m_arr, m_i, M_max, j_ind_1D, X_diff_1D, N_tot, N_span, dist_arr, w_arr,    \
	Auto_arr, W_sum, Auto_sum \
begin


	; Check if options have been set in wt_opts, otherwise use defaults
	if (isatt(wt_opts,"neighbour"))
		neighbour = wt_opts@neighbour
		
		if (neighbour)
			order = 1
		else
			if (isatt(wt_opts,"order"))
				order = wt_opts@order
				if (order .ne. 1 .and. order .ne. 2)
					print("calc_morans_i: WARNING - order is recommended to be 1 or 2")
				end if
			else
				order = 1
			end if		
			
			if (isatt(wt_opts,"exp_opt"))
				exp_opt = wt_opts@order
			else
				exp_opt = False
			end if		
		end if
	else
		
		print("calc_morans_i: neighbour option not defined, setting default = true")
		neighbour = True
	end if

	if (isatt(wt_opts,"manhattan"))
		manhattan = wt_opts@manhattan
	else
		manhattan = False
	end if
	

	; Fist get stats, mean and sttdev
	X_mean   = avg(data)
	X_sttdev = stddev(data)
	X_diff   = data-X_mean
	
	if (all(X_diff .eq. 0.0))
		Morans_I = 0.0
		return(Morans_I)
	end if
	
	dims_sub = dimsizes(data)
	n_j = dims_sub(0)
	n_i = dims_sub(1)

	; Create 2D arrays of the i and j indices.
	i_ind = new((/n_j, n_i/), float)
	j_ind = new((/n_j, n_i/), float)
	
	do i = 0, n_i-1
		i_ind(:,i) = int2flt(i)
	end do
	
	do j = 0, n_j-1
		j_ind(j,:) = int2flt(j)
	end do

	; Sort everything into 1D arrays
	i_ind_1D = ndtooned(i_ind)
	j_ind_1D = ndtooned(j_ind)
	X_diff_1D = ndtooned(X_diff)
	
	N_tot = dimsizes(X_diff_1D)
	N_span = ispan(0,N_tot-1,1)

	
	if (neighbour)
		; if in Neighbour mode, we explicitly define only those displacements
		; which result in neighbouring combinations (8 per cell)
		m_arr = (/ 1,          \
		           n_i-1,       n_i,       n_i+1, \
		           N_tot-n_i-1, N_tot-n_i, N_tot-n_i+1, \
		           N_tot-1 /)
		M_max = dimsizes(m_arr)-1
	
	else
		if (max_loop .le. 0)
			M_max = N_tot-1
		else
			M_max = min((/N_tot-1, max_loop/))
		end if
	
		if (M_max .le. 2*max((/n_i,n_j/)) )
			print("calc_morans_i: WARNING - Max loop should be > 2*n_i to give accurate results")
		end if
	end if
	
	; Note using doubles because we may generate v. large numbers
	dist_arr = new(N_tot, double)
	dist_arr(:) = 0
	w_arr = new(N_tot, double)
	w_arr(:) = 0
	Auto_arr = new(N_tot, double)
	Auto_arr(:) = 0
	
	W_sum = new(1, double)
	W_sum = 0.0
	Auto_sum = new(1, double)
	Auto_sum = 0.0


	do m_i = 0, M_max 
	
		if (neighbour)
			m = m_arr(m_i)
		else
			m = m_i
		end if
		
		if (m .eq. 0)
			continue
		end if
		
		; Modulate N_span by m
		N_mod = mod(N_span+m, N_tot)
		
		; Calculate distance across range
		if (manhattan)
			; Manhattan mode: calculate distance in right angle moves
			dist_arr =  sqrt((i_ind_1D(N_span)-i_ind_1D(N_mod))^2) + \
						sqrt((j_ind_1D(N_span)-j_ind_1D(N_mod))^2)
		else
			; Otherwise distance calculate "as crow flies"
			dist_arr = sqrt( (i_ind_1D(N_span)-(i_ind_1D(N_mod)))^2 + \
							 (j_ind_1D(N_span)-(j_ind_1D(N_mod)))^2 )
		end if
	
		; if neighbour mode is on, weight = 1, if cells touching, else = 0
		; If in manhattan mode as well, diagonals are not included
		if (neighbour)
			w_arr = where(dist_arr .lt. 2 .and. dist_arr .gt. 0, 1, 0)
		else
			; weight can be exponential or inverse of distance
			; weight = 0  if dist = 0
			if(exp_opt)
				w_arr = where(dist_arr .eq. 0, 0, exp(-order*dist_arr))
			else
				w_arr = where(dist_arr .eq. 0, 0, (1/dist_arr^order) )
			end if
		end if
		
		if (sum(w_arr) .lt. 1e-2)
			dist_arr(:) = 0
			; print("calc_morans_i: no significant weight, skipping m = "+m)
			continue
		end if
		
		; Calculate the autocorrelation
		Auto_arr(N_span) = w_arr(N_span)*X_diff_1D(N_span)*X_diff_1D(N_mod)
		
		; sum and save values
		Auto_sum = Auto_sum + sum(Auto_arr)
		W_sum = W_sum + sum(w_arr)
		
		; reset to 0
		dist_arr(:) = 0
		w_arr(:) = 0
		Auto_arr(:) = 0
		
	end do
 	
	Morans_I = tofloat((Auto_sum)/(W_sum*X_sttdev^2))
	return(Morans_I)

end



;-------------------------------------------------------------------
; Calc all stats:
; calculates, standard stats for domain averages5th, 25th , median, 75th and 95th percentiles. 
; Returns them an array of size 9:
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

; Input is a single array of data. The data may be multi- or single-dimensional. 

undef("calc_standard_stats")
function calc_standard_stats(data:numeric)
local wt_opts, oneD_data, ndims, numel, oneD_data_in, ii, boxplot_stats
begin

	;;; control options for Moran's i calculations
	wt_opts = True
	wt_opts@neighbour = True
	wt_opts@manhattan = True


	ndims = dimsizes(dimsizes(data))
	if (ndims .gt. 1)
		oneD_data_in = ndtooned(data)
	else
		oneD_data_in = data 
	end if

	; declare stats array, of type data, to hold values
	stats = new(13, typeof(data), -9999)

	; remove missing values
	if (any(ismissing(oneD_data_in)))
		if (any(.not.ismissing(oneD_data_in)))
			ii = ind(.not. ismissing(oneD_data_in))  ; find locations of non-missing values
			oneD_data = oneD_data_in(ii)            ; new array containing no missing values
		else	; if no data then let's get out of here
			stats = 0.0
			return(stats)
		end if
	else
		oneD_data = oneD_data_in
	end if

	qsort(oneD_data)				; sort data - ascending order
	numel = dimsizes(oneD_data)		; find number of elements
	
	; calculate stats
	stats(0) = avg(oneD_data)    ; Average/mean
	stats(1) = stddev(oneD_data) ; standard deviation
	stats(2) = min(oneD_data)    ; minimum
	stats(3) = max(oneD_data)    ; maximum
	
	; Calculate boxplot stats, percentiles =  (5th, 25th, 50th, 75th, 95th) 
	boxplot_stats = calc_boxplot_stats(oneD_data)
	
	stats(4) = boxplot_stats(2)  ; median
	stats(5) = boxplot_stats(1)  ; lower quartile
	stats(6) = boxplot_stats(3)  ; upper quartile
	stats(7) = boxplot_stats(0)  ; 5th percentile
	stats(8) = boxplot_stats(4)  ; 95th percentile
	stats(9) = stats(1) / sqrt(numel) ; standard error
	
	
	if(ndims .eq. 2)
		stats(10) = calc_morans_i_2D(data, wt_opts, -1) ; Moran's I estimate of autocorrelation
		stats(11) = stats(9) * stats(10) ; corrected standard error
	else
		print("Moran's i can only be calculated for 2D data - skipping this step")
		stats(10) = -9999
		stats(11) = -9999
	end if
	
	stats(12) = numel
	
	return(stats)
end

;-------------------------------------------------------------------

; !!!WARNING: the geometric stat calculations run the risk of dividing by 0,
; Caution should be used run running with.
; calculate geometric mean
; mu_g = (A_1*A_2*...*A_N)^(1/N) = exp( SUM(ln(A_i))/N )

undef("calc_geom_mean")
function calc_geom_mean(data:numeric)
local oneD_data, numel, log_dat_sq
begin

	sum_log = sum(log(data))		; sum log of data
	oneD_data = ndtooned(data)		; put data into 1 dimension
	numel = dimsizes(oneD_data)	; find number of elements

	mu_g = exp(sum_log/numel)
	return(mu_g)
	
end

; Calculate geometric standard deviation from array of data (sigma_g)
; sigma_g = exp(sqrt(SIGMA_i{ln(A_i/mu_g)}^2/N))

undef("calc_geom_stddev")
function calc_geom_stddev(data:numeric)
local mu_g, oneD_data, numel, log_dat_sq
begin
	mu_g = calc_geom_mean(data)			; calculate average
	oneD_data = ndtooned(data)
	numel = dimsizes(oneD_data)	; Find N
	
	if (mu_g .eq. 0.0)
		sigma_g = 0
	else
	
		log_dat_sq = log(data/mu_g) ^ 2 ; calculate bit that goes inside sum	
		sigma_g = exp( sqrt( sum(log_dat_sq)/numel) )
	end if
	
	return(sigma_g)
end

; Calculate geometric standard deviation from array of data (sigma_g)
; weighted by air mass. Needs air mass (AM) in same dimensions as data
; sigma_g = exp(sqrt(SIGMA_i{ln[weight*(A_i/mu_g)^2]/total_weighting))
; SAN - is this really meaningful?... - need to revise...

;undef("calc_geom_stddev_wt")
;function calc_geom_stddev_wt(data:numeric,AM:numeric)
;local mu_g, log_dat_sq,  tot_AM
;begin
;	tot_AM = sum(AM)			; find total air mass
;	mu_g = sum(data*AM)/tot_AM			; calculate weighted average
;	
;	log_dat_sq = log(data/mu_g) ^ 2 	; calculate bit that goes inside sum
;	
;	sigma_g_wt = exp( sqrt( sum(log_dat_sq*AM)/tot_AM) )
;	return(sigma_g_wt)
;end

;--------------------------------------------------------------------------------






end