*! xtcd2 2.1 10Jun2019
*! author Jan Ditzen
*! see viewsource xtcd2.ado for more info.

/* 
Jan Ditzen - jd219@hw.ac.uk

xtcd2 performs the CD test as poposed by Chudik and Pesaran (2013). Balanced as well as unbalanced panels are supported.
xtcd2 is a postestimation command, a sample has to be marked by e(sample) in advance. 
Syntax:
xtcd2 residuals [,KDENsity name(string) rho NOESTimation]
, where residuals are saved residuals from an estimation which are tested to be cross sectional independent. 
H0: cross sectional independence / weak cross sectional dependence (E(u_it u_jt) = 0)
The value of the CD statistic is saved in r(CD) and it's p-value in r(p).
Options:
	KDENsity: plots a kernal density graph of the cross correlations together with some descriptive statistics.
	name(name): if specified kernal density graph is saved under "name", histogram not drawn.
	rho: saves the matrix with the cross correlations in r(rho).
	NOESTimation: If command performed on residuals not after estimation.
	
Literature References: 
					Pesaran, M. H. 2015. Testing Weak Cross-Sectional Dependence in Large Panels.
						Econometric Reviews 34(6-10): 1089-1117.
							
Changelog:
	05.03.2015 Option NOESTimation added
	09.03.2015 Error in r(rho) corrected.
	20.01.2016 Mata matrices are being dropped now
	06.03.2016 Mata function RHO added
	23.06.2016 Error in e(sample) fixed.
	09.01.2017 Error in histogram fixed
	03.03.2017 kdensity instead of histogram
	10.03.2017 Changed output of CD and p-value
	01.06.2017 Added version function.
	31.10.2017 Cross sectional into cross-sectional renamed
	05.06.2019 Added check which command used before
	10.06.2019 Uses xtset2 to detect type of panel rather than xtset.
*/
cap program drop xtcd2
program define xtcd2, rclass
	syntax [varlist(default=none max=1)] [if] [, KDENsity name(string) rho NOESTimation VERsion]
	
	version 10
	
	if "`version'" != "" {
			di in gr "Version 1.21"
			*ereturn clear
			ereturn local version 1.21
			exit	
	}
	
	tempvar id_n time_new 
	
	display "Pesaran (2015) test for weak cross-sectional dependence."
	preserve
		if "`if'" != "" {
			qui keep `if'
		}
		
		** Check which estimation command
		if "`noestimation'" == ""  & "`varlist'" == "" {
			*** xtdcce2
			if regexm("`e(cmd)'","xtdcce2") == 1 {
				local restype residuals
				disp in smcl "Residuals calculated using {it: predict, residuals} from {it:xtdcce2}."
			}
			else if regexm("`e(cmd)'","xtreg") == 1 {
				local restype e
				disp in smcl "Residuals calculated using {it: predict, e} from {it:`e(cmd)'}."
			}
			else {
				local restype residuals
				disp in smcl "Residuals calculated using {it: predict, residuals}."
			}
		}
		
		**Check if estimation
		if "`noestimation'" == "" {
			** Drop observations not in estimation

			if "`varlist'" == "" {
				tempname varlist
				
				predict `varlist'  , `restype'
				
			}
			qui keep if e(sample) & `varlist' != .
		}
		else if "`noestimation'" != "" {
			*display "No Postestimation - missing values of variable `varlist' dropped." , _continue
			qui keep if `varlist' != .
		}
		**Test if sample exists
		qui sum `varlist'
		if "`r(N)'" == "0" {

			display in red "Error: no sample set"
			exit
		}
		** Determine N, T and type of panel
		qui tsset 
		local id "`r(panelvar)'" 
		local timevar "`r(timevar)'"
		local balanced  "`r(balanced)'"
		sort `id' `timevar'
		
		egen `id_n' = group(`id')
		egen `time_new' = group(`timevar')
		
		if  "`balanced'" != "strongly balanced" {
			local balanced = 0
			**Fill Panel such that panel has entries for any year and id
			tsfill, full			
			display  "Unbalanced panel detected, test adjusted."
			}
		if "`balanced'" == "strongly balanced" {
			local balanced = 1
		}
		cap qui xtset2 
		if _rc != 0 {
			noi disp "Please install xtset2 from xtdcce2 package."
			noi disp "Panel information might be incorrect."
			sum `id_n'
			local N = r(max)
			sum `time_new'
			local T = r(max)
		}
		else {
			local T = r(Tmax)
			local N = r(N_g)
		}
		qui putmata r= `varlist'  , replace
		mata: r = colshape(r,`T')'
		mata: RHO = xtcd2_make_rho(r,`N',`T',`balanced')
		mata: CD = sqrt(2/(`N'*(`N'-1)))*sum(RHO) // equation 62 and 69 in Chudik, Pesaran (2013) - note: the sqrt(T) from 62 is missing and moved to calculations above
		mata: st_numscalar("CD", CD)
		scalar p_value = 2*(1-normal(abs(CD)))
		disp ""
		display "H0: errors are weakly cross-sectional dependent." , 
		return scalar p = p_value
		return scalar CD = CD
		
		display _col(9) "CD = " _col(14) in gr %-9.3f CD
		display _col(4) "p-value = " _col(14) in gr %-9.3f p_value
		
		if "`kdensity'" == "kdensity" {
			mata: rho_all = colshape(RHO,1) / sqrt(2*`T')
			drop _all
			getmata rho_all, replace
			qui sum rho_all, detail
			foreach s in mean min max p25 p50 p75 {
				local s`s' : di %9.3f `r(`s')'
				local s`s' = trim("`s`s''") 
			}
			local sN: di %9.0f `r(N)'
			local sN = trim("`sN'")
			local cd: di %9.3f  CD
			local cd = trim("`cd'")
			local pval: di %9.3f  p_value
			local pval = trim("`pval'")
			if "`name'" != "" {
				local graphname name(`name' , replace) nodraw
			}
			qui kdensity rho_all, xtitle({&rho}{sub:ij}) title(Cross-Sectional Correlations) `graphname' ///
				note("{bf:Statistics:} CD = `cd', p-value: `pval'" "Obs: `sN', Mean: `smean'" "Min: `smin', Max: `smax'" "Percentiles:" "25%: `sp25' , 50%: `sp50', 75:% `sp75'")  
		}		
		if "`rho'" == "rho" {
			mata: RHO_output = RHO / sqrt(2*`T')
			mata: st_matrix("rho",RHO_output)
			return matrix rho = rho
		}
		foreach s in r i j sumij sqsumi_2 sqsumj_2 RHO nonmissing T_nonmissing RHO_output CD {
			capture mata mata drop `s'
		}
	restore
end


capture mata mata drop xtcd2_make_rho()
mata:
	function xtcd2_make_rho (real matrix r,
							real scalar N,
							real scalar T,
							real scalar balanced )
	{
		RHO = J(N,N,.)
		maxi = N - 1
		for (i=1; i<=maxi; i++) {
			minj = i + 1
			for (j = minj; j<=N; j++) {
				if (i<j) {
					if (balanced == 1) {
						ri = r[,i]
						rj = r[,j]
						//ri = r[i,]'
						//rj = r[j,]'
						sumij = ri'rj
						sqsumi_2 = sqrt(ri'ri)
						sqsumj_2 = sqrt(rj'rj)
						//from p. 34 of Chudik, Pesaran (2013), sqrt added to use same CD equation at the end for balanced and unbalanced data
						RHO[i,j] =sumij /(sqsumi_2*sqsumj_2)*sqrt(T) 
						///ss = quadcorrelation((ri,rj))[2,1]*sqrt(T)
						///RHO[i,j] = ss
					
					}
					if (balanced == 0) {
						// Create dummy vector which contains nonmissings
						nonmissing = rownonmissing(r[,i]):*rownonmissing(r[,j])
						T_nonmissing = sum(nonmissing)
						// Clean Data, i.e. correct missing values into zeros
						ri = editmissing(r[,i],0)
						rj = editmissing(r[,j],0)
						ub_i = ri'*nonmissing / T_nonmissing
						ub_j = rj'*nonmissing / T_nonmissing
						u_i = ri :- ub_i
						u_j = rj :- ub_j
						// from p. 42 of Chudik, Pesaran (2013) - note: sqrt(T) is added here!
						RHO[i,j] =u_i'*u_j /(sqrt(u_i'*u_i)*sqrt(u_j'*u_j))*sqrt(T_nonmissing) 
					}
				}
			}
		}
		return(RHO)	
	}
end
