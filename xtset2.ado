*! xtset2 1.0 - January 2018
*! author Jan Ditzen
*! www.jan.ditzen.net - j.ditzen@hw.ac.uk
*! see viewsource xtset2.ado for more info.
/*
xtset2 extends xtset.
*/

version 11.1

capture program drop xtset2
program define xtset2, rclass
	syntax [varlist(numeric default=none)] [if], [* checkvars(varlist) matrix showxtset version]
	if "`version'" != "" {
		display "xtset2, version 1.0"
		return scalar version = 1
	}
	else {
		qui{
		if "`varlist'" == "" {
			_xt
			local varlist "`r(ivar)' `r(tvar)'"
		}
		preserve
			*noi disp "`if'"
			if "`if'" != "" {
				keep `if'
			}
			tokenize `varlist'			
			local idvar "`1'"
			local tvar "`2'"
				
			**Obtain N amd N_g
			local N = _N 
			tempvar N_gtmp N_gtmp2
			egen `N_gtmp' = tag(`idvar') 
			egen `N_gtmp2' = total(`N_gtmp')
			sum `N_gtmp2' , meanonly
			local N_g = r(mean)
			local N = r(N)			
			**Determine type of balance
			if "`checkvars'" == "" {
				local checkvars "`idvar' `tvar'"
			}
			/*
			1) T = Ti (same number of periods)
			2) Tmin = Tmin_i & Tmax = Tmax_i (beginning and end the same)
			3) T(i) = T(j), where T(t)_i = T(t)_j (i and j have same periods)
			4) no gaps
			5) no missings in variables
			for strongly balanced 1,2, 3 and 4 have to hold
			for weakly balanced 1 and 2 or 1 and 3 or 1 and 4 have to hold
			rest unbalanced
			*/
			local Cond1 = 0
			local Cond2 = 0		
			local Cond2Max = 0
			local Cond2Min = 0
			local Cond3 = 0
			local Cond4 = 0
			local Cond5 = 0
			** check 1)
			tempvar Ttot 
			by `idvar', sort: gen `Ttot' = _N
			inspect `Ttot'
			if `r(N_unique)' == 1 {
				local Cond1 = 1
			}
			
			** Check 2)
			tempvar Tmin Tmax 
			by `idvar', sort: egen `Tmax' = max(`tvar')
			by `idvar', sort: egen `Tmin' = min(`tvar')
			inspect `Tmax'
			if `r(N_unique)' == 1 {
				local Cond2Max = 1
			} 
			inspect `Tmin'
			if `r(N_unique)' == 1 {
				local Cond2Min = 1
			} 
			**set dummy
			if `Cond2Max' == 1 & `Cond2Min' == 1 {
				local Cond2 = 1
			}
			
			** Check 3)
			tempname Tmat
			tab `tvar' , matcell(`Tmat')
			svmat `Tmat' , name(`Tmat')
			inspect `Tmat'
			if `r(N_unique)' == 1 {
				local Cond3 = 1
			}
			
			** Check 4),
			** check if gaps
			tempvar gaps
			by `idvar' (`tvar'), sort: gen `gaps' = `tvar' - `tvar'[_n-1]
			by `idvar' (`tvar'), sort: replace `gaps'  = `gaps'[_n+1] if _n == 1			
			inspect `gaps'
			if `r(N_unique)' == 1 {
				local Cond4 = 1
			}
			
			**count gaps
			sum `Tmat' 
			scalar NumGaps = (r(max) - r(mean))*r(N)
			
			** check 5)
			tempvar Obsmissing Obsmissingtotal
			egen `Obsmissing' = rowmiss(`checkvars')
			by `idvar', sort: egen `Obsmissingtotal' = total(`Obsmissing')
			sum `Obsmissingtotal'	 		
			if `r(mean)' == 0 {
				local Cond5 = 1
			}
			**number of missings
			sum  `Obsmissing'
			local NumMissing = r(sum)
			
			**stats for time periods
			tempvar tvarcount
			by `idvar', sort: egen `tvarcount' = count(`tvar')
			sum `tvarcount' 
			scalar meanT = r(mean)
			scalar minT = r(min)
			scalar maxT = r(max)
			
			**Determine which balanced type
			local balanced = "unbalanced"
			local balancedN = 1
			*noi disp "1: `Cond1' , 2: `Cond2' , 3: `Cond3' , 4: `Cond4'"
			if `Cond1' == 1 & `Cond2' == 1 & `Cond3' == 1 & `Cond4' == 1  & `Cond5' == 1  {
				local balanced = "strongly balanced"
				local balancedN = 2
			}
			else if (`Cond1' == 1 & `Cond2' == 1) | (`Cond1' == 1 & `Cond3' == 1) | (`Cond1' == 1 & `Cond4' == 1) {
				local balanced = "weakly balanced"
				local balancedN = 3
			}
			*else if `Cond1' == 1 & `Cond2' == 1 & `Cond3' == 0 {
			*	local balanced = "weakly balanced"
			*	local balancedN = 3
			*}
			
			if "`matrix'" != "" {
				collapse (count) `var'T = `tvar' (min) `tvar'min=`tvar' (max) `tvar'max=`tvar' (mean) `tvar'bar=`tvar' (max) `Obsmissingtotal'miss=`Obsmissingtotal' , by(`idvar')
				mkmat `idvar' `var'T `tvar'min `tvar'max `tvar'bar `Obsmissingtotal'miss , matrix(PanelMatrix)
				matrix colnames PanelMatrix = Panelid T Tmin Tmax Tbar Missings
			}
		restore	
		
		}
		qui capture _xt
		local rc = _rc
		if `rc' != 0 | "`options'" != "" {
			disp "xtset2 output:"
		}
		di as txt _col(8)     "panel variable:  " as res "`idvar' (`balanced')"
		di as txt _col(6)   "number of groups:  " as res "`N_g'"
		di as txt _col(8)     "number missing:  " as res "`NumMissing'"
		di as txt _col(11)     "number gaps:  " as res NumGaps
		di as txt _col(13)  		   "variables:  " as res "`checkvars'"
		if `balancedN' == 1 {
			di ""
			di as txt _col(10) "number of periods"
			di as txt _col(15) "average:  " as res meanT
			di as txt _col(19) "Min:  " as res minT
			di as txt _col(19) "Max:  " as res maxT
		}
		else {
			di as txt _col(5)  "number of periods:  " as res meanT
		}
		
		**check if data is xtset, if not, set it
		if `rc' != 0 | "`options'" != "" {
			disp ""
			disp "xtset output:"
			xtset `idvar' `tvar' , `options'
		}	
		return add
		return local balanced = "`balanced'"
		return scalar N_g = `N_g'
		return scalar balancedN = `balancedN'
		return scalar NumMissing = `NumMissing'
		return scalar NumGaps = NumGaps
		return scalar Tmin = minT
		return scalar Tmean = meanT
		return scalar Tmax = maxT
		return scalar N = `N'
		
		return local panelvar "`idvar'"
		return local timevar  "`tvar'"
		
		if "`matrix'" != "" {
			return matrix PanelMatrix = PanelMatrix
		}
	}
end
