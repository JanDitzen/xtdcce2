/*
estat for xtdcce2
Requires xtdcce2 version 1.2
Changelog
20.02.2023 - added option dropzero
12.01.2017 - fixed bug if ts vars used
16.10.2017 - fixed bug in box
24.07.2020 - added options ebi and ebistructure
*/

*capture program drop xtdcce2_estat
program define xtdcce2_estat , rclass
	syntax anything [if] [in] , [Combine(string asis) Individual(string asis) nomg CLEARGraph fmt(string) dropzero * ]
	
	marksample touse , nov

	if "`anything'" == "ebi" {
		if "`e(cmd)'" == "xtdcce2" {
			
			if "`fmt'" == "" {
				local fmt "%9.3f"
			}
			
			local mg_vars `e(lr_mg)' `e(p_mg_vars)' `e(p_mg_vars)'
			local mg_vars: list uniq mg_vars
					
			local pooled_vars `e(p_pooled_vars)' `e(lr_pooled)'
			local pooled_vars: list uniq pooled_vars
			
			local cnt = wordcount("`mg_vars' `pooled_vars'")
			
			tempname blockmat
			matrix `blockmat' = J(`e(N_g)',`cnt',.)
			tempname ebb 
			matrix `ebb' = e(bi)

			tempname vali
			qui levelsof `e(idvar)' if e(sample) , clean
			local idi "`r(levels)'"
			local si = 0
			foreach s in `idi' {
				local ki = 1
				local si = `si' + 1
				foreach var in `mg_vars'  {
					cap matrix `vali' = `ebb'[1,"`var'_`s'"]
					if _rc == 0 matrix `blockmat'[`si',`ki'] = `vali'
					else matrix `blockmat'[`si',`ki'] = .
					local ki = `ki' + 1
				}
				foreach var in `pooled_vars' {
					cap matrix `vali' = `ebb'[1,"`var'"]
					if _rc == 0 matrix `blockmat'[`si',`ki'] = `vali'
					else matrix `blockmat'[`si',`ki'] = .
					local ki = `ki' + 1
				}
			}
			matrix colnames `blockmat' = `mg_vars' `pooled_vars'
			
			matrix rownames `blockmat' = `idi'
			
			** output
			noi disp "Individual Coefficients from e(bi)"
			matrix list `blockmat' , f("`fmt'") noheader noblank
			
			return matrix blockmat = `blockmat'		
		}
		else if "`e(cmd)'" == "xtdcce2fast" {
			mata xtdcce2fast_bi
		}
		else {
			noi disp "ebi requires xtdcce2 or xtdcce2fast."
			noi disp "Last command was `e(cmd)'"
		}
				
	}
	else if regexm("`anything'","ebis" ) {
		if "`e(cmd)'" != "xtdcce2" {
			noi disp "ebistructure only possible after xtdcce2."
			noi disp "Last command was `e(cmd)'"
			exit
		}
		display as text "Coefficient Structure from e(bi)"
				*** Output Table
				** get length of variables
				local abname = 14
				local mg_vars `e(lr_mg)' `e(p_mg_vars) '
				local mg_vars: list uniq mg_vars
				
				local pooled_vars `e(p_pooled_vars)' `e(lr_pooled)'
				local pooled_vars: list uniq pooled_vars
				
				local tbl_interval  7
				local j = 1
				foreach var in `mg_vars' `pooled_vars' {
					local tmp = 2 + length(abbrev("`var'",`abname')) + `=word("`tbl_interval'",`j')'
					local tbl_interval `tbl_interval'  `tmp'
					local j = `j' + 1
				}
				local j = `j' - 1			
				*** Output Header
				disp as text _column(3) "CSU" , _c 
				local i = 1
				foreach var in `mg_vars' `pooled_vars'  {
					local cols = word("`tbl_interval'",`i')
					if `i' < `j' {
						local cont "_c"
					}
					else {
						local cont ""
					}
					disp as text _column(`cols') abbrev("`var'",`abname'), `cont'
					local i = `i' + 1
				}
								
				tempname eebb 
				matrix `eebb' = e(bi)
				qui levelsof `e(idvar)' if e(sample) , clean
				local idi "`r(levels)'"
				foreach s in `idi' {
				    local ii = 1
					disp as text _column(4) "`s'" , _c
					
					foreach var in `mg_vars'  {
					  *  noi disp "`var'"
						local cols_start = word("`tbl_interval'",`ii')
						local cols_end = word("`tbl_interval'",`=`ii'+1')
						
						scalar val = `eebb'[1,"`var'_`s'"]
						
						local vali "1"
						if val == 0 {
							local vali  "0"
						}
						else if val == . {
							  local vali "x"
						}
						
						if `=`i'-1' > `ii' {
							local cont "_c"
						}
						else {
							local cont = ""
						}
						
						
						local cols = round(`cols_start' + (`cols_end'-`cols_start')/2)
						*noi disp "`cols' - `cont' `ii'"
						disp as text _col(`cols') "`vali'" , `cont'
						local ii = `ii' + 1
					}
						
					foreach var in `pooled_vars' {
						local cols_start = word("`tbl_interval'",`ii')
						local cols_end = word("`tbl_interval'",`=`ii'+1')
						
						scalar val = `eebb'[1,"`var'"]
						
						local vali "p"
						if val == 0 {
							local vali  "`vali'/0"
						}
						else if val == . {
							local vali "`vali'/x"
						}
						
						if `=`i'-1' > `ii' {
							local cont "_c"
						}
						else {
							local cont = ""
						}
						
						local cols = round(`cols_start' + (`cols_end'-`cols_start')/2)
						disp as text _col(`cols') "`vali'" , `cont'
						local ii = `ii' + 1						
						
					}
						
				}
						
				disp "where CSU is the number of the cross section."
				disp "1 implies coefficient is estimated."
				disp "0 implies coefficient is zero."
				disp "p implies coefficient is pooled."
				disp "x implies coefficient is omitted (missing)."
			
	}
	else if "`anything'" == "bootstrap" {
		bootstrap_xtdcce2 , `options'
	}
	else {
		
		gettoken type vars : anything 
		if "`type'" != "box" & "`type'" != "bar" & "`type'" != "rcap" {
			display "{err}box or bar must be specified."
			exit 498
		}

		if "`e(cmd)'" != "xtdcce2" & "`e(cmd)'" != "xtdcce2fast" {
			display as error "Only after xtdcce2, last command is `e(cmd)'"
			exit
		}
		qui xtdcce2, version
		if `e(version)' < 1.2 {
			display as error "estat requires version 1.2 or higher"
			display as error "To update, from within Stata type " _c
			display as smcl	"{stata ssc install xtdcce2, replace :ssc install xtdcce2, replace}"
			exit
		}
		
		tempvar coeff
		local idvar `e(idvar)'
		preserve
			sort `e(idvar)' `e(tvar)'
			predict `coeff' if `touse' , coeff
			if "`vars'" == "" {
				local graph_vars_pooled `e(p_pooled_vars)' 
				local graph_vars_mg `e(p_mg_vars)' `e(p_lr_vars_mg)' 
			}
			else {
				local mg `e(p_mg_vars)' `e(p_lr_vars_mg)' 
				local graph_vars_mg: list anything & mg
				local pooled `e(p_pooled_vars)'
				local graph_vars_pooled : list anything & pooled
			}
			if "`type'" == "bar" {
				foreach var in `graph_vars_mg' {
					local s_var = subinstr("`var'",".","_",.)
					if "`nomg'" == "" {
						local mg_mean = _b[`var']
						local mg_up = `mg_mean' + cv * _se[`var']
						local mg_lo = `mg_mean' - cv * _se[`var']
						local ylines "yline(`mg_mean') yline(`mg_up', lp(dash)) yline(`mg_lo' , lp(dash)) "
					}				
					if "`cleargraph'" == "" {
						local gbar `"`ylines' ytitle("")   nodraw title("`var'") note("Mean: `=string(_b[`var'])'" "SE: `=string(_se[`var'])'") `individual' "'
					}
					if "`dropzero'" != "" local dropzeroc & `coeff'_`s_var' != 0
					local varname = strtoname("`var'")
					sort `e(idvar)' `e(tvar)'
					graph bar `coeff'_`s_var' if `touse' `dropzeroc' , over(`idvar', label(nolabels)) name(`varname', replace) `gbar'
					local graph_list `graph_list' `varname'
				}
				if "`cleargraph'" == "" {
					local cgbar `"title("Mean Group Variable") name(xtdcce2_combine, replace) "'
				}
				graph combine `graph_list' , `combine' `cgbar'
			}
			if "`type'" == "box" {
				if "`cleargraph'" == "" {
					local gbox `"title("Mean Group Variables") name(xtdcce2_combine, replace)"'
				}
				foreach var in `graph_vars_mg'  {
					capture drop `var'
					if "`var'" == "_cons" {
						rename `coeff'__cons constant
						local box_vars `box_vars' constant
					}
					else {
						local var = subinstr("`var'",".","_",.)
						rename `coeff'_`var' `var'
						local box_vars `box_vars' `var'
					}
				}
				graph box `box_vars' if `touse'  , `gbox'  
			}
			if "`type'" == "rcap" {
				tempname se coeff
				scalar cv = invnorm(1 - ((100-`c(level)')/100)/2)
				predict `se' if `touse'  , se
				predict `coeff' if `touse' , coeff
				*noi sum `coeff'* `se'*
				*noi disp "vars: `graph_vars_mg' - `anything' - `mg'"
				foreach var in `graph_vars_mg' {
					local s_var = subinstr("`var'",".","_",.)
					qui gen `s_var'_up = `coeff'_`s_var' + cv * `se'_`s_var' if `touse'
					qui gen `s_var'_lo = `coeff'_`s_var' - cv * `se'_`s_var' if `touse'
					*noi sum `s_var'_up `s_var'_lo
					if "`nomg'" == "" {
						local mg_mean = _b[`var']
						local mg_up = `mg_mean' + cv * _se[`var']
						local mg_lo = `mg_mean' - cv * _se[`var']
						local ylines "yline(`mg_mean') yline(`mg_up', lp(dash)) yline(`mg_lo' , lp(dash)) "
					}
					if "`cleargraph'" == "" {
						local grcap `"legend(off) `ylines' nodraw  ytitle("") title("`var'") note("Mean: `=string(_b[`var'])'" "SE: `=string(_se[`var'])'")"'
					}				
					if "`dropzero'" != "" local dropzeroc & `coeff'_`s_var' != 0
					local varname = strtoname("`var'")
					
					twoway	(scatter `coeff'_`s_var' `idvar' , m(X)  ) /*
							 */ (rcap `s_var'_up `s_var'_lo `idvar' , lp(dash) ) /*
							*/ if `touse' `dropzeroc' , name(rc`varname', replace) `grcap' `individual'
					local graph_list `graph_list' rc`varname' 

				}
				if "`cleargraph'" == "" {
					local cgrcap `"title("Mean Group Variables") note("Point estimates are indicated by a cross, mean group estimates by the red line and " "the `c(level)'% confidence interval by the upper and lower range and dashed red line.", size(tiny )) name(xtdcce2_combine, replace)"'
				}
				graph combine `graph_list' , `combine' `cgrcap'

			}
			
			if "`cleargraph'" == "" {
				display as text "Combined graph saved as " as error "xtdcce2_combine" as text "."
				return local graph_name "xtdcce2_combine"
			}
			
		restore
	}
end
