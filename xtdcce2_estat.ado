/*
estat for xtdcce2
Requires xtdcce2 version 1.2
Changelog
12.01.2017 - fixed bug if ts vars used
16.10.2017 - fixed bug in box
*/

*capture program drop xtdcce2_estat
program define xtdcce2_estat , rclass
	syntax anything [if] [in] , [Combine(string asis) Individual(string asis) nomg CLEARGraph ]
	
	marksample touse , nov
	gettoken type vars : anything 
	if "`type'" != "box" & "`type'" != "bar" & "`type'" != "rcap" {
	    display "{err}box or bar must be specified."
        exit 498
    }

	if "`e(cmd)'" != "xtdcce2" {
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
			local graph_vars_mg `e(p_mg_vars)' 
		}
		else {
			local mg `e(p_mg_vars)'
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
				local varname = strtoname("`var'")
				sort `e(idvar)' `e(tvar)'
				graph bar `coeff'_`s_var' if `touse' , over(`idvar', label(nolabels)) name(`varname', replace) `gbar'
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
			foreach var in `graph_vars_mg' {
				local s_var = subinstr("`var'",".","_",.)
				qui gen `s_var'_up = `coeff'_`s_var' + cv * `se'_`s_var' if `touse'
				qui gen `s_var'_lo = `coeff'_`s_var' - cv * `se'_`s_var' if `touse'
				
				if "`nomg'" == "" {
					local mg_mean = _b[`var']
					local mg_up = `mg_mean' + cv * _se[`var']
					local mg_lo = `mg_mean' - cv * _se[`var']
					local ylines "yline(`mg_mean') yline(`mg_up', lp(dash)) yline(`mg_lo' , lp(dash)) "
				}
				if "`cleargraph'" == "" {
					local grcap `"legend(off) `ylines' nodraw  ytitle("") title("`var'") note("Mean: `=string(_b[`var'])'" "SE: `=string(_se[`var'])'")"'
				}				
				
				local varname = strtoname("`var'")
				
				twoway	(scatter `coeff'_`s_var' `idvar' , m(X)  ) /*
						 */ (rcap `s_var'_up `s_var'_lo `idvar' , lp(dash) ) /*
						*/ if `touse' , name(rc`varname', replace) `grcap' `individual'
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
end
