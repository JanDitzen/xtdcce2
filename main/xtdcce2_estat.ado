/*
estat for xtdcce2
Requires xtdcce2 version 2.7
Changelog
20.02.2023 - added option dropzero
12.01.2017 - fixed bug if ts vars used
16.10.2017 - fixed bug in box
24.07.2020 - added options ebi and ebistructure
10.05.2024 - added ICs 
*/

*capture program drop xtdcce2_estat
program define xtdcce2_estat , rclass
	syntax anything [if] [in] , [Combine(string asis) Individual(string asis) nomg CLEARGraph fmt(string) dropzero lags(string) NOGRAPH axis2(string) Horizontal Ivar(string) zero level(string) * ]
	
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
						
						cap scalar val = `eebb'[1,"`var'_`s'"]
						if _rc != 0 scalar val = .

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
	else if "`anything'" == "ic" {

		local 0 , `options' 
		syntax [anything], [model(string) single SEQuential NOPROGress ]
		
		if "`model'`single'`sequential'" == "" {
			matrix ICs = e(IC1),e(IC2),.,.
			local model1 "`e(csa)'"
			local Min_IC1 = 1
			local Min_IC2 = 1
			local Min_PC1 = 0
			local Min_PC2 = 0
		}
		else {
			
			
			if regexm("`model'","\(") {
				
				gettoken one two : model , bind
				local model1 = subinstr(subinstr("`one'",")","",.),"(","",.)
				local i = 1
				while "`two'" != "" {
					gettoken one two : two , bind
					local i = `i'+1
					local model`i' = subinstr(subinstr("`one'",")","",.),"(","",.)
					
				}
				local NModel = `i'
				local MainModel = 1
			}
			else {
				if "`single'" != "" {
					local model1 "`model'"
					local NModel = 1
				}
				else {
					if "`model'" == "" local model "`e(csa)'"
					tuples `model'
					local NModel = `ntuples'
					forvalues i=1(1)`ntuples' {
						local j = `ntuples'-`i'+1
						local model`j' `tuple`j''
					}
					local MainModel = `ntuples'
				}
			}

			/// Running Block
			if "`noprogress'" == "" noi disp "Running `NModel' combinations of cross-section averages:"
				
			local cmd_Main "`e(cmdline)'"

			tempname eps smpl ici res SigmaMF ResultsO
			est sto `ResultsO'
			_xt
			local ivar r(ivar)

			local SigmaMFbar = 0

			matrix  ICs = J(`NModel',4,.)

			forvalues i = 1(1)`NModel' {
				*local j = `NModel'-`i'+1
				local j = `i'
				local 0 `cmd_Main'
				syntax [anything] [if], CRosssectional(string) * [icopt(string)]
				local options_cmd `options'
				
				local 0 `crosssectional'
				syntax anything(name=cr_vars) , *
				local options_cr `options'
				if "`noprogress'" == "" noi disp "." , _c
				local icinit `SigmaMFbar' `=`i'==1'
				
				qui `anything' `if' , `options_cmd' cr(`model`j'' ,`options_cr') icopt(`icinit')
				
				local SigmaMFbar = e(SigmaMF)

				matrix ICs[`i',1] = e(IC1)
				matrix ICs[`i',2] = e(IC2)
				matrix ICs[`i',3] = e(PC1)
				matrix ICs[`i',4] = e(PC1)

				local model_cmd`i' = subinstr("`e(cmdline)'","icopt(`icinit')","",.)
				
			}

			qui est restore `ResultsO'
			mata `res' = st_matrix("ICs")
			mata `res' = colmin(`res'):==`res'
			mata st_local("Min_IC1",strofreal(selectindex(`res'[.,1])))
			mata st_local("Min_IC2",strofreal(selectindex(`res'[.,2])))
			mata st_local("Min_PC1",strofreal(selectindex(`res'[.,3])))
			mata st_local("Min_PC2",strofreal(selectindex(`res'[.,4])))
			
		}
		di as text ""
		disp as text "IC from Margaritella & Westerlund (2023)"
		di as text "{hline 10}{c TT}{hline 70}"
		di as text "  Model  " _col(11) "{c |}" _col(16) "IC1" _col(32) "IC2" _col(46) "PC1" _col(61) "PC2"
		di as text "{hline 10}{c +}{hline 70}"

		local Nrows : rowsof ICs

		forvalues i = 1(1)`Nrows' {
			local star1
			local star2
			local star3
			local star4

			local j = `Nrows'-`i'+1
			local j = `i'
			if `j' == `Min_IC1' local star1 "*"
			if `j' == `Min_IC2' local star2 "*"
			if `j' == `Min_PC1' local star3 "*"
			if `j' == `Min_PC2' local star4 "*"

			
			di as text _col(4) `i' _col(11) "{c |}" _col(12) %9.3g ICs[`j',1] "`star1'" _col(27) %9.3g ICs[`j',2] "`star2'" _col(42) %9.3g ICs[`j',3] "`star3'" _col(55) %9.3g ICs[`j',4] "`star4'"
		}
		di as text "{hline 10}{c BT}{hline 70}"
		
		if `Nrows' > 1 {
			di as text " * indicates minimum."
			di as text ""
			di as text " Cross Section Averages:"
			forvalues i = 1(1)`Nrows' {
				local j = `Nrows'-`i'+1
				local j = `i'
				local textadd
				if `i' == `MainModel' local textadd "(Main Model)"
				dis as smcl "{stata `model_cmd`j'':  Model `i'}: `model`j'' `textadd'" 
			}
			di as text " Click on Model to run in xtdcce2."
		}
		return matrix ICs = ICs 
	
	}
	else if "`anything'" == "crlags" {



		local lags_loop `lags'

		tempname ResultsO
		est sto `ResultsO'


		if "`lags_loop'" == "" local lags_loop = floor(`e(T)'^(1/3))
		numlist "`lags_loop'"
		local lags_loop "`r(numlist)'"
		if wordcount("`lags_loop'") == 1 local lags_loop "-1/`lags_loop'"
		numlist "`lags_loop'"
		local lags_loop "`r(numlist)'"
		local n_loop = wordcount("`lags_loop'")

		local cmd_Main "`e(cmdline)'"
		local 0 `cmd_Main'
		syntax [anything] [if], [NOCROSSsectional CRosssectional(string)] * [cr_lags(string)]
		local options_cmd `options'
				
		local 0 `crosssectional' 
		syntax [anything(name=cr_vars)] , * [cr_lags(string)]
		local cr_opt `options'
		if "`cr_vars'" == "" local cr_vars "_all"
		
		/// saved values
		tempname saved_values
		mata `saved_values' = J(`n_loop',3,.)

		
		if "`noprogress'" == "" di as text "  Lags  " _col(10) "{c |}" _col(15) "CD" _col(25) "p-value" 
		if "`noprogress'" == "" di as text "{hline 9}{c +}{hline 20}"
		
		forvalues i = 1(1)`n_loop' {
			local lagi = word("`lags_loop'",`i')
			if "`lagi'" == "-1" {
				qui `anything' `if' , `options_cmd' nocross
			}
			else qui `anything' `if' , `options_cmd' cr(`cr_vars',`cr_opt' cr_lags(`lagi'))
			mata `saved_values'[`i',.] = `lagi',`e(cd)',`e(cdp)'

			if "`noprogress'" == "" di as text _col(3) "`lagi'" _col(10) "{c |}" _col(12) %8.3g `e(cd)'  _col(22) %5.3g `e(cdp)' 
		}
		

		mata st_matrix("`saved_values'",`saved_values')
		matrix colnames `saved_values' = Lags CD CDp
		matrix rownames `saved_values' = `lags_loop'

		return matrix cd_struct = `saved_values'

		tempname CD CDp
		mata `CD' = `saved_values'[.,(2,1)]
		mata `CDp' = `saved_values'[.,(3,1)]

		if "`nograph'" == "" {
			twoway line matamatrix(`CD') , ytitle("CD", axis(1)) || line matamatrix(`CDp') , ytitle("p-value", axis(2)) yaxis(2) yline(1.96 -1.96 , axis(1) lp(dash)) , ///
								legend(label(1 "CD") label(2 "CDp")  pos(6) rows(1)) note("Dashed line indicates 5% signifiance level. -1 stands for no cross-section averages.") xtitle("Lags")
		}

		mata mata drop `CD' `CDp' `saved_values'
		qui est restore `ResultsO'

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
				if "`level'" == "" local level = `c(level)'
				scalar cv = invnorm(1 - ((100-`level')/100)/2)
				predict `se' if `touse'  , se
				predict `coeff' if `touse' , coeff
				*noi sum `coeff'* `se'*
				*noi disp "vars: `graph_vars_mg' - `anything' - `mg'"

				local tivar `idvar' 

				if "`ivar'" != "" {
						local 0 `ivar'
						syntax varlist(min=1 max=1) , [SORTivar *]
						local tmpi tmpl
						encode `varlist', gen(`tmpi') label(tmpl)
						local labelinfo 
						qui sum `tmpi'
						local NN = r(max)
						if "`horizontal'" == "" local labelinfo xlabel(#`NN', valuelabel `options') 
						else local labelinfo ylabel(#`NN', valuelabel `options' ) 
						local tivar `tmpi'
				}


				foreach var in `graph_vars_mg' {
					local s_var = subinstr("`var'",".","_",.)
					qui gen `s_var'_up = `coeff'_`s_var' + cv * `se'_`s_var' if `touse'
					qui gen `s_var'_lo = `coeff'_`s_var' - cv * `se'_`s_var' if `touse'
					*noi sum `s_var'_up `s_var'_lo
					if "`nomg'" == "" {
						local mg_mean = _b[`var']
						local mg_up = `mg_mean' + cv * _se[`var']
						local mg_lo = `mg_mean' - cv * _se[`var']

						if "`horizontal'" == "" {							
							if "`zero'" != "" local ylines "yline(`mg_mean') yline(`mg_up', lp(dash)) yline(`mg_lo' , lp(dash)) yline(0, lp(solid) lw(thick) lc(black))"
							else local ylines "yline(`mg_mean') yline(`mg_up', lp(dash)) yline(`mg_lo' , lp(dash)) " 
						}
						else {
							
							if "`zero'" != "" local xlines "xline(`mg_mean') xline(`mg_up', lp(dash)) xline(`mg_lo' , lp(dash))  xline(0, lp(solid) lw(thin) lc(black))"
							else local xlines "xline(`mg_mean') xline(`mg_up', lp(dash)) xline(`mg_lo' , lp(dash)) "
						}
					}
					if "`cleargraph'" == "" {
						local grcap `"legend(off) `ylines' `xlines' nodraw  ytitle("") title("`var'") note("Mean: `=string(_b[`var'])'" "SE: `=string(_se[`var'])'")"'
					}				
					if "`dropzero'" != "" local dropzeroc & `coeff'_`s_var' != 0
					
					local varname = strtoname("`var'")
					
					if "`sortivar'" != "" sort `coeff'_`s_var' `idvar' `tvar'
					
					if "`horizontal'" == "" {
						twoway	(scatter `coeff'_`s_var' `tivar' , m(X)  ) /*
								 */ (rcap `s_var'_up `s_var'_lo `tivar' , lp(dash) ) /*
								*/ if `touse' `dropzeroc' , name(rc`varname', replace) `grcap' `individual' `labelinfo'
					}
					else {
						twoway	(scatter `tivar' `coeff'_`s_var'  , m(X)  ) /*
								 */ (rcap  `s_var'_up `s_var'_lo `tivar' , lp(dash) horizontal ) /*
								*/ if `touse' `dropzeroc' , name(rc`varname', replace) `grcap' `individual' `labelinfo'
					}
					local graph_list `graph_list' rc`varname' 

				}
				if "`cleargraph'" == "" {
					local cgrcap `"title("Mean Group Variables") note("Point estimates are indicated by a cross, mean group estimates by the red line and " "the `level'% confidence interval by the upper and lower range and dashed red line.", size(tiny )) name(xtdcce2_combine, replace)"'
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
