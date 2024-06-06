*! bootstrap_xtdcce2 1.0 - 20.02.2023
*! author Jan Ditzen
*! www.jan.ditzen.net - jan.ditzen@unibz.it
/*
Version History
	- 23.01.2020: - variances posted in V rather than only standard errors in bSE; old results are put into V_modelbased


*/
cap program drop bootstrap_xtdcce2
program define bootstrap_xtdcce2, eclass
	syntax , [reps(integer 100) seed(string) CFResiduals Percentile SHOWIndividual Wild ]
	disp as text "(running on xtdcce2 sample)"
	disp ""
	qui{
	
		if "`wild'" == "" local cross cross
		*** save est results
		tempname EResults EResultsToUse V_modelbased
		local coln : colnames e(b)
		matrix `V_modelbased' = e(V)
		cap _estimates hold `EResults' ,  restore copy  varname(`EResultsToUse')
		
		mata b = st_matrix("e(b)")
		mata se = sqrt(diagonal(st_matrix("e(V)")))'
		mata t = b:/se
		
		if "`e(cmd)'" == "xtdcce2fast" {
			local showindividual ""
			local cfresiduals ""
		}
		
		*** showindividual options
		tokenize `e(cmd)' , p(",")
		if regexm("`3'","showi") == 1 | "`showindividual'" != "" {
			local showindividual showindividual 
			mata bi = st_matrix("e(bi)")
			mata sei = sqrt(diagonal(st_matrix("e(Vi)")))'
			mata ti = bi:/sei
			local colni : colnames e(bi)
			mata namesi = tokens("`colni'")
		}
		
		** Type of residuals
		local cf "residuals"
		local cfname "residuals"
		if "`cfresiduals'" != "" {
			local cf cfresiduals
			local cfname "residuals and common factors"
		}
		
		** Cross sectional units
		local N_g = e(N_g)
		
		preserve
			** get dep var and save it
			tsrevar `e(depvar)', list
			local yOriginalName "`r(varlist)'" 
			tempvar y_original res_original 

			gen double `y_original' = `yOriginalName'	
			
			if "`cross'" == "" {
				predict double `res_original' , `cf' wildbootstrap
			}
			else {
				predict double `res_original' , `cf' 
			}
			** work around, if lags used, residuals will be zero, but these obs do not matter
			** because they will be removed by xtdcce2 anyway
			replace `res_original' = 0 if `res_original' == .
			
			if "`cross'" == "" {
				** Remove residuals from y
				replace `yOriginalName' = `yOriginalName' - `res_original'
			}
			

			tempvar vitb
			gen double `vitb' = .
		
		
			if "`seed'" != "" {
				set seed `seed'
			}
			** Initalize Results
			mata K = cols(b)
			mata tBoot = J(`reps',K,.)
			mata seBoot = J(`reps',K,.)
			mata bBoot = J(`reps',K,.)
			
			if "`showindividual'" != "" {
				mata Ki = cols(bi)
				mata biBoot = J(`reps',Ki,.)
				mata tiBoot = J(`reps',Ki,.)
				mata seiBoot = J(`reps',Ki,.)
			}
			if "`cross'" == "" {
				noi disp as text "Wild-Bootstrap replications (", _c			
				noi disp as result "`reps'", _c
				noi disp as text ") using `cfname'"
				
				_xt
				local idvar "`r(ivar)'"
				local tvar "`r(tvar)'"
				noi _dots 0
						
				forvalues r=1(1)`reps' {
					
					** Generate weighting, one for each cluster (cross-sectional unit)
					by `idvar' (`tvar'), sort: replace `vitb' = 2*runiformint(0,1)-1 if _n == 1
					by `idvar' (`tvar'), sort: replace `vitb' = `vitb'[1]
					
					replace `yOriginalName' = `yOriginalName' + `res_original' * `vitb'
					
					** run xtdcce2
					cap  `e(cmdline)' 
					local dots = (_rc != 0)
					
					** save results
					mata bBoot[`r',.] = st_matrix("e(b)")
					mata seBoot[`r',.] = sqrt(diagonal(st_matrix("e(V)")))'
					mata tBoot[`r',.] = ((bBoot[`r',.]:-b) :/ seBoot[`r',.])
					
					if "`showindividual'" != "" {
						
						mata index = xtdcce2_mm_which2(namesi,st_matrixcolstripe("e(bi)")[.,2])
						
						mata biBoot[`r',index] = st_matrix("e(bi)")
						mata seiBoot[`r',index] = sqrt(diagonal(st_matrix("e(Vi)")))'
						mata tiBoot[`r',.] = ((biBoot[`r',.]:-bi) :/ seiBoot[`r',.])
						
						mata st_local("check",strofreal(rows(index):==Ki))

						if `check' == 0 {
							local dots 2
							local checked 1
						}
					
					}
					
					*** Output for each run (dots)
					if round(`=`r'/50')==`=`r'/50' {
						noi _dots 1 `dots'
						noi disp "     `r'"
					}
					else if `r' == `reps' {
						noi _dots 1 `dots'
						noi disp _col(55) "`r'"
					}
					else{
						noi _dots 1 `dots'
					}
				}	
			}
			
			else {
				noi disp as text "Cross-sectional Bootstrap replications (", _c			
				noi disp as result "`reps'", _c
				noi disp as text ") using `cfname'"
				
				noi _dots 0
				
				tempfile initial
				save `initial', replace
				
				xtset
				local idvar "`r(panelvar)'"
				local tvar "`r(timevar)'"
				
				mata Nuniq = uniqrows(st_data(.,"`idvar'"))
				mata st_local("N",strofreal(rows(Nuniq)))
				
				tempfile tobuild
				
				
				forvalues r=1(1)`reps' {
					clear
					save `tobuild', emptyok replace
								
					mata index = runiformint(`N',1,1,`N')
					forvalues i = 1(1)`N' {
						use `initial'
						*noi disp "`i'"
						mata st_local("tokeep",strofreal(Nuniq[index[`i']]))
						keep if `idvar' == `tokeep'
						drop `idvar'
						gen `idvar' = `i'
						append using `tobuild'
						save `tobuild', replace
					}
					use `tobuild' , replace
					
					xtset `idvar' `tvar'
					
					
					** run xtdcce2
					cap  `e(cmdline)' 
					local dots = (_rc != 0)
					
					** save results
					mata bBoot[`r',.] = st_matrix("e(b)")
					mata seBoot[`r',.] = sqrt(diagonal(st_matrix("e(V)")))'
					mata tBoot[`r',.] = ((bBoot[`r',.]:-b) :/ seBoot[`r',.])
					
					*** Output for each run (dots)
					if round(`=`r'/50')==`=`r'/50' {
						noi _dots 1 `dots'
						noi disp "     `r'"
					}
					else if `r' == `reps' {
						noi _dots 1 `dots'
						noi disp _col(55) "`r'"
					}
					else{
						noi _dots 1 `dots'
					}
					
					*frame change BootstrapOriginal
					
				}
				*frame change `initframe'
			}
			
		
		restore
		
		*** calculate bootstrap SE
		tempname BootstrappedSE BootstrappedSEi BootstrappedV
		if "`percentile'" == "" {
			mata bV = quadvariance(bBoot)
			mata bSE = sqrt(diagonal(bV))'			
			mata st_matrix("`BootstrappedSE'",bSE)
			matrix colnames `BootstrappedSE' = `coln'
			
			mata st_matrix("`BootstrappedV'",bV)
			matrix colnames `BootstrappedV' = `coln'
			matrix rownames `BootstrappedV' = `coln'			
			
			if "`showindividual'" != "" {
				mata biSE = sqrt(diagonal(quadvariance(biBoot)))'			
				mata st_matrix("`BootstrappedSEi'",biSE)
				matrix colnames `BootstrappedSEi' = `colni'
			}
		}
		else {
			mata st_local("K",strofreal(K))
			mata tstar = J(cols(tBoot),2,.)
			forvalues c=1(1)`K' {
					mata tstar[`c',.] = mm_quantile(tBoot[.,`c'],1,(1-(100-c("level"))/200,(100-c("level"))/200))
				}				
			mata tstar = (b':-se':*tstar[.,1] , b':-se':*tstar[.,2])'
			
			tempname BootstrapCI
			mata st_matrix("`BootstrapCI'",tstar)
			matrix colnames `BootstrapCI' = `coln'
			
			if "`showindividual'" != "" {
				mata st_local("Ki",strofreal(Ki))
				mata tistar = J(cols(tiBoot),2,.)
				forvalues c=1(1)`Ki' {
					mata tistar[`c',.] = mm_quantile(tiBoot[xtdcce_selectindex(tiBoot[.,`c']:!=.),`c'],1,(1-(100-c("level"))/200,(100-c("level"))/200))
				}				
				mata tistar = (bi':-sei':*tistar[.,1] , bi':-sei':*tistar[.,2])'
			
				tempname BootstrapCIi
				mata st_matrix("`BootstrapCIi'",tistar)
				matrix colnames `BootstrapCIi' = `colni'
			}
		}
	}
	_est unhold `EResults'
	if "`percentile'" != "" {
		mata bSE = sqrt(diagonal(st_matrix("e(V)")))'
		mata st_matrix("`BootstrappedSE'",bSE)
		matrix colnames `BootstrappedSE' = `coln'
		
		*** add just for posting purpose
		matrix `BootstrappedV'  = e(V)
		
		if "`showindividual'" != "" {
			mata biSE = sqrt(diagonal(st_matrix("e(Vi)")))'
			mata st_matrix("`BootstrappedSEi'",biSE)
			matrix colnames `BootstrappedSEi' = `colni'
		}
	}
	** T-stats
	tempname t b_mg
	mata st_matrix("`t'",b:/bSE)
	matrix colnames `t' = `coln'	
	matrix `b_mg' = e(b)
	
	if "`showindividual'" != "" {
		tempname ti b_mgi
		mata st_matrix("`ti'",bi:/biSE)
		matrix colnames `ti' = `colni'	
		matrix `b_mgi' = e(bi)
	
	}
	
	
	**Assume standard of 80 and extend only if variable names larger than 80 and linesize sufficient.
	local maxline = c(linesize)	
	**allow max linesize of 100
	if `maxline' > 100 {
		local maxline = 100
	}
	**get var length
	local maxlength = 0
	foreach var in `e(indepvar)' `e(lr)' {
		 local tmp = strlen("`var'")
		 if `tmp' > `maxlength' {
			local maxlength = `tmp'
		 }
	}

	*check if maxlength larger than 14 (standard)
	local abname = 14
	if `maxlength' > 14 {
		local abname = 14+(`maxline'-80)
		if `abname' > `maxlength' {
			local abname `maxlength'
			
		}
		**66 is remaining lines for output
		
	}
	**set standard to 80, only if more vars needed, then extend. maximum is 100
	local maxline = `abname' + 66	
	
	
	local level =  `c(level)'
	local col_i = `abname' + 1
	local maxline = `maxline' - 2
	
	di as text "{hline `col_i'}{c TT}{hline `=`maxline'-`col_i''}"
	*** First row
	di as text _col(`col_i') " {c |}" 	, _c
	local col = `col_i' + 1 + 2
	di as text _col(`col') "Observed" _c
	local col = `col' + 5 + 6
	if "`percentile'" == "" {
		di as text _col(`col') "Bootstrap"  _c		
	}
	else {
		di as text _col(`col') "Observed"  _c
	}
	local col = `col' + 9 + 6
	di as text _col(`col') " "  _c
	local col = `col' + 1 + 4
	di as text _col(`col') "    "  _c
	local col = `col' + 5 + 5+5
	if "`percentile'" == "" {
		di as text _col(`col') "Normal-based"  
	}
	else {
		di as text _col(`col') "percentile t" 
	}
	*** Second Row
	di as text %`col_i's  abbrev("`lhs'",`abname') "{c |}" _c
	local col = `col_i' + 1 + 5
	di as text _col(`col') "Coef." _c
	local col = `col' + 5 + 3
	di as text _col(`col') "Std. Err."  _c
	local col = `col' + 9 + 6
	di as text _col(`col') "z"  _c
	local col = `col' + 1 + 5
	di as text _col(`col') "P>|z|"  _c
	local col = `col' + 5 + 5
	di as text _col(`col') "[`level'% Conf. Interval]"
	di as text "{hline `col_i'}{c +}{hline `=`maxline'-`col_i''}"
	
	scalar cv = invnorm(1 - ((100-`level')/100)/2)
	
	
	if "`e(lr)'" != "" {
		local lr "`e(lr)'"
	}
	
	local pooled "`e(p_pooled_vars)'"
	local rhs "`e(p_mg_vars)'"
	
	
	if "`lr'" != "" {
		di as text _col(2) _col(2) "Short Run Est." _col(`col_i')  "{c |}"
		di as text "{hline `col_i'}{c +}{hline `=`maxline'-`col_i''}"
		local sr_text "  "
	}
	local ardl_indic = ( regexm("`e(lr_options)'","ardl")==1)
	if `ardl_indic'== 1 {
		local lr_np : list lr - pooled
		local lr_p : list lr - lr_np
		local rhs_vars  `rhs' `lr_np'
		local pooled_vars `pooled' `endo_pooled' `lr_p'
		local pooled_vars: list uniq pooled_vars
		local rhs_vars: list uniq rhs_vars
	}
	else {
		local rhs_vars `rhs' 
		local pooled_vars `pooled' 

		local pooled_vars : list pooled_vars - lr
		local rhs_vars : list rhs_vars - lr
	}
	if "`pooled_vars'" != "" { 
		di as text _col(2) "`sr_text'Pooled: " _col(`col_i') " {c |}"
		foreach var in `pooled_vars' {
			xtdcce_output_table `var' `col_i' `b_mg' `BootstrappedSE' `t' cv `var' `BootstrapCI'
		}
	}
	
	
	if "`rhs_vars'" != ""   {
		di _col(2) as text "`sr_text'Mean Group:"  _col(`col_i')  " {c |}"
		local lrcount = wordcount("`rhs_vars'")
		foreach var in `rhs_vars' {
			if "`showindividual'" != "" {
				di "" _col(`col_i') " {c |}"
			}
			xtdcce_output_table `var' `col_i' `b_mg' `BootstrappedSE' `t' cv `var' `BootstrapCI'
			if "`showindividual'" != "" {
				local lrcount = `lrcount' - 1
				di as text "{hline `col_i'}{c BT}{hline `=`maxline'-`col_i''}"
				di as text "`sr_text'`sr_text'Individual Results" _col(`col_i')
				di as text "{hline `col_i'}{c TT}{hline `=`maxline'-`col_i''}"
				forvalues j = 1(1)`N_g' {
					xtdcce_output_table `var'_`j' `col_i' `b_mgi' `BootstrappedSEi' `ti' cv `var'_`j' `BootstrapCIi'
				}
				if "`lr'" == "" & `lrcount' > 0 {
					di as text "{hline `col_i'}{c +}{hline `=`maxline'-`col_i''}"
				}
			}
		}
	}
	
	
	if strtrim("`lr'") != "" {
		if `ardl_indic' == 0 {
			local lr_pooled : list lr & pooled
			local lr_rest : list lr - lr_pooled
		}
		else {
			local lr_pooled `e(p_lr_vars_pooled)'
			local lr_rest `e(p_lr_vars_mg)'
		}
		
		
		if "`rhs_vars'" != "" {
			di as text "{hline `col_i'}{c +}{hline `=`maxline'-`col_i''}"
		}
		di as text _col(2) "Long Run Est." _col(`col_i')  " {c |}"
		di as text "{hline `col_i'}{c +}{hline `=`maxline'-`col_i''}"
		
		if "`lr_pooled'" != "" { 
			di as text _col(2) "`sr_text'Pooled:" _col(`col_i') " {c |}" 
			foreach var in `lr_pooled' {
				xtdcce_output_table `var' `col_i' `b_mg' `BootstrappedSE' `t' cv `var' `BootstrapCI'
			}
		}
		if strtrim("`lr_rest'") != ""   {
			di as text _col(2) "`sr_text'Mean Group:" _col(`col_i') " {c |}" 
			local lrcount = wordcount("`lr_rest'")
			foreach var in `lr_rest' {
				xtdcce_output_table `var' `col_i' `b_mg' `BootstrappedSE' `t' cv `var' `BootstrapCI'
				if "`showindividual'" != "" {
					local lrcount = `lrcount' - 1
					di as text "{hline `col_i'}{c BT}{hline `=`maxline'-`col_i''}"
					di as text _col(2) "`sr_text'`sr_text'Individual Results" _col(`col_i')
					di as text "{hline `col_i'}{c TT}{hline `=`maxline'-`col_i''}"
					forvalues j = 1(1)`N_g' {
						xtdcce_output_table `var'_`j' `col_i' `b_mgi' `BootstrappedSEi' `ti' cv `var'_`j' `BootstrapCIi'
					}
					if `lrcount' > 0 {
						di as text "{hline `col_i'}{c +}{hline `=`maxline'-`col_i''}"
					}
				}
			}		
		}
	}	
	di as text "{hline `col_i'}{c BT}{hline `=`maxline'-`col_i''}"
	
	if "`checked'" == "1" {
	di as error "e" , _c
	di as text " implies missing values in individual coefficient matrix due to dropped cross-sectional units."
	}
	
	if "`percentile'" == "" {
		ereturn repost V = `BootstrappedV'
		ereturn matrix V_modelbased = `V_modelbased' 
	}
	
	ereturn matrix bt = `t'
		
	if "`showindividual'" != "" {
		ereturn matrix biSE = `BootstrappedSEi'
		ereturn matrix bti = `ti'
	}
		
	if "`percentile'" != "" {
		matrix rownames `BootstrapCI' = lower upper
		ereturn matrix bCI = `BootstrapCI'
		if "`showindividual'" != "" {
			matrix rownames `BootstrapCIi' = lower upper
			ereturn matrix bCIi `BootstrapCIi'
		}
	}	
	
	
end


*capture program drop xtdcce_output_table
cap program drop xtdcce_output_table
program define xtdcce_output_table
	syntax anything

	tokenize `anything'
	local var `1'
	local col =  `2'
	local b_p_mg `3'
	local se_p_mg `4'
	local t_p_mg `5' 
	local cv  `6'
	local i `7'
	local ci `8'
	
	di as text %`col's abbrev("`var' ",`=`col'-1') "{c |}"  _continue
	local col = `col' + 3
	di as result _column(`col') %9.8g `b_p_mg'[1,colnumb(`b_p_mg',"`i'")] _continue
	local col = `col' + 8 + 3
	di as result _column(`col') %9.8g `se_p_mg'[1,colnumb(`se_p_mg',"`i'")] _continue
	local col = `col' + 8 + 3
	di as result _column(`col') %6.2f `t_p_mg'[1,colnumb(`t_p_mg',"`i'")] _continue
	scalar pval= 2*(1 - normal(abs(`t_p_mg'[1,colnumb(`t_p_mg',"`i'")])))
	local col = `col' + 10
	di as result _column(`col') %5.3f pval _continue
	local col = `col' + 10
	if "`ci'" == "" {
		di as result _column(`col') %9.7g ( `b_p_mg'[1,colnumb(`b_p_mg',"`i'")] - `cv'*`se_p_mg'[1,colnumb(`se_p_mg',"`i'")]) _continue
		local col = `col' + 11
		di as result _column(`col') %9.7g ( `b_p_mg'[1,colnumb(`b_p_mg',"`i'")] + `cv'*`se_p_mg'[1,colnumb(`se_p_mg',"`i'")])
	}
	else {
		di as result _column(`col') %9.7g `ci'[1,colnumb(`ci',"`i'")] _continue
		local col = `col' + 11
		di as result _column(`col') %9.7g `ci'[2,colnumb(`ci',"`i'")] 
	}
end
** auxiliary file with auxiliary programs
findfile "xtdcce2_auxiliary.ado"
include "`r(fn)'"

