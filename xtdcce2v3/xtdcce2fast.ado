*! xtdcce2fast - fast version of xtdcce2
*! version 1.0 - August 2021

capture program drop xtdcce2fast
program define xtdcce2fast, eclass sortpreserve

	local xtdcce2fast_version = 0.9
	if replay() {
		syntax [, VERsion replay * ] 
		if "`version'" != "" {
			di in gr "`xtdcce2fast_version'"
			*ereturn clear
			ereturn local version `xtdcce2fast_version'
			exit
		}
	}	
	else {
	
		syntax varlist(min=1 ts) [if/] , [cr_lags(numlist) CRosssectional(string) GLOBALCRosssectional(string) CLUSTERCRosssectional(string) noconstant lr(string) fullsample NOTable lr_options(string) NOCROSSsectional cd trace postframe NOPOST pooled pooledvce(string) POOLEDConstant ] 
		qui{
			version 15
						
			local cmdline xtdcce2fast `0'
			
			local ifstart `if'
			
			tempvar touse
			marksample touse			
			local lhsrhs `varlist'
			
			if "`trace'" == "trace" {
				local trace noi
			}
			
			local posttype "none"
			if c(stata_version) > 16 & "`postframe'" != "" & "`nopost'" == ""  {
				local posttype "frame"
			}
			else if  "`postframe'" == "" & "`nopost'" == "" {
				local posttype "mata"				
			}
			else if "`cd'" != "" {
				local posttype "temp"			
			}
			
			
			** get idvar and tvar
			qui xtset
			local idvar "`r(panelvar)'"
			local tvar "`r(timevar)'"
			
			*** pooled options
			if "`pooled'" == "" {
				local pooled = 0
				local pooledvce = 0
				local hasFEPooled = 0
			}
			else {
				local pooled = 1
			
				** pooled VCE
				if "`pooledvce'" == "np" {
					local pooledvce = 1
				}
				else if "`pooledvce'" == "nw" {
					local pooledvce = 2
				}
				else {
					local pooledvce = 1
				}
				
				if "`pooledconstant'" == "" {
					/// has FE
					local hasFEPooled = 1
					local constant noconstant
				}
				else {
					local hasFEPooled = 0
				}
			}	
			** create cross sectional lags	
			if "`nocrosssectional'" != "" {
				local crosssectional ""
				local clustercrosssectional ""
				local globalcrosssectional ""
			}
			
			if "`crosssectional'" != "" {
			    if "`cr_lags'" == "" {
					local 0 `crosssectional'
					syntax varlist(ts) , [cr_lags(numlist)]
					local crosssectional `varlist'
					local checkscsa "`varlist'"
				}				
				
				if "`cr_lags'" == "" {
					local scr_lags = 0
				}
				else {
				    local scr_lags `cr_lags'					
				}
				
			}
			
			*** Global Crosssectional averages
			if "`globalcrosssectional'" != "" {
				local 0 `globalcrosssectional'
				syntax varlist(ts) , [cr_lags(numlist)]
				local globalcrosssectional `varlist'
				local checkgcsa "`varlist'"				
				local checkcsa: list checkgcsa & checkscsa
				
				if "`checkcsa'" != "" {
					noi disp as error "Variables (`checkcsa') occur in globalcrosssectional() and crosssectional()."
					exit
				}
				
				if "`cr_lags'" == "" {
					local gcr_lags = 0
				}
				else {
				    local gcr_lags `cr_lags'
				}			
			}		
			*** Clustered Crosssectional averages
			if "`clustercrosssectional'" != "" {
				local 0 `clustercrosssectional'
				syntax varlist(ts) , [cr_lags(numlist) CLuster(varlist) ]
				local clustercrosssectional `varlist'
				local csa_cluster "`cluster'"
				
				*** not necessary!?
				*local checkccsa "`varlist'"
				*local checkcsa1: list checkgcsa & checkccsa
				*local checkcsa2: list checkscsa & checkccsa
				
				*if "`checkcsa1'`checkcsa2'" != "" {
				*	noi disp as error "Variables (`checkcsa1' `checkcsa2') occur in clustercrosssectional() and globalcrosssectional() or crosssectional()."
				*	exit
				*}				
				if "`cluster'" == "" {
				    noi disp "No cluster set."
					exit
				}	
							
				if "`cr_lags'" == "" {
					local ccr_lags = 0
				}
				else {
				    local ccr_lags `cr_lags'
				}				
			}			
			
			
			
			if "`lr_options'" == "" & "`lr'" != "" {
				local lr_options "ecm"
			}
			
			** markout
			*sort `idvar' `tvar'
			*markout `touse' `lhsrhs' `lr' `clistfull'
			
			** get numbers of lr bases in varlistnames
			if "`lr'" != "" {
				local rest `lr'
				while "`rest'" != "" {
					gettoken next rest : rest , match(paren)
					tsunab next: `next' 
					local CleanLR `CleanLR' `next'
					if wordcount("`next'") == 1 {
						tsrevar `next', list
						local BaseListe `BaseListe' `r(varlist)'
					}
					else {
						local FirstBase = word("`next'",1)
						tsrevar `FirstBase', list
						local FirstBase = "`r(varlist)' " * wordcount("`next'")
						local BaseListe `BaseListe' `FirstBase'
					}
				}
				local BaseUniq: list uniq BaseListe
				local rest `lhsrhs' `lr' 
				//// BaseIndex gives the index (i.e) position of each base. For example if
				//// ... y x1 x2 , lr(L.x1 L2.x1 x3) Baseindex is 0 0 0 1 1 2
				while "`rest'" != "" {
					gettoken next rest : rest , match(paren)
					tsunab next: `next'
					foreach var in `next' {
						local posi: list posof "`var'" in CleanLR
						if `posi' > 0 {
							local basei = word("`BaseListe'",`posi')
							local posi : list posof "`basei'" in BaseListe
							local posi = `posi' /* + `num_var'*/
							local BaseIndex `BaseIndex' `posi'
						}
						else {
							local BaseIndex `BaseIndex' 0
						}
						
					}
				}
			}
			`trace' disp "vars `CleanLR'"
			** parse varlist and create tempvars
			*sort `idvar' `tvar'
			tsrevar `lhsrhs' `lr'
			local varlistnew "`r(varlist)'"
			tsunab varlistnames: `lhsrhs' `CleanLR'

			** add constant
			if "`constant'" == "" {
				tempvar constant 
				gen double `constant' = 1
				local varlistnew `varlistnew' `constant'
				local varlistnames `varlistnames' _cons

			}
			
			markout `touse' `varlistnew' `clustercrosssectional' `crosssectional' `globalcrosssectional'

			tempvar tousecr			
			if "`fullsample'" != "" {
				if "`ifstart'" != ""  {
					gen `tousecr' = (`ifstart') 
				}
				else {
					gen `tousecr' = 1						
				}
			}
			else {
				gen `tousecr' = `touse'
			}			
						
			if "`crosssectional'" != "" {
				tempname scsa
				xtdcce2_csa `crosssectional' , idvar(`idvar') tvar(`tvar') cr_lags(`scr_lags') touse(`tousecr') csa(`scsa') tousets(`touse')
				local scsa `r(varlist)'
				local scross_structure `r(cross_structure)'	
			}
			
			if "`globalcrosssectional'" != "" {
				tempvar gtouse
				tempname gcsa
				gen `gtouse' = 1
				xtdcce2_csa `globalcrosssectional' , idvar(`idvar') tvar(`tvar') cr_lags(`gcr_lags') touse(`gtouse') csa(`gcsa') tousets(`touse')		
				local gcsa `r(varlist)'	
				local gcross_structure "`r(cross_structure)'"
				drop `gtouse'
			}			
						
			if "`clustercrosssectional'" != "" {
				tempname ccsa	
				xtdcce2_csa `clustercrosssectional' , idvar(`idvar') tvar(`tvar') cr_lags(`ccr_lags') touse(`tousecr') csa(`ccsa') cluster(`cluster') tousets(`touse')		
				local ccsa `r(varlist)'	
				local ccross_structure "`r(cross_structure)'"
			
			}
						
			local clistfull `scsa' `gcsa' `ccsa'
			
			*** update 
			markout `touse' `varlistnew' `clistfull'
			
			*** Save touse to frame or mata. necessary for predict
			if "`posttype'" == "frame" {
				cap frame drop xtdcce2fast
				*frame create xtdcce2fast
				timer clear 97
				timer on 97
				frame put `idvar' `tvar' `tousecr' `touse'  , into(xtdcce2fast)
				frame xtdcce2fast: rename `tousecr' tousecr
				frame xtdcce2fast: rename `touse' touse
				tempvar residual
				gen `residual' = .				
				timer off 97
			}
			else if "`posttype'" == "mata" {
				putmata xtdcce2fast_p = (`idvar' `tvar' `tousecr' `touse'), replace		
				tempvar residual
				gen `residual' = .
			}
			else if "`posttype'" == "temp" {
				tempvar residual
				gen `residual' = .
			}
			
			drop `tousecr'
			
			** run regressions; make sure data is sorted
			issorted `idvar' `tvar'
			tempname b_output V_output stats b_i stats_i v_i
			
			`trace' mata m_xtdcce2fast("`varlistnew'","`clistfull'","`BaseIndex'","`touse'","`idvar' `tvar'",`pooled',`pooledvce',`hasFEPooled',"`b_output' `V_output' `stats'",`b_i'=.,`v_i'=.,`stats_i'=.,"`lr_options'","`residual'")
			
			** link resdiduals
			if "`posttype'" == "frame" {
				tempname linkvar
				timer clear 98
				timer on 98
				frame xtdcce2fast: frlink 1:1 `idvar' `tvar' , frame(default) gen(`linkvar')
				frame xtdcce2fast: frget residuals = `residual', from(`linkvar')
				timer off 98
			}
			else if "`posttype'" == "mata" {
				putmata `residual', replace
				mata xtdcce2fast_p = xtdcce2fast_p , `residual'			
			}
			
			** matrix 
			if "`lr'" != "" {
				mata st_local("varlr",invtokens("lr_":+tokens("`BaseUniq'")))
			}
			tokenize `varlistnames'
			local lhs `1'
			macro shift
			local rhs `*' `varlr'
			local rhsi `*'
			
			
			matrix colnames `b_output' = `rhs'
			matrix colnames `V_output' = `rhs'
			matrix rownames `V_output' = `rhs'
		}
		tempname r2mg N_g N K Tmin Tmax T K_csa
		scalar `N' = `stats'[1,1]
		scalar `K' = `stats'[1,2]
		scalar `N_g' = `stats'[1,3]
		scalar `r2mg' = `stats'[1,4]
		scalar `Tmin' = `stats'[1,5]
		scalar `Tmax' = `stats'[1,6]
		scalar `T' = `stats'[1,7]
		scalar `K_csa' = wordcount("`clistfull'")

		
		ereturn clear	
		ereturn post `b_output' `V_output' , depname(`lhs')  esample(`touse') obs(`=`N'')
		
		ereturn local cmd "xtdcce2fast"
		ereturn local cmdline "`cmdline'"
		ereturn local idvar "`idvar'"
		ereturn local tvar "`tvar'"		
		ereturn local indepvar "`rhs'"
		
		ereturn hidden local postresults  = "`posttype'"
		
		
		ereturn hidden local p_if "`if'"
		ereturn hidden local p_in "`in'"
		
		
		if "`crosssectional'" != "" {
			ereturn local cr_lags  "`scr_lags'"
			ereturn local csa "`crosssectional'"
		}
		if "`globalcrosssectional'" != "" {
			ereturn local gcr_lags "`gcr_lags'"
			ereturn local gcsa "`globalcrosssectional'"
		}
		if "`clustercrosssectional'" != "" {
			ereturn local ccr_lags	 "`ccr_lags'"
			ereturn local ccsa "`clustercrosssectional'"
			ereturn local ccsa_cluster "`csa_cluster'"
		}
		
		ereturn hidden local p_mg_vars "`rhsi'"
		
		ereturn local predict "xtdcce2`pred'_p"
		ereturn local estat_cmd "xtdcce2_estat"
		
		if "`lr'" != "" {
			ereturn local lr "`CleanLR'"
			ereturn hidden local p_lr_vars_mg "`varlr'"
			ereturn hidden local lr_options "ardl"
		}	
		
		ereturn scalar N_g = `N_g'
		ereturn scalar K_mg = `K'
		ereturn scalar r2_pmg = `r2mg'
		
		if `Tmin' == `Tmax' {
			ereturn scalar T = `T'
		}
		else {
			ereturn scalar T = `T'
			ereturn scalar Tbar = `T'
			ereturn scalar Tmin = `Tmin'
			ereturn scalar Tmax = `Tmax'
		}
		
		mata xtdcce2fast_bi = `b_i'
		mata xtdcce2fast_Vi = `v_i'
		mata mata drop `b_i' `v_i'
		
		if "`cd'" == "cd" {			
			qui xtcd2 `residual' if e(sample)
			ereturn scalar cd = r(CD)
			ereturn scalar cdp = r(p)
			return clear
		}
		
		
		if "`notable'" == "" {
				
			tempname b_mg sd
			matrix `b_mg' = e(b)
			*** SD is variance here!
			matrix `sd' = e(V)
				
			**Assume standard of 80 and extend only if variable names larger than 80 and linesize sufficient.
			local maxline = c(linesize)	
			**allow max linesize of 100
			if `maxline' > 100 {
				local maxline = 100
			}
			**get var length
			local maxlength = 0
			foreach var in `e(p_mg_vars)' `e(lr)' {
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

			if `pooled' == 0 {
				local mgtext "Mean Group"
			}
			else {
				local mgtext "Pooled"
			}
			display as text "(Dynamic) Common Correlated Effects Estimator - `mgtext'"
			
			#delimit ;
			di _n in gr "Panel Variable (i): " in ye e(idvar) 
					   _col(`=`maxline'-80+50') in gr "Number of obs" _col(`=`maxline'-80+68') "=" 
					   _col(`=`maxline'-80+71') in ye %9.0f e(N) ;
			di in gr "Time Variable (t): " in ye abbrev(e(tvar),`abname') in gr
						_col(`=`maxline'-80+50') "Number of groups" _col(`=`maxline'-80+68') "="
						_col(`=`maxline'-80+71') in ye %9.0g e(N_g) ;

			
			#delimit cr
			di "" 
			if `Tmin' == `Tmax'  {
				#delimit ;
					di in gr "Degrees of freedom per group:" 
							_col(`=`maxline'-80+50') in gr "Obs per group (T)" _col(`=`maxline'-80+68') "="
							_col(`=`maxline'-80+71') in ye %9.0f e(T) ;
					di in gr _col(2) "without cross-sectional averages"
								_col(37) "=" _col(39) e(T)- e(K_mg);
					di in gr _col(2) "with cross-sectional averages"
							_col(37) "=" _col(39) e(T)-e(K_mg) - `K_csa' ;
				#delimit cr
			}
			else {	
				#delimit ;
					di in gr "Degrees of freedom per group:" 
						_col(`=`maxline'-80+50') in gr in gr "Obs per group:" _col(`=`maxline'-80+68') ;
					di in gr _col(2) "without cross-sectional avg."
						_col(31) "min"
						_col(37) "=" _col(39) e(Tmin)- e(K_mg)
						_col(`=`maxline'-80+68-4') in gr "min = "
						_col(`=`maxline'-80+71') in ye %9.0f e(Tmin) ;	
					di in gr _col(31) "max"
						_col(37) "=" _col(39) e(Tmax) - e(K_mg)
						_col(`=`maxline'-80+68-4') in gr "avg = " 
						_col(`=`maxline'-80+71') in ye %9.0f e(Tbar) ;
					di in gr _col(2) "with cross-sectional avg."
						_col(31) "min"
						_col(37) "=" _col(39) e(Tmin) - e(K_mg)- `K_csa'
						_col(`=`maxline'-80+68-4') in gr "max = " 
						_col(`=`maxline'-80+71') in ye %9.0f e(Tmax) ;
					di in gr _col(31) "max"
						_col(37) "=" _col(39) e(Tmax) - e(K_mg) - `K_csa' ;
				#delimit cr
			}
			if wordcount("`scr_lags' `ccr_lags' `gcr_lags'") > 1 {
				mata st_local("cr_lags_min",strofreal(min(strtoreal(tokens("`scr_lags' `ccr_lags' `gcr_lags'")))))
				mata st_local("cr_lags_max",strofreal(max(strtoreal(tokens("`scr_lags' `ccr_lags' `gcr_lags'")))))
				local cr_lags_disp "`cr_lags_min' to `cr_lags_max'"
			}
			else if wordcount("`scr_lags' `ccr_lags' `gcr_lags'") == 1 {
				local cr_lags_disp "= `scr_lags' `ccr_lags' `gcr_lags'"
			}
			else {
				local cr_lags_disp "none"
			}
			
			if "`cd'" == "cd" {
				local line1 `"_col(`=`maxline'-80+50') in gr "CD Statistic" _col(`=`maxline'-80+68') "=" _col(`=`maxline'-80+71') in ye in ye %9.2f e(cd) "'
				local line2 `"_col(`=`maxline'-80+50') in gr "   p-value" _col(`=`maxline'-80+68') "=" _col(`=`maxline'-80+71') in ye in ye %9.4f e(cdp) "'	
			}
			
			#delimit ;
			di in gr "Number of "
						_col(`=`maxline'-80+50') in gr "R-squared (mg)" _col(`=`maxline'-80+68') "="
						_col(`=`maxline'-80+71') in ye %9.2f e(r2_pmg) ;
			di in gr _col(2) "cross-sectional lags" 
						_col(37) "`cr_lags_disp'"	`line1' ;

			di in gr _col(2) "variables in mean group regression"
						_col(37) "=" _col(39) e(K_mg)*e(N_g) `line2' ;

			di in gr _col(2) "variables partialled out"
						_col(37) "=" _col(39)  `K_csa'*`N_g' ;
			#delimit cr		
		
			
			local level =  `c(level)'
			local col_i = `abname' + 1
			local maxline = `maxline' - 2
			
			di as text "{hline `col_i'}{c TT}{hline `=`maxline'-`col_i''}"
			di as text %`col_i's  abbrev("`lhs'",`abname') "{c |}" _c
			local col = `col_i' + 1 + 6
			di as text _col(`col') "Coef." _c
			local col = `col' + 5 + 3
			di as text _col(`col') "Std. Err."  _c
			local col = `col' + 9 + 6
			di as text _col(`col') "z"  _c
			local col = `col' + 1 + 4
			di as text _col(`col') "P>|z|"  _c
			local col = `col' + 5 + 5
			di as text _col(`col') "[`level'% Conf. Interval]"    
			di as text "{hline `col_i'}{c +}{hline `=`maxline'-`col_i''}"
			
			scalar cv = invnorm(1 - ((100-`level')/100)/2)
			
			if "`lr'" != "" {
				di as text _col(2) _col(2) "Short Run Est." _col(`col_i')  "{c |}"
				di as text "{hline `col_i'}{c +}{hline `=`maxline'-`col_i''}"
				local sr_text "  "
			}
			
			if "`lr_options'" == "ecm" {
				local rhsi `lr_1' `rhsi'
				local varlr: list varlr - lr_1
			}
			
			if "`rhsi'" != ""   {
				di _col(2) as text "`sr_text'Mean Group:"  _col(`col_i')  " {c |}"
				local lrcount = wordcount("`CleanLR'")
				foreach var in `rhsi' {
					xtdcce_output_table `var' `col_i' `b_mg' `sd' cv `var'
				}
				if "`lr'" == "" & `lrcount' > 0 {
					di as text "{hline `col_i'}{c +}{hline `=`maxline'-`col_i''}"
				}
				
			}
			if strtrim("`CleanLR'") != ""   {
				if "`rhsi'" != "" {
					di as text "{hline `col_i'}{c +}{hline `=`maxline'-`col_i''}"
				}
				di as text _col(2) "Long Run Est." _col(`col_i')  " {c |}"
				di as text "{hline `col_i'}{c +}{hline `=`maxline'-`col_i''}"
				di as text _col(2) "`sr_text'Mean Group:" _col(`col_i') " {c |}" 
				local lrcount = wordcount("`CleanLR'")
				foreach var in `varlr' {
					xtdcce_output_table `var' `col_i' `b_mg' `sd'  cv `var'
				}
			}
			di as text "{hline `col_i'}{c BT}{hline `=`maxline'-`col_i''}"
			if strtrim("`rhsi'") != "" | strtrim("`CleanLR'") != "" {
				di as text  "Mean Group Variables: `rhsi'"
			}
			if strtrim("`crosssectional'") != "" {
				if wordcount("`scr_lags'") > 1 {
					local crosssectional_output "`scross_structure'"
				}
				else {
					local crosssectional_output "`crosssectional'"
				}
				display  as text "Cross Sectional Averaged Variables: `crosssectional_output'"
			}
			
			if strtrim("`globalcrosssectional'") != "" {
				if wordcount("`gcr_lags'") > 1 {
					local crosssectional_output "`gcross_structure'"
				}
				else {
					local crosssectional_output "`globalcrosssectional'"
				}
				display  as text "Global Cross Sectional Averaged Variables: `crosssectional_output'"
			}
			
			if strtrim("`clustercrosssectional'") != "" {
				if wordcount("`ccr_lags'") > 1 {
					local crosssectional_output "`ccross_structure'"
				}
				else {
					local crosssectional_output "`clustercrosssectional'"
				}
				display  as text "Clustered Cross Sectional Averaged Variables: `crosssectional_output'"
			}
			
			
			
			if strtrim("`lr'") != "" { 
				display  as text "Long Run Variables: `CleanLR'"
				local lr_1 = word("`BaseListe'",1)
				display  as text "Cointegration variable(s): `lr_1'"
			}
		}
		*cap drop `clistfull'
	}
end


** auxiliary file with auxiliary programs
findfile "xtdcce2_auxiliary.ado"
include "`r(fn)'"

*capture program drop xtdcce_output_table
program define xtdcce_output_table
	syntax anything

	tokenize `anything'
	local var `1'
	local col =  `2'
	local b_p_mg `3'
	local se_p_mg `4'
	local cv  `5'
	local i `6'
	
	tempname b se tt pval
	scalar `se' = sqrt(`se_p_mg'[colnumb(`se_p_mg',"`i'"),colnumb(`se_p_mg',"`i'")])
	scalar `b' = `b_p_mg'[1,colnumb(`b_p_mg',"`i'")]
	scalar `tt' = `b' / `se'

	scalar `pval'= 2*(1 - normal(abs(`tt')))
	
	
	di as text %`col's abbrev("`var' ",`=`col'-1') "{c |}"  _continue
	local col = `col' + 3
	di as result _column(`col') %9.8g `b' _continue
	local col = `col' + 8 + 3
	di as result _column(`col') %9.8g `se' _continue
	local col = `col' + 8 + 3
	di as result _column(`col') %6.2f `tt' _continue	
	local col = `col' + 10
	di as result _column(`col') %5.3f `pval' _continue
	local col = `col' + 10
	di as result _column(`col') %9.7g ( `b_p_mg'[1,colnumb(`b_p_mg',"`i'")] - `cv'*sqrt(`se_p_mg'[rownumb(`se_p_mg',"`i'"),colnumb(`se_p_mg',"`i'")])) _continue
	local col = `col' + 11
	di as result _column(`col') %9.7g ( `b_p_mg'[1,colnumb(`b_p_mg',"`i'")] + `cv'*sqrt(`se_p_mg'[rownumb(`se_p_mg',"`i'"),colnumb(`se_p_mg',"`i'")]))
end

capture mata mata drop m_xtdcce2fast()
mata:
	function m_xtdcce2fast (string scalar varlist, 			///
							string scalar csaName,			///
							string scalar LongRunBases,		///
							string scalar touse,			///
							string scalar idtnames,			///
							real scalar pooled,				///
							real scalar vcepooled,			///
							real scalar PooledHasFE,		///
							string scalar outputnames,		///
							real matrix b_iOutput,			///
							real matrix V_iOutput,			///
							real matrix stats_i_name,		///
							string scalar lr_options,		///
							string scalar resname)
							
	{
		real matrix X
		real matrix Y
		real matrix csa
		real scalar residual
		
		X = st_data(.,varlist,touse)
		Y = X[.,1]
		X = X[.,(2..cols(X))]
		
		
		if (resname[1,1] != "") {
			st_view(residual,.,resname,touse)
		}
		else {
			residual = J(rows(X),1,.)
		}
		
		nocsa = 0
		K_csa = 0
		if (csaName[1,1] != "") {
			csa = st_data(.,csaName,touse)
			nocsa = 1
			K_csa =  cols(csa)
		}
		(nocsa, K_csa)
		idt = st_data(.,idtnames,touse)
		Nuniq = uniqrows(idt[.,1])
		N_g = rows(Nuniq)
		N = rows(X)
		T = N/N_g
		K = cols(X)
		
		b_i = J(N_g,K,.)
		s2_i = J(N_g,1,.)
		lower_i = J(N_g,1,.)
		
		stats_i = J(N_g,2,.)
		
		cov_i = J(N_g*K,K+1,.)
		
		index = panelsetup(idt[.,1],1)
		
		up_XY = J(K,1,0)
		low_XX = J(K,K,0)

		i=1
		covend = K
		covstart = 1

		X_tilde = J(N,K,.)
		Y_tilde = J(N,1,.)

		while (i<=N_g) {
			Xi = X[|index[i,1],. \ index[i,2],.|]
			Yi = Y[|index[i,1],. \ index[i,2],.|]

			if (PooledHasFE == 1 & pooled == 1) {
				/// demeaning, only for pooled
				Xi = Xi :-mean(Xi)
				Yi = Yi :-mean(Yi)
			}

			lower_i[i] =  quadcolsum(((Yi :- mean(Yi)):^2))
			/// partial out
			if (nocsa == 1) {
				csai = csa[|index[i,1],. \ index[i,2],.|]
				if (PooledHasFE == 1) {
					csai = csai :- mean(csai)
				}
				x_p = (Yi,Xi)
				tmp_xp = quadcross(csai,x_p)
				tmp_csa = quadcross(csai,csai)
				x_p = (x_p - csai*m_xtdcce_solver(tmp_csa,tmp_xp))
						
				Yi = x_p[.,1]
				Xi = x_p[.,(2..cols(x_p))]
			}
			
			tmp_xx = quadcross(Xi,Xi)
			tmp_xy = quadcross(Xi,Yi)
			tmp_xx1 = qrinv(tmp_xx)
			b_i[i,.] = m_xtdcce_solver(tmp_xx,tmp_xy)'
			
			residi = Yi - Xi * b_i[i,.]'

			/// pooled
			if (pooled==1) {
				up_XY = up_XY + tmp_xy
				low_XX = low_XX + tmp_xx
			}
			else {			
				residual[|index[i,1],. \ index[i,2],.|] = residi 
			}

			X_tilde[|index[i,1],. \ index[i,2],.|] = Xi
			Y_tilde[|index[i,1],. \ index[i,2],.|] = Yi

			s2_i[i] = residi'residi 
			
			stats_i[i,.] = (rows(Xi),cols(Xi))
			
			dfr = rows(Xi) - K- K_csa

			cov_i[(covstart..covend),.] = (J(K,1,Nuniq[i,1]) , tmp_xx1 :* s2_i[i]  / dfr)
			
			covstart = covend + 1
			covend = covend + K

			i++
		}

		if (pooled==1) {
			b_p = qrinv(low_XX) * up_XY
			b_mgi = b_i
			b_i = b_p'#J(N_g,1,1)
			residual = Y_tilde - X_tilde * b_p
		}

		K_csa
		Ktotal = K + K_csa
		upper = quadcolsum(s2_i)/ (N_g * (T - Ktotal))
		"s2_i"
		quadcolsum(s2_i)
		"lower i"
		quadcolsum(lower_i)
		"upper"
		upper
		"N_g" 
		N_g
		"T" 
		T
		"Ktotal" 
		Ktotal
		r2mg = 1 - upper /  (quadcolsum(lower_i)/(N_g * (T-1)) )
		"r2 mg"
		r2mg
		/// Long Run Coefficients (only ECM/ARDL)
		
		if (LongRunBases[1,1]!= "") {
			"in LR"
			LongRunBases = strtoreal(tokens(LongRunBases))
			LongRunBases = LongRunBases[2..cols(LongRunBases)]
			uniqBase = uniqrows(LongRunBases')
			/// remove 0s
			uniqBase = uniqBase[xtdcce_selectindex(uniqBase:!=0)]
			"lr base"
			LongRunBases
			uniqBase
			i=1
			s=1
			
			if (lr_options:=="ecm") {
				ecmadjust1 = 0
				ecmadjust2 = -1
			}
			else {
				ecmadjust1 = 1
				ecmadjust2 = -1
			}
			
			while (i<=rows(uniqBase)) {
				index = xtdcce_selectindex(uniqBase[i]:==LongRunBases)
				"Indexes"
				index
				if (s==1) {
					omega_lag = ecmadjust1 :+ ecmadjust2 :* quadrowsum(b_i[.,index])
					b_i = b_i , -omega_lag
				}
				else {
					omega_x = quadrowsum(b_i[.,index]):/(omega_lag)
					b_i = b_i, omega_x
				}
				
				i++
				s++
			}			
		}
		
		if (pooled == 0) {
			b_mg = quadmeanvariance(b_i)
			V = b_mg[(2..rows(b_mg)),.]:/N_g
			b_mg = b_mg[1,.]
		}
		else {
			b_mg = b_p'
			if (vcepooled==1) {
				/// var cov from Pesaran 2006, non parametric 
				V = VarCov_NP(X_tilde,b_mgi,b_p,index,N_g)
			}
			else if (vcepooled == 2) {
				"xx"
				quadcross(residual,residual)
				V = VarCov_HAC(X_tilde,residual,index,N_g)
			}
		}
		"b_mg"
		b_mg
		outputb = tokens(outputnames)[1]
		outputV = tokens(outputnames)[2]
		statsName = tokens(outputnames)[3]

		
		st_matrix(outputb,b_mg)
		st_matrix(outputV,V)
		stats = (rows(X),K,N_g,r2mg,min(stats_i[.,1]),max(stats_i[.,1]),mean(stats_i[.,1]))
		st_matrix(statsName,stats)
		
		b_iOutput = b_i
		V_iOutput = cov_i
		
		stats_i_name = stats_i 
		
		"done"
	}
end

// -------------------------------------------------------------------------------------------------
// VarCov Estimators
// -------------------------------------------------------------------------------------------------
/// Implements the non parametric covariance estimator from Pesaran (2006)
capture mata mata drop VarCov_NP()
mata:
	function VarCov_NP(real matrix X, real matrix beta_mgi, real matrix betaP, real matrix index,real matrix N)
	{		
		/// MG estimation for variance
		beta_mg = mean(beta_mgi)
		beta_diff = beta_mgi:- beta_mg 
		"beta p"
		betaP
		/// covariance estimation
		/// Standard VarianceCovarianceEstimator from Pesaran 2006, Eq 67 - 69.
		cov_PSI = J(rows(betaP),cols(betaP),0) 
		cov_R =  J(rows(betaP),cols(betaP),0)
		cov_w = J(N,1,1/N)
		cov_w_s = sum(cov_w:^2)		
		
		i = 1
		while (i<=N) {
			i
			cov_w_i = cov_w[i]
			cov_w_tilde = cov_w_i :/ sqrt(1/N :* cov_w_s)
			b_i1 = beta_diff[i,.]'
			
			tmp_x = panelsubmatrix(X,i,index)
			tmptmp = quadcross(tmp_x,tmp_x):/ rows(tmp_x)
				cov_w_tilde:^2 :* tmptmp*b_i1*b_i1'*tmptmp	
				"C_r"
				cov_R	
			/// eq. 67 Pesaran 2006
			cov_R = cov_R :+ cov_w_tilde:^2 :* tmptmp*b_i1*b_i1'*tmptmp	
			"cov_R"
			cov_R
			/// eq. 68 Pesaran 2006
			cov_PSI = cov_PSI :+ cov_w_i :* tmptmp	
			i++
					
		}
		cov_R = cov_R / (N - 1)
		PSI1 = invsym(cov_PSI)
		//// eq. 69 Pesaran 2006 
		cov =  cov_w_s :* PSI1 * cov_R * PSI1 	

		return(cov)
	}
end




/// VarCov from Karavias, Westerlund, Persyn (2021)
/// VarCov is HAC robust
capture mata mata drop VarCov_HAC()
mata:
	function VarCov_HAC(real matrix X, real matrix e, real matrix idt, real scalar N)
	{
		
		Shat = J(cols(X),cols(X),0)
		Shat0 = J(cols(X),cols(X),0)	
		tmp_xx = J(cols(X),cols(X),0)
		T_avg = 0
		i = 1
		while ( i <= N) {					
			/// select data
			tmp_x = panelsubmatrix(X,i,idt)
			tmp_e = panelsubmatrix(e,i,idt)

			Ti = rows(tmp_x)

			tmp_xx = tmp_xx + quadcross(tmp_x,tmp_x) 
			tmp_xe = tmp_e :* tmp_x
				
			
			Shat0 = Shat0 + quadcross(tmp_xe,tmp_xe) / (N*Ti)
			sij = 0
			bandwith = floor( 4 * (Ti:/100)^(2/9))	
			j = 1
			while (j <= (bandwith)){
				tmp_xep =  tmp_xe[|j+1,. \ Ti,.|]
				tmp_xepJ =  tmp_xe[|1,. \ Ti-j,.|]
				tmp_tmp = quadcross(tmp_xep,tmp_xepJ) / (N*Ti)
				sij = sij :+  (1- (j)/(bandwith+1)) :* (tmp_tmp + tmp_tmp')
				j++
			}
			Shat = Shat +  sij 
			T_avg = Ti + T_avg
			i++
		}
		T_avg = T_avg / N
		Shat = (Shat + Shat0) 
		sigma1 = invsym(tmp_xx) 				
		cov = sigma1 * Shat * sigma1 * (T_avg*N)
		return(cov)
	}
end

