*! xtdcce2 2.21 - xx.xx.2020
*! author Jan Ditzen
*! www.jan.ditzen.net - jan.ditzen@unibz.it
*! see viewsource xtdcce2.ado for more info.
/*
Packages Required:
- ivreg2
- moremata

Changelog:
18.06.2015 Added lhs to variables which are cross sectioned 
19.06.2015 Added option CD for CD test (xtcd2 test required).	
		   Added pooled variables to cross section if not specified cr() not specified.
23.06.2015 Changed calculation of partialling out. Now a regression with cholsolve is performed.
		   Mata matrices are deleted
24.06.2015 - Pooled variables are now removed from rhs variables, thus pooled variables can appear in rhs as well
		   - long Variablenames (>24 characters are allowed)
		   - no output if pooled or mean group is empty.
		   - Corrected Error in number of periods
		   - Clustered SE work again
25.06.2015 - Added degrees of freedom
		   - Added option noi, which showes the regression
		   - Omitted Variables and their number saved in e()		   
01.12.2015 - added check if cce_lags > 0 and "local drop_before = (`=`tmin'-1*`tdelta'') + `cr_lags' * `tdelta'" when comuputing the number of lags		   
xx.12.2015 - added IV	
11.01.2016 - added e(cmd), fixed naming errors in macros for instruments and naming if varnames are too long, temp vars are removed	
18.01.2016 - naming of matrices correct (in case of endogenous regressors)
28.01.2016 - fixed errors if noconstant is not active and constant is pooled variable
05.02.2016 - fixed error if "if" is used; "if `touse' " was missing when sperate was used.
11.02.2016 - changed option CD into nocd
15.02.2016 - partial out on unit level
15.02.2016 - unbalanced panels: missing values are added for balanced panel
17-20.02.2016 	- long run effects included
21-22.02.2016 - ts added, all variables now in varlists
23.02.2016 - xtdcce_err program added
24.02.2016 - added trend
02.03.2016 - added xtpmg names option for lr_options
05.03.2016 - added post_full option
08.03.2016 - changed 	mata b[lr_index] = (b[lr_index] :/ lr_tmp) into mata b[lr_index] = -(b[lr_index] :/ lr_tmp)
07.04.2016 - check for moremata
29.05.2016 - partial out program added, option for reportconstant added
Up to 19.10.2016
		   - Added function selectindex if Stata Version < 13
		   - Syntax for IV now as for ivreg2
		   - Replaced Mata matrices with temporary names and matrices are deleted
		   - Revised error message if packages are missing.
		   - Added version
		   - Revised Output
		   - minor bug fixes
		   - factor variables support
Up to 28.11.2016
		   - included predict and estat function
14.12.2016 - fixed error in t-identifier in case of unbalanced panel 
		   - added temporary options; fd and demean option
04.01.2017 - residuals are calculated predict, avoids error with jackknife
09.01.2017 - implemented fullsample option
		   - fixed error in variance calculation if jackknife used
		   - cov/variance from m_reg symmetric
14.01.2017 - wrapper for selectindex
----------------------------------------xtdcce113
04.03.2017 - error in F-Test Spacing
----------------------------------------xtdcce2131
May 2017   - Major overhaul including program for individual and mean group estimation 
June 2017  - fixed errors in unbalanced panel
		   - Error in naming for LR b and V matrix when xtpmgnames used
		   - removed options: cluster (correct SE for pooled), noomit (not needed), post_full (default bi and Vi are given)
		   - replaced putmata and getmata with st_view and st_data. Only necessary for touse variable. Requires mm_which2 (xtdcce2 version).
		   - options changed fill into showindividual and fulliv into e_ivreg2 changed. 
05.07.2017 - added endogenous(), exogenous() and residual() for legacy	
----------------------------------------xtdcce2132	   
07.07.2017 - bug in max. number of groups fixed. 
18.07.2017 - bug if unbalanced panel and predict used fixed. Removed "tsfill".
20.07.2017 - fixed bug in predict command. replaced subinstr with subinword for var changes.
22.07.2017 - check for rank condition enabled
15.08.2017 - bug in xtdcce_m_meangroup fixed.
20.08.2017 - added option fast, does not calculate individual covariances.
20.09.2017 - bug if option showi used fixed. sd, sd_i, t and t_i now as row vectors.
10.10.2017 - bug if ecm mg, but rest of lr pooled. see lr_with_pooled_ecm file
		   - bug if pooled used, no F and R2 are shown.
----------------------------------------xtdcce2133
16.10.2017 - bug with if statements fixed.  
31.10.2017 - "Degrees of freedom per country" into "Degrees of freedom per cross-sectional unit" 
02.11.2017 - bug with cr(_all) removed (strlower disabled) and option noomoit added again.
16.01.2018 - added xtset2 (N_g now set in beginning)
		   - replaced var[_n-1] with L.var for calculation of RMA method
		   - corrected calculation of time identifier
Jan - February
		   - added mata_varlist, matrix with detailed information about variables
		   - added lr(ardl) function; changes to xtdcce_m_lrcalc and xtdcce_m_reg
		   - new procedures for processing variable names
		   - cr_lags() supports now variable specific lags
14.02.2018 - error in covariance with delta method fixed. off diagonals should have no minus, diagonal should
		   - delta method for lr(ardl) implemented
19.02.2018 - bug in output fixed. wrong results for b_i and cov_i shown when IV + LR and full option
03.04.2018 - in m_reg for mg all (option 3), sd_i, t_i and stats are new tempname, otherwise they will be overwritten if pooled vars used.
21.08.2018 - output adjusted to Stata Journal version
28.08.2018 - check if dependend variable occurs multiple times on rhs.
29.08.2018 - added lr_pooled and lr_mg to hidden output for predict
----------------------------------------xtdcce2134
26.10.2018 - fixed bug that mean group variables are not showen for CS ARDL model
		   - renamed p and estat to xtdcce2_p and xtdcce2_estat rather than including the version.
		   - removed capture program drop for program definitions
----------------------------------------xtdcce2135/xtdcce2 2.0
23.01.2019 - added option "nodimcheck" to bypass dimension checks. 
25.01.2019 - added program xtdcce_m_touseupdate for a more efficient and quicker way to update touse after restore and preserve
		   - blockdiaguse option for use of block diagonal matrix in m_reg program (thanks to Achim Ahrens!)
		   - fixed bug in jackknife in combination with if (thanks to Collin Rabe). Changed calculation of jackknife split time point.
		   - capture around tsfill
13.02.2019 - fixed bug in alterantive for blockdiag use
14.02.2019 - fixed bug in jackknife, see https://github.com/JanDitzen/xtdcce2/issues/1
		   - added option trace instead of noi
21.02.2019 - fixed bug if binary variable and no reportconstant is used, partialling out can fail. if fails, then xtdcce2 restarts but does not partial the constant out	   
07.03.2019 - fixed bug if "if" used on panel ids. In old version the partialling out was done on the wrong units.
03.06.2019 - fixed bug in T. SSR and SSE were mixed up.
		   - added option pooledvce(wpn) for westerlund et al standard errors for pooled regression with fixed T.
10.06.2019 - added R2 for pooled and mg regressions. 
		   - t-statistic in mg_reg was 1/t
28.06.2019 - new program for matrix inversion and solver. 
13.07.2019 - added xtcse2 for estimation of alpha with option exponent
----------------------------------------xtdcce2 2.01
19.07.2019 - added options residuals for xtcse2, passthrough options for xtcse2
30.07.2019 - changed mm_which2 to xtdcce2_mm_which2
01.08.2019 - support for factor variables enabled again
22.08.2019 - auxiliary programs moved out, use findfile to find auxiliary.ado
11.10.2019 - error in calculation for cross-section unit specific standard errors 
fixed. was before assuming same s2 for all csu
----------------------------------------xtdcce2 2.1
02.12.2019 - error if mixed models used fixed
03.12.2019 - pooled and ardl works. 
20.12.2019 - added nominus options for ARDL in lr_options. this is essentially an ECM, but with features of the ARDL (SE and sum of LR)
19.02.2020 - bug in predict program fixed if option nodivide was used
15.06.2020 - replaced selectindex with xtdcce_selectindex
08.07.2020 - bug with if/in and new CSA syntax corrected
24.07.2020 - moved option showomitted to estat.
		   - added cluster csa and global csa.
03.10.2020 - if option jackknife used, check added if both halfs have the same number of cross-sectional units.
*/

program define xtdcce221 , eclass sortpreserve
	** Stata Version check - version > 11.1 needed for putmata commands
	if `c(version)' < 11.1 {
		di in gr "xtdcce2 requires version 11.1 or higher."
		exit
	}
	version 11.1
	local xtdcce2_version = 2.1
	if replay() {
		syntax [, VERsion replay * ] 
		if "`version'" != "" {
			di in gr "`xtdcce2_version'"
			*ereturn clear
			ereturn local version `xtdcce2_version'
			exit
		}
		if "`replay'" != "" {
		
		}
	}	
	else {
		syntax anything [if/] [in/] , [  /* 
			*/ Pooled(string) /*
			 lr(varlist ts fv) 
			*/ lr(string)/*
			*/ CRosssectional(string) /*
			*/ GLOBALCRosssectional(string) /*
			*/ CLUSTERCRosssectional(string) /*
			*/ NOCROSSsectional /*
			*/ cr_lags(string) /*
			*/ lr_options(string) /*
			*/ IVREG2options(string)  /*
			*/ POOLEDConstant /*
			*/ REPORTConstant /*
			*/ NOCONSTant /*
			*/ pooledvce(string) /*
			*/ full /* keep for legacy, replaced by showindividual
			*/ SHOWIndividual /*
			*/ nocd /*
			*/ EXPOnent /*
			*/ XTCSE2options(string asis) /* options for xtcse2
			*/ NOIsily /*
			*/ trace /* nondocumented, for checking purpose only
			*/ trend /*
			*/ POOLEDTrend /*
			*/ fulliv  /* keep for legacy, replaced by e_ivreg2
			*/ e_ivreg2 /*
			*/ JACKknife RECursive fullsample /*
			*/ ivslow  /*
			*/ fast /*
			*/ BLOCKDIAGuse /* Use block diagonal rather than own routine. Much slower!
			*/ NODIMcheck /* time dimension check
			*/ NOOMITted  /* option included again for omitting omitted variable tests.
			*/ useqr useinvsym /* use qrinversion rather than invsym; or use invsym rather than cholinv
			*/ showomitted /* detailed overview of omitted variables
			For Legacy:
			*/ EXOgenous_vars(varlist ts fv) ENDOgenous_vars(varlist ts fv) RESiduals(string) /*
			Working options: */ oldrestore demean demeant demeanid  Weight(string)  xtdcceold ]
				
		local xtdcce2v xtdcce221
		local cmd_line `xtdcce2v' `0'
		
		** save if and in
		local cmd_if `if'
		local cmd_in `in'
		
		* Legacy Locals
		if "`e_ivreg2'" != "" {
			local fulliv "fulliv"
		}
		if "`showindividual'" != "" {
			local full "full"
		}
		
		if "`trace'" != "" {
			local tracenoi "noisily"
			noi disp "this is version 221"
		}
		
		*Legacy for IV options and 
		
		if "`exogenous_vars'`endogenous_vars'" != "" {
			tokenize "`0'" , parse(",")
			while "`3'" != "" {
				gettoken next 3: 3
				if strmatch("`next'","*(`endogenous_vars')") == 0 & strmatch("`next'","*(`exogenous_vars')") == 0 {
					local op_woiv "`op_woiv' `next'"
				}			
			}
			noi disp as text "Options 'endogenous_vars' and 'exogenous_vars' not supported since version 1.2." , _continue
			noi disp as smcl "See {help xtdcce2:help xtdcce2}."
			noi disp as text "Please run instead:"
			noi disp in smcl " {stata xtdcce2 `anything' (`endogenous_vars' = `exogenous_vars') `cmd_if' `cmd_in', `op_woiv'}"
			exit			
		}
		if "`residuals'" != "" {
			local residuals_old `residuals'
		}
		
		* alternative vce estimator for pooled covariance
		if strlower("`pooledvce'") == "wpn" {
			local pooledvce = 1
		}
		else if strlower("`pooledvce'") == "nw" {
			local pooledvce = 2
		}
		else {
			local pooledvce = 0
		}
		
		if "`xtcse2options'" != "" {
			local exponent "exponent"
		}
		
		* fast option
		if "`fast'" == "fast" {
			local fast = 1
		}
		else {
			local fast = 0
		}
		*blockdiag option
		if "`blockdiaguse'" == "" {
			local blockdiaguse = 0
		}
		else {
			local blockdiaguse = 1
		}
		*change noomitted NOOMITted
		if "`noomitted'" == ""  {
			local noomitted noomitted
			local omitted
		}
		*Check for moremata
		capture mata mm_nunique((10,1))
		if _rc != 0 {
		**check if moremata works!!
			xtdcce_err 3499  , msg("moremata not installed.") msg2("To update, from within Stata type ") msg_smcl("{stata ssc install moremata, replace :ssc install moremata, replace}"	)
		}
		
		**check if xtset2 is installed
		capture xtset2, version		
		if _rc != 0 {
			xtdcce_err 199 `d_idvar' `d_tvar' , msg("xtset2 not installed.") msg2("To update, from within Stata type ")	msg_smcl(`"{net "describe xtset2 , from(http://www.ditzen.net/Stata/) "}"')
		}
		
		** add auto check: if in lr variables have joint base, but ECM used, change to ARDL
		if "`lr'" != "" & strmatch("`lr_options'","*forceecm*") == 0 & strmatch("`lr_options'","*ardl*") == 0 {
			qui tsrevar `lr', list
			tsunab lrunab : `lr'
			if wordcount("`r(varlist)'") != wordcount("`lrunab'") {
				disp ""
				disp "Multiple variables per base in long run vector detected. Changed to ARDL with option nominus to allow for single long run coefficient for variables with same base. Use option forceecm to prevent behaviour."
				disp ""
				local lr_options "ardl nominus"
			}
		}
		
		** check if lr_options are ok. if ardl used, no nodivide and xtpmgnames cannot be used
		if strmatch("`lr_options'","*ardl*") == 1 & (strmatch("`lr_options'","*nodivide*") == 1 | strmatch("`lr_options'","*xtpmgnames*") == 1)  {
			xtdcce_err 184 `d_idvar' `d_tvar' , msg("options ardl and xtpmgnames or nodivide may not be combined.")			
		}
		
		** check if lr_options are ok. if ecm used, nominus cannoot be used
		if strmatch("`lr_options'","*ardl*") == 0 & (strmatch("`lr_options'","*nominus*") == 1 )  {
			xtdcce_err 184 `d_idvar' `d_tvar' , msg("options ecm (default) and nominus may not be combined.")			
		}
		
		** Which inverter to use 
		if "`useqr'" != "" & "`useinvsym'" != "" {
			xtdcce_err 184 `d_idvar' `d_tvar' , msg("options useqr and useinvsym may not be combined.")	
		}
		if "`useqr'" == "" {
			local useqr = 0
		}
		else {
			local useqr = 1
		}
		if "`useinvsym'" != "" {
			local useqr = 2
		}
		
		qui{		
			tempname m_idt
			tempvar id_t tvar idvar
			local mata_drop `m_idt'
			xtset2
			local d_idvar  `r(panelvar)'
			local d_tvar  `r(timevar)'
			local d_balanced  `r(balanced)'
		
			
			* create in program indicators for time and unit variable	
			egen `idvar' = group(`d_idvar')	
			
			*fill dataset to make sure gaps and missings are accounted for,
			*then create tvar. Important for all types of datasets if time identifier
			*is in date format.
			tempvar inital_touse
			gen `inital_touse' = 1
			capture tsfill, full
			if _rc == 0 {
				egen `tvar' = group(`d_tvar')
				keep if `inital_touse' == 1 
				drop `inital_touse'	
				`tracenoi' disp "Panel balanced."
			}
			else {
				xtdcce_err 199 `d_idvar' `d_tvar' , msg("Cannot balance panel. Please make sure neither `d_idvar' nor `d_tvar' contain missings.")
			}
			sort `idvar' `tvar'
			gen `id_t' = _n

			preserve
				marksample touse	
				tempvar touse_start
				gen `touse_start' = `touse'
				tsset `idvar' `tvar'
				* save lr for later use
				local lr_save "`lr'"
				
				***Assign lhs, rhs, exo and endo vars
				gettoken lhs 0 : anything
				while "`0'" != "" {
					gettoken next 0 : 0 , match(paren)
					if strmatch("`next'","*=*") == 1 {
						tokenize `next' , parse("*=*")
						local endogenous_vars `endogenous_vars' `1'
						local exogenous_vars `exogenous_vars' `3'
					}
					else {
						local rhs `rhs' `next'
					}
				}				
				* process pooled options
				if strmatch("`pooled'","*_all*") == 1 {
					local uniq  `rhs' `exogenous_vars' `endogenous_vars' `lr' 
					local pooled : list uniq uniq 
				}
				
				*process crosssectional options
				
			*	clustercrosssectional globalcrosssectional
				
				if "`crosssectional'`nocrosssectional'`clustercrosssectional'`globalcrosssectional'" == "" {
					xtdcce_err 198  , msg("option (cluster-|global-|)crosssectional() or nocrosssectional required")
				}
				else {
					if "`crosssectional'" == "" | strmatch("`crosssectional'","*_none*") == 1 {
						local nocrosssectional "nocrosssectional"
						local crosssectional ""
					}
					else if "`nocrosssectional'" == "nocrosssectional" {
						local crosssectional ""
					}
					else if strmatch("`crosssectional'`globalcrosssectional'`clustercrosssectional'","*_all*") == 1 {
						*** only use base of ts variables, omit fv variables
						fvexpand `lhs' `rhs' `pooled' `exogenous_vars' `endogenous_vars' `lr'
						if "`r(fvops)'" == "true" {
							** build list 
							foreach var in `lhs' `rhs' `pooled' `exogenous_vars' `endogenous_vars' `lr' {
								fvexpand `var'
								if "`r(fvops)'" != "true" {
									local tmplist `tmplist' `var'
								}
							}
							tsrevar `tmplist', list
							local uniq `r(varlist)'
							local uniq : list uniq uniq 
							local crosssectional = subinstr("`crosssectional'","_all","`uniq'",.)
							local globalcrosssectional = subinstr("`globalcrosssectional'","_all","`uniq'",.)
							local clustercrosssectional = subinstr("`clustercrosssectional'","_all","`uniq'",.)
						}
						else {
							*** check for factor variables
							tsrevar `lhs' `rhs' `pooled' `exogenous_vars' `endogenous_vars' `lr' , list
							local uniq `r(varlist)'
							local uniq : list uniq uniq 
							local crosssectional = subinstr("`crosssectional'","_all","`uniq'",.)
							local globalcrosssectional = subinstr("`globalcrosssectional'","_all","`uniq'",.)
							local clustercrosssectional = subinstr("`clustercrosssectional'","_all","`uniq'",.)
						}
					}
					
					
					if "`crosssectional'`globalcrosssectional'`clustercrosssectional'" != "" {
						
						if "`crosssectional'" != "" {
							local scrosssectional "`crosssectional'"
							local scr_lags "`cr_lags'"
							
							if "`cr_lags'" == "" {
								local 0 `crosssectional'
								syntax varlist(ts) , [cr_lags(numlist)]
								local scrosssectional `varlist'
							}
							if "`cr_lags'" == "" {
								local scr_lags = 0
							}
							else {
								local scr_lags `cr_lags'					
							}
							
						}
						
						if "`globalcrosssectional'" != "" {
							local 0 `globalcrosssectional'
							syntax varlist(ts) , [cr_lags(numlist)]
							local globalcrosssectional `varlist'
							if "`cr_lags'" == "" {
								local gcr_lags = 0
							}
							else {
								local gcr_lags `cr_lags'
							}
						}
						
						if "`clustercrosssectional'" != "" {
							local 0 `clustercrosssectional'
							syntax varlist(ts) , [cr_lags(numlist) CLustercr(varlist) ]
							local clustercrosssectional `varlist'
							if "`clustercr'" == "" {
								xtdcce_err 198 `d_idvar' `d_tvar' , msg("No clustervariable set.")
								
							}
							local csa_cluster "`clustercr'"
							if "`cr_lags'" == "" {
								local ccr_lags = 0
							}
							else {
								local ccr_lags `cr_lags'
							}
						}
						
						local crosssectional "`scrosssectional' `globalcrosssectional' `clustercrosssectional'"
						
						
						tempname cross_mat
						local mata_drop `mata_drop' `cross_mat'
						mata `cross_mat' = J(0,2,"")
						
						local n_cr = wordcount("`scrosssectional'")
						local n_crl = wordcount("`scr_lags'")

						forvalues i = 1(1)`n_cr' {
							local tmpcr = word("`scrosssectional'",`i')
							if `i' <= `n_crl' {
								local tmpcrl = word("`scr_lags'",`i')
							}
							fvunab tmpcr : `tmpcr'
							
							mata `cross_mat' = (`cross_mat' \ (tokens("`tmpcr'")' , J(cols(tokens("`tmpcr'")),1,"`tmpcrl'")))
						}
						
						local n_cr = wordcount("`globalcrosssectional'")
						local n_crl = wordcount("`gcr_lags'")

						forvalues i = 1(1)`n_cr' {
							local tmpcr = word("`globalcrosssectional'",`i')
							if `i' <= `n_crl' {
								local tmpcrl = word("`gcr_lags'",`i')
							}
							fvunab tmpcr : `tmpcr'
							
							mata `cross_mat' = (`cross_mat' \ (tokens("`tmpcr'")' , J(cols(tokens("`tmpcr'")),1,"`tmpcrl'")))
						}
						
						local n_cr = wordcount("`clustercrosssectional'")
						local n_crl = wordcount("`ccr_lags'")

						forvalues i = 1(1)`n_cr' {
							local tmpcr = word("`clustercrosssectional'",`i')
							if `i' <= `n_crl' {
								local tmpcrl = word("`ccr_lags'",`i')
							}
							fvunab tmpcr : `tmpcr'
							
							mata `cross_mat' = (`cross_mat' \ (tokens("`tmpcr'")' , J(cols(tokens("`tmpcr'")),1,"`tmpcrl'")))
						}
						
						
						** check for duplicates
						*mata st_local("dupcheck",strofreal(rows(uniqrows(`cross_mat'[.,1])):==rows(`cross_mat')))
						*if `dupcheck' == 0 {
						*	xtdcce_err 198  , msg("Variables cannot appear in more than cross-sectional average list.")							
						*}
						
						mata st_local("cr_lags_min",strofreal(min(strtoreal(`cross_mat'[.,2]))))
						mata st_local("cr_lags_max",strofreal(max(strtoreal(`cross_mat'[.,2]))))
					}					
				}				

				
				
				*Check for IV and if ivreg2 is installed
				if "`exogenous_vars'" != "" & "`endogenous_vars'" != "" {
					local IV = 1
					capture ivreg2, version
					if _rc != 0 {
						restore					
						xtdcce_err 199 `d_idvar' `d_tvar' , msg("ivreg2 not installed.") msg2("To update, from within Stata type ")	msg_smcl("{stata ssc install ivreg2, replace :ssc install ivreg2, replace}")			
					}
					
				}
				else {
					local endogenous_vars ""
					local exogenous_vars ""
					local IV = 0
				}
				*Check for bias correction
				if "`jackknife'" == "jackknife" & "`recursive'" == "recursive" {
					restore
					xtdcce_err 184 `d_idvar' `d_tvar' , msg("options jackknife and recursive may not be combined.")	
				}
				
				*Check for clustered SE
				*if "`cluster'" != "" {
				*	local cluster "cluster(`cluster')"
				*}
						
				*Check for long run coefficients
				if "`lr'" == "" {
					local lr_options ""
				}	
				local ardl_indic = 0
				if strmatch("`lr_options'","*ardl*") == 1 {
					local ardl_indic = 1
				}
				
				*markout varlists not defined as varlist in syntax
				markout `touse' `lhs' `rhs' `exogenous_vars' `endogenous_vars' `crosssectional' `pooled' `lr'
				xtset2 if `touse'
				local N_g = `r(N_g)'
				local N = `r(N)'
				
				*** show message for "large" panels
				if `N_g' > 500 | `N' > 50000 {
					noi disp ""
					noi disp "Large number of observations, xtdcce2 might be very slow and problems occur if maximum of matrix size is reached."
					noi disp as smcl "Consider the use of {help xtdcce2fast} instead of {help xtdcce2}."
					noi disp ""
				}
				
				*** make sure all var lists contain unique elements
				gettoken lr_1 lr_rest : lr , match(paren)
				foreach varl in rhs exogenous_vars endogenous_vars crosssectional pooled lr_1 lr_rest {
					local `varl' : list uniq `varl'
				}
				**get lr without paren:
				local rest `lr'
				while "`rest'" != "" {
					gettoken next rest : rest , match(paren)
					local lr_wop `lr_wop' `next'
				}
				*** unab ts and fv vars
				local all_vars "`pooled' `crosssectional' `exogenous_vars' `endogenous_vars' `lr_wop' `lhs' `rhs'"
				
				fvunab all_vars : `all_vars'
				local all_vars : list uniq all_vars
				local n_vars : list sizeof all_vars
				
				tempname mata_varlist
				local mata_drop `mata_drop' `mata_varlist'
				mata `mata_varlist' = (J(1,1,tokens("`all_vars'")') , J(`n_vars',11,"0"))	
				
				**change var lists such that all vars only included once (with exception for crosssectional)		
				**add indics
				local i = 3
				*				3   4    5      6				7    			8				9  10
				foreach list in lhs rhs pooled crosssectional exogenous_vars endogenous_vars lr_1 lr_rest  {
					if "``list''" != "" {
						fvunab tmp_list : ``list''
						tempname mm_which_n
						mata `mm_which_n' = xtdcce2_mm_which2(`mata_varlist'[.,1],tokens("`tmp_list'"))
						mata `mata_varlist'[`mm_which_n',`i'] = J(rows(`mm_which_n'),1,"1")
						mata mata drop `mm_which_n'
					}
					local i = `i' + 1
				}
				mata st_local("full_list",invtokens(`mata_varlist'[.,1]'))
				
				**ts and fv check
				foreach var in `full_list' {
					fvexpand `var' 
					if "`r(tsops)'" == "true" {
						tsrevar `var'
						local new_names `r(varlist)'
												
						tsunab change : `var'
						
						**insert list in col 2:
						mata `mata_varlist'[ xtdcce2_mm_which2(`mata_varlist'[.,1] , tokens("`change'")),2] = tokens("`new_names'")'
						
					}
					else if "`r(fvops)'" == "true" {
						
						**factor variables
						fvexpand `var'
							local fvname "`r(varlist)'"
							*** get base variable from mata_varlist, then create factor var and copy over
							tempname factorbase factorbasei factorbase_i
							mata `factorbasei' = xtdcce_selectindex(`mata_varlist'[.,1]:=="`var'")
							mata `factorbase_i' = xtdcce_selectindex(`mata_varlist'[.,1]:!="`var'")
							mata `factorbase' = `mata_varlist'[`factorbasei',.]
							** remove variable base name
							mata `mata_varlist'=`mata_varlist'[`factorbase_i',.]
							
							fvrevar `var'
							local fvrevar_tmpnames "`r(varlist)'"
							
							*** remove first var from both as it is base variable
							local num_fv = wordcount("`fvrevar_tmpnames'")-1						
							gettoken first fvname: fvname
							gettoken first fvrevar_tmpnames: fvrevar_tmpnames
							drop `first'
														
							mata `factorbase' = J(`num_fv',1,`factorbase')
							mata `factorbase'[.,1] = tokens("`fvname'")'
							mata `factorbase'[.,2] = tokens("`fvrevar_tmpnames'")'
							mata `mata_varlist' = `mata_varlist' \ `factorbase'
							
							mata mata drop `factorbasei' `factorbase' `factorbase_i'
							
							local nodimcheck nodimcheck
						
					}
				}
				
				**get those with more than 24 string char
				mata st_local("list_long",invtokens(`mata_varlist'[xtdcce_selectindex(strlen(`mata_varlist'[.,2]):>23),2]'))
				local i = 1
				foreach var in `list_long' {
					tempname short_`i'
					rename `var' `short_`i''
					mata `mata_varlist'[xtdcce_selectindex(`mata_varlist'[.,1]:=="`var'"),2] = "`short_`i''"
				}			
				
				*process lr list
				local rest "`lr'"
				tempname mata_indic
				while "`rest'" != "" {
					gettoken next rest : rest , match(paren)
					tsunab next: `next'
					mata `mata_indic' = xtdcce2_mm_which2(`mata_varlist'[.,1],tokens("`next'"))
					**get base as identifiers for lags
					tsrevar `next', list
					**use only first base (in case different var names identify same base (eg> y d_y)
					mata `mata_varlist'[`mata_indic',12] = J(rows(`mata_indic'),1,"`=word("`r(varlist)'",1)'")
				}
				capture mata mata drop `mata_indic'
				
				
				** make all other vars tempnames
				mata st_local("varsToChange",strofreal(rows(`mata_varlist')))
				forvalues s = 1(1)`varsToChange' {
					mata st_local("VarTemp",`mata_varlist'[`s',2])
					if "`VarTemp'" == "0" {
						mata st_local("VarToChange",`mata_varlist'[`s',1])
						tempname short_`VarToChange'
						rename `VarToChange' `short_`VarToChange''
						mata `mata_varlist'[`s',2] = "`short_`VarToChange''"
					}
				}			
				
				**change varlists
				local i = 3
				foreach list in lhs rhs pooled crosssectional exogenous_vars endogenous_vars lr_1 lr_rest  {
					mata st_local("`list'",invtokens(`mata_varlist'[xtdcce_selectindex(`mata_varlist'[.,`i']:=="1"),2]'))
					local i = `i' + 1
				}
				local lr `lr_1' `lr_rest'		
				
				* all variables as doubles
				recast double `lhs' `rhs' `crosssectional' `pooled' `exogenous_vars' `endogenous_vars' `lr'
				
				*Remove pooled variables from rhs
				local rhs : list rhs - pooled
				local rhs : list rhs - endogenous_vars
				local rhs : list rhs - exogenous_vars
				
				*Identify LR not in pooled, endogenous or exogenous vars
				local lr_single: list lr - pooled
				local lr_single: list lr_single - rhs
				local lr_single: list lr_single - endogenous_vars
				local lr_single: list lr_single - exogenous_vars
				
				local rhs `rhs' `lr_single'
				
				local endo_pooled: list endogenous_vars & pooled
				local exo_pooled: list exogenous_vars & pooled
				
				local endogenous_vars : list endogenous_vars - endo_pooled
				local exogenous_vars : list exogenous_vars - exo_pooled
				
				local pooled: list pooled - endo_pooled
				local pooled: list pooled - exo_pooled
			
				sort `idvar' `tvar'				
				
				**Recursive Mean adjustment - before constant and trends are added
				if "`recursive'" == "recursive" {
					tempvar s_mean
					gen double `s_mean' = .
					local r_varlist `lhs' `rhs' `pooled' `crosssectional' `endogenous_vars' `exogenous_vars' `endo_pooled' `exo_pooled'
					local r_varlist: list uniq r_varlist
					*noi disp "cons `constant'"
					*local r_varlist: list r_varlist - constant
					
					foreach var in `r_varlist' {
						by `idvar' (`tvar'), sort: replace `s_mean' = sum(L.`var') / (_n-1) if `touse' & L.`var' != .
						replace `var' = `var' - `s_mean'
						replace `s_mean' = .						
					}					
					sort `idvar' `tvar'
					
					markout `touse' `lhs' `rhs' `pooled' `crosssectional' `endogenous_vars' `exogenous_vars' `endo_pooled' `exo_pooled'
				}				
				
				*Add trend
				if "`pooledtrend'" != "" | "`trend'" != "" {
					tempvar trendv
					sum `d_tvar'
					gen double `trendv' = `tvar'
					
					mata `mata_varlist'  = (`mata_varlist'  \ ("trend" , "`trendv'", J(1,10,"0")))
					
					if "`pooledtrend'" == "" & "`trend'" != "" {
						local rhs `rhs' `trendv' 
						mata `mata_varlist'[xtdcce2_mm_which2(`mata_varlist'[.,1],"trend"),4] = "1"
					}
					else if "`pooledtrend'" != "" & "`trend'" == "" {
						local pooled `pooled' `trendv'
						mata `mata_varlist'[xtdcce2_mm_which2(`mata_varlist'[.,1],"trend"),5] = "1"
					}
					else {
						restore
						xtdcce_err 184 `d_idvar' `d_tvar' , msg("options trend and pooledtrend may not be combined.")	
					}
				}				
				
				/*
				Constant/Intercept block
				if reportconstant is switched on, constant will be reported, 
				otherwise it will be partialled out (if poolconstant not defined) 
				or all variables will be demeaned to remove homogenous constant
				 Types of constant: 
					0 no constant
					1 heterogenous & partialled out
					2 homogenous (pooled) & removed (set to zero) as it is zero (only in case of balanced panel)
					3 heterogenous & displayed
					4 homogenous & not displayed (calculated but supressed)
					5 homogenous & displayed
				*/
				if "`noconstant'" == "" {
					tempvar constant
					
					mata `mata_varlist'  = (`mata_varlist'  \ ("_cons" , "`constant'", J(1,10,"0")))
					
					if "`reportconstant'" != "reportconstant" {
						if "`pooledconstant'" != "pooledconstant" {							
							gen double `constant' = 1	
							local constant_type = 1
							*mata `mata_varlist'[xtdcce2_mm_which2(`mata_varlist'[.,1],"_cons"),4] = "1"
						}
						else if "`pooledconstant'" == "pooledconstant" {
							*Check if all vars are in crosssectional mean, if so, no constant needed
							local lhsrhs `lhs' `rhs' `pooled' `endogenous_vars' `endo_pooled' 
							local const_check : list lhsrhs - crosssectional
							if "`d_balanced'" == "strongly balanced" & "`rhs'`endogenous_vars'" == "" & "`const_check'" == "" {
								local constant_type = 2	
								local noconstant noconstant					
							}
							else {
								*tempvar constant 
								gen double `constant' = 1
								local pooled `pooled' `constant'
								local constant_type = 4 
								mata `mata_varlist'[xtdcce2_mm_which2(`mata_varlist'[.,1],"_cons"),5] = "1"
							}
						}
						local noconstant_reg noconstant
					}
					else if "`reportconstant'" == "reportconstant" {
						*tempvar constant 
						gen double `constant' = 1
						local noconstant_reg noconstant
						if "`pooledconstant'" == "pooledconstant" {
							local pooled `pooled' `constant'
							local constant_type = 5
							mata `mata_varlist'[xtdcce2_mm_which2(`mata_varlist'[.,1],"_cons"),5] = "1"
						}
						else if "`pooledconstant'" != "pooledconstant" {
							*constant not pooled
							local rhs `rhs' `constant'
							local constant_type = 3 
							mata `mata_varlist'[xtdcce2_mm_which2(`mata_varlist'[.,1],"_cons"),4] = "1"
						}
						** add constant if report constant and long run
						if "`lr'" != "" {
							mata `mata_varlist'[xtdcce2_mm_which2(`mata_varlist'[.,1],"_cons"),10] = "1"
							** add base
							mata `mata_varlist'[xtdcce2_mm_which2(`mata_varlist'[.,1],"_cons"),12] = "_cons"
							local lr "`lr' `constant'"
						}
					}
				}
				else {
					local noconstant_reg noconstant
					local constant_type = 0
				}	
			
				if "`demeant'`demeanid'`fd'`demean'" != "" {
					local r_varlist `lhs' `rhs' `pooled' `crosssectional' `endogenous_vars' `exogenous_vars' `endo_pooled' `exo_pooled'
					local r_varlist: list uniq r_varlist
					local r_varlist: list r_varlist - constant
					if "`demeant'`demeanid'`demean'" != "" {
						noi disp "Data is demeaned with `demeant'`demeanid'`demean'"	
						foreach var in `r_varlist' {
							tempvar id_mean t_mean
							if "`demean'" != "" {
								egen double `id_mean' = mean(`var') if `touse'
								replace `var' = `var' - `id_mean' if `touse'
								drop `id_mean'
							}							
							if "`demeanid'" != "" {
								by `idvar' (`tvar') , sort: egen double `id_mean' = mean(`var') if `touse'
								replace `var' = `var' - `id_mean' if `touse'
								drop `id_mean'
							}
							if "`demeant'" != "" {
								by `tvar' (`idvar') , sort: egen double `t_mean' = mean(`var') if `touse'
								replace `var' = `var' - `t_mean' if `touse'
								drop `t_mean'
							}	
							
						}
					}

				}			
				
				**add lags to var matrix		
				if "`crosssectional'" != "" {
					mata `mata_varlist'[xtdcce2_mm_which2(`mata_varlist'[.,1],`cross_mat'[.,1]),11] = `cross_mat'[.,2]
					mata mata drop `cross_mat'
				}
							
				*Number of coefficients
				mata `mata_varlist'2 = strtoreal(`mata_varlist'[.,(3..cols(`mata_varlist'))])
				mata st_local("num_rhs",strofreal(`N_g'*sum(((`mata_varlist'2[.,3]:==0):*`mata_varlist'2[.,2]))))
				mata st_local("num_pooled",strofreal(sum(`mata_varlist'2[.,3])))
				mata st_local("num_lr_np",strofreal(`N_g'*sum(((`mata_varlist'2[.,3]:==0):*(`mata_varlist'2[.,7]+`mata_varlist'2[.,8])))))
				mata st_local("num_exo_np",strofreal(`N_g'*sum(((`mata_varlist'2[.,3]:==0):*`mata_varlist'2[.,5]))))
				mata st_local("num_endo_np",strofreal(`N_g'*sum(((`mata_varlist'2[.,3]:==0):*`mata_varlist'2[.,6]))))
				mata st_local("num_crosssectional",strofreal(`N_g'*sum((`mata_varlist'2[.,4]:*(`mata_varlist'2[.,9]:+1)))))
				
				** correct number of rhs if constant is in lr and rhs list
				if "`lr'" != "" & `constant_type' > 0 {
					mata st_local("constant_lr",strofreal((`mata_varlist'2[xtdcce_selectindex(`mata_varlist'[.,1]:=="_cons"),2]:==1):*(`mata_varlist'2[xtdcce_selectindex(`mata_varlist'[.,1]:=="_cons"),8]:==1)))
					
					if `constant_lr' == 1 {
						local num_rhs = `num_rhs' - `N_g'
					}
				}
				mata mata drop `mata_varlist'2
				local num_mg_regression =  `num_rhs'  + `num_lr_np' + `num_exo_np' + `num_endo_np'
				local num_K = `num_mg_regression' + `num_pooled' 				
				**add constant here
				local num_partialled_out = `num_crosssectional' + `N_g' * (`=`constant_type'==1')
				local K_total = `num_K' + `num_partialled_out'

				***Check if enough observations per cross sectional unit. If not, then remove unit and display message.	
				if "`nodimcheck'" == "" {

					`noi' xtset2 if `touse' , matrix					
					tempname PanelMatrix					
					mata `PanelMatrix' = (st_matrix("r(PanelMatrix)"))[.,(1,2)]
					mata `PanelMatrix'[.,2] = (`PanelMatrix'[.,2] :+ (-`num_pooled' - `num_mg_regression' / `N_g' - `num_partialled_out' / `N_g'))
					mata `PanelMatrix' = `PanelMatrix'[xtdcce_selectindex(`PanelMatrix'[.,2]:<1),.]
					mata strofreal(rows(`PanelMatrix'))
					mata st_local("NUnitsToRemove",strofreal(rows(`PanelMatrix'))) 
					
					if `NUnitsToRemove' > 0 {
						forvalues i = 1(1)`NUnitsToRemove' {
							mata st_local("iUnitToRemove",strofreal(`PanelMatrix'[`i',1]))
							replace `touse' = 0 if (`idvar' == `iUnitToRemove')
							sum `d_idvar' if `idvar' == `iUnitToRemove'
							local SummaryToRemove "`SummaryToRemove' `r(mean)'"
							*local N_g = `N_g' - 1
						}
						noi disp "Units (`d_idvar') to be removed due to insufficient numbers of observations: `SummaryToRemove'"
						
						*** check if touse has non zero
						sum `touse' , meanonly
						if r(mean) == 0 {
							restore
							xtdcce_err 2001 `d_idvar' `d_tvar' , msg("No observations left.")
						}
						
						**refresh stats
						drop `idvar'
						egen `idvar' = group(`d_idvar') if `touse'
						xtset2 `idvar' `tvar' if `touse'
						local N_g = `r(N_g)'
						local N = `r(N)'
						local d_balanced  `r(balanced)'
						
						mata `mata_varlist'2 = strtoreal(`mata_varlist'[.,(3..cols(`mata_varlist'))])
						mata st_local("num_rhs",strofreal(`N_g'*sum(((`mata_varlist'2[.,3]:==0):*`mata_varlist'2[.,2]))))
						mata st_local("num_pooled",strofreal(sum(`mata_varlist'2[.,3])))
						mata st_local("num_lr_np",strofreal(`N_g'*sum(((`mata_varlist'2[.,3]:==0):*(`mata_varlist'2[.,7]+`mata_varlist'2[.,8])))))
						mata st_local("num_exo_np",strofreal(`N_g'*sum(((`mata_varlist'2[.,3]:==0):*`mata_varlist'2[.,5]))))
						mata st_local("num_endo_np",strofreal(`N_g'*sum(((`mata_varlist'2[.,3]:==0):*`mata_varlist'2[.,6]))))
						mata st_local("num_crosssectional",strofreal(`N_g'*sum((`mata_varlist'2[.,4]:*(`mata_varlist'2[.,9]:+1)))))
						mata mata drop `mata_varlist'2
						
						local num_mg_regression =  `num_rhs'  + `num_lr_np' + `num_exo_np' + `num_endo_np'
						local num_K = `num_mg_regression' + `num_pooled' 				
					
						local num_partialled_out = `num_crosssectional' + `N_g' * (`=`constant_type'==1')
						local K_total = `num_K' + `num_partialled_out'
						noi disp "New dimension are: N_g=`N_g', T=``N'/`N_g'' with `K_total' number of regressors."
					}
					mata mata drop `PanelMatrix'
				}
				if `N' < `K_total' {
					restore
					xtdcce_err 2001 `d_idvar' `d_tvar' , msg("More variables (`K_total') than observations (`N').")
				}
								
				**Only working, for old results (<version 1.1)
				if "`xtdcceold'" == "xtdcceold" {
					if "`cr_lags'" > "0" {
						local cr_lags = words("`cr_lags'",1)
						replace `touse' = 0 if `tvar' <= `cr_lags' 
					}
				}

*******************************************************************************************************		
********************** Calculation of CSA
*******************************************************************************************************					
				***Specify sample - if fullsample then ignore touse
				tempvar tousecr
				
				if "`fullsample'" != "" {
					*gen `tousecr' = `touse_start'
					if "`cmd_if'" != "" & "`cmd_in'" == ""  {
						gen `tousecr' = (`cmd_if') 
					}
					else if "`cmd_in'" != "" & "`cmd_if'" == "" {
						gen `tousecr' = `touse' in `cmd_in'
					}
					else if "`cmd_in'" != "" & "`cmd_if'" != "" {
						replace `tousecr' = (`cmd_if') in `cmd_in'
					}
					else {
					    gen `tousecr' = 1						
					}
					*noi disp "`ifinct'"
					*gen `tousecr' = (`cmd_if') `ifinct'
					
				}
				else {
					gen `tousecr' = `touse'
				}
				
				*create CR Lags
				if "`crosssectional'" != "" {					
					if "`scrosssectional'" != "" {						
						mata st_local("scrosssectionalt",invtokens(`mata_varlist'[xtdcce2_mm_which2(`mata_varlist'[.,1],(tokens("`scrosssectional'"))'),2]'))
						tempname scsa
												
						xtdcce2_csa `scrosssectionalt' , idvar(`idvar') tvar(`tvar') cr_lags(`scr_lags') touse(`tousecr') csa(`scsa')  numberonly tousets(`touse')
						local scsa `r(varlist)'	
						local cr_lags "`r(cross_structure)'"
					}
					
					if "`globalcrosssectional'" != "" {
						tempvar touseglobal
						gen `touseglobal' = 1
						mata st_local("globalcrosssectionalt",invtokens(`mata_varlist'[xtdcce2_mm_which2(`mata_varlist'[.,1],(tokens("`globalcrosssectional'"))'),2]'))
						
						tempname gcsa
						xtdcce2_csa `globalcrosssectionalt' , idvar(`idvar') tvar(`tvar') cr_lags(`gcr_lags') touse(`touseglobal') csa(`gcsa')  numberonly
						local gcsa `r(varlist)'	
						local gcr_lags "`r(cross_structure)'"
						drop `touseglobal'
					}
					
					if "`clustercrosssectional'" != "" {
						
						mata st_local("clustercrosssectionalt",invtokens(`mata_varlist'[xtdcce2_mm_which2(`mata_varlist'[.,1],(tokens("`clustercrosssectional'"))'),2]'))
						
						tempname ccsa
						xtdcce2_csa `clustercrosssectionalt' , idvar(`idvar') tvar(`tvar') cr_lags(`ccr_lags') touse(`tousecr') csa(`ccsa') cluster(`csa_cluster') numberonly
						local ccsa `r(varlist)'	
						local ccr_lags "`r(cross_structure)'"
					}
					local clist1 `scsa' `gcsa' `ccsa'
										
				}
				**Add constant if heterogenous to list with variable to partialled out
				if  "`constant_type'" == "1" {
					local clist1 `clist1' `constant'
					local crosssectional `crosssectional' `constant'
				}

				local num_adjusted = `num_partialled_out' 				
				
				
				*Restrict set and exclude variables with missings (25.1.2017 added clist)
				markout `touse' `lhs' `pooled' `rhs' `exogenous_vars' `endogenous_vars' `endo_pooled' `exo_pooled' 	`clist1'
								
				** Check for omitted variables
				if "`omitted'" == "" {
					
					local omitted `lhs' `rhs'  `pooled'  `endo_pooled' 
					_rmcoll `omitted' if `touse' , noconstant 
					local omitted_N  = r(k_omitted)
					local omitted_var `r(varlist)'
					local omitted_var : list omitted - omitted_var
										
					local rhs : list rhs - omitted_var
					local pooled : list pooled - omitted_var
					local endo_pooled : list endo_pooled - omitted_var
				}
				
				** Check that lhs not in pooled, rhs, lr_1, lr_rest
				if "`fast'" == "0" {
					tempname check
					mata `check' = xtdcce2_mm_which2(`mata_varlist'[.,2],"`lhs'")
					mata st_local("checker",strofreal(rows(`check')))
					if `checker' > 1 {
						restore
						xtdcce_err 103 `d_idvar' `d_tvar' , msg("Dependent variable occurs multiple times.")
					}
					mata `check' = `mata_varlist'[`check',(4,5,7,8,9,10)]
					mata st_local("checker",strofreal(sum(strtoreal((`check')))))
					if `checker' != 0 {
						restore
						xtdcce_err 103 `d_idvar' `d_tvar' , msg("Dependent variable occurs on right hand side.")
					}
					mata mata drop `check'
				}
				
	**********************************************************************************************************
	******************************************* Jackknife settings *******************************************
	**********************************************************************************************************			
			if "`jackknife'" == "jackknife" {
				tempvar jack_indicator_a jack_indicator_b

				sum `tvar' if `touse'
				local jack_T = int((`r(max)'-`r(min)') / 2) + `r(min)'
				
				gen `jack_indicator_a' = `touse' * (`tvar' <= `jack_T')
				gen `jack_indicator_b' = `touse' * (`tvar' > `jack_T')
				
				*** check if each id in both panels occurs
				tempname jackcheck_a  jackcheck_b jackcheck_total
				mata `jackcheck_a' = uniqrows(st_data(.,"`d_idvar'","`jack_indicator_a'"))
				mata `jackcheck_b' = uniqrows(st_data(.,"`d_idvar'","`jack_indicator_b'"))
				mata `jackcheck_total' = uniqrows(st_data(.,"`d_idvar'","`touse'"))
				
				mata st_local("jackcheck",strofreal((`jackcheck_a'==`jackcheck_b'==`jackcheck_total')))
				if `jackcheck' == 0 {
					mata st_local("N1",strofreal(rows(`jackcheck_a')))
					mata st_local("N2",strofreal(rows(`jackcheck_b')))
					mata st_local("N3",strofreal(rows(`jackcheck_total')))
					
					mata st_local("jackcheck_a_list",invtokens(strofreal(`jackcheck_a'')))
					mata st_local("jackcheck_b_list",invtokens(strofreal(`jackcheck_b'')))
					mata st_local("jackcheck_t_list",invtokens(strofreal(`jackcheck_total'')))
					
					local missing_jack_a : list jackcheck_t_list - jackcheck_a_list
					local missing_jack_b : list jackcheck_t_list - jackcheck_b_list
					local missing_jack : list missing_jack_a | missing_jack_b
					
					xtdcce_err 451 `d_idvar' `d_tvar', msg("One or more cross-sectional units have no observations in one of the half panels. Missing panels are: `missing_jack'") msg2("Number of ross-sectional units are: `N3' (total), `N1' (first half), `N2' (second half), half point: `jack_T'.")
				}
			
				mata `mata_varlist' = (`mata_varlist', J(rows(`mata_varlist'),1,""))
				
				foreach var in `lhs' `pooled' `rhs' `exogenous_vars' `endogenous_vars' `endo_pooled' `exo_pooled' {
					tempvar `var'_jk
					
					gen double `var'_jk = `var'
					
					mata `mata_varlist'[xtdcce_selectindex(`mata_varlist'[.,2]:=="`var'"),13] = "`var'_jk"
					
					local jackvars "`jackvars' `var'_jk"
				}
								
			}
		*noi disp "jack vars `jackvars' - `clist1'"
		*noi disp "`lhs' `pooled' `rhs' `exogenous_vars' `endogenous_vars' `endo_pooled' `exo_pooled'"
	**********************************************************************************************************
	*******************************************  Partialling Out *********************************************
	**********************************************************************************************************
			local rank_cond = 0
			*only partial out if list with variables to partial out is not empty
			if "`clist1'" != "" {			
				tempname mrk
				local mata_drop `mata_drop' `mrk'
				
				sort `idvar' `tvar'	
				`tracenoi' mata xtdcce_m_partialout2("`lhs' `pooled' `rhs' `exogenous_vars' `endogenous_vars' `endo_pooled' `exo_pooled'","`clist1'","`touse'","`idvar'",`useqr',`mrk'=.)
				
				
				if "`jackknife'" == "jackknife" {
					tempvar touse_ctry_jack
					gen  `touse_ctry_jack' = `touse' * `jack_indicator_a'
					
					`tracenoi' mata xtdcce_m_partialout2("`jackvars'","`clist1'","`touse_ctry_jack'","`idvar'",`useqr',`mrk'=.)
					
					replace  `touse_ctry_jack' = `touse' * `jack_indicator_b'
					`tracenoi' mata xtdcce_m_partialout2("`jackvars'","`clist1'","`touse_ctry_jack'","`idvar'",`useqr',`mrk'=.)
					
					drop `touse_ctry_jack'
				}
							
				mata st_local("rank_cond",strofreal(`mrk'[1,1]))
				
				**** If rank_cond fails, at least one variable is a dummy and reportc not used (const_type ==1), then restart but with reportc on
				if `rank_cond' == 1 & `constant_type' == 1 {
				
					** Check if dummy variable
					foreach var in `lhs' `pooled' `rhs' `exogenous_vars' `endogenous_vars' `endo_pooled' `exo_pooled' {
						sum `var'
						capture assert `var' == `r(max)' | `var' == `r(min)'
						if _rc == 0 {						
							mata st_local("tmp_var",`mata_varlist'[xtdcce_selectindex(`mata_varlist'[.,2]:=="`var'"),1])
							local dummy_var "`dummy_var' `tmp_var'"
						}
					}
					if "`dummy_var'" != "" {
						noi disp as error "xtdcce2 detected dummy or binary variables and constant partialled out."
						noi disp as error "Partialling failed likely due to binary variables."
						noi disp as text "binary variables are: `dummy_var'."
						noi disp as text "xtdcce2 restarts with option reportconstant."
						noi disp as text "********************************************"
						restore
						tsset `d_idvar' `d_tvar'
						noi `cmd_line' reportconst
						exit
					}					
				}
			}
			`tracenoi' disp "Partialled out with touse"			
			`tracenoi' tabstat `lhs' `pooled' `rhs' `exogenous_vars' `endogenous_vars' `endo_pooled' `exo_pooled' if `touse', s(N mean sd min max) save
			tempname PartialOutStat
			matrix `PartialOutStat' = r(StatTotal)
			cap mata st_local("RowNameP",invtokens(`mata_varlist'[xtdcce2_mm_which2(`mata_varlist'[.,2],tokens("`lhs' `pooled' `rhs' `exogenous_vars' `endogenous_vars' `endo_pooled' `exo_pooled'")),1]'))
			cap matrix colnames `PartialOutStat' = `RowNameP'

	*************************************************************************************************************
	**************************************Regression*************************************************************
	*************************************************************************************************************
			** 	renew touse
			markout `touse' `rhs' `pooled' `endogenous_vars' `exogenous_vars'
			sort `idvar' `tvar'
			*noi sum `lhs' `rhs' if `touse'
			tempname cov_i sd_i t_i stats_i b_i RankReg UsedCols
			tempvar residuals_var	 
			local residuals `residuals_var'
			local mata_drop `mata_drop'	`RankReg' `UsedCols' `useqr'
			matrix `UsedCols' = 0
			*1 check if IV
			*2 run for IV and none IV 3 regressions: 
			*		i) all pooled, 
			*		ii) mix (as is), 
			*		iii) full mg
			*3 run program for mg calculation to correct b and V
			
			*** 1 - non IV case			
			if `IV' == 0 {
				gen double `residuals' = 0				
				*i) all pooled
				if "`pooled'" != "" & "`rhs'" == "" {
					tempname eb_pi
					`tracenoi' mata xtdcce_m_reg("`lhs'","`touse'","`idvar'","`rhs' `pooled'","`lr'","`lr_options'",`num_adjusted',"`residuals'","`eb_pi'","`cov_i'","`sd_i'","`t_i'","`stats_i'","`jack_indicator_a' `jack_indicator_b'",`mata_varlist',`fast',"",`blockdiaguse',`useqr',"`RankReg'","`UsedCols'")
					matrix `b_i' = `eb_pi'
				}
				*ii) as is (inculdes all mg)
				if "`rhs'" != "" {
					tempname eb_asisi
					`tracenoi' mata xtdcce_m_reg("`lhs' `rhs'","`touse'","`idvar'","`pooled'","`lr'","`lr_options'",`num_adjusted',"`residuals'","`eb_asisi'","`cov_i'","`sd_i'","`t_i'","`stats_i'","`jack_indicator_a' `jack_indicator_b'",`mata_varlist',`fast',"",`blockdiaguse',`useqr',"`RankReg'","`UsedCols'")
					matrix `b_i' = 	`eb_asisi'	
				}
				*iii) all MG (only needed if pooled var is used, needed for cov estimation). If all MG not used. better speed option
				* Use fast option which does not calculate residuals, covariance and stats.
				* *_i_pooled not necessary as not used for later use.
				if "`pooled'" != ""  {
					tempname eb_mgi cov_i_1_pooled stats_i_pooled sd_i_pooled t_i_pooled
					`tracenoi'  mata xtdcce_m_reg("`lhs' `rhs' `pooled'","`touse'","`idvar'","","`lr'","`lr_options'",`num_adjusted',"`residuals'","`eb_mgi'","`cov_i_1_pooled'","`sd_i_pooled'","`t_i_pooled'","`stats_i_pooled'","`jack_indicator_a' `jack_indicator_b'",`mata_varlist',1,"",`blockdiaguse',`useqr')
				}
			}
			** 2 - IV case
			else if `IV' == 1 {

				tempname iv_stats iv_mg
				***seperate data
				xtdcce2_separate , rhs(`rhs') exogenous_vars(`exogenous_vars') endogenous_vars(`endogenous_vars') /*
						*/ touse(`touse') idvar(`idvar')		
				local exo_list  `r(exo_list)'
				local endo_list `r(endo_list)'
				local rhs_list `r(rhs_list)'				
				
				** jackknife lists
				if "`jackknife'" == "jackknife" {
					
					foreach list in lhs pooled  rhs endogenous_vars endo_pooled exogenous_vars exo_pooled {
						if "``list''" != "" {
							mata st_local("`list'j",invtokens((`mata_varlist'[xtdcce2_mm_which2(`mata_varlist'[.,2],tokens("``list''")'),13])'))
						}
					}
					
					xtdcce2_separate , rhs(`rhsj') exogenous_vars(`exogenous_varsj') endogenous_vars(`endogenous_varsj') /*
						*/ touse(`touse') idvar(`idvar')		
					local exo_listj  `r(exo_list)'
					local endo_listj `r(endo_list)'
					local rhs_listj `r(rhs_list)'
					
				}
				
				*i) all pooled
				if "`rhs'" == "" & "`exogenous_vars'" == "" & "`endognous_vars'" == "" {
					tempname eb_pi
					`noisily' ivreg2 `lhs' `pooled' `rhs' (`endogenous_vars' `endo_pooled' = `exogenous_vars' `exo_pooled') if `touse' , /*
					*/ `cluster' `noconstant_reg' `ivdiag' `ivreg2options' sdofminus(`num_partialled_out')  
					matrix `eb_pi' = e(b)
					predict double `residuals' if `touse' , xb
										
					matrix `cov_i' = e(V)

					mata st_matrix("`sd_i'",sqrt(diagonal(st_matrix("`cov_i'"))))
					mata st_matrix("`t_i'",st_matrix("`eb_pi'")':/st_matrix("`sd_i'"))					
					
					** save stats
					_estimates hold `iv_stats'
					
					**jackknife
					if "`jackknife'" == "jackknife" {
						matrix tmp_col : colnames `eb_pi'
						tempname ba bb
						tempvar xba xbb
						`tracenoi' ivreg2 `lhsj' `pooledj' `rhsj' (`endogenous_varsj' `endo_pooledj' = `exogenous_varsj' `exo_pooledj') if `jack_indicator_a' , /*
						*/ `cluster' `noconstant_reg' `ivdiag' `ivreg2options' sdofminus(`num_partialled_out')   
						matrix `ba' = e(b)
						predict `xba' , xb
						
						`tracenoi' ivreg2 `lhsj' `pooledj' `rhsj' (`endogenous_varsj' `endo_pooledj' = `exogenous_varsj' `exo_pooledj') if `jack_indicator_b' , /*
						*/ `cluster' `noconstant_reg' `ivdiag' `ivreg2options' sdofminus(`num_partialled_out')  
						matrix `bb' = e(b)
						predict `xbb' , xb	
						matrix `eb_pi' = 2*`eb_pi' - 0.5*(`bb' + `ba')
						matrix colnames `eb_pi' = `tmp_colj'
						*correct y_hat (called residual)
						replace `residuals' = 2*`residuals' - 0.5*(`xba' + `xbb')
					}
					replace `residuals' = `lhs' - `residuals'
				}
				*ii) as is
				if "`rhs'" != "" | "`exogenous_vars'" != "" | "`endognous_vars'" != ""{	
					tempname eb_asisi
					`noisily' ivreg2 `lhs' `pooled' `rhs_list' (`endo_list' `endo_pooled' = `exo_list' `exo_pooled') if `touse' , /*
					*/ `cluster' `noconstant_reg' `ivdiag' `ivreg2options' sdofminus(`num_partialled_out')  
					matrix `eb_asisi' = e(b)
					predict double `residuals' if `touse', xb
					
					
					matrix `cov_i' = e(V)
					mata st_matrix("`sd_i'",sqrt(diagonal(st_matrix("`cov_i'"))))
					mata st_matrix("`t_i'",st_matrix("`eb_asisi'")':/st_matrix("`sd_i'"))
					
					local tmp_col : colnames `cov_i'
					
					matrix rownames `sd_i' = `tmp_col'
					matrix rownames `t_i' = `tmp_col'
					
					** save stats
					capture _estimates drop `iv_stats'
					_estimates hold `iv_stats'
			
					**jackknife
					if "`jackknife'" == "jackknife" {
						local tmp_colj : colnames `eb_asisi'
						tempname ba bb
						tempvar xba xbb
						`tracenoi' ivreg2 `lhsj' `pooledj' `rhs_listj' (`endo_listj' `endo_pooledj' = `exo_listj' `exo_pooledj') if `jack_indicator_a' , /*
						*/ `cluster' `noconstant_reg' `ivdiag' `ivreg2options' sdofminus(`num_partialled_out')  
						matrix `ba' = e(b)
						predict double  `xba' , xb
						
						`tracenoi' ivreg2 `lhsj' `pooledj' `rhs_listj' (`endo_listj' `endo_pooledj' = `exo_listj' `exo_pooledj') if `jack_indicator_b' , /*
						*/ `cluster' `noconstant_reg' `ivdiag' `ivreg2options' sdofminus(`num_partialled_out')  
						matrix `bb' = e(b)
						predict double  `xbb' , xb	
						
						matrix `eb_asisi' = 2*`eb_asisi' - 0.5*(`bb' + `ba')
						matrix colnames `eb_asisi' = `tmp_colj'
						*correct y_hat (called residual)
						replace `residuals' = 2*`residuals' - 0.5*(`xba' + `xbb')
					}
					replace `residuals' = `lhs' - `residuals'	
				}
				*iii) all MG (only needed if pooled is used), no need for stats and residual
				if "`pooled'" != ""  | "`exo_pooled'" != "" | "`endo_pooled'" != ""  {					
					if "`ivslow'" == "ivslow" {
						xtdcce2_separate , rhs(`pooled') exogenous_vars(`exo_pooled') endogenous_vars(`endo_pooled') /*
							*/ touse(`touse') idvar(`idvar')					
						
						tempname eb_mgi iv_mg

						local rhs_tmp `r(rhs_list)'
						local exo_tmp `r(exo_list)'
						local endo_tmp `r(endo_list)'						
						
						
						local rhs_list2 `rhs_listj' `rhs_tmp'
						local exo_list2 `exo_listj' `exo_tmp'
						local endo_list2 `endo_listj' `endo_tmp'
						
						`noisily' ivreg2 `lhs'  `rhs_list2' (`endo_list2'  = `exo_list2' ) if `touse' , /*
							*/ `cluster' `noconstant_reg' `ivdiag' `ivreg2options' sdofminus(`num_partialled_out')  
						
						matrix `eb_mgi'  = e(b)				
						
						** save stats
						capture _estimates drop `iv_mg'
						_estimates hold `iv_mg'		

			
						**jackknife
						if "`jackknife'" == "jackknife" {
						
							xtdcce2_separate , rhs(`pooledj') exogenous_vars(`exo_pooledj') endogenous_vars(`endo_pooledj') /*
							*/ touse(`touse') idvar(`idvar')	
						
							local tmp_colj: colnames `eb_mgi'							
							
							local rhs_tmp `r(rhs_list)'
							local exo_tmp `r(exo_list)'
							local endo_tmp `r(endo_list)'					
							
							
							local rhs_list2j `rhs_list' `rhs_tmp'
							local exo_list2j `exo_list' `exo_tmp'
							local endo_list2j `endo_list' `endo_tmp'
							
							tempname ba bb
							`tracenoi' ivreg2 `lhsj'  `rhs_list2j' (`endo_list2j'  = `exo_list2j' )  if `jack_indicator_a' , /*
							*/ `cluster' `noconstant_reg' `ivdiag' `ivreg2options' sdofminus(`num_partialled_out')  
							matrix `ba' = e(b)
							`tracenoi' ivreg2 `lhsj'  `rhs_list2j' (`endo_list2j'  = `exo_list2j' )  if `jack_indicator_b' , /*
							*/ `cluster' `noconstant_reg' `ivdiag' `ivreg2options' sdofminus(`num_partialled_out')  
							matrix `bb' = e(b)
							
							matrix `eb_mgi' = 2*`eb_mgi' - 0.5*(`bb' + `ba')
							matrix colnames `eb_mgi' = `tmp_colj'
						}
						drop `rhs_tmp' `exo_tmp' `endo_tmp'	
					}
					else {
						tempname eb_mgi  resid2 cov_i1
						`tracenoi' mata xtdcce_m_reg("`lhs' `endogenous_vars' `endo_pooled' `rhs' `pooled'","`touse'","`idvar'","","`lr'","`lr_options'",`num_adjusted',"`resid2'","`eb_mgi'","`cov_i1'","`sd_i'","`t_i'","`stats_i'","`jack_indicator_a' `jack_indicator_b'",`mata_varlist',1,"`exogenous_vars' `exo_pooled' `rhs' `pooled'",`blockdiaguse',`useqr',"`RankReg'","`UsedCols'")
					}	
				}
				*claculate long run coefficients
				if "`lr'" != "" {
					*run over all three possibilities for coefficients
					foreach mat in eb_asisi eb_mgi eb_pi  {
						if "``mat''" != "" {
							tempname m_blr_names m_blr m_covlr 
							*cov only needed for asisi and pooled case
							local ff = 1
							if "``mat''" == "`eb_asisi'" | "``mat''" == "`eb_pi'" {
								local ff = 0
								*noi disp "`ff'"
							}
							mata `m_blr' = st_matrix("``mat''")
							mata `m_covlr' = st_matrix("`cov_i'")
							mata `m_blr_names' = st_matrixcolstripe("``mat''")[.,2]

							qui mata `m_blr' = xtdcce_m_lrcalc(`m_blr',`m_covlr',`m_blr_names',"`lr'","`lr_options'",`ff',`mata_varlist',`idvar',`touse')	
							
							mata st_matrix("``mat''",`m_blr'[.,1]')
							mata st_matrixcolstripe("``mat''", (J(cols(`m_blr_names'),1,""),`m_blr_names'') )
							if `ff' == 0 {	
								mata st_matrix("`cov_i'`mat'",`m_blr'[.,2..cols(`m_blr')])
								mata st_matrixcolstripe("`cov_i'`mat'", (J(cols(`m_blr_names'),1,""),`m_blr_names'') )
								mata st_matrixrowstripe("`cov_i'`mat'", (J(cols(`m_blr_names'),1,""),`m_blr_names'') )
							}
							mata mata drop `m_blr' `m_covlr	' `m_blr_names'				
						}
					}
				}
				**correct b_i, cov_i, sd_i and t_i
				if "`rhs'" == "" & "`exogenous_vars'" == "" & "`endognous_vars'" == "" {
					matrix `b_i' = `eb_pi'
					matrix `cov_i' = `cov_i'``eb_pi''
					mata st_matrix("`sd_i'",sqrt(diagonal(st_matrix("`cov_i'``eb_pi''")))')
					mata st_matrix("`t_i'",(st_matrix("`b_i'"):/st_matrix("`sd_i'")))
				}
				if "`rhs'" != "" | "`exogenous_vars'" != "" | "`endognous_vars'" != ""{	
					matrix `b_i' = `eb_asisi'
					matrix `cov_i' = `cov_i'``eb_asisi''
					mata st_matrix("`sd_i'",sqrt(diagonal(st_matrix("`cov_i'``eb_asisi''")))')
					mata st_matrix("`t_i'",(st_matrix("`b_i'"):/st_matrix("`sd_i'")))
				}
				local tmp_row : rownames `b_i'
				local tmp_col : colnames `b_i'
				matrix colnames `sd_i' = `tmp_col'
				matrix rownames `sd_i' = `tmp_row'
				
				matrix colnames `t_i' = `tmp_col'
				matrix rownames `t_i' = `tmp_row'
				*}
				*Creat Stats matrix
				tempname stats_i
				_estimates unhold `iv_stats'
				matrix `stats_i' = (e(rss),e(mss),e(yyc) , 0 , e(Fdf2), e(rmse) , e(F) , e(df_m) , `N_g' , e(N) , e(r2), e(r2_a) )
				**Save stats if full iv used
				if "`fulliv'" == "fulliv" {					
					foreach scal in N yy yyc rss mss df_m df_r r2u r2c r2 r2_a ll rankxx rankzz rankV ranks rmse F N_clust /*
						*/ N_clust1 N_clust2 bw lambda kclass full sargan sarganp sargandf j jp arubin /*
						*/ arubinp arubin_lin arubin_linp arubindf idstat idp iddf widstat arf arfp archi2 /*
						*/ archi2p ardf ardf_r redstat redp reddf cstat cstatp cstatdf cons center partialcons partial_ct {
							if "`e(`scal')'" != "" scalar ivreg2_`scal' = e(`scal')
					}
					foreach macr in cmd cmdline ivreg2cmd version model depvar instd insts inexog exexog collin dups ecollin clist redlist partial small wtype /*	
						*/ wexp clustvar vcetype kernel firsteqs rfeq sfirsteq predict {
							if "`e(`macr')'" != "" local ivreg2_`macr'  "`e(`macr')'"
					}
					foreach matr in S W first ccev dcef {
							if "`e(`matr')'" != "" matrix  ivreg2_`matr' = e(`matr') 
					}
				}
			}
			`tracenoi' display "Regression done"
							*** save row names for omitted output
				*noi matrix list `b_i'
				local bi_cols_omit : colnames `b_i'
			******************************Regression End************************************
			***CD Test			
			if "`cd'" == "" & "`fast'" == "0" {	
				`tracenoi' display "Residuals"
				`tracenoi' sum `residuals' if `touse'
				tempname res_check
				matrix `res_check' = (r(N),r(mean),r(sd),r(min),r(max))
				capture xtcd2 `residuals' if `touse', noest 
				if _rc == 199 {
					noi display as error "xtcd2 not installed" 
					local cd nocd
				}
				else if _rc != 0 {
					noi display as error "xtcd2 caused error. Please do test by hand."
					local cd nocd
				}
				else {
					tempname cds cdp
					scalar `cds' = r(CD)
					scalar `cdp' = r(p)
				}
			} 
			if "`exponent'" != "" & "`fast'" == "0" {
				local xtcse2reps = 0
				
				if strmatch("`xtcse2options'","*reps(*") == 0 {
					local xtcse2optionsp "`xtcse2options' reps(100)"
					local xtcse2reps = 100
				}
				else {
					tokenize `xtcse2options'
					if strmatch("`1'","*reps(*") {
						local xtcse2reps= subinstr(subinstr("`1'","reps(","",.),")","",.)
					}
					local xtcse2optionsp `xtcse2options'
				}
				capture xtcse2 `residuals' if `touse' , nocd inprog residual `xtcse2optionsp'
				if _rc == 199 {
					noi display as error "xtcse2 not installed" 
					local exponent noexponent
				}
				else if _rc != 0 {
					noi display as error "xtcse2 caused error. Please do estimation by hand."
					local exponent noexponent
				}
				else {
					tempname alphaM alphaSEM
					matrix `alphaM' = r(alpha)
					matrix `alphaSEM' = r(alphaSE)
					matrix colnames `alphaM' = "residuals"
					matrix colnames `alphaSEM' = "residuals"
				}
			}
			*noi mata `mata_varlist'
			if `ardl_indic' == 1 {
				tempname lr_pooled lr_bases lr_select lr1_check	
				**if lr1 pooled, then check, otherwise all mg
				mata `lr1_check' = (`mata_varlist'[.,9]:!="0"):*(`mata_varlist'[.,5]:!="0")
				mata st_local("num_lr1_pooled",strofreal(colsum(`lr1_check'):==sum(`mata_varlist'[.,9]:!="0")))
				
				**lr1 is mg, so lr_rest is mg too
				if `num_lr1_pooled' == 0 {
					mata st_local("lr_vars_mg",invtokens("lr_":+uniqrows(`mata_varlist'[xtdcce_selectindex(`mata_varlist'[.,12]:!="0"),12])'))
				}
				else {
					**all lr1 vars pooled, so add to list for mean group program
					mata st_local("lr_vars_pooled","lr_":+uniqrows((`mata_varlist'[xtdcce_selectindex((`mata_varlist'[.,12]:!="0"):*(`mata_varlist'[.,9]:!="0")),12])))
					mata `lr_bases' = uniqrows(`mata_varlist'[xtdcce_selectindex(`mata_varlist'[.,12]:!="0"),12])
					mata `lr_pooled' = colsum(strtoreal(J(1,rows(`lr_bases'),`mata_varlist'[.,5])):*(J(rows(`mata_varlist'),1,`lr_bases''):==`mata_varlist'[.,12]))
					mata `lr_pooled' = `lr_pooled' :/ colsum(J(rows(`mata_varlist'),1,`lr_bases''):==`mata_varlist'[.,12])
					mata `lr_select' = xtdcce_selectindex(("lr_":+`lr_bases'):!="`lr_vars_pooled'")
					mata `lr_bases' =  `lr_bases'[`lr_select']
					mata `lr_pooled' = `lr_pooled'[`lr_select']
					
					mata st_local("num_lr_vars_pooled",strofreal(sum(`lr_pooled':==1)))
					if `num_lr_vars_pooled' > 0 {
						mata st_local("lr_vars_pooled1",invtokens("lr_":+`lr_bases'[xtdcce_selectindex(`lr_pooled':==1)]'))
					}
					*check if no mg 
					mata st_local("num_lr_vars_mg",strofreal(sum(`lr_pooled':!=1)))
					if `num_lr_vars_mg' > 0 {	
						mata st_local("lr_vars_mg",invtokens("lr_":+`lr_bases'[xtdcce_selectindex(`lr_pooled':!=1)]'))
					}
					else {
						local lr_vars_mg ""
					}	
					local lr_vars_pooled "`lr_vars_pooled' `lr_vars_pooled1'"
				}

				if "`lr_vars_pooled'" != "" {
					noi disp "Pooled Variables in ARDL not possible. No correct Standard Errors available"
				}
				
				capture mata mata drop `lr_pooled' `lr_bases' `lr_select' `lr1_check'	
			}
			
			***MG program
			tempname b_mg cov sd t
			`tracenoi' mata xtdcce_m_meangroup("`eb_asisi'","`eb_mgi'","`eb_pi'","`rhs' `endogenous_vars'  `lr_vars_mg'","`pooled' `endo_pooled' `lr_vars_pooled'","","`idvar'","`touse'","`b_mg'","`cov'","`sd'","`t'","`lr_vars_pooled'",`mata_varlist',`pooledvce',"`residuals'",`useqr' )
			**read varlists back
			local i = 3
			foreach list in lhs rhs pooled crosssectional exogenous_vars endogenous_vars lr_1 lr_rest  {
				mata st_local("`list'",invtokens(`mata_varlist'[xtdcce_selectindex(`mata_varlist'[.,`i']:=="1"),1]'))
				local i = `i' + 1
			}
			*read varnames back
			mata st_local("list_long",invtokens(`mata_varlist'[xtdcce_selectindex(strlen(`mata_varlist'[.,1]):>23),1]'))
			local i = 1
			foreach var in `list_long' {
				mata st_local("short",`mata_varlist'[xtdcce_selectindex(`mata_varlist'[.,1]:=="`var'"),2])
				rename `short' `var' 
			}
			local rhs: list rhs - pooled

			local lr `lr_1' `lr_rest'
			
			mata st_local("old_list",invtokens(`mata_varlist'[xtdcce_selectindex(`mata_varlist'[.,1]:!=`mata_varlist'[.,2]),1]'))
			mata st_local("change_list",invtokens(`mata_varlist'[xtdcce_selectindex(`mata_varlist'[.,1]:!=`mata_varlist'[.,2]),2]'))
	
			
			*Change names back for b_mg cov sd t b_i cov_i sd_i and t_i
			foreach mat in `b_mg' `cov' `sd' `t'  {
				local tmp_row : rownames `mat'
				local tmp_col : colnames `mat'
				local i = 1
				foreach var in `change_list' { 
					local old_name = word("`old_list'",`i')
					local tmp_row = subinstr("`tmp_row'","`var'","`old_name'",.)
					local tmp_col = subinstr("`tmp_col'","`var'","`old_name'",.)
					local i = `i'+1
				}
				
				local tmp_row = subinstr("`tmp_row'","`constant'","_cons",.)
				local tmp_col = subinstr("`tmp_col'","`constant'","_cons",.)				

				matrix colnames `mat' = `tmp_col'
				matrix rownames `mat' = `tmp_row'
				
			}

			**seperate for unit specific matrices			
			foreach mat in `b_i' `cov_i' `sd_i' `t_i' {
				local tmp_row : rownames `mat'
				local tmp_col : colnames `mat'
				local i = 1
				foreach var in `change_list' { 					
					local old_name = word("`old_list'",`i')
					local tmp_row = subinstr("`tmp_row'","`var'","`old_name'",.)
					local tmp_col = subinstr("`tmp_col'","`var'","`old_name'",.)
					local i = `i'+1
				}
				local tmp_row = subinstr("`tmp_row'","`constant'","_cons",.)
				local tmp_col = subinstr("`tmp_col'","`constant'","_cons",.)

				matrix colnames `mat' = `tmp_col'
				matrix rownames `mat' = `tmp_row'
			}

			local constant _cons
			
			**Remove omitted variables from variablelists
			*foreach var in rhs rhs_list pooled endogenous_vars exogenous_vars endo_list endo_pooled exo_pooled lr {
			*	local `var' : list `var' - omitted		
			*}		
			
			*Remove constant if type 4 from pooled list and matrices
			if "`constant_type'" == "4" {
				local pooled: list pooled - constant
			}	
			
			*Correct names if xtpmgnames option used
			if strmatch("`lr_options'","*xtpmg*") == 1 {
				gettoken lr_1 lr_rest : lr
				
				local lr = subinstr("`lr'","`lr_1'","ec",.)
				
				*change lists
				foreach liste in pooled endo_pooled  endogenous_vars rhs {
					local `liste' = subinstr("``liste''","`lr_1'","ec",.)
				}
				*change b_mg cov sd and t
				foreach mat in `b_mg' `cov'  `sd' `t' `b_i' `cov_i' `sd_i' `t_i' {
					local col_eq ""
					local row_eq ""
				
					local tmp_row : rownames `mat'
					local tmp_col : colnames `mat'
					
					local tmp_row = subinstr("`tmp_row'","`lr_1'","ec",.)
					local tmp_col = subinstr("`tmp_col'","`lr_1'","ec",.)
					
					matrix colnames `mat' = `tmp_col'
					matrix rownames `mat' = `tmp_row'
					
					*add rowcolumns only for cov
					if "`mat'" == "`cov'" | "`mat'" == "`sd'" {
						foreach row in `tmp_row' {
							if regexm("`lr_rest'",strtrim("`row'")) == 1 {
								local tmp "ec"
							}
							else {
								local tmp "SR"
							}
							local row_eq `row_eq' `tmp'
						}
					}
					foreach col in `tmp_col' {	
						if regexm("`lr_rest'",strtrim("`col'")) == 1 {
							local tmp "ec"
						}
						else {
							local tmp "SR"
						}
						local col_eq `col_eq' `tmp'
					}
					matrix coleq `mat' = `col_eq'
					matrix roweq `mat' = `row_eq'	
				}				
			}
	
			*Get Tmin, Tmax and Tmin in case of unbalanced dataset
			if "`d_balanced'" != "strongly balanced" {
				tempvar ts_stats
				by `touse' `idvar' , sort: gen `ts_stats' = _N 
				sum `ts_stats' if `touse'
				local minT = `r(min)'
				local maxT = `r(max)'
				local meanT = `r(mean)'
			}
			/*
			if "`oldrestore'" != "" {
			**put touse into mata to preserve it after restore
			mata st_view(`touse'=.,.,"`touse' `id_t'")	
			mata `touse'_s = `touse'			
		restore	
		**read back
		mata st_view(`touse'=.,.,"`id_t'")
		mata `touse'_s = `touse'_s[xtdcce2_mm_which2(`touse'_s[.,2],`touse',1),1]
		mata st_view(`touse',.,st_addvar("double","`touse'"))
		mata `touse'[.] = `touse'_s
		mata mata drop `touse' `touse'_s 		
		
		}
		else {*/
			mata st_view(`touse'=.,.,"`touse' `id_t'")
			mata `touse'_p = select(`touse'[.,2],`touse'[.,1])
		restore
		sort `id_t' 
		gen byte `touse' = 0
		`tracenoi' mata xtdcce_m_touseupdate("`touse'","`id_t'",`touse'_p)
		mata mata drop `touse'_p `touse'
		
	}

	****************************************************************************
	***********************************Return***********************************
	****************************************************************************
		qui tsset `d_idvar' `d_tvar'
		
		matrix b = `b_mg'
		matrix V = `cov'
		
		**load stats
		local i = 1
		foreach stat in SSR SSE SST S2 dfr rmse F K N_g N r2 r2_a  {
			scalar `stat' = `stats_i'[1,`i']		
			local i = `i' + 1
		}
		** correct r2_pmg
		tempname yybar yybarv yybarm
		
		qui gen `yybarv' = `lhs'
		qui by `d_idvar' (`d_tvar') , sort : egen `yybarm' = mean(`yybarv') if `touse'
		qui replace `yybarv' = (`yybarv' - `yybarm')^2
		qui sum `yybarv'  if `touse', meanonly
		scalar `yybar' = r(sum)
		return clear
		ereturn clear
		cap 
		mata st_local("hasmissb",strofreal(hasmissing(st_matrix("b"))))
		mata st_local("hasmissv",strofreal(hasmissing(st_matrix("V"))))
		if `hasmissb' > 0 | `hasmissv' > 0 {
			noi disp "Missing values in estimated coefficients found. Coefficients set to zero. Check results for omitted variables."
			local tmp_row : colnames b
			mata st_matrix("b",editmissing(st_matrix("b"),0))
			mata st_matrix("V",editmissing(st_matrix("V"),0))
						
			matrix colnames b = `tmp_row'
			matrix colnames V = `tmp_row'
			matrix rownames V = `tmp_row'			
		}
		
		ereturn post b V , obs(`N') esample(`touse') depname(`lhs') 
		
		if `IV' == 1 {
			ereturn local insts "`exogenous_vars' `exo_pooled'"
			ereturn local instd "`endogenous_vars' `endo_pooled'"
			*ereturn local enogenous_vars `endogenous_vars'
			*ereturn local exogenous_vars `exogenous_vars'
			
			if "`fulliv'" == "fulliv" {
				foreach scal in N yy yyc rss mss df_m df_r r2u r2c r2 r2_a ll rankxx rankzz rankV ranks rmse F N_clust /*
					*/ N_clust1 N_clust2 bw lambda kclass full sargan sarganp sargandf j jp arubin /*
					*/ arubinp arubin_lin arubin_linp arubindf idstat idp iddf widstat arf arfp archi2 /*
					*/ archi2p ardf ardf_r redstat redp reddf cstat cstatp cstatdf cons center partialcons partial_ct {
						if "`ivreg2_`scal''" != "" ereturn scalar ivreg2_`scal' = `ivreg2_`scal''
				}
				foreach macr in cmd cmdline ivreg2cmd version model depvar instd insts inexog exexog collin dups ecollin clist redlist partial small wtype /*	
					*/ wexp clustvar vcetype kernel firsteqs rfeq sfirsteq predict{
					if "`ivreg2_`macr''" != "" 	ereturn local  ivreg2_`macr'  "`ivreg2_`macr''"
				}
				foreach matr in b V S W first ccev dcef {
					if "`ivreg2_`matr''" != "" matrix  ivreg2_`matr' = `ivreg2_`matr''
				}
			} 			
		}
		*noi disp "`N_g' , `N'"
		novarabbrev {		
			ereturn scalar N = N
			ereturn scalar N_g = N_g
			ereturn scalar T = e(N) / `N_g'
			if "`d_balanced'" != "strongly balanced" {
				ereturn scalar Tmin = `minT'
				ereturn scalar Tmax = `maxT'
				ereturn scalar Tbar = `meanT'
				
				scalar Ttmp = e(Tmax)
			}	
			else {
				scalar Ttmp = e(T)
			}
			*** Calculation of r2_tmp
			scalar Ki = `num_pooled' + `num_mg_regression' / `N_g' + `num_partialled_out' / `N_g' 
			** Pooled
			if `num_pooled' > 0 & `num_mg_regression' == 0 {
				*** use Ki, error in HPY pooled only takes partialled out into account
				scalar r2_pmg = 1 - (SSE / (N_g * (Ttmp - Ki)  ) ) / (`yybar' / (N_g * (Ttmp - 1)))
				ereturn scalar r2_pmg = r2_pmg
			}
			else if `num_pooled' == 0 & `num_mg_regression' > 0 {
				*** use Ki as it includes CSA, HPY do not include CSA in k, ignore -2
				scalar r2_pmg = 1 - (SSE / (N_g * (Ttmp - Ki))) / (`yybar' / (N_g * (Ttmp - 1)))
				ereturn scalar r2_pmg = r2_pmg
			}
			
			ereturn scalar df_m = K
			ereturn scalar K_mg = K - `num_partialled_out'
			ereturn scalar K_partial = `num_partialled_out'			
			ereturn scalar F = F
			ereturn scalar r2 = r2
			ereturn scalar r2_a = r2_a			
			ereturn scalar rmse = rmse
			ereturn scalar df_r = dfr
			ereturn scalar rss = SSR
			ereturn scalar mss = SSE	
			
			if "`scrosssectional'" != "" {
				ereturn local cr_lags  "`scr_lags'"
				ereturn local csa "`scrosssectional'"
			}
			if "`globalcrosssectional'" != "" {
				ereturn local gcr_lags  "`gcr_lags'"
				ereturn local gcsa "`globalcrosssectional'"
			}
			if "`clustercrosssectional'" != "" {
				ereturn local ccr_lags	 "`ccr_lags'"
				ereturn local ccsa "`clustercrosssectional'"
				ereturn local ccsa_cluster "`csa_cluster'"
			}
			
			ereturn local gcr_lags = "`gcr_lags'"
			ereturn local ccr_lags = "`ccr_lags'"
			
			ereturn local cr_vars ""
			
			ereturn local indepvar "`pooled' `rhs'"
			ereturn local idvar = "`d_idvar'"
			ereturn local tvar = "`d_tvar'"	
			
			ereturn local cmdline "`cmd_line'"
			ereturn local cmd "xtdcce2"								
			ereturn local predict "xtdcce2_p"
			ereturn local estat_cmd "xtdcce2_estat"
			*ereturn local version = `xtdcce2_version'
			
			if "`pooled'`endo_pooled'" != "" {
				ereturn local pooled "`pooled' `endo_pooled'"
				ereturn scalar K_pooled = `num_pooled'
			}
			if "`lr'" != "" {
				ereturn local lr "`lr'"
			}
			if "`omitted'" != "" {
				ereturn local omitted_var "`omitted_var'"
				ereturn scalar K_omitted = `omitted_N'
			}
			
			ereturn matrix bi = `b_i' , copy
			ereturn matrix Vi = `cov_i' , copy
			
			if "`exponent'" != "" {
				ereturn matrix alpha = `alphaM', copy
				ereturn matrix alphaSE = `alphaSEM', copy
			}
			
			**Hidden returns for estat and predict
			ereturn hidden local p_mg_vars "`rhs' `endogenous_vars'"
			ereturn hidden local p_pooled_vars "`pooled' `endo_pooled'"
			ereturn hidden local p_cr_vars "`crosssectional'"
			ereturn hidden local p_lr_1 "`lr_1'"
			ereturn hidden local p_lr_vars_mg "`lr_vars_mg'"
			ereturn hidden local p_lr_vars_pooled "`lr_vars_pooled'"
			ereturn hidden local p_if "`cmd_if'"
			ereturn hidden local p_in "`cmd_in'"
			ereturn hidden scalar constant_type = `constant_type'
			ereturn hidden local lr_options "`lr_options'"
			if "`res_check'" != "" {
				ereturn hidden matrix ResidualStat = `res_check'
			}
			ereturn hidden matrix PartialOutStat = `PartialOutStat'
			ereturn hidden scalar useqr = `useqr'
		
			if "`cd'" == "" {
				cap ereturn scalar cd = `cds'
				cap ereturn scalar cdp = `cdp'
			}
		}
		
		*local pf = 1- chi2(`=`e(df_r)'-1',e(F))
		local pf = Ftail(e(df_m),e(df_r),e(F))
	**qui ends here!
	}
	
	****************************************************************************
	***********************************Output***********************************
	****************************************************************************
	if "`pooled'" != "" {
		local textpooled "Pooled "
	}
	if "`rhs'`endogenous_vars'" != "" {
		local textmg "Mean Group"
	}
	if `IV' == 1 {
		local textiv " IV"
	}
	if `ardl_indic' == 1 {
		local textcsardl " (CS-ARDL)"
	}
	else if `ardl_indic' == 0 & "`lr'" != "" {
	    local textcsardl " (CS-ECM)"
	}
	display as text "(Dynamic) Common Correlated Effects Estimator - `textpooled'`textmg'`textiv'`textcsardl'"
	
	**Assume standard of 80 and extend only if variable names larger than 80 and linesize sufficient.
	local maxline = c(linesize)	
	**allow max linesize of 100
	if `maxline' > 100 {
		local maxline = 100
	}
	**get var length
	local maxlength = 0
	foreach var in `endogenous_vars' `rhs' `lhs' `pooled' `endo_pooled' `lr' `lr_pooled' {
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
		
	#delimit ;
		di _n in gr "Panel Variable (i): " in ye e(idvar) 
				   _col(`=`maxline'-80+50') in gr "Number of obs" _col(`=`maxline'-80+68') "=" 
				   _col(`=`maxline'-80+71') in ye %9.0f e(N) ;
		di in gr "Time Variable (t): " in ye abbrev(e(tvar),`abname') in gr
					_col(`=`maxline'-80+50') "Number of groups" _col(`=`maxline'-80+68') "="
					_col(`=`maxline'-80+71') in ye %9.0g e(N_g) ;

	#delimit cr
	di "" 
	if "`d_balanced'" == "strongly balanced" {
		#delimit ;
			di in gr "Degrees of freedom per group:" 
					_col(`=`maxline'-80+50') in gr "Obs per group (T)" _col(`=`maxline'-80+68') "="
					_col(`=`maxline'-80+71') in ye %9.0f e(T) ;
			di in gr _col(2) "without cross-sectional averages"
						_col(37) "=" _col(39) e(T)-`num_pooled' - `num_mg_regression' / `N_g' - `num_partialled_out' / `N_g' +  `num_crosssectional' / `N_g'; 
			di in gr _col(2) "with cross-sectional averages"
					_col(37) "=" _col(39) e(T)-`num_pooled' - `num_mg_regression' / `N_g' - `num_partialled_out' / `N_g' ;
		#delimit cr
	}
	else {	
		#delimit ;
			di in gr "Degrees of freedom per group:" 
				_col(`=`maxline'-80+50') in gr in gr "Obs per group:" _col(`=`maxline'-80+68') ;
			di in gr _col(2) "without cross-sectional avg."
				_col(31) "min"
				_col(37) "=" _col(39) e(Tmin) -`num_pooled' - `num_mg_regression' / `N_g' - `num_partialled_out' / `N_g' +  `num_crosssectional' / `N_g'
				_col(`=`maxline'-80+68-4') in gr "min = "
				_col(`=`maxline'-80+71') in ye %9.0f e(Tmin) ;	
			di in gr _col(31) "max"
				_col(37) "=" _col(39) e(Tmax) -`num_pooled' - `num_mg_regression' / `N_g' - `num_partialled_out' / `N_g' +  `num_crosssectional' / `N_g' 
				_col(`=`maxline'-80+68-4') in gr "avg = " 
				_col(`=`maxline'-80+71') in ye %9.0f e(Tbar) ;
			di in gr _col(2) "with cross-sectional avg."
				_col(31) "min"
				_col(37) "=" _col(39) e(Tmin) -`num_pooled' - `num_mg_regression' / `N_g' - `num_partialled_out' / `N_g' 
				_col(`=`maxline'-80+68-4') in gr "max = " 
				_col(`=`maxline'-80+71') in ye %9.0f e(Tmax) ;
			di in gr _col(31) "max"
				_col(37) "=" _col(39) e(Tmax) -`num_pooled' - `num_mg_regression' / `N_g' - `num_partialled_out' / `N_g'  ;
		#delimit cr
	}
	if wordcount("`cr_lags'") > 1 {
		local cr_lags_disp "`cr_lags_min' to `cr_lags_max'"
	}
	else if wordcount("`cr_lags'") == 1 {
		local cr_lags_disp "= `cr_lags'"
	}
	else {
		local cr_lags_disp "none"
	}
	
	if `num_pooled' > 0 & `num_mg_regression' == 0 {		
		local tmp_r2pmg = e(r2_pmg)
		local tmp_r2text "R-squared (P)"
	}
	else if `num_pooled' == 0 & `num_mg_regression' > 0 {	
		local tmp_r2pmg = e(r2_pmg)
		local tmp_r2text "R-squared (MG)"
	}
	else {
		local tmp_r2pmg = e(r2_a)
		local tmp_r2text "Adj. R-squared"
	}
	#delimit ;
		di in gr "Number of "
					_col(`=`maxline'-80+50') in gr "F(`e(df_m)', `e(df_r)')" _col(`=`maxline'-80+68') "="
					_col(`=`maxline'-80+71') in ye %9.2f e(F) ;
		di in gr _col(2) "cross-sectional lags" 
					_col(37) "`cr_lags_disp'"	
					_col(`=`maxline'-80+50') in gr "Prob > F" _col(`=`maxline'-80+68') "="
					_col(`=`maxline'-80+71') in ye %9.2f `pf' ;
		di in gr _col(2) "variables in mean group regression"
					_col(37) "=" _col(39) e(K_mg)
					_col(`=`maxline'-80+50') in gr "R-squared" _col(`=`maxline'-80+68') "="
					_col(`=`maxline'-80+71') in ye %9.2f e(r2) ;
		di in gr _col(2) "variables partialled out"
					_col(37) "=" _col(39)  "`num_partialled_out'"
					_col(`=`maxline'-80+50') in gr "`tmp_r2text'" _col(`=`maxline'-80+68') "="
					_col(`=`maxline'-80+71') in ye %9.2f `tmp_r2pmg' ;
	#delimit cr			
	
	if e(K_omitted) != 0 {
		#delimit ;
			di in gr  _col(2) "omitted Variables:" _col(37) "=" _col(39) e(K_omitted)	
					_col(`=`maxline'-80+50') in gr "Root MSE" _col(`=`maxline'-80+68') "="
					_col(`=`maxline'-80+71') in ye %9.2f e(rmse) ;
		#delimit cr
	}
	else {
			di in gr	_col(`=`maxline'-80+50') in gr "Root MSE" _col(`=`maxline'-80+68') "=" _col(`=`maxline'-80+71') in ye %9.2f e(rmse) 
	}

	if "`cd'" == "" {
		#delimit ;
			di _col(`=`maxline'-80+50') in gr "CD Statistic" _col(`=`maxline'-80+68') "="
			   _col(`=`maxline'-80+71') in ye in ye %9.2f e(cd) ;
			di _col(`=`maxline'-80+50') in gr "   p-value" _col(`=`maxline'-80+68') "="
			   _col(`=`maxline'-80+71') in ye in ye %9.4f e(cdp) ;	
		#delimit cr
	
	}

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
	
	
	if `ardl_indic' == 1 {
		local lr_np : list lr - pooled
		local lr_p : list lr - lr_np
		local rhs_vars `endogenous_vars' `rhs' `lr_np'
		local pooled_vars `pooled' `endo_pooled' `lr_p'
		local pooled_vars: list uniq pooled_vars
		local rhs_vars: list uniq rhs_vars
	}
	else {
		local rhs_vars `endogenous_vars' `rhs' 
		local pooled_vars `pooled' `endo_pooled' 

		local lr_p1: list lr_1 & pooled_vars
		local lr_mg1: list lr_1  - lr_p1
		
		local pooled_vars : list pooled_vars - lr
		local rhs_vars : list rhs_vars - lr
	}
	
	if "`lr'" != "" {
		di as text _col(2) _col(2) "Short Run Est." _col(`col_i')  "{c |}"
		di as text "{hline `col_i'}{c +}{hline `=`maxline'-`col_i''}"
		local sr_text "  "
	}
	
	
	if "`pooled_vars'`lr_p1'" != "" { 
		di as text _col(2) "`sr_text'Pooled: " _col(`col_i') " {c |}"
		foreach var in `lr_p1' `pooled_vars' {
			xtdcce_output_table `var' `col_i' `b_mg' `sd' `t' cv `var'
		}
	}	
	
	if "`rhs_vars'`lr_mg1'" != ""   {
		di _col(2) as text "`sr_text'Mean Group:"  _col(`col_i')  " {c |}"
		local lrcount = wordcount("`rhs_vars'")
		foreach var in `lr_mg1' `rhs_vars' {
			if "`full'" != "" {
				di "" _col(`col_i') " {c |}"
			}
			xtdcce_output_table `var' `col_i' `b_mg' `sd' `t' cv `var'
			if "`full'" != "" {
				local lrcount = `lrcount' - 1
				di as text "{hline `col_i'}{c BT}{hline `=`maxline'-`col_i''}"
				di as text "`sr_text'`sr_text'Individual Results" _col(`col_i')
				di as text "{hline `col_i'}{c TT}{hline `=`maxline'-`col_i''}"
				forvalues j = 1(1)`N_g' {
					xtdcce_output_table `var'_`j' `col_i' `b_i' `sd_i' `t_i' cv `var'_`j'
				}
				if "`lr'" == "" & `lrcount' > 0 {
					di as text "{hline `col_i'}{c +}{hline `=`maxline'-`col_i''}"
				}
			}
		}
	}	
	if strtrim("`lr'") != "" {
		
		if `ardl_indic' == 1 {
		    
			local lr_p1: list lr_1 & lr_p
			local lr_mg1: list lr_1 & lr_np		
			
			
		    tsrevar `lr_1' , list
			local lr_1_ardl = word("`r(varlist)'",1)
			local lr_1_ardl "lr_`lr_1_ardl'"
			
			di as text "{hline `col_i'}{c +}{hline `=`maxline'-`col_i''}"	
			di as text _col(2) "Adjust. Term" _col(`col_i') " {c |}"
			di as text "{hline `col_i'}{c +}{hline `=`maxline'-`col_i''}"			
						
			if "`lr_p1'" != "" di as text _col(2) "`sr_text'Pooled: " _col(`col_i') " {c |}"
			if "`lr_mg1'" != "" di as text _col(2) "`sr_text'Mean Group:" _col(`col_i') " {c |}"

			xtdcce_output_table `lr_1_ardl' `col_i' `b_mg' `sd' `t' cv `lr_1_ardl'
						
			local lr_p1 
			local lr_mg1
			
			local lr_pooled `lr_vars_pooled'
			local lr_rest `lr_vars_mg'		
			
		}		
		else if `ardl_indic' == 0 {
			local lr_pooled : list lr & pooled
			local lr_rest : list lr - lr_pooled					
		}
		
		ereturn hidden local lr_pooled "`lr_pooled'"
		ereturn hidden local lr_mg "`lr_rest'"
		
		*** remove lr_1
		if `ardl_indic' == 0 {
			local lr_pooled: list lr_pooled - lr_1
			local lr_rest: list lr_rest - lr_1
		}		
		
		if "`rhs_vars'" != "" {
			di as text "{hline `col_i'}{c +}{hline `=`maxline'-`col_i''}"
		}
		di as text _col(2) "Long Run Est." _col(`col_i')  " {c |}"
		di as text "{hline `col_i'}{c +}{hline `=`maxline'-`col_i''}"
		if "`lr_pooled'" != "" { 
			di as text _col(2) "`sr_text'Pooled:" _col(`col_i') " {c |}" 
			foreach var in `lr_pooled' {
				xtdcce_output_table `var' `col_i' `b_mg' `sd' `t' cv `var'
			}
		}
		if strtrim("`lr_rest'") != ""   {
			di as text _col(2) "`sr_text'Mean Group:" _col(`col_i') " {c |}" 
			local lrcount = wordcount("`lr_rest'")
			foreach var in `lr_rest' {
				xtdcce_output_table `var' `col_i' `b_mg' `sd' `t' cv `var'
				if "`full'" != "" {
					local lrcount = `lrcount' - 1
					di as text "{hline `col_i'}{c BT}{hline `=`maxline'-`col_i''}"
					di as text _col(2) "`sr_text'`sr_text'Individual Results" _col(`col_i')
					di as text "{hline `col_i'}{c TT}{hline `=`maxline'-`col_i''}"
					forvalues j = 1(1)`N_g' {
						xtdcce_output_table `var'_`j' `col_i' `b_i' `sd_i' `t_i' cv `var'_`j'
					}
					if `lrcount' > 0 {
						di as text "{hline `col_i'}{c +}{hline `=`maxline'-`col_i''}"
					}
				}
			}		
		}
	}	
	di as text "{hline `col_i'}{c BT}{hline `=`maxline'-`col_i''}"
	
	
	if wordcount("`pooled'") > 0 | wordcount("`lr_pooled'") > 0 {
		di as text  "Pooled Variables: `endo_pooled' `pooled'"
	}
	if strtrim("`rhs'") != "" | strtrim("`lr_rest'") != "" {
		di as text  "Mean Group Variables: `rhs'"
	}
	if strtrim("`crosssectional'") != "" {
		
		if "`scrosssectional'" != "" {
			local crosssectional_output ""
			if wordcount("`scr_lags'") > 1 {
				forvalues i = 1(1)`=wordcount("`scrosssectional'")' {
					local crosssectional_output "`crosssectional_output' `=word("`scrosssectional'",`i')'(`=word("`scr_lags'",`i')')"				
				}
			}
			else {
				local crosssectional_output "`scrosssectional'"				
			}
			display  as text "Cross Sectional Averaged Variables: `crosssectional_output'"
		}
		if "`globalcrosssectional'" != "" {
			local crosssectional_output ""
			if wordcount("`gcr_lags'") > 1 {
				forvalues i = 1(1)`=wordcount("`globalcrosssectional'")' {
					local crosssectional_output "`crosssectional_output' `=word("`globalcrosssectional'",`i')'(`=word("`gcr_lags'",`i')')"				
				}
			}
			else {
				local crosssectional_output "`globalcrosssectional'"				
			}
			display  as text "Global Cross Sectional Averaged Variables: `crosssectional_output'"
		}
		
		if "`clustercrosssectional'" != "" {
			local crosssectional_output ""
			if wordcount("`ccr_lags'") > 1 {
				local cluster_def `=word("`csa_cluster'",`i')'
				
				forvalues i = 1(1)`=wordcount("`clustercrosssectional'")' {
					
					local clusteri = word("`csa_cluster'",`i')
					if "`clusteri'" == "" {
						local clusteri `cluster_def'
					}
					else {
						local cluster_def `clusteri'
					}
					
					local crosssectional_output "`crosssectional_output' `=word("`clustercrosssectional'",`i')'(`=word("`ccr_lags'",`i')' - `clusteri')"				
				}
			}
			else {
				local crosssectional_output "`clustercrosssectional'"				
			}
			display  as text "Clustered Cross Sectional Averaged Variables: `crosssectional_output'"
		}
		
		
		/*
		if wordcount("`cr_lags'") > 1 {
			tempname mata_cr_output
			mata `mata_cr_output' = `mata_varlist'[xtdcce_selectindex((`mata_varlist'[.,6]:=="1")),(1,11)]			
			mata `mata_cr_output' = (`mata_cr_output'[.,1] :+ "(":+`mata_cr_output'[.,2]:+")")
			mata st_local("crosssectional_output",invtokens(`mata_cr_output''))
			mata mata drop `mata_cr_output'
		}
		else {
			local crosssectional_output "`crosssectional'"
		}
		
		if "`constant_type'" == "2" | "`constant_type'" == "1" {
			if "`crosssectional_output'" != "" {
				display  as text "Cross Sectional Averaged Variables: `crosssectional_output'"
			}
		}
		else {
			display  as text "Cross-sectional Averaged Variables: `crosssectional_output'"
		}
		*ereturn hidden local cr_options "`crosssectional_output'" 
		*/
		
		
	}
		if strtrim("`lr'") != "" { 
		display  as text "Long Run Variables: `lr_pooled' `lr_rest'"
		if `ardl_indic' == 0 {
			display  as text "Cointegration variable(s): `lr_1'"
		}
		else {
			tsrevar `lr_1' , list
			local list1 `r(varlist)'
			tsunab tslistun : `lr_1' 
			display  as text "Adjustment variable(s): lr_`list1' (`tslistun')"
		}
	}
	if `IV' == 1 {
		display  as text "Endogenous Variables: `endo_pooled' `endogenous_vars'"
		display  as text "Exogenous Variables: `exo_pooled' `exogenous_vars'"
	}
	
	if strtrim("`omitted_var'") != "" {
		display  as text "Omitted Variables:"
		mata st_local("omitted_var",`mata_varlist'[xtdcce2_mm_which2(`mata_varlist'[.,2],tokens("`omitted_var'")'),1])
		display  as text _col(2) "`omitted_var'"
	}
	if "`constant_type'" == "1" {
		display  as text "Heterogenous constant partialled out." , 
	}
	if "`constant_type'" == "2" {
		display  as text "Homogenous constant removed from model." ,
	}
	if "`constant_type'" == "4" {
		display  as text "Homogenous constant not displayed." , 
	}
	if "`jackknife'" == "jackknife" {
		display  as text "Jackknife bias correction used." ,
		ereturn local bias_correction = "jackknife" 
	}
	if "`recursive'" == "recursive" {
		display  as text "Recursive mean adjustment used to correct for small sample time series bias." , _c
		ereturn local bias_correction = "recursive mean correction" 
	}
	if `pooledvce' == 1 {
		display as text "Westerlund, Perova, Norkute fixed-T standard errors for pooled coefficients."
	}
	if `pooledvce' == 2 {
		display as text "Newey-West standard errors for pooled coefficients."
	}
	** check for failure of rank condition for CSA regression
	if `rank_cond' > 0 {
		display in red "Warning:" 
		display as text "Rank condition on matrix of cross product of cross sectional averages not satisfied!" _n "Only mean group estimates are consistent, unit specific estimates are inconsistent." /*_n "See Chudik, Pesaran (2015, Journal of Econometrics), Assumption 6 and page 398." */ 
	}
	** check for failure of rank condition for main regression, only for non-IV case
	if `IV' == 0 {
		mata `RankReg' = st_matrix("`RankReg'")
		mata: st_local("rk_indic",strofreal(all(`RankReg'[.,1]:==`RankReg'[.,2])))
		if `rk_indic' != 1 {
			display in red "Warning:" 
			display as text "Collinearities detected. One or more variables are dropped and set to zero."
			display as text in smcl "Use {stata estat ebistructure} to display more details." 
				
			matrix colnames `UsedCols' = `rhs_vars' `pooled_vars'
			ereturn matrix omitted_var_i = `UsedCols'
			ereturn hidden matrix rankreg = `RankReg'			
		}
	}	
	
	if "`exponent'" != "" & "`fast'" == "0" {
		di ""
		noi disp as text "Estimation of Cross-Sectional Exponent (alpha)"
	
		local level =  `c(level)'
		local col_i = `abname' + 1
		local maxline = `maxline' - 2
		scalar cv = invnorm(1 - ((100-`level')/100)/2)
	
		di as text "{hline `col_i'}{c TT}{hline `=`maxline'-`col_i'-15'}"
		di as text %`col_i's  abbrev("variable",`abname') "{c |}" _c
		local col = `col_i' + 1 + 6
		di as text _col(`col') "alpha" _c
		local col = `col' + 5 + 3
		di as text _col(`col') "Std. Err."  _c
		local col = `col' + 9 + 4
		di as text _col(`col') "[`level'% Conf. Interval]"    
		di as text "{hline `col_i'}{c +}{hline `=`maxline'-`col_i'-15'}"
		
		xtdcce_output_table_alpha residuals `col_i' `alphaM' `alphaSEM' cv residuals
		
		di as text "{hline `col_i'}{c BT}{hline `=`maxline'-`col_i'-15'}"
		di "0.5 <= alpha < 1 implies strong cross sectional dependence."
		if `xtcse2reps' > 0 {
			di as text "SE and CI bootstrapped with `xtcse2reps' repetitions."
		}
		else {
			di as text "SE and CI not available. Use option reps() to bootstrap SE and CI."
		}		
		di ""
	}
	if "`tracenoi'" != "" {
		mata `mata_varlist'
		
	}
	foreach tmp in `mata_drop' `cov_i1'{
		*capture mata mata drop `tmp'
	}
	
	if "`residuals_old'" != "" {
		predict `residuals_old' if e(sample) , residuals
		display  as text "", _newline
		display  as text "Option 'residuals()' not supported anymore. Residuals calculated using:" 
		display  as text in smcl "{stata predict `residuals_old' if e(sample) , residuals: predict `residuals_old' if e(sample) , residuals}"
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
	local t_p_mg `5' 
	local cv  `6'
	local i `7'

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
	di as result _column(`col') %9.7g ( `b_p_mg'[1,colnumb(`b_p_mg',"`i'")] - `cv'*`se_p_mg'[1,colnumb(`se_p_mg',"`i'")]) _continue
	local col = `col' + 11
	di as result _column(`col') %9.7g ( `b_p_mg'[1,colnumb(`b_p_mg',"`i'")] + `cv'*`se_p_mg'[1,colnumb(`se_p_mg',"`i'")])
end

**** Error program
capture program drop xtdcce_err
program define xtdcce_err
	syntax anything , msg(string) [msg2(string) msg_smcl(string)]
	tokenize `anything'
	local code `1'
	local idvar `2'
	local tvar `3'
	
	tsset `2' `3'
	
	di as error _n  "`msg'"
	if "`msg2'" != "" {
		di as error  "`msg2'" _c
		di in smcl   `"`msg_smcl'"'
	}
	exit `code'
end



/*
Mata OLS Program
returns coefficient, error terms, covariance matrix, statistics
v4
includes
	- variance/covariance matrix estimation
	- statistics
	- long run estimation
	- new method for blockdiag
	
order for stats:
SSR, SSE, SST, S2, dfr, rmse, F, K, N_g, N

*/
capture mata mata drop xtdcce_m_reg()
mata:
	function xtdcce_m_reg (  string scalar variablenames, ///variable names, first var independent, from second vars mean group - 1
									string scalar touse,  ///touse - 2
									string scalar id_var, /// idvar - 3
									string scalar ccep,   ///names of pooled vars - 4
									string scalar lr_vars , /// names of long run variables - 5
									string scalar lr_options, /// lr options - 6
									real scalar input_no_partial, /// number of partialled out variables - 7
									string scalar e_output_name, ///name of error term - 8
									string scalar output_eb, /// names of b matrix - 9
									string scalar output_cov_name, /// name of covariance matrix - 10 
									string scalar output_sd_name, /// name of sd matrix 11
									string scalar output_t_name, /// name of t-sts - 12
									string scalar output_stats_name, /// names of stats outpt	- 13
									string scalar jackknife_names, ///name of jackknife touse - 14
									string matrix mata_var_names, /// name of mata matrix with var names - 15
									|real scalar fast, /// if 1 then no cov and stats are calculated -16
									string scalar input_exo, /// name of exogenous vars - 17
									real scalar blockdiaguse, /// blockdiag is used rather than own program - 18
									real scalar useqr,		/// use qr for matrix inversion - 19
									string scalar rank_name,		/// name of rank matrix - 20
									string scalar UsedCols_name) /// name of used columns matrix - 21
		{
		"start m_reg"
		(variablenames , ccep , lr_vars)
		lhs = tokens(variablenames)[1]
		mg_d = 0
		pooled_d = 0
		exo = 0
		num_K = 0
		num_Kp = 0
		num_Kmg = 0
		if (cols(tokens(variablenames)) > 1) {
			rhs = tokens(variablenames)[2..cols(tokens(variablenames))]
			mg_d = 1
			num_Kmg = cols(rhs)
			num_K = num_Kmg
		}
		if (cols(tokens(ccep)) > 0) {
			pooled = tokens(ccep)
			pooled_d = 1
			num_Kp = cols(pooled)
			num_K = num_K + num_Kp
		}
		if (args() < 16) {
			fast = 0
		}
		if (args() < 17) {
			if (input_exo != "") {
				exo_vars = tokens(input_exo)
				if (cols(exo_vars) > 0 ) {
					exo = 1
					fast = 1
				}
			}
		}
		else {
			input_exo = ""
		}
		if (args() < 18) {
			blockdiaguse = 0
		}
		if (args() < 16) {
			if (fast == 1) { 
				/// dummy output_cov
				output_cov = .
			}
			else {
				fast = 0
			}
		}
		if (args() < 19) {
			useqr = 0
		}
		/// arg 20
		rank = .
		
		if (args() < 21) {
			UsedCols = 0
		}
		else {
			UsedCols = st_matrix(UsedCols_name)
		}
		"mg - pooled "
		(mg_d , pooled_d)
		"fast, exo"
		(fast, exo)
		"rhs"
		(rhs)
		"blockdiag"
		(blockdiaguse)
		"K_mg , K_pooled, K_total"
		(num_Kmg , num_Kp, num_K)
		"args processed"
		id = st_data(.,id_var,touse)
		
		Y = st_data(.,lhs,touse)
		"data loaded"
		///pooled estimation only	
		if (pooled_d == 1 & mg_d == 0) {
			"only pooled"
			X = st_data(.,pooled,touse)
			X_p_X_p = quadcross(X,X)
			X_p_Y = quadcross(X,Y)
			XX_cov = X_p_X_p
			b_output = m_xtdcce_solver(X_p_X_p,X_p_Y,useqr,rank,UsedCols=0,method="")	
			outputnames = pooled
			"inverter used:"
			method
		}
		
		if (mg_d == 1 ){
			"rows X"
			(rows(X),cols(X))
			"mg estimation"
			outputnames = J(1,0,"")
			X_o = st_data(.,rhs,touse)
			"dimension X_o"
			(rows(X_o),cols(X_o))
			sum(X_o)
			b_output = J(0,1,.)
			XX_cov = J(0,0,.)
			uniqueid = uniqrows(id)
			"uniqueid names"
			uniqueid
			N = rows(uniqueid)
			if (exo == 0) {
				"exo = 0"				
				"over following ids"
				uniqueid
				if (blockdiaguse==0){
					/// build X block diagonal					
					NT = rows(X_o)
					///"num_k, N, T"
					///(num_K,N,T)
					/// here no pooled vars. if mixed model, pooled vars will be added later
					X = J(NT,num_Kmg*N,0)
					XX_cov=J(num_Kmg*N,num_Kmg*N,0)
					"rows X, cols X"
					(rows(X),cols(X))
					posCol = 1
					posColC=1
					posRow = 1
				}
				"start doing cross specific reg"
				i = 1
				rank = J(rows(uniqueid),2,.)
				UsedCols = J(rows(uniqueid),cols(X_o),0)
				while (i <= rows(uniqueid)) {
					indic = (id :== uniqueid[i])
					tmp_x = select(X_o,indic)
					/// if mg only, do regression for each country seperately					
					if (pooled_d == 0) {
						tmp_y = select(Y,indic)
						tmp_xx = quadcross(tmp_x,tmp_x)
						tmp_xy = quadcross(tmp_x,tmp_y)
						"start calculating b"
						bii = m_xtdcce_solver(tmp_xx,tmp_xy,useqr,ranki=.,colni=0,method="")
						b_output = (b_output \ bii)
						"cols"
						colni
						"inverter used:"
						method
						if (colni == 0) {
							UsedCols[i,.] = J(1,cols(X_o),1)
						}
						else {
							UsedCols[i,colni] = J(1,cols(colni),1)
						}
						rank[i,.] = ranki
						
						//for covariance
						if (fast == 0) {
							"start cov"
							rows(bii)
							eii = tmp_y - tmp_x * bii
							Ti = rows(tmp_x)
							s2ii = eii'eii / (Ti-cols(tmp_x) - input_no_partial/rows(uniqueid))							
						
							if (blockdiaguse==0) {
								
								posColCEnd = posColC + num_Kmg-1
								XX_cov[(posColC..posColCEnd),(posColC..posColCEnd)] = m_xtdcce_inverter(tmp_xx,useqr)*s2ii
								posColC = posColCEnd + 1
							}
							else {
								XX_cov = blockdiag(XX_cov,m_xtdcce_inverter(tmp_xx,useqr)*s2ii)
							}
						}						
					} 
					/// now build X matrix for later use. needed if mixed model and for residual calculation
					
					if (blockdiaguse==0){
						Ti = rows(tmp_x)
						///posCol = num_K*i - num_K + 1
						///posRow = T*i - T + 1
						posColEnd = posCol + num_Kmg-1
						posRowEnd = posRow + Ti-1
						X[(posRow..posRowEnd),(posCol..posColEnd)] = tmp_x
						posCol = posColEnd + 1
						posRow = posRowEnd + 1
					}
					else {
						X = blockdiag(X,tmp_x)
					}
					outputnames = (outputnames , (rhs:+"_":+strofreal(uniqueid[i])))
					i++
				}
				"cross specific done"
				UsedCols
				if (pooled_d == 1) {
					"pooled included, mixed"
					X = (X, st_data(.,pooled,touse))
					//outputnames_pooled = pooled
					(rows(X),cols(X))
					XX = quadcross(X,X)
					XY = quadcross(X,Y)
					b_output = m_xtdcce_solver(XX,XY,useqr,rank,UsedCols=0,method="")
					"inverter used"
					method
					
					/// Used Cols are zero, create a 1xK vector with ones
					if (cols(UsedCols)==1) {
						UsedCols = J(1,cols(X),1)
					}
					/// build matrix in case MG coefficients exist
					if (num_Kmg > 0) {
						"col check of omitted vars, if 0 problem"
						num_Kmg*N <= cols(UsedCols)
						num_Kmg*N , cols(UsedCols)
						//// problem: used cols too small!
						/// does not work? why
						if (num_Kmg*N > cols(UsedCols)) {
							"adjust UsedCols for mixed model"
							/// dirty solution
							UsedMG = J(1,N*num_Kmg,0)
							UsedPooled = J(N,num_Kp,0)

							UsedMGi = UsedCols[selectindex(UsedCols:<=N*num_Kmg)]							
							UsedMG[1,UsedMGi] = J(1,cols(UsedMGi),1)					
							UsedMG = rowshape(UsedMG,N)							
											
							UsedPooledi = UsedCols[selectindex(UsedCols:>N*num_Kmg)]:-N*num_Kmg
							UsedPooled[.,UsedPooledi] = J(N,cols(UsedPooledi),1) 
							
							UsedCols = UsedMG, UsedPooled
						}
					}
					"used cols"
					UsedCols
					outputnames = (outputnames , pooled)	
					//for covariance
					if (fast == 0) {
						eii = Y - X * b_output
						Ti = rows(tmp_x)
						
						s2ii = eii'eii / (Ti-cols(tmp_x) - input_no_partial/rows(uniqueid))			
						XX_cov = m_xtdcce_inverter(XX,useqr)*s2ii				
					}
					"done"
				}
			}
			else {
				"Exogenous vars used"
				/// This is only the mg all case and no covariance etc. is calculated (also only fast case!)
				X_o = st_data(.,rhs,touse)
				X = X_o
				Z_o = st_data(.,input_exo,touse)
				///check for overid case (more instruments than instrumented vars), if overid, use GMM estimator
				i = 1
				///while (i <= rows(uniqrows(id))) {
				///	indic = (id :== i)
				uniqueid = uniqrows(id)
				while (i <= rows(uniqueid)) {
					indic = (id :== uniqueid[i])	
					tmp_x = select(X_o,indic)
					tmp_z = select(Z_o,indic)				
					tmp_y = select(Y,indic)
					if (cols(tmp_x) == cols(tmp_z)) {
						tmp_zx = quadcross(tmp_z,tmp_x)
						tmp_zy = quadcross(tmp_z,tmp_y)
						tmp_b = m_xtdcce_solver(tmp_zx,tmp_zy,useqr)
					}
					else {
						tmp_zz = qrinv(quadcross(tmp_z,tmp_z))
						tmp_zx = quadcross(tmp_z,tmp_x)
						tmp_xz = quadcross(tmp_x,tmp_z)
						tmp_zy = quadcross(tmp_z,tmp_y)
						upper = tmp_xz*tmp_zz*tmp_zy
						lower = qrinv(tmp_xz*tmp_zz*tmp_zx)
						"lower"
						(rows(lower),cols(lower))
						"upper"
						(rows(upper),cols(upper))
						tmp_b = m_xtdcce_solver(lower,upper,qrinv)						
					}
					(rows(b_output),cols(b_output))
					(rows(tmp_b),cols(tmp_b))
					b_output = (b_output \ tmp_b )
					"b done"
					outputnames = (outputnames , (rhs:+"_":+strofreal(uniqueid[i])))
					i++
				}
			}
		}
		"coeff done"
		/// Jackknife
		if (cols(tokens(jackknife_names)) == 2) {
			"jack main instance"
			jack_indic_a = tokens(jackknife_names)[1]
			jack_indic_b = tokens(jackknife_names)[2]
			variabaljack = invtokens((mata_var_names[xtdcce2_mm_which2(mata_var_names[.,2],tokens(variablenames)'),13])')
			if (pooled_d != 0) {
				ccep_jack = invtokens((mata_var_names[xtdcce2_mm_which2(mata_var_names[.,2],tokens(ccep)'),13])')
			}
			else {
				ccep_jack = ""
			}
			if (exo != 0) {	
				input_exo_j = invtokens((mata_var_names[xtdcce2_mm_which2(mata_var_names[.,2],tokens(input_exo)'),13])')
			}
			else {
				input_exo_j = ""
			}
			"start regs"
			"vars for jack"
			variabaljack
			b_a = xtdcce_m_reg(variabaljack,jack_indic_a,id_var,ccep_jack,"","",0,"e","eb","cov","sd","t","st","jack1",mata_var_names,1,input_exo_j,blockdiaguse,useqr)
			"in jack"
			b_b = xtdcce_m_reg(variabaljack,jack_indic_b,id_var,ccep_jack,"","",0,"e","eb","cov","sd","t","st","jack2",mata_var_names,1,input_exo_j,blockdiaguse,useqr)
			"outputs"
			"first half"
			b_a
			"second half"
			b_b
			"adjust for jack"
			b_output = 2:*b_output :- 0.5:*(b_a :+ b_b)
			"jack done"
		}
				
		/// Return Part
		/// if jackknife nested, return b_output to mata, no other output needed
		if (cols(tokens(jackknife_names)) == 1) {
			return(b_output)
		}
		////no output needed if jackknife
		else {		
			"start output"
			K = rows(b_output) + input_no_partial
			N = rows(id)
			N_g = rows(uniqrows(id))
			/// check if fast option is used
			if (fast == 0) {
				"start stats in non fast mode"
				if (missing(X) > 0) {
					"Missings detected, replaced by zero!!"
					_editmissing(X,0)
				}
				///Error Term				
				"dimension X"				
				(rows(X),cols(X))
				sum(X)
				"dimension b_output"
				(rows(b_output),cols(b_output))
				sum(b_output)
				Xb_output = X * b_output
				Y_hat = Xb_output
				"dimension Y_hat"
				(rows(Y_hat),cols(Y_hat))
				"Sum Y_hat"
				sum(Y_hat)
				"dimension Y"
				(rows(Y),cols(Y))
				"error calculation"
				st_view(e_output=.,.,e_output_name,touse)
				"dimension error"
				(rows(e_output),cols(e_output))
				"write error"
				e_output[.,1] = Y - Y_hat	
				"error written and done"
				///(Y, Y_hat, e_output)
				/// variance/covariance matrix and stats for entire sample	
				SSE = e_output' * e_output
				dfr = N - K
				s2 = SSE / dfr
				SST = sum((Y :- mean(Y)):^2)
				SSR = SST - SSE
				rmse = sqrt(s2)
				/// variance covariance matrix				
				output_cov = XX_cov 
				// check if constant	
				has_c = 0
				
				if (sum((colsum(X):==rows(X)))) {
					has_c = 1	
					r2 = 1 - SSE/SST       

				} 
				else {					
                        SSR = sum((Y_hat):^2)
                        r2 = 1- SSR/SST

				}
				r2_a = 1 - (1-r2) * (N - 1) / (N - K - 1)
				F = SSR/(K-has_c) / (SSE/(N-K))
				
				
				
			}
			"outputnames before lr"
			outputnames
			"stats done"
			tmp = xtdcce_m_lrcalc(b_output,output_cov,outputnames,lr_vars,lr_options,fast,mata_var_names,id_var,touse)
			b_output = tmp[.,1]
			output_cov = tmp[.,2..cols(tmp)] 			
			outputnames = (J(1,cols(outputnames),"") \ outputnames)'
			"lr done"
			///Output Coefficient (as 1 x K)	
			st_matrix(output_eb,b_output')			
			st_matrixcolstripe(output_eb,outputnames)
			"eb done"
			if (fast == 1) {
				"in fast - SD only 0s"
				output_cov = J(rows(outputnames),rows(outputnames),0)
				
				SD = sqrt(diagonal(output_cov))'	
				
				st_matrix(output_sd_name,SD)
				st_matrixcolstripe(output_sd_name,outputnames)
				"sd done"
				t = SD:/b_output'			
				st_matrix(output_t_name,t)
				st_matrixcolstripe(output_t_name,outputnames)
				"cov done"
				output_cov_name
				(rows(output_cov),cols(output_cov))
				///output cov
				st_matrix(output_cov_name,output_cov)
				st_matrixcolstripe(output_cov_name,outputnames)
				st_matrixrowstripe(output_cov_name,outputnames) 
				"cov written"
				///stats into matrix
				(K,N_g,N)
				stats = (0, 0, 0, 0, 0, 0,0, K, N_g,N,0,0,0)
				st_matrix(output_stats_name,stats)	
				"done with fast SD"
			}
			if (fast == 0) {				
				///SD and T-Stat
				SD = sqrt(diagonal(output_cov))'	
				st_matrix(output_sd_name,SD)
				st_matrixcolstripe(output_sd_name,outputnames)
				
				t = b_output':/SD			
				st_matrix(output_t_name,t)
				st_matrixcolstripe(output_t_name,outputnames)
				
				///output cov
				st_matrix(output_cov_name,output_cov)
				st_matrixcolstripe(output_cov_name,outputnames)
				st_matrixrowstripe(output_cov_name,outputnames) 
				
				///stats into matrix
				stats = (SSR, SSE, SST, s2, dfr, rmse,F, K, N_g,N,r2,r2_a)
				st_matrix(output_stats_name,stats)	
				"output written"
			
			}		
		}
		if (args() > 19) {
			st_matrix(rank_name,rank)
		}
		if (args() > 20) {
			"omitted list:"
			UsedCols
			st_matrix(UsedCols_name,UsedCols)
			colnames = (J(1,rows(UsedCols),"")\strofreal((1..rows(UsedCols))))'
			st_matrixrowstripe(UsedCols_name,colnames)
		}
		"m_reg done"
	}	
end

/*Mata MG program
1. select coefficients for MG 
2. calculate MG coefficients
3. build bi and b matrix
4. calculate Vi and V, correct if pooled
5. calculate sei and ti
6. Output b, V, sd and t directly into Stata with st_matrix and st_matrixrowstripe
*/

capture mata mata drop xtdcce_m_meangroup()
mata:
	function xtdcce_m_meangroup( ///
						string scalar b_asis_name , /// 1. asis matrix name , matrix is 1xK
						string scalar b_mg_name , /// 2. all mg matrix name, matrix is 1xK
						string scalar b_pooled_name, /// 3. only pooled matrix name, matrix is 1xK
						string scalar input_mg_vnames, /// 4. names of mean group variables 
						string scalar input_pooled_vnames, /// 5. names of pooled
						string scalar weights, ///6. for later use
						string scalar idvar, /// 7. name of id variable
						string scalar touse, /// 8. name of touse variable
						///Output: post directly using st_matrix and st_matrixrowstripe
						string scalar output_eb, ///9. 
						string scalar output_cov, /// 10
						string scalar output_sd, /// 11
						string scalar output_t, /// 12
						| string scalar exclude_p_vars,  /// 13 variables to be excluded for pooled covariance)
						string matrix mata_varlist , //// 14 varlist
						real scalar FixedTVCE_name , //// 15 fixed T VCE estimator for pooled models, 1 then WPN estimator used, 0 otherwise
						string scalar residuals_name, //// 16 name of residuals variable in stata. needed for fixed t vce estimator
						real scalar useqr)			//// 17 useqr
	{
		FixedTVCE = 0
		if (args() > 14) {
			FixedTVCE = FixedTVCE_name
		}		
		
		if (args() < 17) {
			useqr = 0
		}
		"start mean group program"
		id = st_data(.,idvar,touse) 
		
	
		uniqueid = uniqrows(id)
		N = (rows(uniqueid))
	
		input_pooled_vnames = tokens(input_pooled_vnames)
		input_mg_vnames = tokens(input_mg_vnames)
		
		/// pooled only
		if (cols(input_pooled_vnames) > 0 & cols(input_mg_vnames) == 0 )  {
			"pooled only"
			bi = st_matrix(b_mg_name)
			bi_names =  st_matrixcolstripe(b_mg_name)
			bi_names = bi_names[.,2]
			v_names_mg= input_pooled_vnames
			
			bp = st_matrix(b_pooled_name)
			bp_names = st_matrixcolstripe(b_pooled_name)
			bp_names = bp_names[.,2]
			
			///v_names_bp_mg = input_pooled_vnames
			
			ind_pooled = 1		
		}
		/// mg only 
		else if (cols(input_mg_vnames) > 0 & cols(input_pooled_vnames) == 0) {
			"mg only"
			bi = st_matrix(b_asis_name)
			bi_names =  st_matrixcolstripe(b_asis_name)
			bi_names = bi_names[.,2]
			v_names_mg = input_mg_vnames
			
			ind_pooled = -1
		}
		/// mixed 
		else if (cols(input_mg_vnames) > 0 & cols(input_pooled_vnames) > 0) {
			"mixed"
			bi = st_matrix(b_asis_name)
			bi_names =  st_matrixcolstripe(b_asis_name)

			bi_names = bi_names[.,2]
			v_names_mg = input_mg_vnames
			
			bp = bi
			bp_names = bi_names
			
			bp_mg = st_matrix(b_mg_name)
			bp_mg_names =  st_matrixcolstripe(b_mg_name)
			
			bp_mg_names = bp_mg_names[.,2]
			v_names_bp_mg = input_pooled_vnames
			
			ind_pooled = 0
		}
		else {
			"error no case!!"
		}
		if (ind_pooled != -1 ) {
			"order pooled"
			i = 1
			b_pooled = J(1,0,.)
			while (i<= cols(input_pooled_vnames)) {
				xtdcce_selectindex(bp_names:==input_pooled_vnames[1,i])
				b_pooled = (b_pooled,bp[1,xtdcce_selectindex(bp_names:==input_pooled_vnames[1,i])])
				i++
			}
		}
		
		"beginning done"
		//loop over cross sectional units and mg vars
		"dimension bi"
		i = 1
		b_mg_w = J(cols(v_names_mg),N,0)
		"bi names"
		bi_names
		"bi"
		bi
		"v_names_mg"
		v_names_mg
		"N"
		N
		"bi_mg_w dim"
		(rows(b_mg_w),cols(b_mg_w))
		while (i <= cols(v_names_mg)) {
			j = 1
			while (j <= N) {				
				varn = v_names_mg[1,i]+"_"+strofreal(uniqueid[j])
				b_mg_w[i,j] = bi[1,xtdcce_selectindex(bi_names:==varn)]
				j++
			}
			i++
		}
		"mg done"
		b_mg = mean(b_mg_w')		
		/// if pooled only then b_mg used for pooled
		b_1 = b_mg_w :- b_mg'
		b_1p = b_1
		/// loop over additional mg if mixed
		if (ind_pooled == 0) {
			"additional loop"
			i = 1
			b_mg_wmix = J(cols(v_names_bp_mg),N,0)
			while (i <= cols(v_names_bp_mg)) {
				j = 1
				while (j <= N) {
					varn = v_names_bp_mg[1,i]+"_"+strofreal(uniqueid[j])
					b_mg_wmix[i,j] = bp_mg[1,xtdcce_selectindex(bp_mg_names:==varn)]
					j++
				}
				i++
			}
			b_mg4p = mean(b_mg_wmix')
			
			///replace b_1p
			b_1p = b_mg_wmix :- b_mg4p'
		}
		"coff done"
		///covariance for pooled vars
		if (ind_pooled != -1) {
			"in pooled"
			///construct R and PSI for Cov	
			args()
			if (args()  >12) {
				exclude_p_vars
				
				if (exclude_p_vars[1]!= "") {
					"remove excluded p vars"
					exclude_p_vars= tokens(exclude_p_vars)
					indics = xtdcce2_mm_which2(input_pooled_vnames',exclude_p_vars')
					input_pooled_vnames_tmp = input_pooled_vnames
					input_pooled_vnames_tmp[indics'] = J(1,rows(indics),"")
					xtdcce_selectindex(input_pooled_vnames_tmp[1,.]:!="")
					input_pooled_vnames_tmp = input_pooled_vnames_tmp[1,xtdcce_selectindex(input_pooled_vnames_tmp:!="")]
					input_pooled_vnames_tmp
					X = st_data(.,input_pooled_vnames_tmp,touse)
					X = (X,J(rows(X),cols(exclude_p_vars),0))
					input_pooled_vnames = (input_pooled_vnames_tmp, exclude_p_vars)
				}
				else {
					X = st_data(.,input_pooled_vnames,touse)
				}
			}
			else {
				X = st_data(.,input_pooled_vnames,touse)
			}			
			
			if (FixedTVCE == 0) {
				"standard vce"
				/// Standard VarianceCovarianceEstimator from Pesaran 2006, Eq 67 - 69.
				/// PSI is directly calculated as in gauss
				PSI = J(rows(b_pooled),cols(b_pooled),0)
				/// R is Omega HS in gauss file
				R = J(rows(b_pooled),cols(b_pooled),0) 					
				/// w contains weights, w_s is the sum of the squares.
				w = J(N,1,1/N)
				w_s = sum(w:^2)
				i = 1	
				while (i <= N) {			
						///weight	
						w_i = w[i]
						w_tilde = w_i :/ sqrt(1/N :* w_s)
						b_i1 = b_1p[.,i]
						indic = (id :== uniqueid[i])
						tmp_x = select(X,indic)
						tmptmp = quadcross(tmp_x,tmp_x):/ rows(tmp_x)
						/// eq. 68 Pesaran 2006
						PSI = PSI :+ w_i :* tmptmp
						/// eq. 26 from Pesaran, Tosetti (2011); no difference as long as weights 1/N then w_tilde = 1
						/// eq. 67 Pesaran 2006
						///tmp_R = w_tilde:^2 :* tmptmp*b_i1*b_i1'*tmptmp
						if (sum(b_i1:==0) == 0 & hasmissing(tmptmp*b_i1*b_i1'*tmptmp) == 0) {
							R = R :+ w_tilde:^2 :* tmptmp*b_i1*b_i1'*tmptmp
						}
						else {
							"no se for" 
							i
						}
						i++
				}
				///divide by N-1
				R = R / (N - 1)
				PSI1 = m_xtdcce_inverter(PSI,useqr)
				"psi"
				PSI1
				"R"
				R
				//// eq. 69 Pesaran 2006
				cov_p =  w_s :* PSI1 * R * PSI1 
			}
			if (FixedTVCE == 1)  {
				"wpn vce"
				/// fixed T covariance estimator, from Westerlund et. al 2019. 
				/// follows Cov = sigma^-1 S sigma^-1 with 
				/// S = 1/N sum VeeV (Eq. 11)
				/// sigma = 1/N sum VV (Eq. 10)
				/// where V is X partialled out with cross sectional averages				
					
				e = st_data(.,residuals_name,touse)
				sigma= J(cols(X),cols(X),0)
				S = J(cols(X),cols(X),0) 
				
				i = 1
				while (i <= N) {			
						indic = (id :== uniqueid[i])
						tmp_x = select(X,indic)
						tmp_e = select(e,indic)
						sigma = sigma +  quadcross(tmp_x,tmp_x)
						///xe = quadcross(tmp_x,tmp_e)
						///xexe = quadcross(xe,xe)
						S = S+ tmp_x' * tmp_e * tmp_e'*tmp_x
						///S = S:+ quadcross(tmp_x',tmp_e) * quadcross(tmp_e,tmp_x)
						i++
				}				
				S = S
				sigma = sigma
				///sigma1 = pinv(sigma)
				sigma1 = m_xtdcce_inverter(sigma,useqr)
				///sigma = cholqrinv(sigma)
				cov_p = (sigma1*S*sigma1)
			}
			if (FixedTVCE == 2) {
				"NW estimator for pooled coefficients"
				/// Eq 50 - 52 from Pesaran 2006, with p = round(4 * (Ti/100)^(2/9)), see Gauss code
				e = st_data(.,residuals_name,touse)
				Sigma = J(cols(X),cols(X),0)
				Shat = J(cols(X),cols(X),0)
				
				w = J(N,1,1/N)						
				
				i = 1
				while ( i <= N) {
					/// select data
					indic = (id :== uniqueid[i])
					tmp_x = select(X,indic)
					tmp_e = select(e,indic)
					tmp_xe = tmp_e :* tmp_x
					
					Ti = rows(tmp_x)					
					
					w_i = w[i]
					
					
					p = round( 4 * (Ti:/100)^(2/9))						
					sij = 0
					sij0 = quadcross(tmp_xe,tmp_xe)	:/ Ti		
					j = 1
					while (j <= p) {
						tmp_xep =  tmp_xe[j+1..Ti,.]
						tmp_xepJ = tmp_xe[1..Ti-j,.]						
						tmp_tmp = tmp_xep' * tmp_xepJ :/ Ti		
						sij = sij :+  (1- j/(p+1)) :* (tmp_tmp + tmp_tmp')						
						j++
					}
					///tmp_tmp = tmp_tmp :/ Ti
					///wi^2 here because it is in the sums			
					Shat = Shat +  w_i:^2 * (sij + sij0 )	
					tmp_xx = quadcross(tmp_x,tmp_x)
					//// Ti from tmp_xx, w_i from the sum
					Sigma = Sigma + 1/Ti * tmp_xx *w_i					
					i++
				}
				sigma1 = m_xtdcce_inverter(Sigma,useqr)
				//// Eq. 74 
				cov_p = 1/Ti *  sigma1 * Shat * sigma1
			}
			"covariance for pooled done"
			cov_p
			
			if (args() >12) {				
				if (exclude_p_vars[1]!= "") {
					"covariance for LR pooled vars start"
					cov_srp = cov_p[1..cols(input_pooled_vnames_tmp),1..cols(input_pooled_vnames_tmp)]					
					i = 1
					m_g = J(cols(exclude_p_vars),cols(cov_srp),0)
					while (i<=cols(exclude_p_vars)) {
						vari = exclude_p_vars[i]
						///select bases
						indic = xtdcce_selectindex("lr_":+mata_varlist[.,12]:==vari)
						///get varnames
						indic = mata_varlist[indic,2]
						///get pos in input_pooled_vnames
						indic = xtdcce2_mm_which2(input_pooled_vnames,indic)
						m_g[i,indic] = J(1,rows(indic),1)
						i++
					}
					///cov_lrp = 1/N :* m_g * cov_srp * (1/N:*m_g)'
					cov_lrp = m_g * cov_srp * m_g'
					///input_pooled_vnames = (input_pooled_vnames,exclude_p_vars)
					cov_p = blockdiag(cov_srp,cov_lrp)
					"lr cov with delta method"
					cov_p
				}				
			}
			
		}		
		///covariance for mg vars
		if (ind_pooled != 1) {
			///Divide by rows (N) as this is the variance of a sample mean (see Eberhardts xtmg for example), small sample adjustment (-1) done in quadvariance! 
			///Pesaran 2006, eq. (58) & Chudik Pesaran 2015, eq. (32)			
			w_i = 1
			cov_mg = quadvariance(b_1',w_i)/N
			"covariance for mg done"
		}

		/// select output b and cov matrix
		/// mixed
		if (ind_pooled == -1) {
			output_b = b_mg
			cov = cov_mg
			output_names = input_mg_vnames
			"Only MG output"
		}	
		/// pooled only
		else if (ind_pooled == 1) {
			output_b = b_pooled
			cov = cov_p
			output_names = input_pooled_vnames
			"Only pooled output"
		}
		/// mixed
		else if (ind_pooled == 0) {
			output_b = (b_mg,b_pooled)
			cov = blockdiag(cov_mg,cov_p)
			output_names = (input_mg_vnames,input_pooled_vnames)
			"Mixed output"
		}	
		else {
			"Error no b matrix output!"
		}
		output_names
		sd = sqrt(diagonal(cov))
		t = output_b' :/ sd	
		
		///output
		st_matrix(output_eb,output_b)
		st_matrixcolstripe(output_eb,(J(cols(output_names),1,""),output_names'))

		///covariance
		st_matrix(output_cov,cov)
		st_matrixcolstripe(output_cov,(J(cols(output_names),1,""),output_names'))
		st_matrixrowstripe(output_cov,(J(cols(output_names),1,""),output_names'))
		
		///cov
		st_matrix(output_sd,sd')
		st_matrixcolstripe(output_sd,(J(cols(output_names),1,""),output_names'))
		
		st_matrix(output_t,t')
		st_matrixcolstripe(output_t,(J(cols(output_names),1,""),output_names'))
	}

end
**LR Computation program 
**program is called after IVreg2 or within xtdcce2_mreg program
capture mata mata drop xtdcce_m_lrcalc()
mata:
	function xtdcce_m_lrcalc( ///
								real matrix b_output , /// coefficients comes as Kx1
								real matrix output_cov , /// covariance
								string matrix outputnames , /// col/rownames
								string matrix lr_vars, /// lr variables
								string matrix lr_options, /// options
								real matrix fast, /// fast option
								string matrix mata_varlist, /// name of mata matrix with varnames
								string matrix idvar, /// name of idvar
								string scalar touse /// name of touse
								)
	{
		lr_vars = tokens(lr_vars)
		/// make sure b_output is Kx1 and outputnames is 1xK - required for later programs
		"start lr calc"
		if (cols(b_output)>1) {
			b_output = b_output'
		}
		if (rows(outputnames) > 1 ) {
			outputnames = outputnames'
		}	
		
		if (cols(lr_vars)>1) {
			"Long Run corrections"
			//// ecm/pmg part
			if (strmatch(lr_options,"*ardl*") == 0) {
				if (strmatch(lr_options,"*nodivide*") == 0) {			
					lr_1 = lr_vars[1]
					lr_rest = lr_vars[2..cols(lr_vars)]
					/// only +* to allow for pooled	
					m_lr_1_indic = strmatch(outputnames,lr_1+"*")	
					m_lr_1 = select(b_output,m_lr_1_indic')
					///m_lr_1 = ( 1 :- m_lr_1)
					m_lr_1_index = xtdcce_selectindex(m_lr_1_indic)				
					m_lr_tmp = m_lr_1
					///b_output
					///outputnames
					m_g = I(rows(b_output))
					i=1
					while (i <= cols(lr_rest)) {
						/// only +* to allow for pooled					
						m_lr_indic = strmatch(outputnames,lr_rest[i]+"*") 
						"current var"
						lr_rest[i]
						m_lr_index = xtdcce_selectindex(m_lr_indic)
						///check if running lr variable is pooled, if so, then take mean of lr_1
						m_lr_tmp = m_lr_1
						if (fast == 0) {
							/// diagonal elements 
							/// error fixed: minus needs to be for diagonal elements, no minus for off diagonal
							m_g = xtdcce_PointByPoint(m_lr_index,m_lr_index, -(1:/ m_lr_tmp),m_g)
							/// off diagonal elements
							m_g = xtdcce_PointByPoint(m_lr_index,m_lr_1_index, ( b_output[m_lr_index] :/ (m_lr_tmp:^2)), m_g)

						}
						if (sum(m_lr_indic) == 1 ) {
							m_lr_tmp = mean(m_lr_1)
							"adjusted phi as running var pooled"
						}
						b_output[m_lr_index]
						m_lr_tmp
						"done with m_g"
						b_output[m_lr_index] = -(b_output[m_lr_index] :/ m_lr_tmp)
						i++
						"b adjusted"
					}
					"b correction made"
					if (fast == 0 ) {
						output_cov = m_g'*output_cov*m_g	
						"cov corrected"
					}				
				}
			}
			////cs ardl part
			else {
				"CS-ARDL estimator"		
				/// get first lr, only names required
				lr_1 = mata_varlist[xtdcce_selectindex(mata_varlist[.,9]:=="1"),2]
				//basename
				lr_1_base = uniqrows(mata_varlist[xtdcce_selectindex(mata_varlist[.,9]:=="1"),12])
				"basename lr1"
				lr_1_base

				b_lr1_tmp_indic = xtdcce_selectindex(strmatch(outputnames,lr_1[1]+"*"))
				
				b_lr1 = b_output[b_lr1_tmp_indic]				
				
				if (fast == 0) {
					m_g_i1 = J(rows(b_lr1),cols(output_cov),0)
					m_g_i1 = xtdcce_PointByPoint((1..rows(m_g_i1)),b_lr1_tmp_indic,1,m_g_i1)
				}

				
				i = 2
				while (i<=rows(lr_1)) {
					b_lr1_tmp_indic = xtdcce_selectindex(strmatch(outputnames,lr_1[i]+"*") )					
					if (fast == 0) {
						if (cols(b_lr1_tmp_indic) > rows(m_g_i1)) {
							"m_g_i1 adjusted"
							m_g_i1 = J(cols(b_lr1_tmp_indic),1,m_g_i1)
						}
						m_g_i1 = xtdcce_PointByPoint((1..rows(m_g_i1)),b_lr1_tmp_indic,1,m_g_i1)					
					}
					b_lr1_tmp = b_output[b_lr1_tmp_indic]
					b_lr1 = b_lr1:+b_lr1_tmp	
					
					i++
				}
				if (strmatch(lr_options,"*nominus*") == 0) {
					/// correct b_lr1 such that  - (1 - sum phi_i) [ minus corrected before output]
					b_lr1 = (1 :- b_lr1)
					"b_lr1"
					b_lr1						
				}
				else {
					b_lr1 = - b_lr1
				}
				
				/// add to outputnames and b_output (here minus to stick with notation from xtpmg! 			
				b_output = (b_output \ - b_lr1)
				if (rows(b_lr1) == 1) {
					outputnames = (outputnames , ("lr_":+lr_1_base))
				}
				else {
					"touse idvaf"
					touse
					idvar
					"before sss"				
					uniqids = uniqrows(st_data(.,idvar,touse))
					uniqids
					lr_1_base
					outputnames = (outputnames , ("lr_":+lr_1_base:+"_":+strofreal((uniqids'))))
				}
				"b_lr1 done"			
				
				/// rest of lr vars, get basenames
				lr_rest = mata_varlist[xtdcce_selectindex(mata_varlist[.,10]:=="1"),(2,12)]
				lr_rest_base = uniqrows(lr_rest[.,2])
				basei=1
				"lr rest calc"
				"remaining base vars"
				lr_rest_base
				
				
				if (fast == 0) {
					"initalise m_g"
					"dim cov"
					(rows(output_cov),cols(output_cov))
					"dim mg"
					(rows(m_g_i1),cols(m_g_i1))
					m_g = (I(cols(output_cov)) \ m_g_i1)						
				}
			
				while (basei <= rows(lr_rest_base)) {
					/// select vars for base
					lr_base_i = lr_rest_base[basei]
					"base i"
					lr_base_i
					lr_rest_tmpvars = lr_rest[xtdcce_selectindex(lr_rest[.,2]:==lr_base_i),1]					
					"first var"
					lr_rest_tmpvars[1]
					//first of base_i
					b_lr_rest_tmp_indic = xtdcce_selectindex(strmatch(outputnames,lr_rest_tmpvars[1]+"*"))
					b_lr_rest = b_output[b_lr_rest_tmp_indic]
					"dim b_lr_rest"
					(rows(b_lr_rest),cols(b_lr_rest))
					b_lr_rest
					if (fast == 0) {
						//m_g parts
						m_g_ir = J(rows(b_lr_rest),cols(output_cov),0)					
						m_g_ir = xtdcce_PointByPoint((1..rows(m_g_ir)),b_lr_rest_tmp_indic,1,m_g_ir)
						
					}
					"start with next vars"
					i = 2
					"rest vars"
					lr_rest_tmpvars
					while (i<=rows(lr_rest_tmpvars)) {
						"starting with i"
						i
						b_lr_rest_tmp_indic = xtdcce_selectindex(strmatch(outputnames,lr_rest_tmpvars[i]+"*"))
						"tempvar:"
						lr_rest_tmpvars[i]
						if (fast == 0) {
							if (cols(b_lr_rest_tmp_indic) > rows(m_g_ir)) {
								"m_g_ir adjusted"
								m_g_ir = J(cols(b_lr_rest_tmp_indic),1,m_g_ir)
							}
							m_g_ir = xtdcce_PointByPoint((1..rows(m_g_ir)),b_lr_rest_tmp_indic,1,m_g_ir)
												
						}
						
						b_lr_rest_tmp = b_output[b_lr_rest_tmp_indic]
						b_lr_rest = b_lr_rest:+ b_lr_rest_tmp
						i++
					}
					
					/// variance calculation using delta method
					if (fast == 0) { 
						blr_1_tmp = (1:/(b_lr1))
						blr_rest_tmp =  b_lr_rest :/ ((blr_1_tmp):^2)
						
						
						if (rows(blr_rest_tmp) > rows(m_g_i1)) {
							/// bl1 pooled, adjust m_g_i1
							m_g_i1 = J(rows(blr_rest_tmp),1,m_g_i1)
						}
						if (rows(blr_1_tmp) > rows(m_g_ir)  ) {
							/// bl rest pooled, adjust m_g_ir
							m_g_ir = J(rows(blr_1_tmp),1,m_g_ir)
						}
						m_g_i = m_g_ir :* (blr_1_tmp) :+ m_g_i1 :* (blr_rest_tmp )
						"dimensions mg"
						(rows(m_g_i1),cols(m_g_i1))
						(rows(m_g_i),cols(m_g_i))
						
						
						m_g = (m_g \ m_g_i)
					}
					/// calcualte CS-ARDL(i), eq. 36, p. 102 Chudik et. al. 2016 with b_lr1 = 1 - sum phi
					///b_lr_rest = b_lr_rest :/ (1:-b_lr1)
					b_lr_rest = b_lr_rest :/ (b_lr1)
					/// add to outputnames, b_output and cov_output
					b_output = (b_output \ b_lr_rest)
					if (rows(b_lr_rest) == 1) {
						outputnames = (outputnames , ("lr_":+lr_base_i))
					}
					else {
						uniqids = uniqrows(st_data(.,idvar,touse))
						uid = 1
						"names before"
						outputnames
						while (uid <= rows(lr_base_i)) {
							outputnames = (outputnames , ("lr_":+lr_base_i[uid]:+"_":+strofreal(uniqids')))
							uid++
						}
						
						"names after"
						outputnames
						
					}					
					
					/// next base
					basei++
				}
				/// while loop ends here.
				if (fast == 0) {
					"delta method used mg * cov * mg'"
					"dimension: mg"
					m_g
					(rows(m_g),cols(m_g))
					"dimension: cov"
					(rows(output_cov),cols(output_cov))
					output_cov = m_g * output_cov * m_g'
				}
				else {
					output_cov = J(rows(b_output),rows(b_output),.)
				}
			}
			/// ARDL ends here
			
		}
		"lr done"
		if (fast == 1) {
			"fast cov created"
			output_cov = J(rows(b_output),rows(b_output),.)
		}
		"b output dimensions"
		(rows(b_output), cols(b_output))
		"cov output dimensions"
		(rows(output_cov),cols(output_cov))
		return((b_output,output_cov))
		"lr calcualtions done"
	} 
end



capture program drop xtdcce2_separate
program define xtdcce2_separate , rclass
	syntax [anything] , [ ///
			rhs(string) ///
			exogenous_vars(string) ///
			endogenous_vars(string) ///
			touse(string) ///
			idvar(string) ]
			
			**empty output 
			local rhs_list ""
			local endo_list ""
			local exo_list ""
			
			foreach var in `rhs' {
				qui separate `var' if `touse' , by(`idvar') gen(`var'_)
				local rhs_list `rhs_list' `r(varlist)'
				recode `r(varlist)' (missing = 0) if `touse'	
			}		

			foreach var in `exogenous_vars' {
				qui separate `var' if `touse'  , by(`idvar') gen(`var'_)
				local exo_list `exo_list' `r(varlist)'
				recode `r(varlist)' (missing = 0) if `touse'	
			}
			local exo_list `exo_pooled'	 `exo_list' 
					
			foreach var in `endogenous_vars' {
				qui separate `var' if `touse' , by(`idvar') gen(`var'_)
				local endo_list `endo_list' `r(varlist)'
				recode `r(varlist)' (missing = 0) if `touse'	
			}
			
			return local exo_list `exo_list'
			return local endo_list `endo_list'
			return local rhs_list `rhs_list'

		
end

*capture mata mata drop xtdcce_m_touseupdate()
* This program updates touse. 
mata:
	function xtdcce_m_touseupdate(string scalar tousename, string scalar idname, real vector id_new)
	{
		st_view(touse=.,.,tousename)
		st_view(id=.,.,idname)
		done = 0
		if (rows(id)==rows(id_new)) {
			if (mean(id:==id_new)==1) {			
				"all good, copy touse over"
				touse[(1..rows(touse)),1] = J(rows(touse),1,1)
				done = 1
			}
			
		}
		if (done==0) {
			"uneven dimensions"
			_sort(id_new,1)
			rownum = rows(id_new)
			i = 1
			j = 1
			while (j<=rownum) {
				if (id[i]:==id_new[j]) {
					touse[i] = 1
					j++
					i++
				}
				else {
					touse[i] = 0
					i++
				}		
			}	
		}	
	}
end

capture program drop xtdcce_output_table_alpha
program define xtdcce_output_table_alpha
	syntax anything ,[noci]

	tokenize `anything'
	local var `1'
	local col =  `2'
	local b_p_mg `3'
	local se_p_mg `4'
	local cv  `5'
	local i `6'

	di as text %`col's abbrev("`var' ",`=`col'-1') "{c |}"  _continue
	local col = `col' + 3
	di as result _column(`col') %9.8g `b_p_mg'[1,colnumb(`b_p_mg',"`i'")] _continue
	local col = `col' + 8 + 3
	di as result _column(`col') %9.8g `se_p_mg'[1,colnumb(`se_p_mg',"`i'")] _continue
	local col = `col' + 8 + 5
	di as result _column(`col') %9.7g ( `b_p_mg'[1,colnumb(`b_p_mg',"`i'")] - `cv'*`se_p_mg'[1,colnumb(`se_p_mg',"`i'")]) _continue
	local col = `col' + 12
	di as result _column(`col') %9.7g ( `b_p_mg'[1,colnumb(`b_p_mg',"`i'")] + `cv'*`se_p_mg'[1,colnumb(`se_p_mg',"`i'")])

end
