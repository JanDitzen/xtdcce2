*! xtdcce2fast - fast version of xtdcce2
*! version 1.0 - December 2019

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
	
		syntax varlist(min=1 ts) [if/] , [cr_lags(numlist) CRosssectional(varlist ts) noconstant lr(string) fullsample NOTable lr_options(string) NOCROSSsectional cd trace ]
		qui{
			version 15
						
			local cmdline xtdcce2fast `0'
			
			if "`trace'" == "trace" {
				local trace noi
			}
			
			tempvar touse
			marksample touse
			if "`fullsample'" == "" {
				markout `touse' `varlist' `lr'
			}
			** get idvar and tvar
			qui xtset
			local idvar "`r(panelvar)'"
			local tvar "`r(timevar)'"
			
			** create cross sectional lags	
			if "`nocrosssectional'" != "" {
				local crosssectional ""
			}
			
			if "`crosssectional'" != "" {
				tempname csa
				if "`cr_lags'" == "" {
					local cr_lags = 0
				}
				*noi disp "lags `cr_lags'"
				`trace' xtdcce2_csa `crosssectional' , idvar(`idvar') tvar(`tvar') cr_lags(`cr_lags') touse(`touse') csa(`csa')
				local clistfull `r(varlist)'
				local cross_structure `r(cross_structure)'
			}
			
			if "`lr_options'" == "" & "`lr'" != "" {
				local lr_options "ecm"
			}
			
			** markout
			*sort `idvar' `tvar'
			markout `touse' `varlist' `lr' `clistfull'
			
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
				local rest `varlist' `lr' 
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
			tsrevar `varlist' `lr'
			local varlistnew "`r(varlist)'"
			tsunab varlistnames: `varlist' `CleanLR'

			** add constant
			if "`constant'" == "" {
				tempvar constant 
				gen double `constant' = 1
				local varlistnew `varlistnew' `constant'
				local varlistnames `varlistnames' _cons
			}

			** run regressions
			sort `idvar' `tvar'
			tempname b_output V_output stats b_i stats_i v_i
			
			`trace' mata m_xtdcce2fast("`varlistnew'","`clistfull'","`BaseIndex'","`touse'","`idvar' `tvar'","`b_output' `V_output' `stats'",`b_i'=.,`v_i'=.,`stats_i'=.,"`lr_options'")
			
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
		
		ereturn hidden local p_if "`if'"
		ereturn hidden local p_in "`in'"
		
		ereturn local crosssectional "`crosssectional'"
		ereturn local cr_lags "`cr_lags'"
		ereturn hidden local p_mg_vars "`rhsi'"
		
		ereturn local predict "xtdcce2_p"
		
		if "`lr'" != "" {
			ereturn local lr "`CleanLR'"
			ereturn hidden local p_lr_vars_mg "`varlr'"
			ereturn hidden local lr_options "ardl"
		}	
		
		ereturn scalar N_g = `N_g'
		ereturn scalar Kmg = `K'
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
			qui xtcd2 if e(sample)
			ereturn scalar cd = r(CD)
			ereturn scalar cdp = r(p)
			return clear
		}
		
		
		if "`notable'" == "" {
				
			tempname b_mg sd
			matrix `b_mg' = e(b)
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

			
			display as text "(Dynamic) Common Correlated Effects Estimator - Mean Group"
			
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
								_col(37) "=" _col(39) e(T)- e(Kmg);
					di in gr _col(2) "with cross-sectional averages"
							_col(37) "=" _col(39) e(T)-e(Kmg) - `K_csa' ;
				#delimit cr
			}
			else {	
				#delimit ;
					di in gr "Degrees of freedom per group:" 
						_col(`=`maxline'-80+50') in gr in gr "Obs per group:" _col(`=`maxline'-80+68') ;
					di in gr _col(2) "without cross-sectional avg."
						_col(31) "min"
						_col(37) "=" _col(39) e(Tmin)- e(Kmg)
						_col(`=`maxline'-80+68-4') in gr "min = "
						_col(`=`maxline'-80+71') in ye %9.0f e(Tmin) ;	
					di in gr _col(31) "max"
						_col(37) "=" _col(39) e(Tmax) - e(Kmg)
						_col(`=`maxline'-80+68-4') in gr "avg = " 
						_col(`=`maxline'-80+71') in ye %9.0f e(Tbar) ;
					di in gr _col(2) "with cross-sectional avg."
						_col(31) "min"
						_col(37) "=" _col(39) e(Tmin) - e(Kmg)- `K_csa'
						_col(`=`maxline'-80+68-4') in gr "max = " 
						_col(`=`maxline'-80+71') in ye %9.0f e(Tmax) ;
					di in gr _col(31) "max"
						_col(37) "=" _col(39) e(Tmax) - e(Kmg) - `K_csa' ;
				#delimit cr
			}
			if wordcount("`cr_lags'") > 1 {
				mata st_local("cr_lags_min",strofreal(min(strtoreal(tokens("`cr_lags'")))))
				mata st_local("cr_lags_max",strofreal(max(strtoreal(tokens("`cr_lags'")))))
				local cr_lags_disp "`cr_lags_min' to `cr_lags_max'"
			}
			else if wordcount("`cr_lags'") == 1 {
				local cr_lags_disp "= `cr_lags'"
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
						_col(37) "=" _col(39) e(Kmg)*e(N_g) `line2' ;

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
				if wordcount("`cr_lags'") > 1 {
					local crosssectional_output "`cross_structure'"
				}
				else {
					local crosssectional_output "`crosssectional'"
				}
				display  as text "Cross Sectional Averaged Variables: `crosssectional_output'"
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
	
	scalar se = sqrt(`se_p_mg'[colnumb(`se_p_mg',"`i'"),colnumb(`se_p_mg',"`i'")])
	scalar b = `b_p_mg'[1,colnumb(`b_p_mg',"`i'")]
	scalar tt = b / se
	
	scalar pval= 2*(1 - normal(abs(tt)))
	
	
	di as text %`col's abbrev("`var' ",`=`col'-1') "{c |}"  _continue
	local col = `col' + 3
	di as result _column(`col') %9.8g b _continue
	local col = `col' + 8 + 3
	di as result _column(`col') %9.8g se _continue
	local col = `col' + 8 + 3
	di as result _column(`col') %6.2f tt _continue	
	local col = `col' + 10
	di as result _column(`col') %5.3f pval _continue
	local col = `col' + 10
	di as result _column(`col') %9.7g ( `b_p_mg'[1,colnumb(`b_p_mg',"`i'")] - `cv'*`se_p_mg'[1,colnumb(`se_p_mg',"`i'")]) _continue
	local col = `col' + 11
	di as result _column(`col') %9.7g ( `b_p_mg'[1,colnumb(`b_p_mg',"`i'")] + `cv'*`se_p_mg'[1,colnumb(`se_p_mg',"`i'")])
end



capture mata mata drop m_xtdcce2fast()
mata:
	function m_xtdcce2fast (string scalar varlist, 			///
							string scalar csaName,			///
							string scalar LongRunBases,		///
							string scalar touse,			///
							string scalar idtnames,			///
							string scalar outputnames,		///
							real matrix b_iOutput,			///
							real matrix V_iOutput,			///
							real matrix stats_i_name,		///
							string scalar lr_options	)		///
							
	{
		real matrix X
		real matrix Y
		real matrix csa
		real scalar residual
		
		X = st_data(.,varlist,touse)
		Y = X[.,1]
		X = X[.,(2..cols(X))]
		
		residual = J(rows(X),1,.)
		
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
	
		i=1
		covend = K
		covstart = 1
		while (i<=N_g) {
			Xi = X[(index[i,1]..index[i,2]),.]
			Yi = Y[(index[i,1]..index[i,2]),.]
			lower_i[i] =  quadcolsum(((Yi :- mean(Yi)):^2))
			/// partial out
			if (nocsa == 1) {
				csai = csa[(index[i,1]..index[i,2]),.]
				x_p = (Yi,Xi)
				tmp_xp = quadcross(csai,x_p)
				tmp_csa = quadcross(csai,csai)
				x_p = (x_p - csai*m_xtdcce_solver(tmp_csa,tmp_xp))
						
				Yi = x_p[.,1]
				Xi = x_p[.,(2..cols(x_p))]
			}
			
			tmp_xx = quadcross(Xi,Xi)
			tmp_xy = quadcross(Xi,Yi)
			tmp_xx1 = invsym(tmp_xx)
			b_i[i,.] = m_xtdcce_solver(tmp_xx,tmp_xy)'
			
			residi = Yi - Xi * b_i[i,.]'
			
			residual[(index[i,1]..index[i,2]),.] = residi 
			
			s2_i[i] = residi'residi 
			
			stats_i[i,.] = (rows(Xi),cols(Xi))
			
			dfr = rows(Xi) - K- K_csa

			cov_i[(covstart..covend),.] = (J(K,1,idt[i,1]) , tmp_xx1 :* s2_i[i]  / dfr)
			
			covstart = covend + 1
			covend = covend + K

			i++
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
			uniqBase = uniqBase[selectindex(uniqBase:!=0)]
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
				index = selectindex(uniqBase[i]:==LongRunBases)
				"Indexes"
				index
				if (s==1) {
					omega_lag = ecmadjust1 :+ ecmadjust2 :* quadrowsum(b_i[.,index])
					b_i = b_i , -omega_lag
				}
				else {
					omega_x = quadrowsum(b_i[.,index]):/(-omega_lag)
					b_i = b_i, omega_x
				}
				
				i++
				s++
			}			
		}
		
		b_mg = quadmeanvariance(b_i)
		V = b_mg[(2..rows(b_mg)),.]:/N_g
		b_mg = b_mg[1,.]
		
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

