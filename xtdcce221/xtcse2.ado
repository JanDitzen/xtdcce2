*! xtcse2, version 1.02, xxxx 2019
*! author Jan Ditzen
*! www.jan.ditzen.net - j.ditzen@hw.ac.uk
/*
Changelog
**********1.01*****************************
- support for unbalanced panels
**********1.02 - xx.0x.2019
- added support for residuals (BKP 2019)
- bug fixes
*/
program define xtcse2, rclass
	syntax [varlist(default=none ts)] [if], [pca(integer 4) STANDardize nocd inprog size(real 0.1) tuning(real 0.5) Reps(integer 0) RESidual lags(integer 0)]
	version 14
	
	tempname xtdcceest xtdcceesttouse
	cap _estimates hold `xtdcceest' ,  restore copy  varname(`xtdcceesttouse')
		
	preserve
	
		qui{		
		
		tempvar touseAll 
		if "`varlist'" == "" {
			tempvar varl
			gen `touseAll' = e(sample)  
			predict double `varl' if `touseAll' , residuals
			local varn "residuals"
		}
		else {
			gen `touseAll' = 1
			tsrevar `varlist'
			local varl "`r(varlist)'"
			local varn "`varlist'"
		}
		
		if "`if'" != "" {
			keep `if'
		}
		
		keep if `touseAll'
		
		*** Get info
		local unbal = 0
		qui xtset
		if "`r(balanced)'" != "strongly balanced" & "`residual" == "" {
				noi disp "Observations will be restricted to union of time periods across cross sectional units."
				noi disp "Number of observations can become very small."
				noi disp ""
		}
		local idvar "`r(panelvar)'"
		local tvar "`r(timevar)'"
		tempvar tmpid tmpt
		
		*** Result matrix
		tempname ResultMatrix
		mata `ResultMatrix' = J(8,`=wordcount("`varl'")',.)		
		local run = 1

		foreach res in `varl' {
			tempvar touse
			sort `idvar' `tvar'
			gen `touse' = (`res' != . & `touseAll')
			
			egen `tmpid' = group(`idvar') if `touse'
			egen `tmpt' = group(`tvar') if `touse'
			
			** correct minimum
			qui sum `tmpt' if `touse'
			replace `tmpt' = `tmpt' - `r(min)' + 1 if `touse'
			
			qui sum `tmpid' if `touse'
			replace `tmpid' = `tmpid' - `r(min)' + 1 if `touse'	
			
			** restrict to same sample
			if "`residual'" == "" {
				tempname NumCross NumCrossIndic
				by `tmpt', sort: egen `NumCross' = total(1) if `touse'
				qui sum `tmpid' if `touse'
				gen `NumCrossIndic' = (`r(max)'==`NumCross') 
				
				qui sum `NumCrossIndic' if `touse'
				local NCI = r(sum)
				qui sum `touse' if `touse'
				local TI = r(sum)
				
				replace `touse' = `NumCrossIndic' 
				
				if `TI' != `NCI'{
					local vartmp = word("`varn'",`run')
					noi disp "Number of observations for variable `vartmp' adjusted from `TI' to `NCI' due to unbalanced panel or variable."
					drop `tmpid' `tmpt'
					egen `tmpid' = group(`idvar') if `touse'
					egen `tmpt' = group(`tvar') if `touse' 
					
					** correct minimum
					qui sum `tmpt' if `touse'
					replace `tmpt' = `tmpt' - `r(min)' + 1 if `touse'
				
					qui sum `tmpid' if `touse'
					replace `tmpid' = `tmpid' - `r(min)' + 1 if `touse'	
					
				}			
				
				drop `NumCrossIndic'  `NumCross'			
			
				*** standardize						
				if "`standardize'" != "" {
					tempvar meantmp sdtmp
					by `tmpid', sort: egen `meantmp' = mean(`res') if `touse'
					by `tmpid', sort: egen `sdtmp' = sd(`res') if `touse'
					replace `res' = (`res' - `meantmp') / `sdtmp' if `touse'
					drop `meantmp' `sdtmp'
				}
				sort `idvar' `tvar'
				
				tempname xx PC 
				tempname eigenval eigenvec
				sum `tmpt' if `touse'
				local T = `r(max)'

				mata `xx' = st_data(.,"`res'","`touse'")
				mata `xx' = colshape(`xx',`T')'

				mata st_local("Nt",strofreal(cols(`xx'))) 
				
				if `Nt' < `T' {
					mata eigensystem(`xx''*`xx',`eigenvec'=.,`eigenval'=.)
					mata `eigenvec' = Re(`eigenvec'[.,(1..`pca')])
					mata `PC'= `xx' * `eigenvec'
				}
				else {
					mata eigensystem(`xx'*`xx'',`eigenvec'=.,`eigenval'=.)
					mata `PC' = Re(`eigenvec'[.,(1..`pca')])
				}
				
				***eit and mub
				tempvar partial eit
				tempname eitm  eitt eitv
				gen double `partial' = `res'
				gen double `eit' = .
				tempvar tousei
				gen `tousei' = 0
				mata `eitv' = .
				
				sort `idvar' `tvar'
				mata st_view(`eitv',.,"`eit'","`touse'")
				
				
				mata `eitm' = `xx' - `PC'*m_xtdcce_inverter(quadcross(`PC',`PC')) * quadcross(`PC',`xx')
				
				sum `tmpid'
				forvalues i = 1(1)`r(max)' {			
					*** eit
					replace `tousei' = (`tmpid' == `i' & `touse')
					mata `eitt' = selectindex(st_data(.,"`tousei'","`touse'"))
					mata `eitv'[`eitt',.] = `eitm'[.,`i']
					
					replace `tousei' = 0
				}		
				
				mata mata drop `eitm'  `eitt' `eitv'
				*** mata program which calcultes alpha and alpha hat
				tempname alphas 
				sort `idvar' `tvar'
				mata `alphas' =  xtdcce_m_alphaest("`res'","`eit'","`tmpid'","`tmpt'","`touse'",`size')
			
				drop `eit'
			}
			else {
				tempname alphas 
				noi mata `alphas' = xtdcce_m_alpha_res("`res'","`tmpid'","`tmpt'","`touse'",`size',`tuning',`lags',`reps')
				
				sum `tmpt' if `touse'
				local T = `r(max)'
				
				sum `tmpid' if `touse'
				local Nt = `r(max)'
			}
			*** CD Test
			if "`cd'" == "" & "`inprog'" == "" {		
				cap xtcd2 `res' , noest
				if _rc == 199 {
					noi display as error "xtcd2 not installed" 
					local cd nocd
				}
				else if _rc != 0 {
					noi display as error "xtcd2 caused error. Please do test by hand."
					local cd nocd
				}
				else {
					tempname CD CDp
					scalar `CD' = r(CD)
					scalar `CDp' = r(p)
				}
			}
			mata `ResultMatrix'[(1..4),`run'] = `alphas' 
			mata `ResultMatrix'[(7,8),`run'] = (`Nt' \ `T')
			if "`cd'" == "" {
				mata `ResultMatrix'[(5,6),`run'] = (`=r(CD)' \ `=r(p)') 
			}
			
			local run = `run' + 1
			drop `touse' `tmpid' `tmpt'
		}
	}
	restore
	*cap _estimates unhold `xtdcceest', copy
	
	** Output
	tempname alpha_circ alpha_circSE CDm CDpm Nm Tm alpham tm
	
	if "`residual'" == "" {
		mata st_matrix("`alpha_circ'",`ResultMatrix'[3,.])
		mata st_matrix("`tm'",`ResultMatrix'[3,.]:/`ResultMatrix'[4,.])
		mata st_matrix("`alpham'",`ResultMatrix'[(1..3),.])
		mata st_matrix("`alpha_circSE'",`ResultMatrix'[4,.])
	}
	else {
		tempname alpha_circLow alpha_circUp
		mata st_matrix("`alpha_circ'",`ResultMatrix'[1,.])
		mata st_matrix("`alpha_circSE'",`ResultMatrix'[2,.])
		mata st_matrix("`alpha_circLow'",`ResultMatrix'[3,.])
		mata st_matrix("`alpha_circUp'",`ResultMatrix'[4,.])
	}
	mata st_matrix("`CDm'",`ResultMatrix'[5,.])
	mata st_matrix("`CDpm'",`ResultMatrix'[6,.])
	mata st_matrix("`Nm'",`ResultMatrix'[7,.])
	mata st_matrix("`Tm'",`ResultMatrix'[8,.])
	
	
	foreach mat in alpha_circ alpha_circLow alpha_circLow alpha_circUp alpha_circSE CDm CDpm Nm Tm alpham tm {
		cap matrix colnames ``mat'' = `varn'
	}
	matrix rownames `alpha_circ' = "alpha"
	matrix rownames `alpha_circSE' = "alpha"
	matrix rownames `CDm' = "CD"
	matrix rownames `CDpm' = "CDp"
	matrix rownames `Nm' = "N_g"
	matrix rownames `Tm' = "T"
	cap matrix rownames `alpham' = "alpha hat" "alpha tilde" "alpha"
	cap matrix rownames `tm' = "t"

	
	
	*** Setting for Output
	local maxline = c(linesize)	
	**allow max linesize of 100
	if `maxline' > 100 {
		local maxline = 100
	}
	
	**get var length
	local maxlength = 0
	foreach var in `varl' {
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
	
	*** Output
	if "`inprog'" == "" {
		noi disp as text "Cross-Sectional Dependence Exponent Estimation and Test"
		#delimit ;
			di _n in gr "Panel Variable (i): " in ye abbrev("`idvar'",`abname') in gr;
			di in gr "Time Variable (t): " in ye abbrev("`tvar'",`abname')  ;
	 
		#delimit cr
		di ""
	}
	noi disp as text "Estimation of Cross-Sectional Exponent (alpha)"
	
	local level =  `c(level)'
	if "`residual'" != "" {
		local level 95
	}
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

	foreach var in `varn' {
		if "`residual'" == "" {
			xtdcce_output_table `var' `col_i' `alpha_circ' `alpha_circSE'  cv `var'	
		}
		else {
			xtdcce_output_table_res `var' `col_i' `alpha_circ' `alpha_circSE' `alpha_circLow' `alpha_circUp' `var'	
		}
	}
	di as text "{hline `col_i'}{c BT}{hline `=`maxline'-`col_i'-15'}"
	di "0.5 <= alpha < 1 implies strong cross-sectional dependence."
	
	if "`residual'" != "" {
		if `reps' > 0 {
			di as text "SE and CI bootstrapped with `reps' repetitions."
		}
		else {
			di as text "SE and CI not available. Use option reps() to bootstrap SE and CI."
		}
	}
	
	if "`cd'" == "" {
		di ""
		di as text "Pesaran (2015) test for weak cross-sectional dependence."
		di as text "H0: errors are weakly cross-sectional dependent."
		
		di as text "{hline `col_i'}{c TT}{hline `=`maxline'-`col_i'-22'}"
		di as text %`col_i's  abbrev("variable",`abname') "{c |}" _c
		local col = `col_i' + 1 + 6
		di as text _col(`col') "CD" _c
		local col = `col' + 5 + 4
		di as text _col(`col') "p-value"  _c
		local col = `col' + 9 + 4
		di as text _col(`col') "N_g"  _c
		local col = `col' + 2 + 9
		di as text _col(`col') "T"  
		di as text "{hline `col_i'}{c +}{hline `=`maxline'-`col_i'-22'}"
		
		foreach var in `varn' {
			xtdcce_output_tableCD `var' `col_i' `CDm' `CDpm' `Nm' `Tm' `var'	 
		}
		di as text "{hline `col_i'}{c BT}{hline `=`maxline'-`col_i'-22'}"
	}
	
	
	*** Return
	
	return matrix alpha = `alpha_circ'
	cap return matrix alphas = `alpham'
	return matrix alphaSE = `alpha_circSE'
	return matrix N_g = `Nm'
	return matrix T = `Tm'	
	
	if "`cd'" == "" & "`inprog'" == "" {
		return matrix CD = `CDm'
		return matrix CDp = `CDpm'
	}
	
	
end

** auxiliary file with auxiliary programs
findfile "xtdcce2_auxiliary.ado"
include "`r(fn)'"

capture mata mata drop xtdcce_m_alphaest()
mata:
	function xtdcce_m_alphaest (
								string scalar x_name,
								string scalar eit_name,
								string scalar id_name,
								string scalar t_name,
								string scalar touse,
								real scalar a_size)
	{
		
		x = st_data(.,x_name,touse)
		///xp = st_data(.,xp_name,touse)
		eit = st_data(.,eit_name,touse)
		id = st_data(.,id_name,touse)
		t = st_data(.,t_name,touse)

		idt = (id,t)
		
		T = rows(uniqrows(t))
		N = rows(uniqrows(id))
		/// cacluate sigma hat x bar (Eq. 10, BKP)
		/// sigma2 = 1/T sum (xbar(t) - xbar)^2, with xbar = 1/T sum x(t); xbar(t) = 1/N sum x(i,t)
		xbart = J(T,cols(x),.)
		et = J(T,cols(x),.)
		xm = J(T,N,.)
		i = 1
		/// loop necessary because difference between xbart and xbar, need to get xbart
		/// build et here (see below)
		while (i <=T) {
			indic = selectindex(idt[.,2] :==i)
			
			xbartt = quadcolsum(x[indic,.]):/N
			xbart[i,.] = xbartt
			
			ett = quadcolsum(eit[indic,.]):/N
			et[i,.] = ett
			xm[i,.] = x[indic,.]'
			i++
		}
		xbar = quadcolsum(xbart):/T
		/// add T-1 for small sample adjustment
		sigma2 = quadcolsum((xbart:-xbar):^2):/(T-1)
		/// alpha hat (eq 11 or 33, BKP)
		/// alpha = 1 + 1/2 * ln(sigma hat 2 xbar)/ln(N) ; in eq. 33 no 1/2?!
		alpha = 1 :+ 1:/2 :* ln(sigma2) :/ ln(N)				
		
		/// alpha tilde (eq 34)
		/// alpha tilde = alpha hat - cn / (2 * ln(N) * N * sigma hat x) 
		/// cnhat = 1/n sum sigma hat i, sigma hat i = 1/T sum u(i,t)^2, with the partialled out variables; 
		/// just the sum over all divided by NT; doesnt work because sum will be zero! (see p. 940)
		/// use alternative for cn, using PCA; PCA done in Stata program
		/// cntilde = 1/T sum sqrt(N) (et(t) - sqrt(N) e)^2
		/// with et = 1/N sum e(i,t); e = 1/T sum et
		
		e=quadcolsum(et):/T
		cntilde = 1:/(T-1) :* quadcolsum((sqrt(N):*(et:-e)):^2)
		alphatilde= alpha :- cntilde :/ (2:*ln(N) :* N :* sigma2)
		
		
		/// mu calculation
		/// Step 1, OLS of x on xbar
		xbart_I = (J(T,1,1),xbart)
		tmp_xx1 = quadcross(xbart_I,xbart_I)
		coef = m_xtdcce_inverter(tmp_xx1)*quadcross(xbart_I,xm)
		etmp = xm - xbart_I * coef
		s2 = etmp'etmp:/(T-cols(xbart_I))
		se = sqrt(diagonal(s2*m_xtdcce_inverter(quadcross(xbart,xbart))))
		/// step 2, calculate t stat
		t_test = (coef[2,.]:/se[1,.])'
		/// step 3, calculate xstr with cp = invnormal(1-p/(2 * (N-i)) [ Holms approach]
		size = J(N,1,0)
		x_str = J(T,1,0)
		order = (1::N)
		/// reverse order (i.e. largest to smallest)
		s_ttest = sort((abs(t_test),order),1)[(rows(t_test)..1),.]
		j=1
		while(j<=cols(coef)) {
			p_n = a_size / (N-j+1)
			theta =  invnormal(1-p_n/2)
			if (abs(s_ttest[j,1]) >= theta) {
				size[j,1]=1
			}
			else {
				size[j,1]=0
			}			
			j++
		}
		s_size = sort((size,s_ttest[.,2]),2)
		
		x_str = (s_size[.,1]':*xm)'
		x_str1 = x_str[selectindex(x_str[.,1]:!=0),.]
		/// step 4
		if (x_str[1,1] == .) {
			"missings"
			theta = 1
		}
		else {
			x_str1 = x_str1'
			x_strb = mean(x_str1')'
			theta = mean((x_strb:-mean(x_strb)'):^2)
		}
		alphacircle = alphatilde :- (1/2) :* ln(theta):/ln(N)
		
		
		/// standard error of alphacircle, from Appendix Eq. B47
		p = ceil(T^(1/3))
		x_bar1 = mean(xm')'
		x_bar1_c = x_bar1
		std_x_bar1 = sqrt(diagonal(quadvariance(x_bar1_c)))
		x_bar = x_bar1_c :/ std_x_bar1
		m_x_bar = mean(x_bar)'
		x_bar_stand = J(T,1,0)
		i=1
		while (i<=T) {
			x_bar_stand[i,.] = x_bar[i,.] - m_x_bar
			i++
		}
		/// As in Gauss/BKP use NW method
		x_bar_2m=x_bar_stand:^2
		m_x_bar_2m=mean(x_bar_2m)'
		x_bar_2m_st=J(T,1,0)
		i = 1
		while (i<=T) {
			x_bar_2m_st[i,.]=x_bar_2m[i,.]:-m_x_bar_2m
			i++
		}
		x_bar_2m_st_lag=J(rows(x_bar_2m_st),p,0)
		i = 1
		while (i<=p) {
			xbartmp = J(rows(x_bar_2m_st),1,.)
			xbartmp[(i+1..rows(xbartmp))] = x_bar_2m_st[(1..rows(x_bar_2m_st)-i),.]
			x_bar_2m_st_lag[.,i]=xbartmp
			i++
		}
		v_all=(x_bar_2m_st,x_bar_2m_st_lag)
		v_1=v_all[(p+1..T),.]
		dv=v_1[.,1]
		rhs=v_1[.,(2..p+1)]
		b=m_xtdcce_inverter(quadcross(rhs,rhs))*quadcross(rhs,dv)
		s_b=colsum(b)
		e_nw=dv-rhs*b
		sse_nw=e_nw'*e_nw
		sig2_nw=sse_nw:/(T-cols(rhs))
		v_f_2=sig2_nw:/(1-s_b)^2
		
		ggg_o = round(N^alphatilde)
		if (ggg_o >= N ){
			ggg_o = N
		}
		else if (ggg_o < 1) {
			ggg_o = 1
		}

		c_avg = m_xtdcce_inverter(quadcross(x_bar,x_bar))*quadcross(x_bar,xm)
		c_avg_sel = (sort((c_avg',abs(c_avg')),2))[(rows(c_avg')..1),.] 
		c_avg_sel = c_avg_sel[(1..ggg_o),1]
		
		m_c_avg_sel=mean(c_avg_sel)'
		frasel=J(1,ggg_o,0)
		i = 1
		while (i <= ggg_o) {
			frasel[.,i]=(c_avg_sel[i,1]-m_c_avg_sel):^2
			i++
		}
		s_frasel=quadcolsum(frasel')
		
		ggg_t=round(N^(alphacircle))
		
		if (ggg_t >= N) {
			ggg_t = N
		}
		else if (ggg_t<1) {
			ggg_t = 1
		}
		c_avg_selt = (sort((c_avg',abs(c_avg')),2))[(rows(c_avg')..1),.]
		c_avg_selt=c_avg_selt[(1..ggg_t),1]
		m_c_avg_selt=mean(c_avg_selt)'
		fraselt = J(1,ggg_t,0)
		
		i = 1
		while (i<=ggg_t) {
			fraselt[.,i]=(c_avg_selt[i,1]:-m_c_avg_selt):^2
			i++
		}
		s_fraselt=quadcolsum(fraselt')
		
		SE = ((1/T)*(v_f_2)+(4/N)*(N^(1-alphacircle)*s_fraselt/(ggg_t-1)))^(1/2)/(2*ln(N))
		C = (alpha \ alphatilde\alphacircle \ SE)
		return(C)		
	}
end


capture program drop xtdcce_output_table
program define xtdcce_output_table
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

capture program drop xtdcce_output_table_res
program define xtdcce_output_table_res
	syntax anything ,[noci]

	tokenize `anything'
	local var `1'
	local col =  `2'
	local b_p_mg `3'
	local se_p_mg `4'
	local cvlow  `5'
	local cvup  `6'
	local i `7'

	di as text %`col's abbrev("`var' ",`=`col'-1') "{c |}"  _continue
	local col = `col' + 3
	di as result _column(`col') %9.8g `b_p_mg'[1,colnumb(`b_p_mg',"`i'")] _continue
	local col = `col' + 8 + 3
	di as result _column(`col') %9.8g `se_p_mg'[1,colnumb(`se_p_mg',"`i'")] _continue
	local col = `col' + 8 + 5
	di as result _column(`col') %9.7g ( `cvlow'[1,colnumb(`cvlow',"`i'")] ) _continue
	local col = `col' + 12
	di as result _column(`col') %9.7g ( `cvup'[1,colnumb(`cvup',"`i'")] )

end

capture program drop xtdcce_output_tableCD
program define xtdcce_output_tableCD
	syntax anything 

	tokenize `anything'
	local var `1'
	local col =  `2'
	local CD `3'
	local CDp `4'
	local N `5' 
	local T  `6'
	local i `7'

	di as text %`col's abbrev("`var' ",`=`col'-1') "{c |}"  _continue
	local col = `col' + 3
	di as result _column(`col') %9.3f `CD'[1,colnumb(`CD',"`i'")] _continue
	local col = `col' + 8 + 3
	di as result _column(`col') %9.3f `CDp'[1,colnumb(`CDp',"`i'")] _continue
	local col = `col' + 8 + 1
	di as result _column(`col') %9.0f `N'[1,colnumb(`N',"`i'")] _continue
	local col = `col' + 9
	di as result _column(`col') %9.0f `T'[1,colnumb(`T',"`i'")] 

end

capture mata mata drop xtdcce_m_alpha_res()
mata:
	function xtdcce_m_alpha_res (
								string scalar resid_name,
								string scalar id_name,
								string scalar t_name,
								string scalar touse,
								real scalar a_size,
								real scalar delta,
								real scalar H,
								real scalar B)
								
								
	{
		r = st_data(.,resid_name,touse)
		id = st_data(.,id_name,touse)
		tvec = st_data(.,t_name,touse)
		
		idt = (id,tvec)
		
		t_uniq = uniqrows(tvec)
		T = rows(t_uniq)
		N = rows(uniqrows(id))

		zm = J(T,N,.)

		i=1
		if (H==0) {	
			/// case of non dynamic panel (strong exogenous regressors)
			/// standardize residuals
			
			"no lags"
			while (i<=N) {
				indic = selectindex(idt[.,1]:==i)
				tindic = idt[indic,2]
				
				eit = r[indic,.]
				sd = sqrt(eit'eit:/rows(eit))
				if (hasmissing(sd):==1) {
					"sd has missing"
					i
				}
				if (hasmissing(eit):==1) {
					"eit has missing in"
					i
				}
				
				tti = xtdcce2_mm_which2(t_uniq,tindic)
				zm[tti,i] = eit / sd
				i++
				
			}	
			
		}
		else {
			/// standardize residuals			
			"with lags"
			
			while (i<=N) {
				indic = selectindex(idt[.,1]:==i)
				tindic = idt[indic,2]
				
				eit = r[indic,.]
				ii = (1::rows(eit))
				/// standard error, sum over eit^2 over ii-1 for small sample adjustment
				sd = sqrt(quadrunningsum((eit:^2)):/(ii:-1))				
				tti = xtdcce2_mm_which2(t_uniq,tindic)
				zm[tti,i] = eit :/ sd
				i++
			}
	
		}
		/// Point Estimate
		alpha=xtdcce_m_alphares_calc(zm,id,tvec,a_size,delta,H) 
		
		/// Bootstrap for SE
		if (B > 0) {
			alphab = J(B,1,.)
			b = 1
			while (b<=B) {
				/// resample columns of r
				indic = runiformint(1,cols(zm),1,cols(zm))				
				rb = zm[.,indic]
				alphab[b] = xtdcce_m_alphares_calc(rb,id,tvec,a_size,delta,H) 
				b++
			}
			alphabq
			if (hasmissing(alphab)==1) {
				alphab = alphab[selectindex(alphab:!=.)]
			}			
			alphabq = mm_quantile(alphab,1,(1-c("level")/100, c("level")/100))
			///alphabq = mm_quantile(alphab,1,((100-c("level"))/2,(1-(100-c("level"))/2,(100-c("level"))/2))
			alphabq
			alpha = (alpha \ sqrt(quadvariance(alphab)) \alphabq')
			
		}
		else {
			alpha = (alpha \ . \ . \ .)
		}
		return(alpha)
	}
end


capture mata mata drop xtdcce_m_alphares_calc()
mata:
	function xtdcce_m_alphares_calc(
								real matrix z,
								real matrix id,
								real matrix tvec,
								real scalar a_size,
								real scalar delta,
								real scalar H)
	{


		idt = (id,tvec)
		T = rows(uniqrows(tvec))
		N = rows(uniqrows(id))
		rho = J(N,N,.)

		if (H==0) {
			/// case of strong exogenous regressors	
			/// calculate rho_ij
			i=1		
			while (i<=N) {
				j = i + 1
				while (j<=N) {
					zi = z[.,i]
					zj = z[.,j]
		
					indici = zi :!=.
					indicj = zj :!=.
					
					indic = selectindex(indici:*indicj)

					///rhoij = quadsum(zi[indic,.]:*zj[indic,.]) :/ (rows(indic))
					rhoij = quadcorrelation((zi[indic,.],zj[indic,.]))[2,1]
					rho[i,j] = rhoij
					rho[j,i] = rhoij
					j++
				}
				i++ 				
			}
		}
		else {
			/// case of weak exogenous regressors			
			/// correct H if H > T
			if (H> rows(z)) {
				H = rows(z)-1
			}
			/// calculate rho_ij
			i=1		
			while (i<=N) {
				j = i + 1
				while (j<=N) {
					zi = z[(H..rows(z)),i]
					zj = z[(H..rows(z)),j]					
					
					indici = zi :!=.
					indicj = zj :!=.
					
					indic = selectindex(indici:*indicj)	
				
					rhoij = quadsum(zi[indic,.]:*zj[indic,.]) :/ (rows(indic)-H)
					///rhoij = quadcorrelation((zi[indic,.],zj[indic,.]))[2,1] :* rows(indic) :/ (rows(indic-H))
					
					rho[i,j] = rhoij
					rho[j,i] = rhoij
					j++
				}
				i++ 				
			}
		}
		/// calcualte indicator
		n = 0.5*N*(N-1)
		cp = invnormal(1-(a_size/2)/(n:^delta)):/sqrt(T)
		indic = ((abs(rho) :> cp))
		rho = abs(rho):*indic
		_editmissing(rho,0)
		/// make diagonal to 1
		deltaij = rho
		_diag(deltaij,1)
		tau = J(N,1,1)
		tau' * deltaij * tau
		alphatilde = ln(tau' * deltaij * tau) :/ (2*ln(N))
		return(alphatilde)
	}
	
end

