*! xtcd2 4.1 May 2023
*! author Jan Ditzen
*! see viewsource xtcd2.ado for more info.

/* 
Jan Ditzen - jan.ditzen@unibz.it
							
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
	25.11.2019 Added heatplot and contour plots for rho
	25.02.2020 Added ts support
	08.02.2021 Added Juodis, Reese weighted CD test (cdw), repetions and pea
	23.02.2021 Added option noadjust; do not remove means from vars; now standard to remove means!
	23.09.2022 Improved calculation for unbalanced panels, fixed bug that CD test statistic becomes 1, added EM algorithm from xtnumfac for CDstar test
	04.11.2022 Added seed() option to control seed for CDW test.
	15.05.2023 Added trace option; bug fixed when using noadjust option
	*/
cap program drop xtcd2
program define xtcd2, rclass
syntax [anything] [if] , [repeatoutput VERsion *]
		version 14

	if "`version'" != "" {
		di in gr "Version 4.0"
		*ereturn clear
		return local version 4.0
		exit	
	}

	if "`repeatoutput'" == "" {
		xtcd2_int `anything' `if' , `options'
	}

	xtcd2_output
	return add
end

cap program drop xtcd2_int
program define xtcd2_int, rclass
	syntax [varlist(default=none ts)] [if] [, KDENsity name(string) rho ///
			NOESTimation /// not needed anymore
			 contour(string) CONTOURmap order(varlist max=1) heatplot(string) HEATPLOTmap reps(integer 1) NOADJust ///
			/// repeat output
			repeatoutput ///
			/// defactor variable first
			DEFACtor ///
			/// which method
			pea  pesaran cdw cdstar pca(real 4)) ///
			///
			seed(string) ///
			/// loop method for unbalanced panels and if noadjust option used
			loop ///
			/// force using unbalanced algorithm
			FORCEUnbalanced ///
			trace ]
	
	
	if "`trace'" != "" local trace noi
	else local trace qui
	
	if "`repeatoutput'" != "" {
		xtcd2_output
		exit
	}

	if `pca' < 1 {
		noi disp "Option pca() must be larger than zero."
		error 199
	}
	
	if "`contour'`contourmap'" != "" & "`heatplot'`heatplotmap'" != ""  {
		noi disp "Options heatplot and contour cannot combined"
		exit
	}
	
	local methods ""
	if "`pesaran'`cdw'`pea'`cdstar'" == "" {
		local methods "CD CDw CDwplus CDstar"
		local pesaran pesaran
		local cdw cdw
		local pea pea
		local cdstar cdstar
	}
	else {
		if "`pesaran'" != "" local methods `methods' CD
		if "`cdw'" != "" | "`pea'" != "" local methods `methods' CDw
		if "`pea'" != "" local methods `methods' CDwplus
		if "`cdstar'" != "" local methods `methods' CDstar
	}

	if "`cdw'" != "" & "`seed'" != "" {
		set seed `seed'
	}

	*if regexm("`methods'","CDw") & 	`reps' == 1 local reps = 30

	
	
	tempvar id_n time_new 
	
	preserve
		if "`if'" != "" {
			`trace' keep `if'
		}
		
		** Check which estimation command
		if "`varlist'" == "" {
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
			local residual_name "residuals"
		}
		
		if "`noadjust'" == "" {
			local noadjust = 0
		}
		else {
			local noadjust = 1
			local loop loop
		}
		tsunab varlist :  `varlist', min(0)
	
		if "`varlist'" == "" {
			tempname varlist
				
			predict `varlist'  , `restype'
			`trace' keep if e(sample)
				
		}
		
		*}
		**Test if sample exists
		`trace' sum `varlist'
		if "`r(N)'" == "0" {
			display in red "Error: no sample set"
			exit
		}
		** Determine N, T and type of panel
		qui xtset2   , checkvars(`varlist') 
		local id "`r(panelvar)'" 
		local timevar "`r(timevar)'"
		local balanced  "`r(balanced)'"
		sort `id' `timevar'
		
		egen `id_n' = group(`id')
		egen `time_new' = group(`timevar')
		
		if  "`balanced'" != "strongly balanced" {
			local balanced = 0
			**Fill Panel such that panel has entries for any year and id
			timer on 97
			tsfill, full
			timer off 97			
			display  "Unbalanced panel detected, test adjusted."
			}
		if "`balanced'" == "strongly balanced" {
			local balanced = 1
		}
		if "`forceunbalanced'" != "" local balanced = 0
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
		
		tempname RhoID
		qui putmata `RhoID' = `id' , replace
		mata `RhoID' = uniqrows(`RhoID')
		
		tempname CD CDp rhoMat
		`trace' sum `varlist'
		`trace' mata xtcd2_CD("`varlist'",`N',`T',`balanced',`noadjust',`pca',"`methods'",("`defactor'"!=""),`reps',"`CD'","`CDp'",("`rho'`heatplot'`heatplotmap'`kdensity'`contour'"!= ""),`rhoMat'=.,("`loop'"!=""))



		**** Return
		return clear

		return matrix CD = `CD', copy
		return matrix p = `CDp', copy
		return local methods "`methods'"
		return hidden local varlist "`varlist'"
		return hidden local residual_name "`residual_name'"
		return hidden local pesaran "`pesaran'"
		return hidden local cdw "`cdw'"
		return hidden local pea "`pea'"
		return hidden local cdstar "`cdstar'"
		return hidden local npca "`pca'"
		return hidden local CDN_g "`N'"
		return hidden local CDT "`T'"
		return hidden local noadjust = `noadjust'

		*** here order program
		if "`order'" != "" {
			tempname rho_tmp
			foreach var in `varlist' {
				mata `rho_tmp' = asarray(`rhoMat',"`var'")
				mata `rho_tmp' = xtcd2_rho_order(`rho_tmp',"`order'","`id'")
				mata `RhoID'_`var' = `rho_tmp'[.,cols(`rho_tmp')]
				mata asarray(`rhoMat',`rho_tmp'[.,1..cols(`rho_tmp')-1],"`var'")
			}
		}		
		
		
		if "`kdensity'" == "kdensity" {
			tempname rho_tmp
			foreach var in `varlist' {
				mata `rho_tmp' = asarray(`rhoMat',"`var'")
				mata: `rho_tmp'  = colshape(`rho_tmp' ,1) / sqrt(2*`T')
				drop _all
				getmata `rho_tmp' , replace
				qui sum `rho_tmp' , detail
				foreach s in mean min max p25 p50 p75 {
					local s`s' : di %9.3f `r(`s')'
					local s`s' = trim("`s`s''") 
				}
				local sN: di %9.0f `r(N)'
				local sN = trim("`sN'")
				tempname CDi pi
				scalar `CDi' = `CD'["`var'",1]
				scalar `pi' = `CDp'["`var'",1]
				local cd: di %9.3f  `CDi'
				local cd = trim("`cd'")
				local pval: di %9.3f  `pi'
				local pval = trim("`pval'")
				if "`name'" != "" {
					local graphname name(`name'_`var' , replace) nodraw
				}
				else if wordcount("`varlist'") > 1 {
					local graphname name(`var', replace)
				}
				qui kdensity `rho_tmp' , xtitle({&rho}{sub:ij}) title(Cross-Sectional Correlations) `graphname' ///
					note("{bf:Statistics:} CD = `cd', p-value: `pval'" "Obs: `sN', Mean: `smean'" "Min: `smin', Max: `smax'" "Percentiles:" "25%: `sp25' , 50%: `sp50', 75:% `sp75'")  
			}
		}	
		
		if "`rho'" != "" {
			tempname rho_tmp
			local NumVar = wordcount("`varlist'")
			foreach var in `varlist' {
				mata st_matrix("`rho_tmp'",asarray(`rhoMat',"`var'"))
				if `NumVar' > 1 {
					return matrix rho_`var' = `rho_tmp'
				}
				else {
					return matrix rho = `rho_tmp'
				}
			}
		}

		
		
	restore
	

	
	if "`contour'`contourmap'`heatplot'`heatplotmap'" != "" {
		tempname rho_tmp
		qui {
			foreach var in `varlist' {
				mata `rho_tmp' = asarray(`rhoMat',"`var'")
				preserve
					clear
					local 0 ", `contour' `heatplot'"

					syntax [anything] , [ABSolute interp(string) levels(real 0) yscale(string) * ]

					if "`absolute'" != "" {
						mata `rho_tmp' = abs(`rho_tmp')
					}
					
					if `levels' == 0 {
						if `N' < 30 {
							local levels = `N'
						}
						else {
							local levels = 30
						}				
					}
					
					if "`interp'" == "" {
						local interp "none"
					}
					
					if "`yscale'" == "" {
						local yscale  "reverse"
					}
					
					///mata _makesymmetric(RHO_output) 
					mata `rho_tmp' = xtcd2_rho_sym(`rho_tmp')	
					mata `rho_tmp' = xtcd2_lowersym(`rho_tmp')
					
					tempname ordermat
					mata `ordermat' = (1::rows(`rho_tmp'))
					
					mata st_local("namelist",(invtokens("rho_" :+ strofreal(`ordermat')')))
					
					
					
					getmata ( `namelist' ) = `rho_tmp' id1=`ordermat'
					
					reshape long rho_ , i(id1) j(id2)
					rename rho_ rho
					label var id1 "`id'"
					label var id2 "`id'"
					sort id1 id2
					
					if "`name'" != "" & wordcount("`varlist'") == 1 {
						local graphname name(`name') 
					}	
					else  {
						local graphname name(`var')
					}
					
					noi dis ""
					

					if "`contour'`contourmap'" != "" {
						twoway contour rho id1 id2 , interp(`interp') `options' level(`levels') yscale(`yscale') `graphname'
					}
					else {

						heatplot rho id1 id2 ,  `options' level(`levels') yscale(`yscale') `graphname'
					}
				restore
			}
		}
	}
	
end

** auxiliary file with auxiliary programs
findfile "xtdcce2_auxiliary.ado"
include "`r(fn)'"

/// output program
cap program drop xtcd2_output
program define xtcd2_output
	disp ""
	disp as text "Testing for weak cross-sectional dependence (CSD)"
	disp as text "{col 4}H0: weak cross-section dependence"
	disp as text "{col 4}H1: strong cross-section dependence"

	local firstline "{col 16}{c |}"
	local nums = 0
	local coli_start = 15
	local coli = `coli_start' + 5
	local step = 14

	tempname CD CDp
	matrix `CD' = r(CD)
	matrix `CDp' = r(p)
	
	local methods: colnames `CD'
	local varlist: rownames `CD'

	foreach type in `methods' {
		
		local firstline "`firstline'  {col `coli'} `type'"
		local coli = `coli' + `step'	
		local nums = `nums' + 1
	}
	

	disp as text "{hline 15}{c TT}{hline `=`nums'*`step''}"
	disp as text "`firstline'"
	disp as text "{hline 15}{c +}{hline `=`nums'*`step''}"
	local i = 1
	foreach var in `varlist' {
		local listi1 ""
		local listi2 ""
		local j = 1
		
		foreach ests in `methods' {
			local val1 : disp %7.2f `CD'[`i',`j']
			local listi1 `"`listi1' {col `=`coli_start'+(`j'-1)*`step'+1'} `val1'"'

			local val2 : disp %4.3f `CDp'[`i',`j']
			local listi2 `"`listi2' {col `=`coli_start'+(`j'-1)*`step'+1'} (`val2')"'
			*disp as text "{col `=`coli_start'+(`j'-1)*`step''}" as result %7.2g  _continue
			local j = `j' + 1
		}
		if "`r(residual_name)'" != "" local var "`r(residual_name)'"
		disp as text  abbrev("`var'",13) "{col `=`coli_start'+1'}{c |}"  as result "`listi1'"
		disp as text "{col `=`coli_start'+1'}{c |}" as result "`listi2'"
		local i = `i' + 1
	}
	local NumVar = `j'

	disp as text "{hline 15}{c BT}{hline `=`nums'*`step''}"
	*disp as text "Under null, CD ~ N(0,1)."
	disp as text "p-values in parenthesis."

	disp as text "References"
	if "`r(pesaran)'" != ""  disp as smcl "  CD: {col 13} Pesaran ({help xtcd2##Pesaran2015:2015}, {help xtcd2##Pesaran2021:2021})"
	if "`r(cdw)'" != "" disp as smcl "  CDw: {col 13} Juodis, Reese ({help xtcd2##JR2021:2021})"
	if "`r(pea)'" != "" disp as smcl "  CDw+: {col 13} CDw with power enhancement from Fan et al. ({help xtcd2##Fan2015:2015})"
	if "`r(cdstar)'" != "" disp as smcl "  CD*: {col 13} Pesaran, Xie ({help xtcd2##PesaranXie2021:2021}) with `r(npca)' PC(s)"
	if `r(noadjust)' == 1 disp as smcl "Cross-sectional units not demeaned for calculation of cross-correlations."
end

/// program to calculate CD tests
capture mata mata drop xtcd2_CD()
mata:
	function xtcd2_CD (		string scalar data_names,
							real scalar N,
							real scalar T,
							real scalar balanced,
							real scalar stand,
							real scalar numpca,
							string scalar methods,
							real scalar defac,
							real scalar reps,
							string scalar CD_return,
							string scalar CDp_return,
							real scalar returnrho,
							real matrix rho,
							real matrix loop )
	{

		data = st_data(.,data_names)
		K = cols(data)
		if (returnrho == 1) {
			rho = asarray_create()
		}

		/// which methods
		names = J(0,1,"")

		methods = tokens(methods)
		if (sum(methods:=="CD")==1) {
			cdpes = 1
			names = names \ "CD"
		}
		if (sum(methods:=="CDw")==1) {
			cdw = 1
			names = names \ "CDw"
		}
		if (sum(methods:=="CDwplus")==1)  {
			pea = 1
			names = names \ "CDw+"
		}
		if (sum(methods:=="CDstar")==1) {
			cdstar = 1
			names = names \ "CD*"
		}

		CD = J(K,1,.)
		CD_star = J(K,1,.)
		CD_w = J(K,reps,.)
		CD_pea = J(K,reps,.) 
		rng = rseed()

		/// inital CD test
		for (i=1;i<=K;i++) {			
			/// reset seed
			rseed(rng)
			data_start = colshape(data[.,i],T)'
			for (r=1;r<=reps;r++) {
				
				/// standard CD stat
				if (r == 1) {
					rho_i = xtcd2_rho(data_start,N,T,stand,balanced,loop)
					CD[i,1] = sqrt(2/(N*(N-1))) * sum(rho_i)					
				}

				if (cdw==1 | pea == 1) {

					data_cdw = data_start :* (2*runiformint(1,N,0,1):-1)
					rho_cdw = xtcd2_rho(data_cdw,N,T,stand,balanced,loop)
					CD_w[i,r] = sqrt(2/(N*(N-1))) * sum(rho_cdw)
				}

				if ((cdstar==1 & r==1) | defac == 1) {
					xtcd2_cdstar(data_start,N,T,numpca,loop,defac,stand,i,r,rho_i=rho_i,CD=CD,CD_w=CD_w,CD_star=CD_star)
				}				
				
				if (pea == 1) {
					rabs = abs(rho_i)
					/// use correction Pesaran, Xie (2021)
					crit =  2*sqrt(log(N)/T) 
					crit2 = (rabs:>crit)					
					CD_pea[i,r] = CD_w[i,r] :+ sum(rabs:*(crit2)) 
				}
				rho_i = rho_i / sqrt(2*T)
				
				if (returnrho == 1 & r==1) {
					asarray(rho,tokens(data_names)[i],rho_i)
				}
			}

		}

		CDo = J(K,0,.)
		if (cdpes == 1) CDo = CDo,CD
		if (cdw == 1) CDo = CDo, rowsum(CD_w) / sqrt(reps)
		if (pea == 1) CDo = CDo, rowsum(CD_pea) / sqrt(reps)
		if (cdstar == 1) CDo = CDo, CD_star[.,1]
		
		CDp = 2*(1:-normal(abs(CDo)))
		
		st_matrix(CD_return,CDo)
		st_matrix(CDp_return,CDp)
		
		cols = J(rows(names),1,""),(names)
		rows = J(K,1,""),tokens(data_names)'

		st_matrixrowstripe(CD_return,rows)
		st_matrixrowstripe(CDp_return,rows)

		st_matrixcolstripe(CD_return,cols)
		st_matrixcolstripe(CDp_return,cols)

	}
end

capture mata mata drop xtcd2_cdstar()
mata:
	function xtcd2_cdstar( ///
				real matrix data_start,real scalar N, real scalar T,real scalar numpca,real scalar loop, real scalar defac, real scalar stand,real scalar i, real scalar r, ///
				real matrix rho_i, real matrix CD, real matrix CD_w, real matrix CD_star)
		{
			meanM = mean(data_start)
			sqrtM = sqrt(diagonal(quadvariance(data_start)))'
			
			if (hasmissing(data_start)) {
				meanM = quadcolsum(data_start):/quadcolsum(data_start!=.)
				sqrtM = colsum((data_start :- meanM):^2):/quadcolsum(data_start!=.)
			}			
			
			data_ii = (data_start:-meanM):/sqrtM

			if (hasmissing(data_ii)==1) data_ii = xtdcce2_EM(data_ii,N,T,numpca,"CD*")
			data_i2 = data_ii * data_ii'			
			eigensystem(data_i2,evec=.,eval=.)
			
			f = evec[.,numpca..1]

			fx = J(T,1,1),f
			fx = Re(fx)

			beta = qrinv(quadcross(fx,fx))*quadcross(fx,data_ii)
			res = data_ii :- fx*beta

			rho_defac = xtcd2_rho(res,N,T,stand,balanced,loop)
			CD_defac = sqrt(2/(N*(N-1))) * sum(rho_defac)

			/// update CD if defactored residuals used; might need to remove later?
			if (defac == 1) {
				if (r == 1) {							
					CD[i,1] = sqrt(2/(N*(N-1))) * sum(rho_defac)
				}
				if (cdw==1 | pea == 1) {
					data_cdw = res :* (2*runiformint(1,N,0,1):-1)
					rho_cdw = xtcd2_rho(data_cdw,N,T,stand,balanced,loop)
					CD_w[i,r] = sqrt(2/(N*(N-1))) * sum(rho_cdw)
				}

				/// update rho
				rho_i = rho_defac
			}

			betai = beta[2..rows(beta),.]

			betaij = betai*betai':/N

			betasum = sqrt(diagonal(betaij))

			/// what does this step do???
			gamma = qrinv(diag(betasum))*betai

			sgm = sqrt(mean(res:^2))
			eps = res :/ sgm
			phi = mean((gamma:/sgm)')'
			ai = (1:-(gamma:*sgm)'*phi)*1/sqrt(N)
			ewt = sum(ai:^2)

			ai2 = ai
			CD_star[i,1] = (CD_defac+sqrt(T/2)*(1:-ewt))/ewt
		}
end



capture mata mata drop xtcd2_rho()
mata:
	function xtcd2_rho (real matrix r, real scalar N, real scalar T, real scalar stand,balanced,real scalar loop)	
	{
		if ((balanced == 1 & hasmissing(meanM) == 0 & loop == 0) | (nonmissing(r)==0)) {
			"balanced"
			RHO = quadcorrelation(r)*sqrt(T)
			RHO = sublowertriangle(RHO)'
			_diag(RHO, 0)
		}
		else {
			"unbalanced - or no demeaning"
			if (stand == 0) {
				meanM = quadcolsum(r) :/quadcolsum(r:!=.)
			}
			else {
				meanM = J(1,N,0) 
			}
			"mean"
			meanM
			RHO = quadcorrelation(r :- meanM)*sqrt(T)
			RHO = sublowertriangle(RHO)'
			_diag(RHO, 0)

			if (hasmissing(RHO) | loop == 1) {
				if (hasmissing(RHO) | loop == 1)  {
					maxi = N - 1
					RHO = J(N,N,.)
					r2 = editmissing(r,0)
					"do loop"
					for (i=1; i<=maxi; i++) {
						minj = i + 1
						for (j = minj; j<=N; j++) {
							if (i<j) {
								// Create dummy vector which contains nonmissings
								nonmissing = rownonmissing(r[,i]):*rownonmissing(r[,j])
								T_nonmissing = sum(nonmissing)
								// Clean Data, i.e. correct missing values into zeros
								
								u_i = r2[,i] :- meanM[i]
								u_j = r2[,j] :- meanM[j]
								// from p. 42 of Chudik, Pesaran (2013) - note: sqrt(T) is added here!
								RHO[i,j] =u_i'*u_j /(sqrt(u_i'*u_i)*sqrt(u_j'*u_j))*sqrt(T_nonmissing) 
								
							}
						}
					}
				}
			}
		}
		return(RHO)	
	}
end


cap mata mata drop xtcd2_rho_order()
mata:
	function xtcd2_rho_order( real matrix rho,		
							 string scalar ordervars,
							 string scalar rhoIDs )
	{
		ordervar = st_data(.,ordervars)
		rhoid = st_data(.,rhoIDs)
		if (sum(ordervar:==.) :== rows(ordervar)) {
			_editmissing(ordervar,0)
		}
		
		rhoiuniq = uniqrows(rhoid)
		matched = J(0,2,.)
		i = 1
		while (i<=rows(rhoiuniq)) {
			idi = rhoiuniq[i]
			index = xtdcce_selectindex(rhoid:==idi)[1]

			matched = matched \ (idi , ordervar[index])
			
			i++
		}
				
		orderuniq = uniqrows(ordervar)
		orderN = rows(orderuniq)
		
		i = 1
		neworder = J(0,1,.)
		while (i <= orderN) {
			orderi = orderuniq[i]
			newi = xtdcce_selectindex(orderi:==matched[.,2])	
			if (sum(newi) > 0 ) {
				neworder = neworder \ newi
			}
			i++
		}
		
		rho = xtcd2_rho_sym(rho)	
		
		rhorder = rho[neworder,neworder]
		
		/// remove lower symmetric part
		rhorder= xtcd2_lowersym(rhorder)	
		rhoID = rhoiuniq[neworder,1]
		///_makesymmetric(rhorder)
		rhorder = rhorder,rhoID
		return(rhorder)
		
		}
end

capture mata mata drop xtcd2_rho_sym()
mata:
	function xtcd2_rho_sym(real matrix rhorder)
	{
		N = rows(rhorder)
		i = 1
		while(i<=N) {
			j=i
			while (j<=N) {
				ij = rhorder[i,j]
				ji = rhorder[j,i]
				
				if (ij:==. & ji:!= .) {
					rhorder[i,j] = ji
				}
				else if (ij:!= . & ji:== .) {
					rhorder[j,i] = ij
				}
				j++
			}
			i++
		}
		return(rhorder)
	}
end

capture mata mata drop xtcd2_lowersym()
mata:
	function xtcd2_lowersym(real matrix rhorder)
	{
		N = rows(rhorder)
		i = 1
		while(i<=N) {
			j=1
			while (j<i) {
				rhorder[i,j] = .
				j++
			}
			i++
		}
		return(rhorder)
	}
end
