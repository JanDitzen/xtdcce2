/*
xtdcce2 auxiliary programs

1. cholqrsolve (mata)
2. cholqrinv (mata)
3. m_xtdcce_inverter (mata)
4. m_xtdcce_solver (mata)
5. xtdcce_PointByPoint (mata)
6.1 xtdcce_m_partialout (mata)
6.2 xtdcce_m_partialout2 (mata)
7. xtdcce_selectindex (mata)
8. xtdcce2_mm_which2 (mata)
9. xtdcce2_m_PutCoeff (mata)
10. xtdcce2_csa 
11. xtdcce2_errorcalc
12. xtdcce2_wbsadj
13. issorted
14. xtdcce2_mata2stata
*/


// Mata utility for sequential use of solvers
// Default is cholesky;
// if that fails, use QR;
// if overridden, use QR.
// By Mark Schaffer 2015
capture mata mata drop cholqrsolve()
mata:
	function cholqrsolve (  numeric matrix A,
							numeric matrix B,
						  | real scalar useqr)
	{
			
			if (args()==2) useqr = 0
			
			real matrix C

			if (!useqr) {
					C = cholsolve(A, B)
					if (C[1,1]==.) {
							C = qrsolve(A, B)
					}
			}
			else {
					C = qrsolve(A, B)
			}
			return(C)

	};
end


capture mata mata drop cholqrinv()
mata:
	function cholqrinv (  numeric matrix A,
						  | real scalar useqr)
	{
			if (args()==2) useqr = 0

			real matrix C

			if (!useqr) {
					C = cholinv(A)
					if (C[1,1]==.) {
							C = qrinv(A)
					}
			}
			else {
					C = qrinv(A)
			}
			return(C)

	};
end

///Program for matrix inversion.
///Default is cholesky
///if not full rank use invsym (Stata standard) 
///and obtain columns to use
///options: 
///1. if columns are specified, force use invsym
///2. allow for old method (cholinv, if fails qrinv)
///output
///return: inverse
///indicator for rank (1x2, rank and rows), which method used and variables used

capture mata mata drop m_xtdcce_inverter()
mata:
	function m_xtdcce_inverter(	numeric matrix A,
								| real scalar useold,
								real matrix rank,
								real matrix coln,
								string scalar method)
								
	{
		real matrix C
		
		if (args() == 1) {
			useold = 0
			coln = 0
		}
		if (args() == 2){
			coln = 0
		}
		if (useold == 2) {
			coln = (1..cols(A))
		}
		
		if (useold == 1) {			
			C = cholqrinv(A)
			qrinv(A,rank)
			method = "cholqr"		
		}
		else {
			if (coln[1,1] == 0) {
				/// calculate rank seperate. if A is not full rank, cholinv still produces results
				/// 1..cols(A) makes sure variables from left are not dropped
				C = invsym(A,(1..cols(A)))
				rank = rows(C)-diag0cnt(C)
				
				if (rank < rows(A)) {	
					/// not full rank, use invsym
					method = "invsym"
					coln = xtdcce_selectindex(colsum(C:==0):==rows(C):==0)			
				}
				else {
					/// full rank use cholsolve
					C = cholinv(A)
					method = "chol"
				}				
			}
			else {
				C = invsym(A,coln)
				rank = rows(C)-diag0cnt(C)
				method = "invsym"
			}			
		}
		rank = (rank, rows(C))
		return(C)
	}

end
/// same as inverter, rank is for matrix A (which is inverted) 
capture mata mata drop m_xtdcce_solver()
mata:
	function m_xtdcce_solver(	numeric matrix A,
								numeric matrix B,
								| real scalar useold,
								real matrix rank,
								real matrix coln,
								string scalar method)
								
	{
		real matrix C
		real scalar A1
		
		if (args() == 2) {
			useold = 0
			coln = 0
		}
		if (args() < 5){
			coln = 0
		}		
		
		if (useold == 2) {
			coln = (1..cols(A))
		}
		
		if (useold == 1) {			
			C = cholqrsolve(A,B)
			qrinv(A,rank)
			method = "cholqr"
			rank = (rank, rows(C))
		}
		else {
			if (coln[1,1] == 0) {
				
				/// calculate rank seperate. if A is not full rank, cholsolve still produces results
				/// 1..cols(A) makes sure variables from left are not dropped
				A1 = invsym(A,(1..cols(A)))
				rank = rows(A1)-diag0cnt(A1)
				if (rank < rows(A)) {	
					/// not full rank, solve by hand
					C = A1 * B
					method = "invsym (not full rank)"					
					if (A1[1,1] != 0 & rows(A1)==1 & cols(A1)==1) coln = xtdcce_selectindex(colsum(A1:==0):==rows(A1):==0)	
									
				}
				else {
					/// full rank use cholsolve
					C = cholsolve(A,B)
					method = "chol"
					coln = 0
				}
			}
			else {
				/// coln is defined, use invsym on specified columns
				A1 = invsym(A,coln)
				C = A1 * B
				method = "invsym"
				rank = rows(A1)-diag0cnt(A1)
			}
			rank = (rank, rows(A1))
		}		
		return(C)		
	}

end

***Point by point mata program
capture mata mata drop xtdcce_PointByPoint()
mata:
	function xtdcce_PointByPoint(real matrix nrows,
						   real matrix ncols,
						   real matrix expres,
						   real matrix XChange)
	{	
		p_ncols = ncols
		p_nrows = nrows
		p_expres = expres
		"Point by Point"
		if (cols(p_ncols) == 1 & cols(p_nrows) > 1) p_ncols = J(1,cols(p_nrows),p_ncols)
		if (cols(p_ncols) > 1 & cols(p_nrows) == 1) p_nrows = J(1,cols(p_ncols),p_nrows)
		if (cols(expres) == 1 & rows(expres) == 1& cols(p_nrows) > 1 & cols(p_ncols) > 1) p_expres = J(1,cols(p_nrows),p_expres)
		for (i=1 ;i <=cols(p_ncols) ; i++) {
			XChange[p_nrows[i],p_ncols[i]] = p_expres[i]
		}
		return(XChange)
	}
end

*** Partial Out Program - single cross-section
** quadcross automatically removes missing values and therefore only uses (and updates) entries without missing values
** X1 variable which is partialled out
capture mata mata drop xtdcce_m_partialout()
mata:
	function xtdcce_m_partialout (  string scalar X2_n,
									string scalar X1_n, 
									string scalar touse,
									real scalar useold,
									| real scalar rk)
	{
		"start partial out"
		real matrix X1
		real matrix X2
		
		st_view(X2,.,tokens(X2_n),touse)
		st_view(X1,.,tokens(X1_n),touse)
		X1X1 = quadcross(X1,X1)
		X1X2 = quadcross(X1,X2)
		"x1x1 and x1x2 calculated"
		//Get rank
		X2[.,.] = (X2 - X1*m_xtdcce_solver(X1X1,X1X2,useold,rk))
		"partial out done"
		"rank condition:"
		rk
	}
end

*** Partial Out Program, loops over all cross-sections. faster than a loop in Stata.
** quadcross automatically removes missing values and therefore only uses (and updates) entries without missing values
** X1 variable which is partialled out
** id_n is the id identifier
capture mata mata drop xtdcce_m_partialout2()
mata:
	function xtdcce_m_partialout2 (  string scalar X2_n,
									string scalar X1_n, 
									string scalar touse,
									string scalar id_n,
									real scalar useold,
									| real scalar rk)
	{
		"start partial out"
		real matrix X1
		real matrix X2
		real matrix X1_i
		real matrix X2_i
		real scalar id
		
		st_view(X2,.,tokens(X2_n),touse)
		st_view(X1,.,tokens(X1_n),touse)		
		
		rk = 0
		id = st_data(.,id_n,touse)
		ids = uniqrows(id)
		
		running = 1
		"start loop"
		while (running<=rows(ids)) {
		
			tousei = xtdcce_selectindex(id:==ids[running])
			
			X1_i = X1[tousei,.]
			X2_i = X2[tousei,.]
			
			X1X1 = quadcross(X1_i,X1_i)
			X1X2 = quadcross(X1_i,X2_i)
			
			//Get rank
			X2[tousei,.] = (X2_i - X1_i*m_xtdcce_solver(X1X1,X1X2,useold,rks=.))
			
			if (rks[1] < rks[2]) {
				rk = 1
			}
			running++
		}
	}
end


**Wrapper for selectindex, checks if version is smaller than 13, then runs code, otherwise uses mata function

	*if `c(version)' < 13 {
		capture mata mata drop xtdcce_selectindex()
		mata: 
			function xtdcce_selectindex(a)
			{
				
					row = rows(a)
					col = cols(a)
					if (row==1) {
						output = J(1,0,.)
						j = 1
						while (j<=col) {
							if (a[1,j] != 0) {
								output = (output , j)
							}
							j++
						}		
					}
					if (col==1) {
						output = J(0,1,.)
						j = 1
						while (j<=row) {
							if (a[j,1] != 0) {
								output = (output \ j)
							}
							j++
						}		
					}

				return(output)
			}
		end
	
** xtdcce2_mm_which2 program
capture mata mata drop xtdcce2_mm_which2()
mata:
	function xtdcce2_mm_which2(sourceo,searcho,|real scalar exact )
	{		
		search = searcho
		source = sourceo
		if (eltype(source):=="string") {
			source = strlower(source)
		}		
		if (eltype(search):=="string") {
			search = strlower(search)
		}
		sums = 0
		for (i=1;i<=length(search);i++) {
			sums = sums + sum(source:==search[i])
		}
		if (sums > 0) {
			real matrix output	
			output = J(0,1,.)
			if (args() == 2) {
				for (i=1; i<=length(search); i++) {
					output = (output \ (mm_which(source:==search[i])==J(0,1,.) ?  0 : mm_which(source:==search[i])))
				}
			}
			if (args() == 3) {

				if (eltype(search) == "string") {
					equals = J(0,1,"")
				}
				else {
					equals = J(0,1,.)
				}				
				
				if (exact == 1) {
					for (i=1; i<=length(search); i++) {
						mm_wh = mm_which(source:==search[i])
						if (sum(mm_wh)>0) {
							equals = (equals \ search[i])
							numb = rows(mm_which(equals:==search[i]))
							mm_wh = mm_wh[numb]
						}
						output = (output \ (mm_wh==J(0,1,.) ?  0 : mm_wh))
					}
				}
				if (exact == 0) {					
					for (i=1; i<=length(search); i++) {
						mm_wh = xtdcce2_mm_which2(equals,search[i])
						if (sum(mm_wh)==0) {
							equals = (equals \ search[i])
							output = (output \ (mm_which(source:==search[i])==J(0,1,.) ?  0 : mm_which(source:==search[i])))
						}
					}
					
				}
			}
		}
		else {
			output = 0
		}		
		return(output)		
	}
end



capture mata mata drop xtdcce2_mm_which2f()
mata:
	function xtdcce2_mm_which2f(source,search )
	{		
		real matrix output
		
		search_N = rows(search)
		output = J(search_N,1,0)
		source_N = rows(source)


		i = 1
		
		while (i<=search_N) {
			new_elvec = strlower(source):==strlower(search[i])	
			if (anyof(new_elvec,1)) {
				output[i]= xtdcce_selectindex(new_elvec)
			}
			i++
		}
		return(output)
	}
end


** program copies coefficients from e(b) matrix into variables
capture mata mata drop xtdcce2_m_PutCoeff()
mata:
	function xtdcce2_m_PutCoeff (string scalar VarName, string scalar MatrixName ,  string scalar idname , string scalar touse , string scalar ColNames)
	{
		real scalar ids
		real scalar output
		real scalar id

		id = st_data(.,idname,touse)
		
		ids = uniqrows(id)
		
		CoeffNames = st_matrixcolstripe(MatrixName)[.,2]
		CoeffOriginal = st_matrix(MatrixName)
		///CoeffNames
		output = J(rows(id),cols(tokens(VarName)),.)
		/// bring Coeff Matrix into NxK order
		K = cols(tokens(VarName))
		coeff = J(rows(ids),K,.)	
		c=1
		while (c<=K) {		
			namei=tokens(ColNames)[c]
			namei=namei[1]:+"_":+strofreal(ids)
			index = xtdcce2_mm_which2(CoeffNames,namei)
			
			
			coeff[.,c] = CoeffOriginal[index]'			
			c++
		}
		/// check that id has same number of vals as y
		if ((rows(ids) ==rows(coeff)) & (cols(coeff)==cols(tokens(VarName)))) {
			r = 1
			while (r<=rows(ids)) {
				index = xtdcce_selectindex(id:==ids[r])
				output[index,.] = J(rows(index),1,coeff[r,.]) 
				r++
			}
			
			real scalar X
			st_view(X,.,VarName,touse)
			X[.,.] = output
		}
		else {			
			output = .
		}
	}
end	

** 10. xtdcce2_csa creates cross-sectional averages
** option numberonly gives only lag number in cross_structure
capture program drop xtdcce2_csa
program define xtdcce2_csa, rclass
	syntax varlist(ts) , idvar(varlist) tvar(varlist) cr_lags(numlist) touse(varlist) csa(string) [cluster(varlist) numberonly tousets(varlist) rcceindex rcce(string) hasconstant ///
		trace 	///
		/// rank condition test
		rctest		///
		rctestopt(string) 	///
		]
		

		if "`cr_lags'" == "" {
			local cr_lags = 0
		}
		
		if "`tousets'" == "" {
			local tousets "`touse'"
		}
		tsrevar `varlist'
		local cvarlist `r(varlist)'
		
		local c_i = 1
		foreach var in `cvarlist' {
			if "`cluster'" != "" {
				local clusteri = word("`cluster'",`c_i')
				if "`clusteri'" == "" {
					local clusteri `cluster_def'					
				}
				else {
					local cluster_def `clusteri'					
				}
			}
			local ii `=strtoname("`var'")'
			tempvar `ii'
		
			*by `tvar' `clusteri' `touse' (`idvar'), sort: gen ``ii'' = sum(`var') if `touse'			
			*by `tvar' `clusteri' `touse'  (`idvar'), sort: replace ``ii'' = ``ii''[_N] / _N
			*** keep slow version :/, is using _N, then if statement does not work
			by `tvar' `clusteri' (`idvar'), sort: egen double ``ii'' = mean(`var') if `touse'				
			
			*** replace CSA with . if touse == 0 to make sure it is missing
			replace ``ii'' = . if `touse' == 0
			
			local clist `clist' ``ii''
			local c_i = `c_i' + 1

			/// get list with correct variables
			
		}
		
		/// here rc test
		if "`rctest'" != "" {
			*noi disp "DO RC TEST"
			local 0 , `rctestopt'
			syntax [anything], [er gr REPlications(real 1000) STANDardize(real 5)]
			local criterion `er' `gr' 
			if "`criterion'" != "" & strlower("`criterion'") != "er" & strlower("`criterion'") != "gr" {
				noi disp "Criterion invalid. Set to GR."
				local criterion GR
			}
			else if wordcount("`criterion'") > 1 {
				noi disp "Criterion invalid. Set to GR."
				local criterion GR
			}
			if strlower("`criterion'") == "er" local criterion 1
			else local criterion 2
			
			tempvar W
			gen `W' = rnormal(0,1)
			by `tvar', sort: replace `W' = `W'[1]
			sort `idvar' `tvar'
			tempname rctest_results
			mata xtdcce2_rctest("`cvarlist'","`clist'","`W'","`touse'","`idvar' `tvar'",`criterion',`replications',`standardize',("`trace'"!=""),"`rctest_results'")		
		}
		local i = 1
		local lagidef = 0
		foreach var in `clist' {
			local lagi = word("`cr_lags'",`i')
			if "`lagi'" == "" {
				local lagi = `lagidef'
			}
			else {
				local lagidef = `lagi'					
			}
			sort `idvar' `tvar'
			
			tsrevar L(0/`lagi').`var'		
			local clistfull `clistfull' `r(varlist)'
			
			tempname touse2
			gen `touse2' = 1 if `tousets'
			foreach var in `r(varlist)' {
				replace `var' = `var' * `touse2'							
			}
			drop `touse2'	
			
			if "`cluster'" != "" {
				local clusteri = word("`cluster'",`c_i')
				if "`clusteri'" == "" {
					local clusteri `cluster_def'					
				}
				else {
					local cluster_def `clusteri'					
				}
				
				if "`numberonly'" == "" {
					local cross_structure "`cross_structure' `=word("`cvarlist'",`i')'(`lagi'; `clusteri')"
				}
				else {
				    local cross_structure "`cross_structure' `lagi'"			
				}
			}
			else {
			    if "`numberonly'" == "" {
					local cross_structure "`cross_structure' `=word("`cvarlist'",`i')'(`lagi')"	
				}
				else {
					local cross_structure "`cross_structure' `lagi'"	
				}
			}
			
			local i = `i' + 1
		}



		local i = 1
		foreach var in `clistfull' {
			*rename `var' `csa'_`i'
			gen double `csa'_`i' = `var'
			drop `var'
			local clistn `clistn' `csa'_`i'
			local i = `i' + 1
		}
		
		if "`rcce'`rcceindex'" != "" {
			/// clistfull needs to be Fhat 
			/// add option to select the criterion
			/// add check that xtnumfac is required
		
			tempname numfac 

			if "`rcceindex'" != "" & "`rcce'" == "" {
				local rcce criterion("ER")
			}
			local 0 , `rcce'
			syntax [anything], [Criterion(string) npc(real 0) SCale] 
			
			if "`criterion'"!= "" & `npc' > 0 {
				disp "rcce(criterion()) and rcce(npc()) cannot be combined. npc(`npc') ignored"
				local npc 0
			}
			if "`criterion'" == "" & `npc' == 0 local criterion "ER"
 			if "`criterion'" == "none" local criterion ""
			
			if "`criterion'" != "" {
				local criterion = strlower("`criterion'")
				/*foreach nname in pc1 pc2 pc3 ic1 ic2 ic3 er gr gos ed {
					if "`nname'" == "`criterion'" local crit = `s'
					local s = `s' + 1
				}*/
				if "`criterion'" == "er" local crit = 1
				else local crit = 2 
			}
			else if `npc' > 0 {
				local crit = 0
				local criterion "none"
				if `npc' > wordcount("`clistn'") {				
					local npc = wordcount("`clistn'")
				}
			}
			else {
				local crit = 8
			}
			if "`scale'" != "" local scale "`cvarlist'"
			noi mata xtdcce2_rcce("`clistn'","`scale'","`touse'","`idvar' `tvar'",`crit',`npc',"`csa'","`numfac'")
			drop `clistn'
			unab clistn: `csa'*
			return local NumPc = `numfac' 
			return local Type "`criterion'"
			local cross_structure 0
		}
	if "`rctest'"!= "" {
		return matrix rctest = `rctest_results'	
		return matrix rctest_det = `rctest_results'_det		
	}
	return local varlist "`clistn'"
	return local cross_structure "`cross_structure'"
end

// -------------------------------------------------------------------------------------------------
// regularised CCE program
// -------------------------------------------------------------------------------------------------

capture mata mata drop xtdcce2_rcce()
mata:
	function xtdcce2_rcce(string scalar varnames, string scalar cvarlist, string scalar tousen, string scalar idtn,real scalar criterion,real scalar npc, string scalar csnames, string scalar numnames)
	{
		"start rcce"
		idt = st_data(.,idtn,tousen)
		idx = panelsetup(idt[.,1],1)
		N = panelstats(idx)[1]
		T = panelstats(idx)[3]
		T_min = panelstats(idx)[3]
		T_max = panelstats(idx)[4]
		
		
		ZbarL = st_data(.,varnames,tousen)

		if (cvarlist != "") {
			Z = st_data(.,cvarlist,tousen)
		}
		"data loaded"
		Sigma = J(cols(ZbarL),cols(ZbarL),0)
		T_min,T_max
		if (T_min:==T_max) {
			"panel balanced"
			Zbar = panelsubmatrix(ZbarL,1,idx)
			if (cvarlist != "") {
				"scale"
				for (i=1;i<=N;i++) {
					Zi = panelsubmatrix(Z,i,idx)					
					ZZ = Zi:-Zbar
					Sigma = Sigma :+ quadcross(ZZ,ZZ)
				}
			}
			"Zbar loaded"
		}
		else {
			"panel unbalanced"
			cols(ZbarL)

			Zbar = J(T_max,cols(ZbarL),.)
			for (i=1;i<=N;i++) {

				Ti = panelsubmatrix(idt[.,2],i,idx)	:- min(idt[.,2]) :+1

				Zbari = panelsubmatrix(ZbarL,i,idx)
				Zbari
				ZZbar = Zbar[Ti,.]
				///ZZbar
				if (hasmissing(Zbar)) {
					"add miss"
					tmp3 = selectindex(Zbari:!=.)
					tmp2 = Ti[tmp3]
					
					Zbar[tmp2,.] = Zbari[tmp3,.]
				}				
				
				if (cvarlist!="") {
					Zi = panelsubmatrix(Z,i,idx)					
					ZZ = Zi:-Zbari
					Sigma[Ti,.] = Sigma[Ti,.] :+ quadcross(ZZ,ZZ)
				}
				if (hasmissing(Zbar)==0 & cvarlist == "") i = N+1
			}
		}
		if (cvarlist == "") {
			F = Zbar
		}
		else {
			Sigma = Sigma / (N*T_max)
			Sigma1 = invsym(sqrt(Sigma))			
			F = Zbar*Sigma1'
		}
		"F is"
		F
		///fullsvd(quadcross(F,F)/T_max, uu_k, mus ,junk2)
		///ER = J(1,MaxNumPC,.)   		 
    	///for (mm1=MaxNumPC; mm1>=1; mm1--) {
        ///	ER[mm1]       = mus[mm1]/mus[mm1+1]
        ///}
   		///mockEV = mean(vec(Z):^2)/ln(min(N,T_min));
    	///ER = (mockEV/mus[1], ER)
    	///maxindex(ER,1,NumPC,junk2)    	  
    	"criterion"
    	criterion
    	if (criterion > 0) {
    		allICs0  = numfac_int(F,cols(ZbarL),5)    
    		allICs0		
    		best_numfac0 = bestnum_ic_int(allICs0)
			///best_numfac = (best_numfac0, allICs0[10,1])
			NumPC = best_numfac0[criterion]

    	}   	
    	if (npc > 0) NumPC = npc

    	"NumPC"
    	NumPC
    	
    	/// Get eigenvectors and then real part
    	eigensystem(quadcross(F',F')/T_max,Evec=.,Eval=.)

    	Evec = Re(Evec)
    	    	
		Fr = sqrt(T) * Evec[.,1..NumPC]  

    	/// Return to Stata
		idxv = st_addvar("double",csnames:+ strofreal((1..NumPC)))
		real matrix viewM, Vi
		st_view(viewM,.,idxv,tousen)

		if (T_min:==T_max) {
			for (i=1;i<=N;i++) {
				panelsubview(Vi,viewM,i,idx)	
				Vi[.,.] = Fr
			}
		}
		else {
			for (i=1;i<=N;i++) {
				i
				panelsubview(Vi,viewM,i,idx)
				Ti = panelsubmatrix(idt[.,2],i,idx)	:- min(idt[.,2]) :+1	
				
				viewM[Ti,.] = Fr[Ti,.]				
			}
		}
		
    	st_numscalar(numnames,NumPC)
	}

end


*** From xtnumfac
mata:
/// mean function which allows for missings
function meanmiss(real matrix X) return(quadcolsum(X,0):/quadcolsum(X:!=.))
function mymean(real matrix X) return(mean(X))

function numfac_int(X0n, kmax0, stan)  {
	"start numfac int"
// numfac_int calculates the Bai&Ng (2002) and Ahn&Horenstein (2013) ICs for 
// the number of factors.
// It has two inputs:
//   X0:    A TxN matrix containing the data of interest.
//   kmax0: The maximum number of factors to consider.
// The output is a matrix providing the IC values for factor models with 
// k=1,2,...,kmax0 factors in its rows. The columns correspond to the following
// statistics: 1:PC_p1,...,6:IC_p3, 7:ER, 8:GR, 9: GOS
	T     = rows(X0n)
	N     = cols(X0n)
    minNT = min((N, T))

	X0_s = X0n

	pointer mfunc
	if (hasmissing(X0_s)==0) mfunc = &mymean()
	else mfunc = &meanmiss()

	if (stan == 2 | stan == 3) X0_s = X0_s - J(T,1,1)*(*mfunc)(X0_s)

	if (stan == 4 | stan == 5) X0_s = X0_s - J(T,1,1)*(*mfunc)(X0_s) - (*mfunc)(X0_s')'*J(1,N,1) + J(T,1,1)*(*mfunc)(vec(X0_s))*J(1,N,1)
	
	if (stan == 3 | stan == 5) {
		X_sd = sqrt(((*mfunc)(X0_s:^2) - (*mfunc)(X0_s):^2))'
		X0_s = X0_s :/(J(T,1,1)*X_sd')
	}

    /// add zero mean column to X0 to ensure max number of factors can be used; see Remark 6, Juodis (2022)
    seed = rseed()
    X0 = X0_s,(rowsum(X0_s):* (2*runiformint(rows(X0_s),1,0,1):-1)):/cols(X0_s)
    rseed(seed)
	missind = X0 :== .
	missnum = sum(sum(missind)')
	st_numscalar("e(missnum)", missnum)

	if ( missnum == 0) {
		if (T > N) {
				xx         = cross(X0,X0)
				fullsvd(xx:/(N*T), junk1, mus, junk2) // N x N
			} 
		else {  
				xx         = cross(X0',X0')
				fullsvd(xx:/(N*T), junk1, mus ,junk2) // T x T	 
		}	
	}
	else {
		
		obsind  = J(T,cols(missind),1) - missind
		
	    X0mean  = J(T,1,1) * mean(editmissing(X0,0))
		X0      = editmissing(X0,0) + X0mean:*missind
		
		conv_crit = (X0 - X0mean):^2
		conv_crit = mean(mean(conv_crit)')
		upd       = conv_crit
		
		while (upd > 0.001*conv_crit) {
			X0_old = X0
			if (T > N) {
				xx         = cross(X0,X0)
				fullsvd(xx:/(N*T), vee_k, mus, junk2)
				vee_k = vee_k[.,1..(kmax0+5)]
				uu_k  = X0*vee_k/sqrt(N*T)
			} 
			else {  
				xx         = cross(X0',X0')
				fullsvd(xx:/(N*T), uu_k, mus ,junk2)
				uu_k  = uu_k[.,1..(kmax0+5)]
				vee_k = X0'*uu_k/sqrt(N*T)
			}
			X0  = X0_old:*obsind + (uu_k*vee_k'):*missind:*sqrt(N*T)
			upd = mean(mean(abs(X0-X0_old))')
		}	
	}				

    V_val     = J(1,kmax0+1,.)

    // These are the three penalties (without mm0 or the estimate of sig2)
    penalties = ((N+T)/(N*T)*ln((N*T)/(N+T)) \ (N+T)/(N*T)*ln(minNT) \ ln(minNT)/minNT)
	
    for (mm0=kmax0; mm0>=1; mm0--) {
    	V_val[mm0]    = sum(mus[mm0+1..minNT])
       	
    }
    V_val
	if (kmax0+2<minNT) {
		V_val[kmax0+1] = sum(mus[kmax0+2..minNT])
	}
	else {
		V_val[kmax0+1] = .
	}

    V0               = mean(vec(X0):^2)
   
// Now do Ahn&Horenstein
    ER       = J(1,kmax0,.)
    GR       = J(1,kmax0,.)
    mutildes = (J(1,kmax0,.), mus[kmax0+1]/V_val[kmax0+1])
    for (mm1=kmax0; mm1>=1; mm1--) {
         ER[mm1]       = mus[mm1]/mus[mm1+1]
         mutildes[mm1] = mus[mm1]/V_val[1,mm1]
         GR[mm1]       = ln(1+mutildes[mm1])/ln(1+mutildes[mm1+1])
    }
    mockEV      = V0/ln(minNT);
    ER          = (mockEV/mus[1], ER)
    GR          = (ln(1 + mockEV)/ln(1+ mutildes[1]), GR)

	allICs0 = ER \ GR
	
	return(allICs0)
}
// mata drop bestnum_ic_int()
function bestnum_ic_int(allICs1) 
{
// bestnum_ic_int picks an estimate for the number of factors from a matrix with IC
// values for n increasing number of factors (starting at 0).
// The function has one input:
//   - allICs1: a matrix containing the ICs corresponding to different
//              numbers of factors (in cols) for different ICs (rows).  
//              We assume that the first 6 rows are the Bai&Ng ICs whereas 
//              rows 7 and 8 are those of Ahn and Horenstein;
//				row 9 is the selection criterion used by GOS.
// The function output is a 1x8 vector of estimates for the number of factors.
	best_numfac0 = J(1,2,.)
	tempmin     = .
	///for (jj=1; jj<=2; jj++) {
	///	minindex(allICs1[jj,.], 1, tempmin, junk1)
	///	best_numfac0[jj] = tempmin-1
    ///}	
	for (jj=1; jj<=2; jj++) {
		maxindex(allICs1[jj,.], 1, tempmin, junk1)
		best_numfac0[jj] = tempmin-1
    }
// selection rule for number of factors is: first number for which the difference is smaller than zero. 
// Here we count how many are larger than zero, which is eqaul to k(min|stat<0) because we have possibility of no factors as well.
    ///best_numfac0[9] = sum(allICs1[9,.]:>0)

	return(best_numfac0)
}
end

** 11. xtdcce2_csa creates cross-sectional averages
capture mata mata drop xtdcce2_error_calc()
mata:
	function xtdcce2_error_calc(	string scalar varnames, 	///
									string scalar csanames,			///
									string scalar touse,		///
									string scalar idvar,		///
									string scalar residname,	///
									real matrix xtdcce2_ebi,	///
									string scalar wbadj,		///
									|real scalar xb)
	
	{
		real matrix id
		real matrix vars
		real matrix csa
		real matrix residuals
		
		if (args()<7) xb = 0

		vars = st_data(.,st_tsrevar(tokens(varnames)),touse)

		nocsa = 1
		
		if (csanames[1,1] != "" ) {
			csa = st_data(.,csanames, touse)
			nocsa = 0
		}

		id = st_data(.,idvar,touse)
		st_view(residuals,.,residname,touse)
		index = panelsetup(id,1)
		
		Nuniq = uniqrows(id)
		N_g = rows(Nuniq)

		K = cols(vars)-1
		varj = J(0,K+1,.)
		i=1
		while(i<=N_g) {
			/// partial out
			varsi = vars[(index[i,1]..index[i,2]),.]
			
			if (nocsa==0 ) {
				csai = csa[(index[i,1]..index[i,2]),.]
				tmp_xp = quadcross(csai,varsi)
				tmp_csa = quadcross(csai,csai)
				varsi = (varsi - csai*m_xtdcce_solver(tmp_csa,tmp_xp))	
			}
			
			if (xb == 0) {
				residuals[(index[i,1]..index[i,2]),.] = varsi[.,1] - varsi[.,(2..K+1)] * xtdcce2_ebi[i,(1..K)]'
			}
			else if (xb == 1 ) {
				/// here xb
				residuals[(index[i,1]..index[i,2]),.] = varsi[.,(2..K+1)] * xtdcce2_ebi[i,(1..K)]'
			}

			if (wbadj[1,1] == "wildbootstrap") {
				varsii = varsi[.,(2..K+1)] 
				h = diagonal(varsii* m_xtdcce_inverter(quadcross(varsii,varsii)) * varsii')

				residuals[(index[i,1]..index[i,2]),.] = residuals[(index[i,1]..index[i,2]),.] :/ ( 1 :- h)
			}
			
			i++
		
		}		
	}
end

*** Program to adjust for wildbootstrap
capture mata mata drop xtdcce2_wbsadj()
mata:
	function xtdcce2_wbsadj(	string scalar residnames, 	///
									string scalar Xnames,			///
									string scalar idtvarname,		///
									string scalar touse)
	
	{
		real matrix resid
		st_view(resid,.,residnames,touse)
		
		X = st_data(.,Xnames,touse)
		idt = st_data(.,idtvarname,touse)

		index = panelsetup(idt[.,1],1)
		
		i = 1
		"start loop"
		while (i<=rows(index)) {
			Xi = X[(index[i,1]..index[i,2]),.]
			h = diagonal(Xi * m_xtdcce_inverter(quadcross(Xi,Xi)) * Xi')
			resid[(index[i,1]..index[i,2]),.] = resid[(index[i,1]..index[i,2]),.] :/ (1 :- h)
			
			i++
		}
		"done"
	}
end


*** checks if data is sorted, if not then sorts it
capture program drop issorted

program define issorted
	syntax	varlist 
	
	local sorted : dis "`:sortedby'"
	if "`sorted'" != "`varlist'" {
	    noi disp "sort data"
	    sort `varlist'
	}

end



*** program to copy mata content into variables given id and t variables
capture mata mata drop xtdcce2_mata2stata()
mata:
	function xtdcce2_mata2stata (string scalar varnames, real matrix source, string scalar idtstata, real matrix idtmata, string scalar touse , real scalar type,|real matrix numb)
		/// type: 0 residual, 1 coeff
	{
		/// check that varnames and source have same number of columns
		varnames
		if ((cols(tokens(varnames))!=cols(source)) * (type != 3)) {
			varnames = varnames[1,1]:+(strofreal(1..cols(source)))	
		}
		else {
			varnames = tokens(varnames)
		}

		st_addvar("double", varnames)
		
		real matrix vars
		st_view(vars,.,varnames,touse)
		 
		idt = st_data(.,idtstata,touse)
		 
		index = panelsetup(idt[.,1],1)
		indexm = panelsetup(idtmata[.,1],1)
		
		Ni = uniqrows(idt[.,1])
		N = rows(Ni)
		
		Nim = uniqrows(idtmata[.,1])
		Nm = rows(Nm)
		
		i = 1 
		j = 1
		real matrix varsi
		///real mateix varsim
		///if (type == 0) {
		while (i<=N) {
			
			idi = Ni[i]
			check = Nim:==idi
			if (sum(check) > 0) {
				
				/// ith element from Stata part, find in mata range
				/// find element in mata list
				/// select rows in mata list
				firstindex = selectindex(idtmata[.,1]:==idt[index[i,1],1])[1,1]
				indexmi = selectindex(indexm[.,1]:==firstindex)
				panelsubview(varsi,vars,i,index)
				varsim = panelsubmatrix(source,indexmi,indexm)
				
				if (type==2) {
					varsim = sqrt(diagonal(varsim))'
				}
				if (type==3) {
					///varsim = (varsim)'
					varsim = varsim[numb[1],numb[2]]
					
				}
				/// check dimensions
				if (rows(varsim) ==1) {
					varsim = J(rows(varsi),1,varsim)					
					varsi[.,.] = varsim
				}
				else if (rows(varsim):==rows(varsi)) {
					varsi[.,.] = varsim
				}
				else  {
					
					idim_pos = xtdcce_selectindex(Nim:==idi)			
				
					indexi = index[i,.]
					indexim = indexm[idim_pos,.]				
				
					/// get t indicator
					ti = idt[|indexi[1,1],2 \ indexi[1,2],2|]
					tim = idtmata[|indexim[1,1],2 \ indexim[1,2],2|]
					t_unionm = xtdcce2_mm_which2f(tim,ti)
					t_unionm = t_unionm[xtdcce_selectindex(t_unionm)]
					t_union = xtdcce2_mm_which2f(ti,tim)
					varsi[t_union,.] = varsim[t_unionm,.]
				}
			
			}
			i++
		}
	}

end


/// EM program from xtnumfac
capture mata mata drop xtdcce2_EM()
mata:
	function xtdcce2_EM(real matrix X0,real scalar N,real scalar T, real scalar kmax0,|string scalar msg_text)
	{
		if (args()==5) stata(sprintf(`"noi disp as smcl in gr " Missing values imputed for %s." "',msg_text))

		missind = X0 :== .
		obsind  = J(T,N,1) - missind
		
	    X0mean  = J(T,1,1) * mean(editmissing(X0,0))
		X0      = editmissing(X0,0) + X0mean:*missind
		
		conv_crit = (X0 - X0mean):^2
		conv_crit = mean(mean(conv_crit)')
		upd       = conv_crit
		while (upd > 0.001*conv_crit) {
			X0_old = X0
			if (T > N) {
				xx         = cross(X0,X0)
				fullsvd(xx:/(N*T), vee_k, mus, junk2)
				vee_k = vee_k[.,1..(kmax0+5)]
				uu_k  = X0*vee_k/sqrt(N*T)
			} 
			else {  
				xx         = cross(X0',X0')
				fullsvd(xx:/(N*T), uu_k, mus ,junk2)
				uu_k  = uu_k[.,1..(kmax0+5)]
				vee_k = X0'*uu_k/sqrt(N*T)
			}
			X0  = X0_old:*obsind + (uu_k*vee_k'):*missind:*sqrt(N*T)
			upd = mean(mean(abs(X0-X0_old))')
		}
		return(X0)
	}	
end

cap program drop xtdcce2_absorb_prog
program define xtdcce2_absorb_prog, rclass
	syntax anything(name=absorb) , [ TRACEhdfeopt partialonly] touse(string) vars(string) [donotoverwrite]

	local singeltons = 0
	if "`keepsingeltons'" == "" local singeltons = 1

	local tracehdfe = 0
		if "`tracehdfeopt'" != "" local noii noi

	*** tempnames for mata objects
	tempname xtdcce2_absorb 

	cap which reghdfe
	loc rc = _rc
	cap which ftools
	if _rc | `rc' {                			
		di as err "option {it: absorb()} requires {help:reghdfe} and {help ftools}:"
		di as err "  click {stata ssc install reghdfe} to install from SSC"
		di as err "  click {stata ssc install ftools} to install from SSC"
		exit 199
	}
	cap include "reghdfe.mata", adopath
	cap {
		mata: `xtdcce2_absorb' = FixedEffects()
		mata: `xtdcce2_absorb'.absvars = "`absorb'"
		mata: `xtdcce2_absorb'.tousevar = "`touse'"
		mata: `xtdcce2_absorb'.init()
		mata: `xtdcce2_absorb'.partial_out("`vars' ",0, 0)	
	}
	if _rc {
		di as err "{bf:reghdfe} Mata library not found or error in {cmd:reghdfe}."
		exit 199
	}
	
	if "`noii'" != "" {
		noi disp "Before reghdfe partial out"
		noi sum `vars'
	}	
	
	mata st_local("var_partial",invtokens("abs":+strofreal(1..cols(tokens("`vars'")))))	
	mata st_store(`xtdcce2_absorb'.sample, st_addvar("double",tokens("`var_partial'")),"`touse'", `xtdcce2_absorb'.solution.data)
	if "`noii'" != "" {
		noi disp "After reghdfe partial out"
		noi mata mean(`xtdcce2_absorb'.solution.data)
		noi sum `var_partial'
	}
	if "`partialonly'" != "" {
		error 199
	}
	mata mata drop  `xtdcce2_absorb'
	
	if "`donotoverwrite'" == "" {
		local i = 1
		foreach source in `var_partial' {
			local aim = word("`vars'",`i')
			replace `aim' = `source'
			local i = 1 + `i'
		}
	}

	return local absorb_vars "`var_partial'"
end

/// RC test
mata:
	function xtdcce2_rctest(string scalar yxvarsn, string scalar csan, string scalar Wn, string scalar tousen, string scalar idtn, real scalar criterion , real scalar boot, real scalar stand, real scalar trace, string scalar resultsn )
	{
		"Rank Test Program"
		real matrix results, idt, idx,  YX, Z, Zbar, YXD, W, Zi, YXi,Wi, sigma, tmp, tmp2, tmp3, A1, A2, V,D,B, C, V1, D1, V2, D2, V3, D3, xx1, pval, ix, Ti, YXL
		real scalar N,T, T_min,T_max, i, K, n0, reject, cnt
		/// output
		real scalar m, p, rc
		timer_on(20)
		st_view(idt,.,idtn,tousen)
		st_view(Z,.,csan,tousen)
		st_view(YX,.,yxvarsn,tousen)
		st_view(W,.,Wn,tousen)

		if (cols(W)==1) W = J(1,cols(Z),W)

		idx = panelsetup(idt[.,1],1)
		N = panelstats(idx)[1]
		T = panelstats(idx)[4]
		T_min = panelstats(idx)[3]
		T_max = panelstats(idx)[4]
		K = cols(Z)

		
		timer_on(21)
		if (trace==1) sprintf("Dimensions T = %s, N= %s, K=%s",strofreal(T_max),strofreal(N),strofreal(K))

		sigma = J(T_max*K,T_max*K,0)
		Zbar = J(T_max,K,0)

		if (T_max:==T_min) {
			"balanced"
			Zbar = panelsubmatrix(Z,1,idx)
			
			for (i=1;i<=N;i++) {
				panelsubview(Wi,W,i,idx)
				panelsubview(YXi,YX,i,idx)
				///vec(Wi:*YXi :- Wi :* Zbar)
				tmp = vec(Wi:*YXi :- Wi :* Zbar)'
				
				///quadcross(tmp,tmp)
				sigma = sigma + quadcross(tmp,tmp) 
			}
			sigma= sigma/N
			YXL = YX
			
			YXL = colshape(YXL',T_max)'
		}
		else {			
			YXL = J(T_max,K*N,.)
			cnt = 1
			Zbar = J(T_max,K,0)
			min_val_ti = min(idt[.,2])
			for (i=1;i<=N;i++) {
				i,T_max,T_min,K,cnt,N

				panelsubview(Ti,idt[.,2],i,idx)					
				panelsubview(Zi,Z,i,idx)
				panelsubview(Wi,W,i,idx)
				panelsubview(YXi, YX,i,idx)

				/// ensure Ti always starts with 1
				if (min_val_ti>1) Ti = Ti :- min_val_ti:+1				

				YXL[Ti,cnt..cnt-1+cols(YXi)] = YXi
				
				cnt = cnt + cols(YXi)
				tmp = vec(Wi:*YXi :- Wi :* Zi)'

				Tii = J(K,1,1)#Ti :+ (((1..K)':-1)*T_max)#J(rows(Ti),1,1)
				sigma[Tii,Tii] = sigma[Tii,Tii] :+ quadcross(tmp,tmp) :/ rows(Ti)

				/// buid CSA
				tmp3 = selectindex((rowmissing(Zi):==0))
				
				tmp2 = Ti[tmp3]

				Zbar[tmp2,.] = Zi[tmp3,.]


				///Wi = Wi[Ti,.]
				//YXi = YXi[Ti,.]
				//Zi = Zi[Ti,.]
				///sigma[Ti,Ti] = sigma[Ti,Ti] + (quadcross(Wi,YXi) * quadcross(Wi,Zi)' ) / N


				///if (hasmissing(Zbar)==0) i = N+1
			}
			"done"
		}
		timer_off(21)
		/// Number of common factors
		timer_on(22)
		
		m  = bestnum_ic_int(numfac_int(YXL,7,stand))[criterion]
		
		    
		if (trace==1) sprintf("Estimated number of common factors %f",m)
		timer_off(22)
		timer_on(23)
		/// Estimation of p
		A1 = Zbar*Zbar'
		A2 = quadcross(Zbar,Zbar)
		
		results = J(K,4,.)
		n0 = 0
		reject = 1
		timer_off(23)
		timer_on(24)
		while (n0 < K & reject == 1) {
			xtdcce2_eigenshuffle(&A1,D1=.,V1=.)	
			"D1"
			rows(D1),cols(D1)
			///mean(D1)
			"V1"	
			rows(V1),cols(V1)
			///mean(V1)	
			C = V1[.,n0+1..cols(V1)]
			/// check if Re() is correct		
			results[n0+1,1] = Re(N*sum(D1[n0+1..min((T,K))]))

			timer_on(26)
			/// Bootstrap Distribution
			xtdcce2_eigenshuffle(&A2,D2=.,V2=.)
			D = V2[.,n0+1..cols(V2)]			
			B = (D'#C')*sigma*(D#C)
			timer_on(28)
			xtdcce2_eigenshuffle(&B,D3=.,V3=.)
			timer_off(28)
			timer_on(29)	
			xx1 = colsum((D3[.,1] * J(1,boot,1) :* rchi2(rows(D3),boot,1) ))'
			timer_off(29)
			timer_on(27)
			results[n0+1,2] = mm_quantile(Re(xx1),1,0.95)
			timer_off(27)
			tmp = Re(xx1)\results[n0+1,1]
			
			tmp = tmp,(1::(boot+1))
			
			tmp = sort(tmp,-1)
			
			tmp[.,2]:==(boot+1)
			ix = selectindex(tmp[.,2]:==(boot+1))
			
			pval = 1 - ix/(boot+1)
			results[n0+1,3] = pval
			timer_off(26)
			/// Decision
			if (results[n0+1,1] > results[n0+1,2] ) {
				results[n0+1,4] = 1
				n0 = n0 + 1
			}
			else {
				results[n0+1,4] =0
				reject =0
			}
		}
		timer_off(24)
		timer_on(25)
		
		p = sum(results[.,4])

		st_matrix(resultsn,(1-(p<m),p,m))
		names = J(3,1,""),("RC"\"p"\"m")
		st_matrixcolstripe(resultsn,names)
		
		st_matrix(resultsn+"_det",results)
		rnames = J(K,1,""),strofreal(1::K)
		cnames = J(4,1,""),("stat"\"prct"\"pval"\"stat>prct")
		st_matrixcolstripe(resultsn+"_det",cnames)
		st_matrixrowstripe(resultsn+"_det",rnames)
		timer_off(25)
		timer_off(20)
		 
	}
	void xtdcce2_eigenshuffle(transmorphic mat, real matrix Eval, real matrix Evec) {
			real matrix idx
			if (eltype(mat)== "real") eigensystem(mat,Evec=.,Eval=.)
			else if (eltype(mat)== "pointer") eigensystem( (*mat),Evec=.,Eval=.)
			else  _error(3250,"Matrix has to be real or pointer.")
			
			idx = order(Eval',-1)
			Eval = Eval[idx]'
			Evec = -Evec[idx,.]

	}
end
