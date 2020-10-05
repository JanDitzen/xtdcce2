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
					method = "invsym"
					coln = xtdcce_selectindex(colsum(A1:==0):==rows(A1):==0)			
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
	function xtdcce2_mm_which2(source,search,|real scalar exact )
	{		
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
			new_elvec = source:==search[i]	
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
	syntax varlist(ts) , idvar(varlist) tvar(varlist) cr_lags(numlist) touse(varlist) csa(string) [cluster(varlist) numberonly tousets(varlist)]
		tsrevar `varlist'
		
		if "`tousets'" == "" {
			local tousets "`touse'"
		}
		
		local varlist `r(varlist)'
		
		local c_i = 1
		foreach var in `varlist' {
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
			by `tvar' `clusteri' (`idvar'), sort: egen ``ii'' = mean(`var') if `touse'				
			
			*** replace CSA with . if touse == 0 to make sure it is missing
			replace ``ii'' = . if `touse' == 0
			
			local clist `clist' ``ii''
			local c_i = `c_i' + 1
			
		}
				
		if "`cr_lags'" == "" {
			local cr_lags = 0
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
					local cross_structure "`cross_structure' `=word("`varlist'",`i')'(`lagi'; `clusteri')"
				}
				else {
				    local cross_structure "`cross_structure' `lagi'"			
				}
			}
			else {
			    if "`numberonly'" == "" {
					local cross_structure "`cross_structure' `=word("`varlist'",`i')'(`lagi')"	
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
				
	return local varlist "`clistn'"
	return local cross_structure "`cross_structure'"
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
									string scalar wbadj)
	
	{
		real matrix id
		real matrix vars
		real matrix csa
		real matrix residuals
		
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
			
			if (nocsa==0) {
				csai = csa[(index[i,1]..index[i,2]),.]
				tmp_xp = quadcross(csai,varsi)
				tmp_csa = quadcross(csai,csai)
				varsi = (varsi - csai*m_xtdcce_solver(tmp_csa,tmp_xp))	
			}
			residuals[(index[i,1]..index[i,2]),.] = varsi[.,1] - varsi[.,(2..K+1)] * xtdcce2_ebi[i,(1..K)]'
			
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
	function xtdcce2_mata2stata (string scalar varnames, real matrix source, string scalar idtstata, real matrix idtmata, string scalar touse , real scalar single)
	{
		/// check that varnames and source have same number of columns
		if (cols(tokens(varnames))!=cols(source)) {
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
		if (single == 0) {
			while (i<=N) {
				
				idi = Ni[i]
				check = Nim:==idi
				if (sum(check) > 0) {
					idim_pos = xtdcce_selectindex(Nim:==idi)				
					
					indexi = index[i,.]
					indexim = indexm[idim_pos,.]				
					
					/// get t indicator
					ti = idt[|indexi[1,1],2 \ indexi[1,2],2|]
					tim = idtmata[|indexim[1,1],2 \ indexim[1,2],2|]
					t_unionm = xtdcce2_mm_which2f(tim,ti)
					t_unionm = t_unionm[xtdcce_selectindex(t_unionm)]
					t_union = xtdcce2_mm_which2f(ti,tim)
					panelsubview(varsi,vars,i,index)
					varsim = panelsubmatrix(source,j,indexm)
					varsi[t_union,.] = varsim[t_unionm,.]
					
					j++
				}
				i++
			}
		}
		else {
			while (i <= N) {
				idi = Ni[i]
				check = Nim:==idi
				if (sum(check) > 0) {
					idim_pos = xtdcce_selectindex(Nim:==idi)				
					
					indexi = index[i,.]
					indexim = indexm[idim_pos,.]	
					
				}
				
			}			
			
		}
	}
end
