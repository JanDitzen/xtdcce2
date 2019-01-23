{smcl}
{hline}
{hi:help xtdcce2}{right: v. 134 - 22. January 2019}
{hline}
{title:Title}

{p 4 4}{cmd:xtdcce2} - estimating heterogeneous coefficient models using common correlated effects in a dynamic panel
with a large number of observations over groups and time periods.{p_end}

{title:Syntax}

{p 4 13}{cmd:xtdcce2} {depvar} [{indepvars}] [{varlist}2 = {varlist}_iv] {ifin} {cmd:,}
{cmdab:cr:osssectional}({varlist})
[
{cmdab:p:ooled}({varlist})
{cmd:cr_lags}({it:string})
{cmdab:nocross:sectional}
{cmdab:ivreg2:options}({it:string}) 
{cmd:e_ivreg2}
{cmd:ivslow}
{cmdab:noi:sily}
{cmd:lr}({varlist})
{cmd:lr_options}({it:string}) 
{cmdab:pooledc:onstant}
{cmdab:reportc:onstant}
{cmdab:noconst:ant}
{cmd:trend}
{cmdab:pooledt:rend}
{cmdab:jack:knife}
{cmdab:rec:ursive}
{cmd:nocd}
{cmdab:showi:ndividual}
{cmd:fullsample}
{cmd:fast}
{cmdab:NOOMIT:ted}]{p_end}


{p 4 4} where {varlist}2 are endogenous variables and {varlist}_iv the instruments.{p_end}
{p 4 4}Data has to be {cmd:xtset} before using {cmd:xtdcce2}; see {help tsset}.
{it:varlists} may contain time-series operators, see {help tsvarlist}, or factor variables, see {help fvvarlist}.{break}
{cmd:xtdcce2} requires the {help moremata} package.{p_end}


{title:Contents}

{p 4}{help xtdcce2##description:Description}{p_end}
{p 4}{help xtdcce2##options:Options}{p_end}
{p 4}{help xtdcce2##model:Econometric and Empirical Model}{p_end}
{p 4}{help xtdcce2##saved_vales:Saved Values}{p_end}
{p 4}{help xtdcce2##postestimation: Postestimation commands}{p_end}
{p 4}{help xtdcce2##examples:Examples}{p_end}
{p 4}{help xtdcce2##references:References}{p_end}
{p 4}{help xtdcce2##about:About}{p_end}


{marker description}{title:Description}

{p 4 4}{cmd:xtdcce2} estimates a heterogeneous coefficient model in a large panel with dependence between cross sectional units.
A panel is large if the number of cross-sectional units (or groups) and the number of time periods are going to infinity.{break}
It fits the following estimation methods:{p_end}
{p 8 8}
i) The Mean Group Estimator (MG, Pesaran and Smith 1995).{break}
ii) The Common Correlated Effects Estimator (CCE, Pesaran 2006),{break} 
iii) The Dynamic Common Correlated Effects Estimator (DCCE, Chudik and Pesaran 2015), and{p_end}
{p 4 4}For a dynamic model, several methods to estimate long run effects are possible:{p_end}
{p 8 8} a) The Pooled Mean Group Estimator (PMG, Shin et. al 1999) based on an Error Correction Model, {break}
b) The Cross-Sectional Augmented Distributed Lag (CS-DL, Chudik et. al 2016) estimator which directly estimates the long run coefficients
from a dynamic equation, and {break}
c) The Cross-Sectional ARDL (CS-ARDL, Chudik et. al 2016) estimator using an ARDL model.{p_end}
{p 4 4}For a further discussion see Ditzen (2018b).
Additionally {cmd:xtdcce2} tests for cross sectional dependence (see {help xtcd2}) and supports instrumental variable estimations (see {help ivreg2}).{p_end}


{marker options}{title:Options}

{p 4 8}{cmdab:cr:osssectional}({varlist}) defines the variables which are added as cross sectional averages to the equation. 
Variables in {cmd:crosssectional()} may be included in {cmd:pooled()}, {cmd:exogenous_vars()}, {cmd:endogenous_vars()} and {cmd:lr()}. 
Variables in {cmd: crosssectional()} are partialled out, the coefficients not estimated and reported.{p_end}
{p 8 8}{cmd:crosssectional}(_all) adds all variables as cross sectional averages. 
No cross sectional averages are added if {cmd:crosssectional}(_none) is used, which is equivalent to {cmd:nocrosssectional}.
{cmd:crosssectional}() is a required option but can be substituted by {cmd:nocrosssectional}.{p_end}

{p 4 8}{cmdab:p:ooled}({varlist}) specifies variables which estimated coefficients are constrained to be equal across all cross sectional units.
Variables may occur in {indepvars}. 
Variables in {cmd:exogenous_vars()}, {cmd:endogenous_vars()} and {cmd:lr()} may be pooled as well.{p_end}

{p 4 8 12}{cmd:cr_lags}({it:integers}) sets the number of lags of the cross sectional averages. 
If not defined but {cmd:crosssectional()} contains a varlist, then only contemporaneous cross sectional averages are added but no lags. 
{cmd:cr_lags(0)} is the equivalent.
The number of lags can be different for different variables, where the order is the same as defined in {cmd:cr()}.
For example if {cmd:cr(y x)} and only contemporaneous cross-sectional averages of y but 2 lags of x are added,
then {cmd:cr_lags(0 2)}.{p_end}

{p 4 8 12}{cmdab:nocross:sectional} suppresses adding any cross sectional averages
Results will be equivalent to the Mean Group estimator.{p_end}

{p 4 8 12}{cmdab:pooledc:onstant} restricts the constant term to be the same across all cross sectional units.{p_end}

{p 4 8 12}{cmdab:reportc:onstant} reports the constant term. If not specified the constant is partialled out.{p_end}

{p 4 8 12}{cmdab:noconst:ant} suppresses the constant term.{p_end}

{p 4 8}{cmd:xtdcce2} supports IV regressions using {help ivreg2}. 
The IV specific options are:{break}
	{cmdab:ivreg2:options}({it:string}) passes further options to {cmd:ivreg2}, see {help ivreg2##s_options:ivreg2, options}.{break}
	{cmd:e_ivreg2} posts all available results from {cmd:ivreg2} in {cmd: e()} with prefix {it:ivreg2_}, see {help ivreg2##s_macros: ivreg2, macros}.{break}
	{cmdab:noi:sily} displays output of {cmd:ivreg2}.{break}
	{cmd:ivslow}: For the calculation of standard errors for pooled coefficients an auxiliary regressions is performed.
	In case of an IV regression, xtdcce2 runs a simple IV regression for the auxiliary regressions. 
	this is faster.
	If option is used {cmd:ivslow}, then xtdcce2 calls ivreg2 for the auxiliary regression. 
	This is advisable as soon as ivreg2 specific options are used.{p_end}

{p 4 8}{cmd:xtdcce2} is able to estimate long run coefficients. 
Three models are supported:
The pooled mean group models (Shin et. al 1999), similar to {help xtpmg}, the CS-DL (see {help xtdcce2##csdl: xtdcce2, csdl}) 
and CS-ARDL method (see {help xtdcce2##ardl: xtdcce2, ardl}) as developed in Chudik et. al 2016. 
No options for the CS-DL model are necessary.{p_end}

{p 8 8}{cmd:lr}({varlist}) specifies the variables to be included in the long-run cointegration vector. 
	The first variable(s) is/are the error-correction speed of adjustment term.
	The default is to use the pmg model. 
	In this case each estimated coefficient is divided by the negative of the long-run cointegration vector (the first variable).
	If the option {cmd:ardl} is used, then the long run coefficients are estimated as the sum over the coefficients relating to a variable,
	divided by the sum of the coefficients of the dependent variable.{break}
	{cmd:lr_options}({it:string}), options for the long run coefficients. Options are:{break}{break}{p_end}
	{col 12}{cmd:ardl} estimates the CS-ARDL estimator. For further details see {help xtdcce2##ardl:xtdcce2, ardl}.
	{col 12}{cmd:nodivide} coefficients are not divided by the error correction speed of adjustment vector. Equation (7) is estimated, see {help xtdcce2##pmg:xtdcce2, pmg}.
	{col 12}{cmd:xtpmgnames} coefficient names in {cmd: e(b_p_mg)} (or {cmd: e(b_full)}) and {cmd: e(V_p_mg)} (or {cmd: e(V_full)}) match the name convention from {help xtpmg}.

{p 4 8 12}{cmd:trend} adds a linear unit specific trend. May not be combined with {cmd:pooledtrend}.{p_end}

{p 4 8 12}{cmdab:pooledt:rend} adds a linear common trend. May not be combined with {cmd:trend}.{p_end}

{p 4 8}Two methods for small sample time series bias correction are supported:{break}
	{cmdab:jack:knife} applies the jackknife bias correction method. May not be combined with {cmd:recursive}.{break}
	{cmdab:rec:ursive} applies the recursive mean adjustment method. May not be combined with {cmd:jackknife}.{p_end}

{p 4 8 12}{cmd: nocd} suppresses calculation of CD test. For details about the CD test see {help xtcd2}.{p_end}

{p 4 8 12}{cmdab:showi:ndividual} reports unit individual estimates in output.{p_end}

{p 4 8 12}{cmd:fullsample} uses entire sample available for calculation of cross sectional averages. 
Any observations which are lost due to lags will be included calculating the cross sectional averages (but are not included in the estimation itself).{p_end}

{p 4 8 12}{cmd:fast} omit calculation of unit specific standard errors.{p_end}

{p 4 8 12}{cmdab:noomit:ted} no omitted variable checks.{p_end}

{marker model}{title:Econometric and Empirical Model}

{p 2}{ul: Econometric Model}{p_end}

{p 4}Assume the following dynamic panel data model with heterogeneous coefficients:{p_end}

{col 10} (1) {col 20} y(i,t) = b0(i) + b1(i)*y(i,t-1) + x(i,t)*b2(i) + x(i,t-1)*b3(i) + u(i,t)
{col 20} u(i,t) = g(i)*f(t) + e(i,t)

{p 4 4} where f(t) is an unobserved common factor loading, g(i) a heterogeneous factor loading, x(i,t) is a (1 x K) vector 
and b2(i) and b3(i) the coefficient vectors.
The error e(i,t) is iid and the heterogeneous coefficients b1(i), b2(i) and b3(i) are randomly distributed around a common mean. 
It is assumed that x(i,t) is strictly exogenous.
In the case of a static panel model (b1(i) = 0) Pesaran (2006) shows that mean of the coefficients 0, b2 and b3 
(for example for b2(mg) = 1/N sum(b2(i))) 
can be consistently estimated by adding cross sectional means of the dependent and all independent variables.
The cross sectional means approximate the unobserved factors. 
In a dynamic panel data model (b1(i) <> 0) pT lags of the cross sectional means are added to achieve consistency (Chudik and Pesaran 2015).
The mean group estimates for b1, b2 and b3 are consistently estimated as long as N,T and pT go to infinity. 
This implies that the number of cross sectional units and time periods is assumed to grow with the same rate. 
In an empirical setting this can be interpreted as N/T being constant. 
A dataset with one dimension being large in comparison to the other would lead to inconsistent estimates, even if both dimension are large in numbers. 
For example a financial dataset on stock markets returns on a monthly basis over 30 years (T=360) of 10,000 firms would not be sufficient. 
While individually both dimension can be interpreted as large, they do not grow with the same rate and the ratio would not be constant. 
Therefore an estimator relying on fixed T asymptotics and large N would be appropriate. 
On the other hand a dataset with lets say N = 30 and T = 34 would qualify as appropriate, if N and T grow with the same rate.{p_end}

{p 4 4}The variance of the mean group coefficient b1(mg) is estimated as:{p_end}

{col 10} var(b(mg)) = 1/N sum(i=1,N) (b1(i) - b1(mg))^2

{p 4 4}or if the vector pi(mg) = (b0(mg),b1(mg)) as:{p_end}

{col 10} var(pi(mg)) = 1/N sum(i=1,N) (pi(i) - pi(mg))(p(i)-pi(mg))'

{p 2}{ul: Empirical Model}{p_end}

{p 4 4}The empirical model of equation (1) without the lag of variable x is:{p_end}

{col 10}(2){col 20} y(i,t) = b0(i) + b1(i)*y(i,t-1) + x(i,t)*b2(i) + sum[d(i)*z(i,s)] + e(i,t),

{p 4 4} where z(i,s) is a (1 x K+1) vector including the cross sectional means at time s and the sum is over s=t...t-pT.
{cmd:xtdcce2} supports several different specifications of equation (2).{p_end}

{p 4 4} {cmd:xtdcce2} partials out the cross sectional means internally. 
For consistency of the cross sectional specific estimates, the matrix z = (z(1,1),...,z(N,T)) has to be of full column rank.
This condition is checked for each cross section. 
{cmd:xtdcce2} will return a warning if z is not full column rank. 
It will, however, continue estimating the cross sectional sepecific coefficients and then calculate the mean group estimates.
The mean group estimates will be consistent.
For further reading see, Chudik, Pesaran (2015, Journal of Econometrics), Assumption 6 and page 398.{p_end}

{p 2}{ul: i) Mean Group}{p_end}

{p 4 4} If no cross sectional averages are added (d(i) = 0), then the estimator is the Mean Group Estimator as proposed by Pesaran and Smith (1995).
The estimated equation is:

{col 10}(3){col 20} y(i,t) = b0(i) + b1(i)*y(i,t-1) + x(i,t)*b2(i) + e(i,t).

{p 4 4} Equation (3) can be estimated by using the {cmd:nocross} option of {cmd:xtdcce2}. The model can be either static (b(1) = 0) or dynamic (b(1) <> 0).{p_end}
{p 4}{help xtdcce2##e_mg:See example}{p_end}

{p 2}{ul: ii) Common Correlated Effects}{p_end}

{p 4 4} The model in equation (3) does not account for unobserved common factors between units.
To do so, cross sectional averages are added in the fashion of Pesaran (2006):{p_end} 

{col 10}(4){col 20} y(i,t) = b0(i) + x(i,t)*b2(i) + d(i)*z(i,t) + e(i,t).

{p 4 4}  Equation (4) is the default equation of {cmd:xtdcce2}.
Including the dependent and independent variables in {cmd:crosssectional()} and setting {cmd:cr_lags(0)} leads to the same result.
{cmd:crosssectional()} defines the variables to be included in z(i,t).
Important to notice is, that b1(i) is set to zero. {p_end}
{p 4}{help xtdcce2##example_cce:See example}{p_end}

{p 2}{ul: iii) Dynamic Common Correlated Effects}{p_end}

{p 4 4} If a lag of the dependent variable is added, endogeneity occurs and adding solely contemporaneous cross sectional averages is not sufficient any longer to achieve consistency.
Chudik and Pesaran (2015) show that consistency is gained if pT lags of the cross sectional averages are added:{p_end}

{col 10}(5){col 20} y(i,t) = b0(i) + b1(i)*y(i,t-1) + x(i,t)*b2(i) + sum [d(i)*z(i,s)] + e(i,t).

{p 4 4} where s = t,...,t-pT. Equation (5) is estimated if the option {cmd:cr_lags()} contains a positive number.{p_end}
{p 4}{help xtdcce2##example_dcce:See example}{p_end}

{p 2}{ul: iv) Pooled Estimators}{p_end} 

{p 4 4} Equations (3) - (5) can be constrained that the parameters are the same across units. Hence the equations become:{p_end}

{col 10}(3-p){col 20} y(i,t) = b0 + b1*y(i,t-1) + x(i,t)*b2 + e(i,t),
{col 10}(4-p){col 20} y(i,t) = b0 + x(i,t)*b2 + d(i)*z(i,t) + e(i,t),
{col 10}(5-p){col 20} y(i,t) = b0 + b1*y(i,t-1) + x(i,t)*b2 + sum [d(i)*z(i,s)] + e(i,t).

{p 4 4}Variables with pooled (homogenous) coefficients are specified using the {cmd:pooled({varlist})} option. 
The constant is pooled by using the option {cmd:pooledconstant}. 
In case of a pooled estimation, the standard errors are obtained from a mean group regression. 
This regression is performed in the background. See Pesaran (2006).{p_end}
{p 4}{help xtdcce2##example_pooled:See example}{p_end}

{p 2}{ul: v) Instrumental Variables}{p_end}

{p 4 4}{cmd:xtdcce2} supports estimations of instrumental variables by using the {help ivreg2} package. 
Endogenous variables (to be instrumented) are defined in {varlist}2 and their instruments are defined in {varlist}_iv.{p_end}
{p 4}{help xtdcce2##example_iv:See example}{p_end}

{marker pmg}{p 2}{ul: vi) Error Correction Models (Pooled Mean Group Estimator)}{p_end}

{p 4 4} As an intermediate between the mean group and a pooled estimation, Shin et. al (1999) differentiate between homogenous long run and heterogeneous short run effects.
Therefore the model includes mean group as well as pooled coefficients.
Equation (1) (without the lag of the explanatory variable x and for a better readability without the cross sectional averages) is transformed into an ARDL model:{p_end}

{col 10}(6){col 20}y(i,t) = phi(i)*(y(i,t-1) - w0(i) - x(i,t)*w2(i)) + g1(i)*[y(i,t)-y(i,t-1)] + [x(i,t) - x(i,t-1)] * g2(i) + e(i,t),

{p 4 4}where phi(i) is the cointegration vector, w(i) captures the long run effects and g1(i) and g2(i) the short run effects.
Shin et. al estimate the long run coefficients by ML and the short run coefficients by OLS.
{cmd:xtdcce2} estimates a slightly different version by OLS:{p_end}

{col 10}(7){col 20}y(i,t) = o0(i) + phi(i)*y(i,t-1) + x(i,t)*o2(i) + g1(i)*[y(i,t)-y(i,t-1)] + [x(i,t) - x(i,t-1)] * g2(i) + e(i,t),

{p 4 4}where w2(i) = - o2(i) / phi(i) and w0(i) = - o0(i)/phi(i). 
Equation (7) is estimated by including the levels of y and x as long run variables 
using the {cmd:lr({varlist})} and {cmd:pooled({varlist})} options and 
adding the first differences as independent variables.
{cmd:xtdcce2} estimates equation (7) but automatically calculates estimates for w(i) = (w0(i),...,wk(i)).
The advantage estimating equation (7) by OLS is that it is possible to use IV regressions and add cross sectional averages to account for dependencies between units.
The variance/covariance matrix is calculated using the delta method,
for a further discussion, see Ditzen (2018).{p_end}
{p 4}{help xtdcce2##example_pmg:See example}{p_end}

{marker csdl}{p 2}{ul: vii) Cross-Section Augmented Distributed Lag (CS-DL)}{p_end}

{p 4 4}Chudik et. al (2016) show that the long run effect of variable x on variable y in equation (1) can be directly estimated.
Therefore they fit the following model, based on equation (1):{p_end}

{col 10}(8){col 20}y(i,t) = w0(i) + x(i,t) * w2(i)  + delta(i) * (x(i,t) - x(i,t-1)) + sum [d(i)*z(i,s)] + e(i,t)

{p 4 4}where w2(i) is the long effect and sum [d(i)*z(i,s)] the cross-sectional averages with 
an appropriate number of lags.
To account for the lags of the dependent variable, the corresponding number of first differences are added. 
If the model is an ARDL(1,1), then only the first difference of the explanatory variable is added.
In the case of an ARDL(1,2) model, the first and the second difference are added.
The advantage of the CS-DL approach is, that no short run coefficients need to be estimated.  {break}
A general ARDL(py,px) model is estimated by:{p_end}

{col 10}(8){col 20}y(i,t) = w0(i) + x(i,t) * w2(i)  + sum(l=1,px) delta(i,l) * (x(i,t-l) - x(i,t-l-1))  + sum [d(i)*z(i,s)] + e(i,t)

{p 4 4}The mean group coefficients are calculated as the unweighted averages of all cross-sectional specific coefficient estimates. 
The variance/covariance matrix is estimated as in the case of a Mean Group Estimation.{p_end}
{p 4}{help xtdcce2##example_csdl:See example}{p_end}

{marker ardl}{p 2}{ul: viii) Cross-Section Augmented ARDL (CS-ARDL)}{p_end}
{p 4 4}As an alternative approach the long run coefficients can be estimated by first estimating the short run coefficients and then the long run coefficients. 
For a general ARDL(py,px) model including cross-sectional averages such as:{p_end}

{col 10}(9){col 20}y(i,t) = b0(i) + sum(l=1,py) b1(i,l) y(i,t-l) + sum(l=0,px) b2(i,l) x(i,t-l) +  sum [d(i)*z(i,s)] + e(i,t),

{p 4 4}the long run coefficients for the independent variables are calculated as:{p_end}

{marker eq_10}{col 10}(10){col 20}w2(i) = sum(l=0,px) b2(i,l) / ( 1 - sum(l=1,py) b1(i,l))

{p 4 4}and for the dependent variable as:{p_end}

{col 10}(11){col 20}w1(i) = 1 - sum(l=1,py) b1(i,l).

{p 4 4}This is the CS-ARDL estimator in Chudik et. al (2016). 
The variables belonging to w(1,i) need to be enclosed in parenthesis, or {help tsvarlist} need to be used.
For example coding {cmd:lr(y x L.x)} is equivalent to {cmd:lr(y (x lx))}, where
lx is a variable containing the first lag of x (lx = L.x).{break}
The disadvantage of this approach is, that py and px need to be known.
The variance/covariance matrix is calculated using the delta method, see Ditzen (2018b).{p_end}
{p 4}{help xtdcce2##example_ardl:See example}{p_end}

{marker saved_vales}{title:Saved Values}

{cmd:xtdcce2} stores the following in {cmd:e()}:

{col 4} Scalars
{col 8}{cmd: e(N)}{col 27} number of observations
{col 8}{cmd: e(N_g)}{col 27} number of groups (cross sectional units)
{col 8}{cmd: e(T)}{col 27} number of time periods
{col 8}{cmd: e(K_mg)}{col 27} number of regressors (excluding variables partialled out)
{col 8}{cmd: e(N_partial)}{col 27} number of partialled out variables
{col 8}{cmd: e(N_omitted)}{col 27} number of omitted variables
{col 8}{cmd: e(N_pooled)}{col 27} number of pooled (homogenous) coefficients
{col 8}{cmd: e(mss)}{col 27} model sum of squares
{col 8}{cmd: e(rss)}{col 27} residual sum of squares
{col 8}{cmd: e(F)}{col 27} F statistic
{col 8}{cmd: e(rmse)}{col 27} root mean squared error
{col 8}{cmd: e(df_m)}{col 27} model degrees of freedom
{col 8}{cmd: e(df_r)}{col 27} residual degree of freedom
{col 8}{cmd: e(r2)}{col 27} R-squared
{col 8}{cmd: e(r2_a)}{col 27} R-squared adjusted
{col 8}{cmd: e(cd)}{col 27} CD test statistic
{col 8}{cmd: e(cdp)}{col 27} p-value of CD test statistic
{col 8}{cmd: e(Tmin)}{col 27} minimum time (only unbalanced panels)
{col 8}{cmd: e(Tbar)}{col 27} average time (only unbalanced panels)
{col 8}{cmd: e(Tmax)}{col 27} maximum time (only unbalanced panels)
{col 8}{cmd: e(cr_lags)}{col 27} number of lags of cross sectional averages

{col 4} Macros
{col 8}{cmd: e(tvar)}{col 27} name of time variable
{col 8}{cmd: e(idvar)}{col 27} name of unit variable
{col 8}{cmd: e(depvar)}{col 27} name of dependent variable
{col 8}{cmd: e(indepvar)}{col 27} name of independent variables
{col 8}{cmd: e(omitted)}{col 27} omitted variables
{col 8}{cmd: e(lr)}{col 27} variables in long run cointegration vector
{col 8}{cmd: e(pooled)}{col 27} pooled (homogenous) coefficients
{col 8}{cmd: e(cmd)}{col 27} command line
{col 8}{cmd: e(cmdline)}{col 27} command line including options
{col 8}{cmd: e(insts)}{col 27} instruments (exogenous) variables (only IV)
{col 8}{cmd: e(istd)}{col 27} instrumented (endogenous) variables (only IV)
{col 8}{cmd: e(version)}{col 27} xtdcce2 version, if {stata xtdcce2, version} used.

{col 4} Matrices
{col 8}{cmd: e(b)}{col 27} coefficient vector 
{col 8}{cmd: e(V)}{col 27} variance-covariance matrix 
{col 8}{cmd: e(bi)}{col 27} coefficient vector of individual and pooled coefficients
{col 8}{cmd: e(Vi)}{col 27} variance-covariance matrix of individual and pooled coefficients

{col 8}Estimated long run coefficients of the ARDL model are marked with the prefix {it:lr_}.

{col 4} Functions
{col 8}{cmd: e(sample)}{col 27} marks estimation sample

{marker postestimation}{title:Postestimation Commands}

{p 4 4}{cmd: predict} and {cmd: estat} can be used after {cmd: xtdcce2}. 

{p 2}{ul: predict}{p_end}
{p 4 4}The syntax for {cmd:predict} is:{p_end}

{p 6 13}{cmd: predict} [type] {newvar} {ifin} [ options ]{p_end}

{col 6}Options {col 25} Description
{hline}
{col 8}{cmd:xb}{col 27} calculate linear prediction
{col 8}{cmd:stdp}{col 27} calculate standard error of the prediction
{col 8}{cmdab:r:esiduals}{col 27} calculate residuals (e(i,t))
{col 8}{cmdab:cfr:esiduals}{col 27} calculate residuals including the common factors (u(i,t))
{col 8}{cmdab:coeff:icients}{col 27} a variable with the estimated cross section specific values for all coefficients is created. The name of the new variable is {newvar}_{varname}.
{col 8}{cmd:se}{col 27} as {cmd: coefficient}, but with standard error instead.
{col 8}{cmd:partial}{col 27} create new variables with the partialled out values.
{col 8}{cmd:replace}{col 27} replace the variable if existing.
{hline}

{p 8 8}{cmd:xtdcce2} is able to calculte both residuals from equation (1). 
{cmd:predict} {newvar} , {cmdab:r:esiduals} calculates e(i,t).
That is, the residuals of the regression with the cross sectional averages partialled out.
{cmd:predict} {newvar} , {cmdab:cfr:esiduals} calculates u(i,t) = g(i)*f(g) + e(i,t). 
That is, the 	by the cross sectional averages. 
Internally, the fitted values are calculated and then subtracted from the dependent variable.
Therefore it is important to note, that if a constant is used, the constant needs to be reported using the {cmd:xtdcce2} option {cmd:reportconstant}.
Otherwise the u(i,t) includes the constant as well (u(i,t) = b0(i) + g(i)*f(g) + e(i,t)).{p_end}


{p 2}{ul: estat}{p_end}
{p 4 4}{cmd: estat} can be used to create a box, bar or range plot. The syntax is:{p_end}

{p 6 13}{cmd: estat} {it:graphtype} [{varlist}] {ifin} [{cmd:, }{cmdab:c:ombine}{cmd:({it:string}) }{cmdab:i:ndividual}{cmd:({it:string})}{cmd: nomg }{cmdab:clearg:raph}]{p_end}

{col 6}graphtype{col 25} Description
{hline}
{col 8}{it:box}{col 27} box plot; see {help graph bar}
{col 8}{it:bar}{col 27} bar plot; see {help graph box}
{col 8}{it:rcap}{col 27} range plot; see {help twoway rcap}
{hline}

{col 6}Options{col 25} Description
{hline}
{col 8}{cmdab:i:ndividual}{cmd:({it:string})}{col 27} passes options for individual graphs (only bar and rcap); see {help twoway_options}
{col 8}{cmdab:c:ombine}{cmd:({it:string})}{col 27} passes options for combined graphs; see {help twoway_options}
{col 8}{cmd:nomg}{col 27} mean group point estimate and confidence interval are not included in bar and range plot graphs
{col 8}{cmdab:clearg:raph}{col 27} clears the option of the graph command and is best used in combination with the {cmd:combine()} and {cmd:individual()} options
{hline}

{p 4} The name of the combined graph is saved in {cmd:r(graph_name)}.{p_end}


{marker examples}{title:Examples}

{p 4 4}An example dataset of the Penn World Tables 8 is available for download {browse "https://www.dropbox.com/s/0087vh8brhid5ws/xtdcce2_sample_dataset.dta?dl=0":here}.
The dataset contains yearly observations from 1960 until 2007 and is already tsset.
To estimate a growth equation the following variables are used:
log_rgdpo (real GDP), log_hc (human capital), log_ck (physical capital) and log_ngd (population growth + break even investments of 5%).{p_end}


{marker e_mg}{p 4}{ul: Mean Group Estimation}{p_end}

{p 4 4}To estimate equation (3), the option {cmd:nocrosssectional} is used.
In order to obtain estimates for the constant, the option {cmd:reportconstant} is enabled. {p_end}

{p 8}{stata xtdcce2 d.log_rgdpo L.log_rgdpo log_hc log_ck log_ngd , nocross reportc}.{p_end}

{p 4 4}Omitting {cmd:reportconstant} leads to the same result, however the constant is partialled out:{p_end}

{p 8}{stata xtdcce2 d.log_rgdpo L.log_rgdpo log_hc log_ck log_ngd , nocross}.{p_end}	


{marker example_cce}{p 4}{ul: Common Correlated Effect}{p_end}

{p 4 4}Common Correlated effects (static) models can be estimated in several ways.
The first possibility is without any cross sectional averages related options:{p_end}

{p 8}{stata xtdcce2 d.log_rgdpo log_hc log_ck log_ngd , cr(_all) reportc}.{p_end}

{p 4 4}Note, that as this is a static model, the lagged dependent variable does not occur and only contemporaneous cross sectional averages are used.
Defining all independent and dependent variables in {cmd:crosssectional({varlist})} leads to the same result:{p_end}

{p 8}{stata xtdcce2 d.log_rgdpo log_hc log_ck log_ngd , reportc cr(d.log_rgdpo log_hc log_ck log_ngd)}.{p_end}

{p 4 4}The default for the number of cross sectional lags is zero, implying only contemporaneous cross sectional averages are used.
Finally the number of lags can be specified as well using the {cmd:cr_lags} option.{p_end}

{p 8}{stata xtdcce2 d.log_rgdpo log_hc log_ck log_ngd , reportc cr(d.log_rgdpo log_hc log_ck log_ngd) cr_lags(0)}.{p_end}

{p 4 4}All three command lines are equivalent and lead to the same estimation results.{p_end}


{marker example_dcce}{p 4}{ul:Dynamic Common Correlated Effect}{p_end}

{p 4 4}The lagged dependent variable is added to the model again.
To estimate the mean group coefficients consistently, the number of lags is set to 3:{p_end}

{p 8}{stata xtdcce2 d.log_rgdpo L.log_rgdpo log_hc log_ck log_ngd , reportc cr(d.log_rgdpo L.log_rgdpo  log_hc log_ck log_ngd) cr_lags(3)}.{p_end}

{p 4}{ul: Using predict}{p_end}

{p 4 4}{cmd:predict, {it:[options]}} can be used to predict the lienar prediction, the residuals, coefficients and the partialled out variables.
To predict the residuals, options {cmd:residuals} is used:{p_end}

{p 8}{stata predict residuas, residuals}{p_end}

{p 4 4}The residuals do not contain the partialled out factors, that is they are e(i,t) in equation (1) and (2). 
To estimate u(i,t), the error term containing the common factors, option {cmd:cfresiduals} is used:{p_end}

{p 8}{stata predict uit, cfresiduals}{p_end}

{p 4 4}In a similar fashion, the linear prediction (option {cmd:xb}, the default) and the standard error of the prediction can be obtained.
The unit specific estimates for each variable and the standard error can be 
obtained using options {cmdab:coeff:icients} and {cmd:se}. 
For example, obtain the coefficients for log_hc from the regression above and calculate the mean, which should be the same as the
mean group estimate:{p_end}

{p 8}{stata predict coeff, coefficients}{p_end}
{p 8}{stata sum coeff_log_hc}.{p_end}

{p 4 4}The partialled out variables can be obtained using{p_end}

{p 8}{stata predict partial, partial}.{p_end}

{p 4 4}Then a regression on the variables would lead to the same results as above.{break}
If the option {cmd:replace} is used, then the {newvar} is replaced if it exists.{p_end}


{marker example_pooled}{p 4}{ul: Pooled Estimations}{p_end}

{p 4 4}All coefficients can be pooled by including them in {cmd:pooled({varlist})}.
The constant is pooled by using the {cmd:pooledconstant} option:{p_end}

{p 8}{stata xtdcce2 d.log_rgdpo L.log_rgdpo log_hc log_ck log_ngd , reportc cr(d.log_rgdpo L.log_rgdpo  log_hc log_ck log_ngd) pooled(L.log_rgdpo  log_hc log_ck log_ngd) cr_lags(3) pooledconstant}.{p_end}


{marker example_iv}{p 4}{ul: Instrumental Variables}{p_end}

{p 4 4}Endogenous variables can be instrumented by using options {cmd:endogenous_vars({varlist})} and {cmd:exogenous_vars({varlist})}.
Internally {help ivreg2} estimates the individual coefficients.
Using the lagged level of physical capital as an instrument for the contemporaneous level, leads to:{p_end}

{p 8}{stata xtdcce2 d.log_rgdpo L.log_rgdpo log_hc log_ck log_ngd  (log_ck = L.log_ck), reportc cr(d.log_rgdpo L.log_rgdpo  log_hc log_ck log_ngd) cr_lags(3) ivreg2options(nocollin noid)}.{p_end}

{p 4 4}Further {cmd:ivreg2} options can be passed through using {cmd:ivreg2options}. Stored values in {cmd:e()} from {cmd:ivreg2options} can be posted using the option {cmd:fulliv}.


{marker example_pmg}{p 4}{ul: Error Correction Models (Pooled Mean Group Estimator)}{p_end}

{p 4 4}Variables of the long run cointegration vector are defined in {cmd:lr({varlist})}, where the first variable is the error correction speed of adjustment term.
To ensure homogeneity of the long run effects, the corresponding variables have to be included in the {cmd:pooled({varlist})} option.
Following the example from Blackburne and Frank (2007) with the {it:jasa2} dataset 
(the dataset is available at {browse www.econ.cam.ac.uk/people-files/emeritus/mhp1/jasa.exe:here} from {browse www.econ.cam.ac.uk/people/emeritus/mhp1/published-articles#1999:Pesaran's webpage}).{p_end}

{p 8}{stata xtdcce2 d.c d.pi d.y if year >= 1962 , lr(L.c pi y) p(L.c pi y) cr(_all) cr_lags(2)}{p_end}

{p 4 4}{cmd:xtdcce2} internally estimates equation (7) and then recalculates the long run coefficients, such that estimation results for equation (8) are obtained.
Equation (7) can be estimated adding {cmd:nodivide} to {cmd:lr_options()}.
A second option is {cmd:xtpmgnames} in order to match the naming convention from {help xtpmg}.{p_end}

{p 8}{stata xtdcce2 d.c d.pi d.y if year >= 1962 , lr(L.c pi y) p(L.c pi y) cr(_all) cr_lags(2) lr_options(nodivide)}{p_end}

{p 8}{stata xtdcce2 d.c d.pi d.y if year >= 1962 , lr(L.c pi y) p(L.c pi y) cr(_all) cr_lags(2) lr_options(xtpmgnames)}{p_end}


{marker example_csdl}{p 4}{ul: Cross-Section Augmented Distributed Lag (CS-DL)}{p_end}

{p 4 4}Chudik et. al (2013) estimate the long run effects of public debt on output growth 
(the data is available {browse www.econ.cam.ac.uk/people-files/faculty/km418/CMPR_Data.zip:here} 
on {browse www.econ.cam.ac.uk/people-files/faculty/km418/research.html:Kamiar Mohaddes' personal webpage}).
In the dataset, the dependent variable is {it:d.y} and the independent variables are the inflation rate (dp) and debt to GDP ratio (d.gd). 
For an ARDL(1,1,1) only the first difference of dp and d.gd are added as further covariates. 
Only a contemporaneous lag of the cross-sectional averages (i.e. cr_lags(0)) of the dependent variable and 
3 lags of the independent variables are added. 
The lag structure is implemented by defining a {it:numlist} rather than a number in {cmd:cr_lags()}. 
For the example here {cmd:cr_lags(0 3 3)} is used, where the first
number refers to the first variable defined in {cmd:cr()}, the second to the second etc.{p_end}

{p 4 4}To replicate the results in Table 18, the following command line is used:{p_end}

{p 8}{stata xtdcce2 d.y dp d.gd d.(dp d.gd), cr(d.y dp d.gd) cr_lags(0 3 3) fullsample}{p_end}

{p 4 4}For an ARDL(1,3,3) model the first and second lag are of the first differences are added by putting {cmd:L(0/2)} in front of the {cmd:d.(dp d.gd)}:{p_end}

{p 8}{stata xtdcce2 d.y dp d.gd L(0/2).d.(dp d.gd), cr(d.y dp d.gd) cr_lags(0 3 3) fullsample}{p_end}

{p 4 4}Note, the {cmd:fullsample} option is used to reproduce the results in Chudik et. al (2013).{p_end} 


{marker example_ardl}{p 4}{ul: Cross-Section Augmented ARDL (CS-ARDL)}{p_end}

{p 4 4}Chudik et. al (2013) estimate besides the CS-DL model a CS-ARDL model. 
To estimate this model all variables are treated as long run coefficients and thus added to {varlist} in {cmd:lr({varlist})}.
{cmd:xtdcce2} first estimates the short run coefficients and the calculates then long run coefficients, 
following {help xtdcce2##eq_10:Equation 10}.
The option {cmd:lr_options(ardl)} is used to invoke the estimation of the long run coefficients.
Variables with the same base (i.e. forming the same long run coefficient) need to be either 
enclosed in parenthesis or {help tsvarlist} operators need to be used.
In Table 17 an ARDL(1,1,1) model is estimated with three lags of the cross-sectional averages:{p_end}

{p 8}{stata xtdcce2 d.y , lr(L.d.y dp L.dp d.gd L.d.gd) lr_options(ardl) cr(d.y dp d.gd) cr_lags(3) fullsample} {p_end}

{p 4 4}{cmd:xtdcce2} calculates the long run effects identifying the variables by their base.
For example it recognizes that {it:dp} and {it:L.dp} relate to the same variable.
If the lag of {it:dp} is called {it:ldp}, then the variables need to be enclosed in parenthesis.{p_end}

{p 4 4}Estimating the same model but as an ARDL(3,3,3) and with enclosed parenthesis reads:{p_end}

{p 8}{stata xtdcce2 d.y , lr((L(1/3).d.y) (L(0/3).dp) (L(0/3).d.gd) ) lr_options(ardl) cr(d.y dp d.gd) cr_lags(3) fullsample}{p_end}

{p 4 4}which is equivalent to coding without parenthesis:{p_end}

{p 8}{stata xtdcce2 d.y , lr(L(1/3).d.y L(0/3).dp L(0/3).d.gd) lr_options(ardl) cr(d.y dp d.gd) cr_lags(3) fullsample}{p_end}


{marker references}{title:References}

{p 4 8}Baum, C. F., M. E. Schaffer, and S. Stillman 2007.
Enhanced routines for instrumental variables/generalized method of moments estimation and testing.
Stata Journal 7(4): 465-506{p_end}

{p 4 8}Chudik, A., K. Mohaddes, M. H. Pesaran, and M. Raissi. 2013.
Debt, Inflation and Growth: Robust Estimation of Long-Run Effects in Dynamic Panel Data Model.{p_end}
  
{p 4 8}Chudik, A., and M. H. Pesaran. 2015.
Common correlated effects estimation of heterogeneous dynamic panel data models with weakly exogenous regressors.
Journal of Econometrics 188(2): 393-420.{p_end}

{p 4 8}Chudik, A., K. Mohaddes, M. H. Pesaran, and M. Raissi. 2016.
Long-Run Effects in Large Heterogeneous Panel Data Models with Cross-Sectionally Correlated Errors
Essays in Honor of Aman Ullah. 85-135.{p_end}

{p 4 8}Ditzen, J. 2018. Estimating Dynamic Common Correlated Effcts in Stata. The Stata Journal, 18:3, 585 - 617.

{p 4 8}Ditzen, J. 2018b. Estimating long run effects in models with cross-sectional dependence using xtdcce2.

{p 4 8}Blackburne, E. F., and M. W. Frank. 2007.
Estimation of nonstationary heterogeneous panels.
Stata Journal 7(2): 197-208.{p_end}

{p 4 8}Eberhardt, M. 2012.
Estimating panel time series models with heterogeneous slopes.
Stata Journal 12(1): 61-71.{p_end}

{p 4 8}Feenstra, R. C., R. Inklaar, and M. Timmer. 2015.
The Next Generation of the Penn World Table. American Economic Review. www.ggdc.net/pwt{p_end}

{p 4 8} Jann, B. 2005. 
moremata: Stata module (Mata) to provide various functions. 
Available from http://ideas.repec.org/c/boc/bocode/s455001.html.

{p 4 8}Pesaran, M. 2006.
Estimation and inference in large heterogeneous panels with a multifactor error structure.
Econometrica 74(4): 967-1012.{p_end}

{p 4 8}Pesaran, M. H., and R. Smith. 1995.
Econometrics Estimating long-run relationships from dynamic heterogeneous panels.
Journal of Econometrics 68: 79-113.{p_end}

{p 4 8}Shin, Y., M. H. Pesaran, and R. P. Smith. 1999.
Pooled Mean Group Estimation of Dynamic Heterogeneous Panels.
Journal of the American Statistical Association 94(446): 621-634.{p_end}


{marker about}{title:Author}

{p 4}Jan Ditzen (Heriot-Watt University){p_end}
{p 4}Email: {browse "mailto:j.ditzen@hw.ac.uk":j.ditzen@hw.ac.uk}{p_end}
{p 4}Web: {browse "www.jan.ditzen.net":www.jan.ditzen.net}{p_end}

{p 4 8}I am grateful to Arnab Bhattacharjee, David M. Drukker, Markus Eberhardt, Tullio Gregori, Erich Gundlach and Mark Schaffer, to the participants of the
2016 Stata Users Group meeting in London and two anonymous referees of The Stata Journal for many valuable comments and suggestions.{p_end}

{p 4}The routine to check for  positive definite or singular matrices was provided by Mark Schaffer, Heriot-Watt University, Edinburgh, UK.{p_end}

{p 4 4}{cmd:xtdcce2} was formally called {cmd:xtdcce}.{p_end}

{p 4 8}Please cite as follows:{break}
Ditzen, J. 2018. xtdcce2: Estimating dynamic common correlated effects in Stata. The Stata Journal. Forthcoming.
{p_end}

{p 4 8}The latest versions can be obtained via {stata "net from https://github.com/JanDitzen/xtdcce2"} 
and beta versions including a full history of 
xtdcce2 from {stata "net from http://www.ditzen.net/Stata/xtdcce2_beta"}.{p_end}

{marker ChangLog}{title:Changelog}
{p 4 8}This version: 1.34 - 22. January 2019{p_end}
{p 4 8}Version 1.33 to Version 1.34{p_end}
{p 8 10} - small bug fixes in code and help file.{p_end}
{p 4 8}Version 1.32 to Version 1.33{p_end}
{p 8 10} - bug in if statements fixed.{p_end}
{p 8 10} - noomitted added, bug in cr(_all) fixed.{p_end}
{p 8 10} - added option "replace" and "cfresiduals" to predict.{p_end}
{p 8 10} - CS-DL and CS-ARDL method added.{p_end}
{p 8 10} - Output as in Stata Journal Version.{p_end}
{p 4 8}Version 1.31 to Version 1.32{p_end}
{p 8 10} - bug number of groups fixed{p_end}
{p 8 10} - predict, residual produced different results within xtdcce2 and after if panel unbalanced or trend used (thanks to Tullio Gregori for the pointer).{p_end}
{p 8 10} - check for rank condition.{p_end}
{p 8 10} - several bugs fixed.{p_end}
{p 4 8}Version 1.2 to Version 1.31{p_end}
{p 8 10} - code for regression in Mata{p_end}
{p 8 10} - corrected standard errors for pooled coefficients, option cluster not necessary any longer. Please rerun estimations if used option pooled(){p_end}
{p 8 10} - Fixed errors in unbalanced panel{p_end}
{p 8 10} - option post_full removed, individual estimates are posted in e(bi) and e(Vi){p_end}
{p 8 10} - added option ivslow.{p_end}
{p 8 10} - legacy control for endogenous_var(), exogenous_var() and residuals().{p_end}

{title:Also see}
{p 4 4}See also: {help xtcd2}, {help ivreg2}, {help xtmg}, {help xtpmg}, {help moremata}{p_end} 
