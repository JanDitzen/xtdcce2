{smcl}
{hline}
{hi:help xtdcce2}{right: v. 4.5 - 13. November 2023}
{right:SJ18-3: st0536}
{right:SJ21-3: st0536_1}
{hline}
{title:Title}

{p 4 4}{cmd:xtdcce2} - estimating heterogeneous coefficient models using common correlated effects in a dynamic panel
with a large number of observations over groups and time periods.{p_end}

{title:Syntax}

{p 4 13}{cmd:xtdcce2} {depvar} [{indepvars}] [{varlist}2 = {varlist}_iv] {ifin} {cmd:,}
{cmdab:cr:osssectional(}{help varlist}{cmd:cr1 [,options1])}
[
{cmdab:globalcr:osssectional(}{help varlist}{cmd:cr2 [,options1])}
{cmdab:clustercr:osssectional(}{help varlist}{cmd:cr3 [,options1])}
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
{cmd:pooledvce}({it:string}) 
{cmdab:reportc:onstant}
{cmdab:noconst:ant}
{cmd:trend}
{cmdab:pooledt:rend}
{cmdab:jack:knife}
{cmdab:rec:ursive}
{cmdab:mgmis:sing}
{cmdab:expo:nent}
{cmdab:xtcse2:options(string)}
{cmd:nocd}
{cmdab:showi:ndividual}
{cmd:fullsample}
{cmd:fast}
{cmd:fast2}
{cmdab:blockdiag:use}
{cmdab:nodim:check}
{cmd:useqr}
{cmd:useinvsym}
{cmdab:NOOMIT:ted}]{p_end}


{p 4}{ul:Options for cross-sectional averages}{p_end}
{synoptset 20}{...}
{synopt:options1}Description{p_end}
{synoptline}
{synopt:{cmd:cr_lags(}{help numlist})}number of lags of cross-section averages{p_end}
{synopt:{cmdab:cl:ustercr(}{help varlist})}name of cluster variable(s), only for {cmd:clustercrosssectional}{p_end}
{synopt:{cmd:rcce}}use regularized CCE, see Juodis (2022) with the ER criterion from Ahn and Horenstein (2013) to select the number of eigenvectors for static panels, see {help xtdcce2##rcce:details}.{p_end}
{synopt:{cmd:rcce(options)}}as {cmd:rcce} but with further options options, see {help xtdcce2##options:Options}.{p_end}
{synoptline}
{p2colreset}{...}

{p 4 4} where {varlist}2 are endogenous variables and {varlist}_iv the instruments.{p_end}
{p 4 4}Data has to be {cmd:xtset} before using {cmd:xtdcce2}; see {help tsset}.
{it:varlists} may contain time-series operators, see {help tsvarlist}, or factor variables, see {help fvvarlist} 
and note on {help xtdcce2##collinearity:collinearity issues}.{break}
{cmd:xtdcce2} requires the {help moremata} package.{p_end}

{p 4 4}For a speed optimized version see {help xtdcce2fast}.{p_end}

{title:Contents}

{p 4}{help xtdcce2##description:Description}{p_end}
{p 4}{help xtdcce2##options:Options}{p_end}
{p 4}{help xtdcce2##model:Econometric and Empirical Model}{p_end}
{p 8}{help xtdcce2##EconometricModel:Econometric Model}{p_end}
{p 8}{help xtdcce2##EmpiricalModel:Empirical Model}{p_end}
{p 8}{help xtdcce2##R2:Coefficient of Determination (R2)}{p_end}
{p 8}{help xtdcce2##collinearity:Issues with Collinearity}{p_end}
{p 4}{help xtdcce2##SpeedLargePanels:Large panels and speed}{p_end}
{p 4}{help xtdcce2##saved_vales:Saved Values}{p_end}
{p 4}{help xtdcce2##postestimation: Postestimation commands}{p_end}
{p 8}{help xtdcce2##postestpredict: predict}{p_end}
{p 8}{help xtdcce2##postestestat: estat}{p_end}
{p 8}{help xtdcce2##postestboot: bootstrap}{p_end}
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
{p 4 4}For a further discussion see Ditzen (2019).
Additionally {cmd:xtdcce2} tests for cross sectional dependence (see {help xtcd2}) and 
estimates the exponent of the cross sectional dependence alpha (see {help xtcse2}).
{cmd:xtdcce2} also implements the regularized CCE estimator from Juodis (2022) for static panels. 
It also supports instrumental variable estimations (see {help ivreg2}).{p_end}


{marker options}{title:Options}

{p 4 8}{cmdab:cr:osssectional(}{help varlist}{cmd:cr1 [,cr_lags(#)} {cmd:rcce[(}{cmdab:c:riterion(string)} {cmdab:sc:ale}{cmd: npc(real)]])}  defines the variables which are added as cross sectional averages to the equation. 
Variables in {cmd:crosssectional()} may be included in {cmd:pooled()}, {cmd:exogenous_vars()}, {cmd:endogenous_vars()} and {cmd:lr()}. 
Variables in {cmd: crosssectional()} are partialled out, the coefficients not estimated and reported.{p_end}
{p 8 8}{cmd:crosssectional}(_all) adds all variables as cross sectional averages. 
No cross sectional averages are added if {cmd:crosssectional}(_none) is used, which is equivalent to {cmd:nocrosssectional}.
{cmd:crosssectional}() is a required option but can be substituted by {cmd:nocrosssectional}.{break}
If {cmd:cr(..., cr_lags())} is used, then the global option {cmd:cr_lags()} (see below) is ignored.{break}
{cmd:rcce[}{cmdab:c:riterion(string)} {cmdab:sc:ale}{cmd: nps(real)]} invokes regularized cross-section averages from Juodis (2022) for {bf:static panels}. 
The number of common factors in the cross-section averages is estimated and 
then the respective number of eigenvectors from the cross-section averages is used. For more details see {help xtdcce2##rcce:details}.{break}
Detailed Options are:{p_end}
{col 12}{cmd:criterion(er|gr)} specifies criterion to identify number of common factors using the ER or GR criterion from Ahn and Horenstein (2013), see {help xtnumfac}.
{col 12}{cmd:scale} scales cross-section averages, see Juodis (2022).
{col 12}{cmd:npc(real)} specifies number of eigenvectors without estimating it. Cannot be combined with {cmd:criterion}.

{p 4 8}{cmdab:globalcr:osssectional(}{help varlist}{cmd:cr1 [,cr_lags(#)])} define global cross-section averages.
{cmd:global} cross-section averages are cross-section averages based on observations which are excluded using {help if} statements.
If {cmd:cr(..., cr_lags())} is used, then the global option {cmd:cr_lags()} (see below) is ignored.{p_end}

{p 4 8}{cmdab:cluster:osssectional(}{help varlist}{cmd:cr1 [,cr_lags(#)] clustercr(varlist))} are clustered or local cross-section averages. 
That is, the cross-section averages are the same for each realisation of the variables defined in {cmd:clustercr()}.
For example, we have data observations regions of multiple countries, defined by variable {it:country}
Now we want to add cross-section averages for each country. 
We can define those by using the option {cmd:clustercr(varlist , clustercr(country))}.{break}
If {cmd:cr(..., cr_lags())} is used, then the global option {cmd:cr_lags()} (see below) is ignored.{p_end}

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

{p 4 8 12}{cmdab:mgmis:sing} if it is not possible to estimate individual coefficient for a cross-section
because of missing data or perfect collinearity, individual coefficient is excluded for MG estimation.
Coefficient will still be displayed as zero in e(bi). {p_end}

{p 4 8 12}{cmd:pooledvce(}{it:type}{cmd:)} specifies the variance estimator for pooled regression.
The default is the non-parametric variance estimator from Pesaran (2006). 
{it:type} can be {cmd:nw} for Newey West heteroscedasticity autocorrelation robust standard errors (Pesaran 2006) or
{cmd:wpn} for fixed T adjusted standard errors from Westerlund et. al (2019).{p_end}

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
	{col 12}{cmd:nominus} does not subtract -1 from the coefficient of the dependent variable.

{p 4 8 12}{cmd:trend} adds a linear unit specific trend. May not be combined with {cmd:pooledtrend}.{p_end}

{p 4 8 12}{cmdab:pooledt:rend} adds a linear common trend. May not be combined with {cmd:trend}.{p_end}

{p 4 8}Two methods for small sample time series bias correction are supported:{break}
	{cmdab:jack:knife} applies the jackknife bias correction method. May not be combined with {cmd:recursive}.{break}
	{cmdab:rec:ursive} applies the recursive mean adjustment method. May not be combined with {cmd:jackknife}.{p_end}

{p 4 8 12}{cmdab:expo:nent}	uses {help xtcse2} to estimate the exponent of the cross-sectional
dependence of the residuals. A value above 0.5 indicates cross-sectional dependence,
see {help xtcse2}.{p_end}

{p 4 8 12}{cmdab:xtcse2:options} passes options to {help xtcse2}, see {help xtcse2##options:xtcse2, options}.{p_end}

{p 4 8 12}{cmd: nocd} suppresses calculation of CD test. For details about the CD test see {help xtcd2}.{p_end}

{p 4 8 12}{cmdab:showi:ndividual} reports unit individual estimates in output.{p_end}

{p 4 8 12}{cmd:fullsample} uses entire sample available for calculation of cross sectional averages. 
Any observations which are lost due to lags will be included in calculating the 
cross sectional averages (but are not included in the estimation itself).
The option does {cmd:not} remove any {help if} statements.
This means, that if an {cmd:if} removes certain cross-sectional units from
the estimation sample, {cmd:xtdcce2} will not use those (as specified by {cmd:if}),
even if {cmd:fullsample} is used. 
{p_end}

{p 4 8 12}{cmd:fast} omit calculation of unit specific standard errors.{p_end}

{p 4 8 12}{cmd:fast2} use {help xtdcce2fast} instead of {cmd:xtdcce2}.{p_end}

{p 4 8 12}{cmdab:blockdiag:use} uses {help mata blockdiag} 
rather than an alternative algorithm. 
{cmd: mata blockdiag} is slower, but might produce more stable results.{p_end}

{p 4 8 12}{cmdab:nodim:check} Does not check for dimension. 
Before estimating a model, {cmd:xtdcce2} automatically checks if the 
time dimension within each panel is long enough to run a mean group regression.
Panel units with an insufficient number are automatically dropped.
{p_end}

{p 4 8}{cmd:xtdcce2} checks for collinearity in three different ways.
It checks if matrix of the cross-sectional averages is of full rank.
After partialling out the cross-sectional averages, it checks if the entire model
across all cross-sectional units exhibits multicollinearity. 
The final check is on a cross-sectional level. 
The outcome of the checks influence which method is used to invert matrices.
If a check fails {cmd:xtdcce2} posts a warning message.
The default is {help mata cholinv:cholinv} and {help mata invsym:invsym} 
if a matrix is of rank-deficient.
For a further discussion see {help xtdcce2##collinearity: collinearity issues}.{break}
The following options are available to alter the behaviour of {cmd:xtdcce2}
with respect to matrices of not full rank:{p_end}
{col 12}{cmd:useqr} calculates the generalized inverse via QR decomposition. This was the default for rank-deficient matrices for {cmd:xtdcce2} pre version 1.35.
{col 12}{cmd:useinvsym} calculates the generalized invers via {help mata invsym}.
{col 12}{cmdab:noomit:ted} no omitted variable checks on the entire model.


{marker model}{title:Econometric and Empirical Model}

{p 2}{ul: Econometric Model}{p_end}
{marker EconometricModel}
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
{marker EmpiricalModel}
{p 4 4}The empirical model of equation (1) without the lag of variable x is:{p_end}

{col 10}(2){col 20} y(i,t) = b0(i) + b1(i)*y(i,t-1) + x(i,t)*b2(i) + sum[d(i)*z(i,s)] + e(i,t),

{p 4 4} where z(i,s) is a (1 x K+1) vector including the cross sectional means at time s and the sum is over s=t...t-pT.
{cmd:xtdcce2} supports several different specifications of equation (2).{p_end}

{p 4 4} {cmd:xtdcce2} partials out the cross sectional means internally. 
For consistency of the cross sectional specific estimates, the matrix z = (z(1,1),...,z(N,T)) has to be of full column rank.
This condition is checked for each cross section. 
{cmd:xtdcce2} will return a warning if z is not full column rank. 
It will, however, continue estimating the cross sectional specific coefficients and then calculate the mean group estimates,
see {help xtdcce2##collinearity: collinearity issues}.
The mean group estimates will be consistent.
For further reading see, Chudik, Pesaran (2015, Journal of Econometrics), Assumption 6 and page 398.{p_end}

{p 4 4} {cmd:xtdcce2} evaluates the rank condition further. 
The rank condition can fail if the constant is partialled out and one or more variables are binary.
In this case {cmd:xtdcce2} restarts, however forces the constant to be calculated.{p_end}

{p 4 4}The following models can be estimated:{p_end}

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
The constant is pooled by using the option {cmd:pooledconstant}. {p_end}

{p 4 4}In case of a pooled estimation {cmd:xtdcce2} offers the estimation of three different type of standard errors. 
The default is the non-parametric variance estimator from Pesaran (2006), Eq. 67 - 69. 
The non-parametric estimator takes the difference between the mean group and individual coefficients into account.
Therefore a mean group regression is performed in the background.
The first alternative estimator is the Newey West type heteroscedasticity and auto correlation robust standard errors from 
Pesaran (2006), Eq. 51, 52 and 74. 
The length of the window size is defined as round(4*(T/100)^(2/9)) and follows the convention in 
for HAC standard errors (see Bai and Ng, 2004). 
Another alternative is the fixed-T variance estimator from Westerlund et. al (2019), Eq. 10 and 11. 
This estimator heteroscedasticity robust and allows for panels with a fixed time dimension.{p_end}

{p 4}{help xtdcce2##example_pooled:See example}{p_end}

{p 2}{ul: v) Instrumental Variables}{p_end}

{p 4 4}{cmd:xtdcce2} supports estimations of instrumental variables by using the {help ivreg2} package. 
Endogenous variables (to be instrumented) are defined in {varlist}2 and their instruments are defined in {varlist}_iv.{p_end}
{p 4}{help xtdcce2##example_iv:See example}{p_end}

{marker pmg}{p 2}{ul: vi) Cross-Section Augmented Error Correction Models (CS-ECM)}{p_end}

{p 4 4} Two version of the CS-ECM are supported. 
The Pooled Mean Group model, which is an intermediate between the mean group and a pooled estimation, Shin et. al (1999) differentiate between homogenous long run and heterogeneous short run effects.
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

{p 4 4}The restriction of homogenous long run relationships can be relaxed. 
This is then an unrestricted CS-ECM.
The results are equivalent to an CS-ARDL if only one lag is used.{p_end}

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
The variance/covariance matrix is calculated using the delta method, see Ditzen (2019).{p_end}
{p 4}{help xtdcce2##example_ardl:See example}{p_end}

{p 2}{ul: ix) Regularized CCE}{p_end}{marker rcce}
{p 4 4}The CCE approach can involve a large number of cross-section averages which is larger than the number of factors and 
can lead to a non-trivial bias for the pooled and mean group estimator, see Karabiyik et. al. (2017).
Juodis (2022) propose a solution for linear static panels which uses singular value decomposition to remove redundant singular values in the cross-section averages.
The so-called rCCE method involves the following steps:
{p_end}

{p 12 14}1. Calculate cross-sectional averages.{p_end}
{p 12 14}2. Estimate number of common factors using the ER or GR criterion from Ahn and Horenstein (2013).{p_end}
{p 12 14}3. Replace the cross-sectional averages with eigenvectors from the cross-section averages. The eigenvectors are the eigenvectors of the largest eigenvalues and the number is obtained in step 2.{p_end}

{p 4 4}The method requires bootstrapped standard errors, see {help xtdcce2##postestboot:bootstrapping}.{p_end}
{p 4}{help xtdcce2##example_rcce:See example}{p_end}

{p 2}{ul:Coefficient of Determination (R2)}{p_end}
{marker R2}
{p 4 4}{cmd:xtdcce2} calculates up to three different coefficients of determination (R2).
It calculates the standard un-adjusted R2 and the adjusted R2 as common in the literature.
If all coefficients are either pooled or heterogeneous, {cmd:xtdcce2} calculates 
an adjusted R2 following Holly et. al (2010); Eq. 3.14 and 3.15.
The R2 and adjusted R2 are calculated even if the pooled or mean group adjusted 
R2 is calculated. 
However the pooled or mean group adjusted R2 is displayed instead of the adjusted R2
if calculated.{p_end}

{p 4 4}In the case of a pure homogenous model, the adjusted R2 is calculated as:{p_end}

{col 10}R2(CCEP) = 1 - s(p)^2 / s^2

{p 4 4}where {it:s(p)^2} is the error variance estimator from the pooled regressions 
and {it:s^2} the overall error variance estimator. They are defined as{p_end}

{col 10}s(p)^2 = sum(i=1,N) e(i)'e(i) / [N ( T - k - 2) - k],

{col 10}s^2 = 1/(N (T -1)) sum(i=1,N) sum(t=1,T) (y(i,t) - ybar(i) )^2.

{p 4 4}k is the number of regressors, {cmd:e(i)} is a vector of residuals 
and {cmd:ybar(i)} is the cross sectional specific mean of the dependent variable.{p_end}

{p 4 4}For mean group regressions the adjusted R2 is the mean of the cross-sectional 
individual R2 weighted by the overall error variance:{p_end}

{col 10}R2(CCEMG) = 1 - s(mg)^2 / s^2

{col 10}s(mg)^2 = 1/N sum(i=1,N) e(i)'e(i) / [T - 2k - 2].

{p 2}{ul: Collinearity Issues}{marker collinearity}

{p 4 4}(Multi-)Collinearity in a regression models means that two or more 
explanatory variables are linearly dependent. 
The individual effect of a collinear explanatory variable on the dependent variable
cannot be differentiated from the effect of another collinear explanatory variable.
This implies it is impossible to estimate the individual coefficient of the collinear explanatory variables.
If the explanatory variables are stacked into matrix X, one or more variables (columns) in x
are collinear, then X'X is rank deficient. 
Therefore it cannot be inverted and the OLS estimate of beta = inverse(X'X)X'Y does not exist.{p_end}

{p 4 4}In a model in which cross-sectional dependence in which dependence is approximated
by cross-sectional averages, collinearity can easily occur. 
The empirical model ({help xtdcce2##EmpiricalModel:2}) can exhibit collinearity in four ways:{p_end}
{col 10}1. In the cross-sectional averages (z(i,s)) stacked in Z are collinear.
{col 10}2. The cross-sectional averages and the explanatory variables are collinear.
{col 10}3. In the global set of model of explanatory variables (the constant, y(i,t-1), x(i,t), x(i,t-1) stacked in X) are collinear for {cmd:all i}.
{col 10}4. In a cross-sectional unit specific model of explanatory variables (the constant, y(i,t-1), x(i,t), x(i,t-1) stacked in X(i)) are collinear for {cmd:some i}.

{p 4 4}{cmd:xtdcce2} checks all types of collinearity and according to the 
prevalent type decides how to continue and invert (X'X).
It uses as a default {help mata cholinv:cholinv}. 
If a matrix is rank deficient it uses {help mata invsym:invsym}, where variables (columns)
are removed from the right. 
If X = (X1 X2 X3 X4) and X1 and X4 are collinear, then X4 will be removed. 
This is done by {help mf_invsym##remarks2:invsym, specifying the order in which columns are dropped}.{break}
Older versions of {cmd:xtdcce2} used {help mata qrinv:qrinv} for rank deficient matrices. 
However results can be unstable and no order of which columns to be dropped can be specified.
The use of {help mata qrinv:qrinv} for rank deficient matrices can be enforced 
with the option {cmd:useqr}.{p_end}

{p 4 4}{cmd:xtdcce2} takes the following steps if:{p_end}
{p 6}{cmd:1. Z'Z is not of full rank}{p_end}
{p 6 6}Before partialling out {cmd:xtdcce2} checks of Z'Z is of full rank. 
In case Z'Z is rank deficient, then {cmd:xtdcce2} will return a warning. 
Cross-section unit specific estimates are not consistent, however the mean group
estimates are. 
See  Chudik, Pesaran (2015, Journal of Econometrics), Assumption 6 and page 398.{p_end}

{p 6}{cmd:2. The cross-sectional averages and the explanatory variables are collinear.}{p_end}
{p 6 6}In this case regressors from the right are dropped, 
this means the cross-sectional averages are dropped.
This case corresponds to the first because the cross-sectional averages
are regressors for the partialling out.{p_end}

{p 6}{cmd:3. X'X is collinear for all i.}{p_end}
{p 6 6}{cmd:xtdcce2} uses {help _rmcoll} to remove any 
variables which are collinear on the global level. 
A message with the list of omitted variables will be posted.
A local of omitted variables is posted in {cmd:e(omitted_var)} 
and the number in {cmd:e(K_omitted)}.
{p_end}

{p 6}{cmd:4. X(i)'X(i) is collinear for some i.}{p_end}
{p 6 6}{cmd:xtdcce2} automatically drops variables (columns) from the right
for those cross-sectional units with collinear variables (columns).
An error message appears. More details can be obtained using
the option {cmd:estat ebistructure} by showing a matrix with a detailed
break down on a cross-section - variable level.{p_end}

{p 4 4}Results obtained with {cmd:xtdcce2} can differ from those obtained
with {help reg} or {help xtmg}. 
The reasons are that {cmd:xtdcce2}, partialles out the 
cross-sectional averages and enforces the use of doubles, both is 
not done in {cmd:xtmg}. 
In addition it use as a default a different algorithm to invert matrices.{p_end}

{p 4 4}The use of factor variables together with cross-sectional averages 
can lead to collineaity issues. For example in a dataset with the time identifier 
{cmd:year} adding a time dummy using {cmd:i.year} implicitly creates a dummy for
each year. These variables will be collinear with the cross-sectional averages.
{cmd:xtdcce2} tries to control for collinearity but might fail when using factor 
variables. In general care is required when combining factor variables and
cross-sectional averages. {p_end}

{marker SpeedLargePanels}{title:Large Panels and Speed issues}

{p 4 4}The common correlated effects estimators are designed for large panels. 
This means the number of cross-sectional units and the time periods {cmd:xtdcce2} 
is converging to infinity, resulting in a large number of observations.
{cmd:xtdcce2} has some limitations in its functionality with such large panels, 
partly from its design, partly limitations arising from Stata.
While the former slows down the estimation, the latter makes an estimation impossible.
{p_end} 

{p 4 4}Before an estimation {cmd:xtdcce2} performs many checks such as collinearity checks,
panel sizes or missing values. 
Often those checks rely on loops over either cross-sectional units, time periods or involve
inverting matrices. 
With a large number of observations those checks become very time consuming and {cmd:xtdcce2}
becomes slow. 
This especially becomes a problem when running simulations or bootstraps.{p_end}

{p 4 4}Stata has limitations on the size of Stata matrices. 
{cmd:xtdcce2} uses Stata matrices to post estimation results, such as a (N_g x N_g)
matrix of covariances for the cross-sectional unit specific coefficients, or the
(N_g x 1) matrix itself.
If the number of cross-sectional units is very large, {cmd:xtdcce2} cannot post
this matrix and aborts with an error. 
The same problems arise when {cmd:xtdcce2} uses Stata commands such as {help rmcoll}
to check for collinearities.
In addition collinearity checks can be computationally intensive for large panels and slow 
an estimation significantly down.{p_end}

{p 4 4}To circumvent the speed and the size issue, 
{cmd:xtdcce2} contains a program called {help xtdcce2fast}. 
{cmd:xtdcc2fast} can only estimate a mean group model and does no collinearity checks. 
Only the mean group coefficients are saved in {cmd:e()}, individual coefficients 
are saved in a {cmd:mata} matrix.{p_end}

{marker saved_vales}{title:Saved Values}

{cmd:xtdcce2} stores the following in {cmd:e()}:

{col 4} Scalars
{col 8}{cmd: e(N)}{col 27} number of observations
{col 8}{cmd: e(N_g)}{col 27} number of groups (cross sectional units)
{col 8}{cmd: e(T)}{col 27} number of time periods
{col 8}{cmd: e(K_mg)}{col 27} number of regressors (excluding variables partialled out)
{col 8}{cmd: e(N_partial)}{col 27} number of partialled out variables
{col 8}{cmd: e(K_omitted)}{col 27} number of omitted variables (global level)
{col 8}{cmd: e(N_pooled)}{col 27} number of pooled (homogenous) coefficients
{col 8}{cmd: e(mss)}{col 27} model sum of squares
{col 8}{cmd: e(rss)}{col 27} residual sum of squares
{col 8}{cmd: e(F)}{col 27} F statistic
{col 8}{cmd: e(rmse)}{col 27} root mean squared error
{col 8}{cmd: e(df_m)}{col 27} model degrees of freedom
{col 8}{cmd: e(df_r)}{col 27} residual degree of freedom
{col 8}{cmd: e(r2)}{col 27} R-squared
{col 8}{cmd: e(r2_a)}{col 27} R-squared adjusted
{col 8}{cmd: e(r2_pmg)}{col 27} pooled or mean group R-squared adjusted
{col 8}{cmd: e(cd)}{col 27} CD test statistic
{col 8}{cmd: e(cdp)}{col 27} p-value of CD test statistic
{col 8}{cmd: e(Tmin)}{col 27} minimum time (only unbalanced panels)
{col 8}{cmd: e(Tbar)}{col 27} average time (only unbalanced panels)
{col 8}{cmd: e(Tmax)}{col 27} maximum time (only unbalanced panels)

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
{col 8}{cmd: e(omitted_var)}{col 27} variable list with omitted variables (global)
{col 8}{cmd: e(omitted_var_i)}{col 27} matrix of omitted variables on cross-section - variable level
{col 8}{cmd: e(version)}{col 27} xtdcce2 version, if {stata xtdcce2, version} used
{col 8}{cmd: e(cr_lags}{col 27} structure of lags of cross-section averages
{col 8}{cmd: e(csa)}{col 27} cross-section averages
{col 8}{cmd: e(gcr_lags)}{col 27} structure of lags of global cross-section averages
{col 8}{cmd: e(gcsa)}{col 27} global cross-section averages
{col 8}{cmd: e(ccr_lags)}{col 27} structure of lags of clustered/local cross-section averages
{col 8}{cmd: e(ccsa)}{col 27} clustered/local cross-section averages
{col 8}{cmd: e(ccsa_cluster)}{col 27} cluster variable(s) for clustered cross-section averages

{col 4} Matrices
{col 8}{cmd: e(b)}{col 27} coefficient vector 
{col 8}{cmd: e(V)}{col 27} variance-covariance matrix 
{col 8}{cmd: e(bi)}{col 27} coefficient vector of individual and pooled coefficients
{col 8}{cmd: e(Vi)}{col 27} variance-covariance matrix of individual and pooled coefficients
{col 8}{cmd: e(alpha)}{col 27} estimates of the exponent of cross-sectional dependence
{col 8}{cmd: e(alpha)}{col 27} estimates of the standard error exponent of cross-sectional dependence

{col 8}Estimated long run coefficients of the ARDL model are marked with the prefix {it:lr_}.

{col 4} Functions
{col 8}{cmd: e(sample)}{col 27} marks estimation sample

{marker postestimation}{title:Postestimation Commands}

{p 4 4}{cmd: predict} and {cmd: estat} can be used after {cmd: xtdcce2}. 
Bootstrapping standard errors and confidence intervals is possible using {cmd:estat bootstrap}.{p_end}
{marker postestpredict}
{p 2}{ul: predict}{p_end}
{p 4 4}The syntax for {cmd:predict} is:{p_end}

{p 6 13}{cmd: predict} [type] {newvar} {ifin} [ options ]{p_end}

{col 6}Options {col 25} Description
{hline}
{col 8}{cmd:xb}{col 27} calculate linear prediction on partialled out variables
{col 8}{cmd:xb2}{col 27} calculate linear prediction on non partialled out variables
{col 8}{cmd:stdp}{col 27} calculate standard error of the prediction
{col 8}{cmdab:r:esiduals}{col 27} calculate residuals (e(i,t))
{col 8}{cmdab:cfr:esiduals}{col 27} calculate residuals including the common factors (u(i,t))
{col 8}{cmdab:coeff:icients}{col 27} a variable with the estimated cross section specific values for all coefficients is created. The name of the new variable is {newvar}_{varname}.
{col 8}{cmd:se}{col 27} as {cmd: coefficient}, but with standard error instead.
{col 8}{cmd:partial}{col 27} create new variables with the partialled out values.
{col 8}{cmd:replace}{col 27} replace the variable if existing.
{hline}

{p 8 8}Option {cmd:xb2} is equivalent to calculate the coefficients and then multiply
the explanatory variables with it, while {cmd:xb} first partialles out
the cross sectional averages and then multiplies the coefficients.{break}
The following Table summarizes the differences with the command line {cmd:xtdcce2 y x , nocross}:{p_end}

{col 10}{cmd:xb} {col 43}{cmd:xb2}
{col 10}1. {stata predict coeff, coeff} {col 43}1. {stata predict coeff, coeff}		
{col 10}2. {stata predict partial, partial} {col 43}2. {stata gen xb2 = coeff_x * x}
{col 10}3. {stata gen xb = coeff_x * partial_x} {col 43}

{p 8 8}{cmd:xtdcce2} is able to calculate both residuals from equation (1). 
{cmd:predict} {newvar} , {cmdab:r:esiduals} calculates e(i,t).
That is, the residuals of the regression with the cross sectional averages partialled out.
{cmd:predict} {newvar} , {cmdab:cfr:esiduals} calculates u(i,t) = g(i)*f(g) + e(i,t). 
That is, the error including the cross-sectional averages. 
Internally, the fitted values are calculated and then subtracted from the dependent variable.
Therefore it is important to note, that if a constant is used, the constant needs to be reported using the {cmd:xtdcce2} option {cmd:reportconstant}.
Otherwise the u(i,t) includes the constant as well (u(i,t) = b0(i) + g(i)*f(g) + e(i,t)).{p_end}

{p 8 8}The following table summarizes this, where y and x are the original variables,
and ytilde and xtilde are y and x with the common factors partialled out:{p_end}

{col 15} y(i,t){col 27} = x(i,t) {col 41}* beta(i) + gamma(i)*f(t) + e(i,t}
{col 15} ytilde(i,t){col 27} = xtilde(i,t)* beta(i) + gamma(i)*f(t) + e(i,t}

{col 10}Variable {col 35} predict option
{col 11} x(i,t)*beta(i) {col 37}{cmd:xb2}
{col 11} xtilde(i,t)*beta(i) {col 37}{cmd:xb}
{col 11} e(i,t} {col 37}{cmd:residuals}
{col 11} gamma(i)*f(t) + e(i,t) {col 37}{cmd:cfresiduals} 

{p 2}{ul: estat}{p_end}
{marker postestestat}
{p 4 4}{cmd: estat} can be used display the structure or values of individual coefficients and
to create a box, bar or range plot. {p_end}

{p 4 4}To display the individual coefficients ordered by the cross-sectional units:{p_end}
{p 6 13}{cmd: estat} {it:ebi}{p_end}

{p 4 4}{cmd: estat} will save the matrix displayed as 
{it:r(blockmatrix)}. 
{p_end}

{p 4 4}{cmd: estat} {cmdab:ebis:tructure} displays the structure for each cross-sectional unit.
It indicates if a coefficient was estimated, dropped, set to zero and if the coefficient 
is pooled.{p_end}

{p 4 4}The syntax for the box, bar or range plot is:{p_end}

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
{col 8}{cmd:dropzero}{col 27} does not display coefficients with zeros in bar or rcap graphs.
{hline}

{p 4} The name of the combined graph is saved in {cmd:r(graph_name)}.{p_end}

{p 2}{ul: Bootstrapping}{p_end}
{marker postestboot}
{p 4 4}{cmd:xtdcce2} can bootstrap confidence intervals and standard errors. 
It supports two types of bootstraps: the {it:wild} bootstrap and the {it:cross-section} bootstrap.
The syntax is:{p_end}

{p 6 13}{cmd: estat bootstrap , [options]}{p_end}

{col 6}Options{col 25} Description
{hline}
{col 8}{cmd:reps(integer)}{col 27}Number of repetitions. Default 100.
{col 8}{cmd:seed(string)}{col 27}Set seed, see {help seed}.
{col 8}{cmdab:w:ild}{col 27}Use wild bootstrap rather than cross-section bootstrap.
{col 8}{cmdab:cfr:esdiduals}{col 27}Use residuals including common factors for wild bootstrap.
{col 8}{cmdab:p:ercentile}{col 27}Bootstrap confidence intervals.
{col 8}{cmdab:showi:ndividual}{col 27}show unit specific results.
{hline}

{p 4 4}{cmd:estat bootstrap} implements two types of bootstraps, the {it:wild} bootstrap and the {it:cross-section} bootstrap.
The {it:cross-section} bootstrap is the default.{p_end}

{p 4 4}The cross-section bootstrap draws with replacement from the cross-sectional dimension. 
That is it draws randomly cross-sectional units with their entire time series. 
It then estimates the model using {cmd:xtdcce2}. 
The cross-section bootstrap has been proposed in Westerlund et. al. (2019) or Goncalves and Perron (2014).{p_end}

{p 4 4}The wild bootstrap is a slower from of the wild bootstrap implemented in {help boottest} (Roodman et. al. 2019). 
It reweighs the residuals with Rademacher weights from the initial regression, recalculates the dependent variable and then 
runs {cmd:xtdcce2}.{p_end}

{p 4 4}The default is to bootstrap standard errors and then use the bootstrapped standard errors to calculate the confidence intervals.
Option {cmd:percentile} directly bootstraps confidence intervals.{p_end}


{marker examples}{title:Examples}

{p 4 4}An example dataset of the Penn World Tables 8 is available for download {browse "https://github.com/JanDitzen/xtdcce2/raw/master/xtdcce2_sample_dataset.dta":here}.
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

{p 4 4}To check if residuals still exhibit cross-sectional dependence, the estimation
of the exponent of cross-sectional dependence can be estimated as well.{p_end}

{p 8}{stata xtdcce2 d.log_rgdpo log_hc log_ck log_ngd , reportc cr(d.log_rgdpo log_hc log_ck log_ngd) cr_lags(0) exponent}.{p_end}

{p 4 4}Alterntively the exponent can be estimated afterwards using {help xtcse2}.{p_end}

{marker example_dcce}{p 4}{ul:Dynamic Common Correlated Effect}{p_end}

{p 4 4}The lagged dependent variable is added to the model again.
To estimate the mean group coefficients consistently, the number of lags is set to 3:{p_end}

{p 8}{stata xtdcce2 d.log_rgdpo L.log_rgdpo log_hc log_ck log_ngd , reportc cr(d.log_rgdpo L.log_rgdpo  log_hc log_ck log_ngd) cr_lags(3)}.{p_end}

{p 4}{ul: Using predict}{p_end}

{p 4 4}{cmd:predict, {it:[options]}} can be used to predict the linear prediction, the residuals, coefficients and the partialled out variables.
To predict the residuals, options {cmd:residuals} is used:{p_end}

{p 8}{stata predict residuals, residuals}{p_end}

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

{marker example_rcce}{p 4}{ul: Regularized CCE and bootstrapping}{p_end}

{p 4 4}The regularized CCE approach is only possible for static models.
To estimate a static model of growth on human, physical captial and population growth, we can use:{p_end}

{p 8}{stata xtdcce2 log_rgdpo log_hc log_ck log_ngd , cr(log_rgdpo log_hc log_ck log_ngd, rcce)}{p_end}

{p 4 4}{cmd:xtdcce2} selects the first and second eigenvector of the cross-section averages and adds it as a variable.
The selection criterion is the ER criterion from Ahn and Horenstein (2013). 
To use the GR criterion instead, the option {cmd:criterion(gr)} is used:{p_end}

{p 8}{stata xtdcce2 log_rgdpo log_hc log_ck log_ngd , cr(log_rgdpo log_hc log_ck log_ngd, rcce(criterion(gr)))}{p_end}

{p 4 4}Three regularized cross-section averages are added. 
To ensure standard errors are correct, a bootstrap is run with a fixed seed:{p_end}

{p 8}{stata estat bootstrap, seed(123)}{p_end}

{p 4 4}To run a wild bootstrap and bootstrap confidence intervals, the options {cmd:wild} and {cmd:percentile} are added:{p_end}

{p 8}{stata estat bootstrap, seed(123) wild percentile}{p_end}

{p 4 4}Instead of specifing the criteria to estimate the number of eigenvectors of the rcce approach, we can hard set it using the option {cmd:npc()}:{p_end}

{p 8}{stata xtdcce2 log_rgdpo log_hc log_ck log_ngd , cr(log_rgdpo log_hc log_ck log_ngd, rcce(npc(3)))}{p_end}

{marker references}{title:References}

{p 4 8}Ahn, S. C., & Horenstein, A. R. 2013. 
Eigenvalue ratio test for the number of factors. 
Econometrica, 81(3), 1203–1227.{p_end}

{p 4 8}Baum, C. F., M. E. Schaffer, and S. Stillman 2007.
Enhanced routines for instrumental variables/generalized method of moments estimation and testing.
Stata Journal 7(4): 465-506{p_end}

{p 4 8}Blackburne, E. F., and M. W. Frank. 2007.
Estimation of nonstationary heterogeneous panels.
Stata Journal 7(2): 197-208.{p_end}

{p 4 8}Chudik, A., K. Mohaddes, M. H. Pesaran, and M. Raissi. 2013.
Debt, Inflation and Growth: Robust Estimation of Long-Run Effects in Dynamic Panel Data Model.{p_end}
  
{p 4 8}Chudik, A., and M. H. Pesaran. 2015.
Common correlated effects estimation of heterogeneous dynamic panel data models with weakly exogenous regressors.
Journal of Econometrics 188(2): 393-420.{p_end}

{p 4 8}Chudik, A., K. Mohaddes, M. H. Pesaran, and M. Raissi. 2016.
Long-Run Effects in Large Heterogeneous Panel Data Models with Cross-Sectionally Correlated Errors
Essays in Honor of Aman Ullah. 85-135.{p_end}

{p 4 8}Ditzen, J. 2018. 
Estimating Dynamic Common Correlated Effcts in Stata. 
The Stata Journal, 18:3, 585 - 617.{p_end}

{p 4 8}Ditzen, J. 2021. 
Estimating long run effects and the exponent of cross-sectional dependence: an update to xtdcce2. 
The Stata Journal 21:3.{p_end}

{p 4 8}Eberhardt, M. 2012.
Estimating panel time series models with heterogeneous slopes.
Stata Journal 12(1): 61-71.{p_end}

{p 4 8}Holly, S., Pesaran, M. H., Yamagata, T. 2010.
A spatio-temporal model of house prices in the USA. 
Journal of Econometrics 158: 160 - 172.{p_end}

{p 4 8}Feenstra, R. C., R. Inklaar, and M. Timmer. 2015.
The Next Generation of the Penn World Table. American Economic Review. www.ggdc.net/pwt{p_end}

{p 4 8}Goncalves, S., & Perron, B. 2014. 
Bootstrapping factor-augmented regression models. 
Journal of Econometrics, 182(1), 156–173.{p_end}

{p 4 8}Jann, B. 2005. 
moremata: Stata module (Mata) to provide various functions. 
Available from http://ideas.repec.org/c/boc/bocode/s455001.html.{p_end}

{p 4 8}Juodis, A. 2022. 
A regularization approach to common correlated effects estimation.
Journal of Applied Econometrics, 37(4), 788– 810.{p_end}

{p 4 8}Karabıyık, H., Reese, S., & Westerlund, J. 2017. 
On the role of the rank condition in cce estimation of factor-augmented panel regressions.
Journal of Econometrics, 197(1), 60–64.{p_end}

{p 4 8}Pesaran, M. H. 2006.
Estimation and inference in large heterogeneous panels with a multifactor error structure.
Econometrica 74(4): 967-1012.{p_end}

{p 4 8}Pesaran, M. H., and R. Smith. 1995.
Econometrics Estimating long-run relationships from dynamic heterogeneous panels.
Journal of Econometrics 68: 79-113.{p_end}

{p 4 8}Roodman, D., Nielsen, M. Ø., MacKinnon, J. G., & Webb, M. D. 2019. 
Fast and wild: Bootstrap inference in Stata using boottest. 
The Stata Journal, 19(1), 4–60.{p_end}

{p 4 8}Shin, Y., M. H. Pesaran, and R. P. Smith. 1999.
Pooled Mean Group Estimation of Dynamic Heterogeneous Panels.
Journal of the American Statistical Association 94(446): 621-634.{p_end}

{p 4 8}Westerlund, J., Perova, Y., Norkute, M. 2019. CCE in fixed-T panels. 
Journal of Applied Econometrics: 1-6.{p_end}


{marker about}{title:Author}

{p 4}Jan Ditzen (Free University of Bozen-Bolzano){p_end}
{p 4}Email: {browse "mailto:jan.ditzen@unibz.it":jan.ditzen@unibz.it}{p_end}
{p 4}Web: {browse "www.jan.ditzen.net":www.jan.ditzen.net}{p_end}

{p 4 8}I am grateful to Arnab Bhattacharjee, David M. Drukker, Markus Eberhardt, Tullio Gregori,
Sebastian Kripfganz,
Erich Gundlach, 
Achim Ahrens,
Kyle McNabb,
Sean Holly and Mark Schaffer, to the participants of the
2016 Stata Users Group meeting in London, 2018 Stata User Group meeting in Zuerich,
 and two anonymous referees of The Stata Journal for many valuable comments and suggestions.
 All remaining errors are my own.{p_end}

{p 4}The routine to check for positive definite or singular matrices was provided by Mark Schaffer, Heriot-Watt University, Edinburgh, UK.{p_end}

{p 4 4}{cmd:xtdcce2} was formally called {cmd:xtdcce}.{p_end}

{p 4 8}Please cite as follows:{break}
Ditzen, J. 2018. xtdcce2: Estimating dynamic common correlated effects in Stata. The Stata Journal. 18:3, 585 - 617.
{p_end}

{p 4 8}The latest versions can be obtained via {stata "net from https://github.com/JanDitzen/xtdcce2"}.{p_end}

{marker ChangLog}{title:Version History}
{p 4 8}Version 4.4 to 4.5 - May 2023{p_end}
{p 8 10} - supports new reghdfe version{p_end}
{p 4 8}Version 4.3 to 4.4 - May 2023{p_end}
{p 8 10} - fixed bug in trend option{p_end}
{p 4 8}Version 4.2 to 4.3 - May 2023{p_end}
{p 8 10} - fixed bug in var/cov estimation when using pooled coefficients and R matrix is zero{p_end}
{p 4 8}Version 4.1 to 4.2 - April 2023{p_end}
{p 8 10} - fixed bug in xtcd2 and xtcse2{p_end}
{p 4 8}Version 4. to 4.1 - March 2023{p_end}
{p 8 10} - fixed bug when using different lag lengths for CSA{p_end}
{p 4 8}Version 3.01 to 4 - Feb 2023{p_end}
{p 8 10} - bootstrap support{p_end}
{p 8 10} - added option {cmd:mgmissing}{p_end}
{p 8 10} - added option rcce{p_end}
{p 8 10} - added option {cmd:fast2}{p_end}
{p 8 10} - fixed error when pooled and ardl used.{p_end}
{p 4 8}Version 3.0 to 3.01{p_end}
{p 8 10} - error if abbreviation is cr() used fixed.{p_end}
{p 4 8}Version 2.0 to 3.0{p_end}
{p 8 10} - improved support for factor variables.{p_end}
{p 8 10} - fix for mm_which2.{p_end}
{p 8 10} - message for large panels.{p_end}
{p 8 10} - error in calculation for variances of cross-sectional unit specific coefficients{p_end}
{p 8 10} - fix predict program: partial now only in-sample and bug fixed when xb2 and reportc was used (thanks to Tullio Gregori for the pointers).{p_end}
{p 8 10} - added global and local cross-sectional averages{p_end}
{p 4 8}Version 1.34 to 2.0{p_end}
{p 8 10} - Bug fix in calculation of minimal T dimension, added option nodimcheck.{p_end}
{p 8 10} - Speed improvements (thanks to Achim Ahrens for the suggestions).{p_end}
{p 8 10} - Bug fixes for jackknife (thanks to Collin Rabe for the pointer).{p_end}
{p 8 10} - Bug fix in predict and if (thanks for Deniey A. Purwanto and Tullio Gregori for the pointers).{p_end}
{p 8 10} - Bug fix if binary variable used and constant partialled out.{p_end}
{p 8 10} - Bug fixed in calculation of R2, added adjusted R2 for pooled and MG regressions.{p_end}
{p 8 10} - Newey West and Westerlund, Petrova, Norkute standard errors for pooled regressions.{p_end}
{p 8 10} - invsym for rank deficient matrices.{p_end}
{p 8 10} - Added {cmd:xtcse2} support.{p_end}
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
{p 4 4}See also: {help xtdcce2fast}, {help xtcd2}, {help xtcse2}, {help ivreg2}, {help xtmg}, {help xtpmg}, {help moremata}{p_end} 
