{smcl}
{hline}
{hi:help xtdcce2fast}{right: v. 1.00 - August 2021}
{right:SJ18-3: st0536}
{hline}
{title:Title}

{p 4 4}{cmd:xtdcce2fast} - estimating heterogeneous coefficient models using common correlated effects in a dynamic panel
with a large number of observations over groups and time periods.
A faster version of {help xtdcce2}{p_end}

{title:Syntax}

{p 4 13}{cmd:xtdcce2fast} {depvar} [{indepvars}] {ifin} {cmd:,}
{cmdab:cr:osssectional(}{help varlist}{cmd:cr1 [,options1])}
[
{cmdab:globalcr:osssectional(}{help varlist}{cmd:cr2 [,options1])}
{cmdab:clustercr:osssectional(}{help varlist}{cmd:cr3 [,options1])}
{cmdab:p:ooled}({varlist})
{cmd:cr_lags}({it:string})
{cmdab:nocross:sectional}
{cmd:lr}({varlist})
{cmd:lr_options}({it:string}) 
{cmdab:noconst:ant}
{cmd:fullsample}
{cmdab:not:able}
{cmd:cd}
{cmd:postframe}
{cmd:nopost}
]{p_end}


{p 4}{ul:Options for cross-sectional averages}{p_end}
{synoptset 20}{...}
{synopt:options1}Description{p_end}
{synoptline}
{synopt:{cmd:cr_lags(}{help numlist})}number of lags of cross-section averages{p_end}
{synopt:{cmdab:cl:ustercr(}{help varlist})}name of cluster variable(s), only for {cmd:clustercrosssectional}{p_end}
{synoptline}
{p2colreset}{...}

{p 4 4}Data has to be {cmd:xtset} before using {cmd:xtdcce2fast}; see {help tsset}.
{it:varlists} may contain time-series operators, see {help tsvarlist}.
{cmd:xtdcce2fast} requires the {help moremata} package.{p_end}


{title:Contents}

{p 4}{help xtdcce2fast##description:Description}{p_end}
{p 4}{help xtdcce2fast##options:Options}{p_end}
{p 4}{help xtdcce2fast##saved_vales:Saved Values}{p_end}
{p 4}{help xtdcce2fast##postestimation: Postestimation commands}{p_end}
{p 4}{help xtdcce2fast##examples:Examples}{p_end}
{p 4}{help xtdcce2fast##references:References}{p_end}
{p 4}{help xtdcce2fast##about:About}{p_end}


{marker description}{title:Description}

{p 4 4}{cmd:xtdcce2fast} estimates a heterogeneous coefficient model in a large panel with dependence between cross sectional units.
A panel is large if the number of cross-sectional units (or groups) and the number of time periods are going to infinity.{break}
It fits the following estimation methods:{p_end}
{p 8 8}
i) The Mean Group Estimator (MG, Pesaran and Smith 1995).{break}
ii) The Common Correlated Effects Estimator (CCE, Pesaran 2006),{break} 
iii) The Dynamic Common Correlated Effects Estimator (DCCE, Chudik and Pesaran 2015), and{p_end}
{p 4 4}For a dynamic model, several methods to estimate long run effects are possible:{p_end}
{p 8 8} a) The Cross-Sectional Augmented Distributed Lag (CS-DL, Chudik et. al 2016) estimator which directly estimates the long run coefficients
from a dynamic equation, and {break}
b) The Cross-Sectional ARDL (CS-ARDL, Chudik et. al 2016) estimator using an ARDL model.{p_end}
{p 4 4}For a further discussion see Ditzen (2021).
Additionally {cmd:xtdcce2fast} can test for cross sectional dependence (see {help xtcd2}).{p_end}

{p 4 4}For an overview of the models and their explanation see {help xtdcce2}.{p_end}

{p 4 4}{cmd:xtdcce2fast} is an optimized version for speed and large datasets. 
In comparison to {help xtdcce2} it does not perform {ul:any} collinearity checks
does {ul:not} support pooled estimations and instrumental variable regressions.
It also stores some estimation results in {help mata} rather than {cmd:e()} to 
circumvent some restrictions on matrix dimensions in Stata, see {help limits}.{p_end}

{marker options}{title:Options}

{p 4 8}{cmdab:cr:osssectional(}{help varlist}{cmd:cr1 [,cr_lags(#)])} defines the variables which are added as cross sectional averages to the equation. 
Variables in {cmd:crosssectional()} may be included in {cmd:pooled()}, {cmd:exogenous_vars()}, {cmd:endogenous_vars()} and {cmd:lr()}. 
Variables in {cmd: crosssectional()} are partialled out, the coefficients not estimated and reported.{p_end}
{p 8 8}{cmd:crosssectional}(_all) adds all variables as cross sectional averages. 
No cross sectional averages are added if {cmd:crosssectional}(_none) is used, which is equivalent to {cmd:nocrosssectional}.
{cmd:crosssectional}() is a required option but can be substituted by {cmd:nocrosssectional}.{break}
If {cmd:cr(..., cr_lags())} is used, then the global option {cmd:cr_lags()} (see below) is ignored.{p_end}

{p 4 8}{cmdab:globalcr:osssectional(}{help varlist}{cmd:cr1 [,cr_lags(#)])} define global cross-section averages.
{cmd:global} cross-section averages are cross-section averages based on observeations which are excluded using {help if} statements.
If {cmd:cr(..., cr_lags())} is used, then the global option {cmd:cr_lags()} (see below) is ignored.{p_end}

{p 4 8}{cmdab:cluster:osssectional(}{help varlist}{cmd:cr1 [,cr_lags(#)] clustercr(varlist))} are clustered or local cross-section averages. 
That is, the cross-section averages are the same for each realisation of the variables defined in {cmd:clustercr()}.
For example, we have data observations regions of multiple countries, defined by variable {it:country}
Now we want to add cross-section averages for each country. 
We can define those by using the option {cmd:clustercr(varlist , clustercr(country))}.{break}
If {cmd:cr(..., cr_lags())} is used, then the global option {cmd:cr_lags()} (see below) is ignored.{p_end}

{p 4 8 12}{cmd:cr_lags}({it:integers}) sets the number of lags of the cross sectional averages. 
If not defined but {cmd:crosssectional()} contains a varlist, then only contemporaneous cross sectional averages are added but no lags. 
{cmd:cr_lags(0)} is the equivalent.
The number of lags can be different for different variables, where the order is the same as defined in {cmd:cr()}.
For example if {cmd:cr(y x)} and only contemporaneous cross-sectional averages of y but 2 lags of x are added,
then {cmd:cr_lags(0 2)}.{p_end}

{p 4 8 12}{cmdab:nocross:sectional} suppresses adding any cross sectional averages
Results will be equivalent to the Mean Group estimator.{p_end}

{p 4 8 12}{cmdab:noconst:ant} suppresses the constant term.{p_end}

{p 4 8}{cmd:xtdcce2fast} is able to estimate long run coefficients. 
Two models are supported:
The CS-DL (see {help xtdcce2##csdl: xtdcce2, csdl}) 
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
	{col 12}{cmd:nominus} does not subtract -1 from the coefficient of the dependent variable.

{p 4 8 12}{cmd: cd} calculate CD test. For details about the CD test see {help xtcd2}.{p_end}

{p 4 8 12}{cmd:fullsample} uses entire sample available for calculation of cross sectional averages. 
Any observations which are lost due to lags will be included in calculating the 
cross sectional averages (but are not included in the estimation itself).
The option does {cmd:not} remove any {help if} statements.
This means, that if an {cmd:if} removes certain cross-sectional units from
the estimation sample, {cmd:xtdcce2} will not use those (as specified by {cmd:if}),
even if {cmd:fullsample} is used. 
{p_end}

{p 4 8}{cmdab:not:able} do not display output.{p_end}

{p 4 8}{cmd:cd} calculate CD test statistic, see {help xtcd2}.{p_end}

{p 4 8}{cmd:postframe} save predicted values to frame. Speeds up {help predict}.{p_end}

{p 4 8}{cmd:nopost} do not save/post predicted values.{p_end}

{marker saved_vales}{title:Saved Values}

{cmd:xtdcce2} stores the following in {cmd:e()}:

{col 4} Scalars
{col 8}{cmd: e(N)}{col 27} number of observations
{col 8}{cmd: e(N_g)}{col 27} number of groups (cross sectional units)
{col 8}{cmd: e(T)}{col 27} number of time periods
{col 8}{cmd: e(K_mg)}{col 27} number of regressors (excluding variables partialled out)
{col 8}{cmd: e(r2_mg)}{col 27} pooled or mean group R-squared adjusted
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
{col 8}{cmd: e(cmd)}{col 27} command line
{col 8}{cmd: e(cmdline)}{col 27} command line including options
{col 8}{cmd: e(cr_lags}{col 27} structure of lags of cross-section averages
{col 8}{cmd: e(csa)}{col 27} cross-section averages
{col 8}{cmd: e(gcr_lags}{col 27} structure of lags of global cross-section averages
{col 8}{cmd: e(gcsa)}{col 27} global cross-section averages
{col 8}{cmd: e(ccr_lags}{col 27} structure of lags of clustered/local cross-section averages
{col 8}{cmd: e(ccsa)}{col 27} clustered/local cross-section averages
{col 8}{cmd: e(ccsa_cluster)}{col 27} cluster variable(s) for clustered cross-section averages

{col 4} Matrices
{col 8}{cmd: e(b)}{col 27} coefficient vector 
{col 8}{cmd: e(V)}{col 27} variance-covariance matrix 

{col 4} Mata
{col 8}{cmd: xtdcce2fast_bi}{col 27} coefficient vector, unit specific estimates
{col 8}{cmd: xtdcce2fast_Vi}{col 27} variance-covariance matrix, unit specific estimates

{col 4} Functions
{col 8}{cmd: e(sample)}{col 27} marks estimation sample

{marker postestimation}{title:Postestimation Commands}

{p 4 4}{cmd: predict} and {cmd: estat} can be used after {cmd: xtdcce2fast}. 

{p 2}{ul: predict}{p_end}
{p 4 4}The syntax for {cmd:predict} is:{p_end}

{p 6 13}{cmd: predict} [type] {newvar} {ifin} [ options ]{p_end}

{col 6}Options {col 25} Description
{hline}
{col 8}{cmdab:r:esiduals}{col 27} calculate residuals (e(i,t))
{col 8}{cmd:replace}{col 27} replace the variable if existing.
{hline}

{p 2}{ul: estat}{p_end}
{p 4 4}{cmd: estat} {cmdab:ebis:tructure} displays the structure for each cross-sectional unit.
It indicates if a coefficient was estimated, dropped, set to zero and if the coefficient 
is pooled.{p_end}

{marker examples}{title:Examples}

{p 4 4}An example dataset of the Penn World Tables 8 is available for download {browse "https://github.com/JanDitzen/xtdcce2/raw/master/xtdcce2_sample_dataset.dta":here}.
The dataset contains yearly observations from 1960 until 2007 and is already tsset.
To estimate a growth equation the following variables are used:
log_rgdpo (real GDP), log_hc (human capital), log_ck (physical capital) and log_ngd (population growth + break even investments of 5%).{p_end}

{marker e_mg}{p 4}{ul: Mean Group Estimation}{p_end}

{p 4 4}To estimate equation (3), the option {cmd:nocrosssectional} is used.
In order to obtain estimates for the constant, the option {cmd:reportconstant} is enabled. {p_end}

{p 8}{stata xtdcce2fast d.log_rgdpo L.log_rgdpo log_hc log_ck log_ngd , nocross}.{p_end}

{marker example_cce}{p 4}{ul: Common Correlated Effect}{p_end}

{p 4 4}Common Correlated effects (static) models can be estimated in several ways.
The first possibility is without any cross sectional averages related options:{p_end}

{p 8}{stata xtdcce2fast d.log_rgdpo log_hc log_ck log_ngd , cr(_all)}.{p_end}

{p 4 4}To check if residuals still exhibit cross-sectional dependence, 
the test for cross-section dependence can be included:{p_end}

{p 8}{stata xtdcce2fast d.log_rgdpo log_hc log_ck log_ngd , cr(_all) cd}.{p_end}

{marker example_dcce}{p 4}{ul:Dynamic Common Correlated Effect}{p_end}

{p 4 4}The lagged dependent variable is added to the model again.
To estimate the mean group coefficients consistently, the number of lags is set to 3:{p_end}

{p 8}{stata xtdcce2fast d.log_rgdpo L.log_rgdpo log_hc log_ck log_ngd , cr(d.log_rgdpo L.log_rgdpo log_hc log_ck log_ngd) cr_lags(3)}.{p_end}

{marker example_pmg}{p 4}{ul: Error Correction Models (CS-ECM)}{p_end}

{p 4 4}Variables of the long run cointegration vector are defined in {cmd:lr({varlist})}, where the first variable is the error correction speed of adjustment term.
Following the example from Blackburne and Frank (2007) with the {it:jasa2} dataset 
(the dataset is available at {browse www.econ.cam.ac.uk/people-files/emeritus/mhp1/jasa.exe:here} from {browse www.econ.cam.ac.uk/people/emeritus/mhp1/published-articles#1999:Pesaran's webpage}).{p_end}

{p 8}{stata xtdcce2fast d.c d.pi d.y if year >= 1962 , lr(L.c pi y) cr(_all) cr_lags(2)}{p_end}

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

{p 8}{stata xtdcce2fast d.y dp d.gd d.(dp d.gd), cr(d.y dp d.gd) cr_lags(0 3 3) fullsample}{p_end}

{p 4 4}For an ARDL(1,3,3) model the first and second lag are of the first differences are added by putting {cmd:L(0/2)} in front of the {cmd:d.(dp d.gd)}:{p_end}

{p 8}{stata xtdcce2fast d.y dp d.gd L(0/2).d.(dp d.gd), cr(d.y dp d.gd) cr_lags(0 3 3) fullsample}{p_end}

{p 4 4}Note, the {cmd:fullsample} option is used to reproduce the results in Chudik et. al (2013).{p_end} 

{marker example_ardl}{p 4}{ul: Cross-Section Augmented ARDL (CS-ARDL)}{p_end}

{p 4 4}Chudik et. al (2013) estimate besides the CS-DL model a CS-ARDL model. 
To estimate this model all variables are treated as long run coefficients and thus added to {varlist} in {cmd:lr({varlist})}.
{cmd:xtdcce2fast} first estimates the short run coefficients and the calculates then long run coefficients, 
following {help xtdcce2##eq_10:Equation 10}.
The option {cmd:lr_options(ardl)} is used to invoke the estimation of the long run coefficients.
Variables with the same base (i.e. forming the same long run coefficient) need to be either 
enclosed in parenthesis or {help tsvarlist} operators need to be used.
In Table 17 an ARDL(1,1,1) model is estimated with three lags of the cross-sectional averages:{p_end}

{p 8}{stata xtdcce2fast d.y , lr(L.d.y dp L.dp d.gd L.d.gd) lr_options(ardl) cr(d.y dp d.gd) cr_lags(3) fullsample} {p_end}

{p 4 4}{cmd:xtdcce2fast} calculates the long run effects identifying the variables by their base.
For example it recognizes that {it:dp} and {it:L.dp} relate to the same variable.
If the lag of {it:dp} is called {it:ldp}, then the variables need to be enclosed in parenthesis.{p_end}

{p 4 4}Estimating the same model but as an ARDL(3,3,3) and with enclosed parenthesis reads:{p_end}

{p 8}{stata xtdcce2fast d.y , lr((L(1/3).d.y) (L(0/3).dp) (L(0/3).d.gd) ) lr_options(ardl) cr(d.y dp d.gd) cr_lags(3) fullsample}{p_end}

{p 4 4}which is equivalent to coding without parenthesis:{p_end}

{p 8}{stata xtdcce2fast d.y , lr(L(1/3).d.y L(0/3).dp L(0/3).d.gd) lr_options(ardl) cr(d.y dp d.gd) cr_lags(3) fullsample}{p_end}


{marker references}{title:References}

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

{p 4 8}Bai, J. and Ng, S. 2004.
A panic attack on unit roots and cointegration. Econometrica 72: 1127-1177.{p_end}

{p 4 8}Ditzen, J. 2018. Estimating Dynamic Common Correlated Effcts in Stata. The Stata Journal, 18:3, 585 - 617.{p_end}

{p 4 8}Ditzen, J. 2021. Estimating long run effects and the exponent of cross-sectional dependence: an update to xtdcce2. The Stata Journal 21:3.{p_end}

{p 4 8}Eberhardt, M. 2012.
Estimating panel time series models with heterogeneous slopes.
Stata Journal 12(1): 61-71.{p_end}

{p 4 8}Holly, S., Pesaran, M. H., Yamagata, T. 2010.
A spatio-temporal model of house prices in the USA. 
Journal of Econometrics 158: 160 - 172.{p_end}

{p 4 8}Feenstra, R. C., R. Inklaar, and M. Timmer. 2015.
The Next Generation of the Penn World Table. American Economic Review. www.ggdc.net/pwt{p_end}

{p 4 8}Jann, B. 2005. 
moremata: Stata module (Mata) to provide various functions. 
Available from http://ideas.repec.org/c/boc/bocode/s455001.html.

{p 4 8}Pesaran, M. H. 2006.
Estimation and inference in large heterogeneous panels with a multifactor error structure.
Econometrica 74(4): 967-1012.{p_end}

{p 4 8}Pesaran, M. H., and R. Smith. 1995.
Econometrics Estimating long-run relationships from dynamic heterogeneous panels.
Journal of Econometrics 68: 79-113.{p_end}

{p 4 8}Shin, Y., M. H. Pesaran, and R. P. Smith. 1999.
Pooled Mean Group Estimation of Dynamic Heterogeneous Panels.
Journal of the American Statistical Association 94(446): 621-634.{p_end}

{p 4 8}Westerlund, J., Perova, Y., Norkute, M. 2019. CCE in fixed-T panels. 
Journal of Applied Econometrics: 1-6.{p_end}


{marker about}{title:Author}

{p 4}Jan Ditzen (Free University of Bozen-Bolzano){p_end}
{p 4}Email: {browse "mailto:jan.ditzen@unibz.it":jan.ditzen@unibz.it}{p_end}
{p 4}Web: {browse "www.jan.ditzen.net":www.jan.ditzen.net}{p_end}

{p 4 8}Please cite as follows:{break}
Ditzen, J. 2018. xtdcce2: Estimating dynamic common correlated effects in Stata. The Stata Journal. 18:3, 585 - 617.
{break}
or{break}
Ditzen, J. 2021. Estimating long run effects and the exponent of cross-sectional dependence: an update to xtdcce2. The Stata Journal 21:3{p_end}

{p 4 8}The latest versions can be obtained via {stata "net from https://github.com/JanDitzen/xtdcce2"} 
and beta versions including a full history of 
xtdcce2 from {stata "net from http://www.ditzen.net/Stata/xtdcce2_beta"}.{p_end}

{marker ChangLog}{title:Version History}
{p 4 8}This version: 1.0 - 16. August 2021{p_end}

{title:Also see}
{p 4 4}See also: {help xtdcce2}, {help xtcd2}, {help xtcse2}, {help ivreg2}, {help xtmg}, {help xtpmg}, {help moremata}{p_end} 
