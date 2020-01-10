{smcl}
{hline}
{hi:help xtcse2}{right: v. 1.02 - xx. July 2019}
{hline}
{title:Title}

{p 4 4}{cmd:xtcse2} - estimating the exponent of cross-sectional dependence in large panels.{p_end}

{title:Syntax}

{p 4 13}{cmd:xtcse2} [{varlist}] [if] [{cmd:,}
{cmd:pca(integer)}
{cmdab:stand:ardize}
{cmd:nocd}
{cmdab:RES:sidual}
{cmdab:R:eps(integer)}
{cmd:size(real)}
{cmd:tuning(real)}
{cmd:lags(integer)}
]

{p 4 4}Data has to be {cmd:xtset} before using {cmd:xtcse2}; see {help tsset}.
{it:varlist} may contain time-series operators, see {help tsvarlist}. 
If {it:varlist} if left empty, {cmd:xtcse2} predicts residuals from the
last estimation command,
see {help predict}.
{cmd:xtcse2} restricts the panel to the same number of time series per cross-sectional 
unit if the panel is unbalanced.{p_end}

{title:Contents}

{p 4}{help xtcse2##description:Description}{p_end}
{p 4}{help xtcse2##options:Options}{p_end}
{p 4}{help xtcse2##model:Econometric Model and Estimation}{p_end}
{p 4}{help xtcse2##saved_vales:Saved Values}{p_end}
{p 4}{help xtcse2##examples:Examples}{p_end}
{p 4}{help xtcse2##references:References}{p_end}
{p 4}{help xtcse2##about:About}{p_end}

{marker description}{title:Description}

{p 4 4}{cmd:xtcse2} estimates the exponent of cross-sectional dependence in a panel
with a large number of observations over time (T) and cross-sectional units (N).
The estimation method follows Bailey, Kapetanios, Pesaran (2016,2019) (henceforth BKP).{break}
A variable or a residual is cross-sectional dependent if it inhibits an
across cross-sectional units common factor.{p_end}

{p 4 4}{cmd:xtcse2} estimates the strength of the factor, for a residual or 
one or more variables. 
It outputs a standard error and confidence interval in the usual estimation output fashion,
however it does not show a t or z statistic and p-value. Generally speaking strong cross-sectional 
dependence occurs if alpha is above 0.5. 
Testing this is done by a separate test of weak cross-sectional dependence.
Therefore a confidence interval is more informative when estimating alpha.{p_end}

{p 4 4}{cmd:xtcse2} is intend to support the decision whether to include cross-sectional averages when using
{help xtdcce2} and accompanies {help xtcd2} in testing for weak cross-sectional dependence.
As a default it uses {help xtcd2} to test for weak cross-sectional dependence.
For a discussion of {cmd:xtdcce2} and {cmd:xtcd2} see Ditzen (2018,2019).{p_end}

{p 4 4}If the panel is unbalanced or observations are missing for a specific cross-section 
unit-time combination, then the sample is restricted to the union of all time periods across
cross-sectional units. 
For unbalanced panels with many missings or a variable with many missings, many
observations might be lost.{p_end}

{marker options}{title:Options}

{p 4 8}{cmd:pca(integer)} sets the number of principle components for the 
calculation of {it:cn}. Default is to use the first 4 components.{p_end}

{p 4 8}{cmdab:stand:ardize} do not standardizes variables.{p_end}

{p 4 8}{cmd:nocd} suppresses test for cross-sectional dependence using {help xtcd2}.{p_end}

{p 4 8}{cmd:size(real)} size of the test. Default is 10% (0.1).{p_end}

{p 4 8}{cmdab:RES:sidual} estimates the exponent of cross-sectional depdendence
in residuals, following BKP 2019.{p_end}

{p 4 8}{cmd:tuning(real)} tuning parameter for estimation of the exponent in residuals.
Default is 0.5.{p_end}

{p 4 8}{cmdab:R:eps(integer)} number of repetitions for bootstrap for calculation of
standard error and confidence interval for exponent in residuals. Default is 0.{p_end}

{p 4 8}{cmd:lags(integer)} number of lags (or training period) for calculation of 
recursive residuals when estimating the exponent after a regression with weakly exogenous regressors.{p_end}

{marker model}{title:Econometric Model and Estimation of the Exponent}

{ul:Econometric Model}

{p 4 4}For the following assume a general factor model with {it:m} factors:{p_end}

{col 10} x(i,t) = sum(j=1,m) b(j,i) f(j,t) + u(i,t)
{col 10} i = 1,...,N and t = 1,...,T

{p 4 4}where {it:x(i,t)} depends on unobserved {it:m} common factors f(j,t) with loading
{it:b(j,i)} and a cross sectionally independent error term {it:u(i,t)}. 
The time dimension (T) and the number of cross-sectional units (N) increases to infinity;
(N,T) -> infinity.{p_end}

{p 4 4}Chudik et al (2011) specify the factors as weak or strong using a constant {it:0<=alpha<=1} such that: 
{p_end}

{col 10}lim N^(-alpha) sum(j=1,m) abs(b(j,i)) = K < infinity.

{p 4 4}The type of dependence of the factors and thus the series then depends on 
the characteristics of {it:b(j,i)}:{p_end}

{col 12}alpha {col 40} dependence
{col 12}{hline 40}
{col 12}alpha = 0 {col 40} weak
{col 12}0 < alpha < 0.5 {col 40} semi weak
{col 12}0.5 <= alpha < 1 {col 40} semi strong
{col 12}alpha = 1 {col 40} strong

{p 4 4}Weak cross-sectional dependence can be thought of as the following: Even if
the number of cross-sectional units increases to infinity, the sum of the effect of the
common factors on the dependent variable remain constant. In the case of strong
cross-sectional dependence, the sum of the effect of the common factors becomes
stronger with an increase in the number of cross-sectional units.{p_end}

{p 4 4}In an estimation ignoring (semi-) strong dependence in the dependent or 
independent variables can cause an omitted
variable bias and therefore lead to inconsistent estimates. 
Pesaran (2015) proposes a test to test for weak cross-sectional dependence,
see {help xtcd2}. 
Pesaran (2006) and Chudik, Peasaran (2015) develop a method to estimate models
with cross-sectional dependence by adding time averages of the dependent and 
independent variables (cross-sectional averages). 
This estimator is implemented in Stata by {help xtdcce2}.{p_end}

{p 4 4}{cmd:xtcse2} estimates {it:alpha} in the equation above.
An {it:alpha} above 0.5 implies strong cross-sectional dependence 
and the appropriate when using a variable is required.{p_end}

{ul:Exponent Estimation (alpha)}

{p 4 4}Bailey, Kapetanios and Pesaran (2016) [BKP] propose a method 
for the estimation of the exponent. This section summarizes their approach,
a careful reading of the assumptions and theorems is strongly encouraged.{p_end}

{p 4 4}BKP derive a bias-adjusted estimator for {it:alpha} in a panel with
{it:N_g} cross-sectional units (see Eq. 13):{p_end}

{col 10} alpha = 1 + 1/2 ln(sigma_x^2)/ln(N_g) - 1/2 ln(mu^2)/ln(N_g) - 1/2 cn / [N_g * ln(N_g) * sigma_x^2]  

{p 4 4}where {it:sigma_x^2} is the variance of the cross-sectional averages. 
{it:mu^2} is average variance of significant regression coefficients of {it:x(i,t)} on
standardized cross-sectional averages with a pre specified size of the test. 
{it:cn} is the variance of scaled errors from a regression of the {it:x(i,t)} on 
its first {it:K(PC)} principle components. 
The number of principle components can be set using the option 
{cmd:pca(integer)}. The default is to use the first 4 principle components.{p_end}

{p 4 4}{cmd:xtcse2} outputs a standard error for alpha and a confidence interval in the 
usual Stata estimation fashion. 
A t- or z-test statistic with p-value is however omitted, because the test is done by the
test for weak cross-sectional dependence (CD-test), see {help xtcd2}. 
{cmd:xtcse2} automatically calculates the CD-test statistic and posts its results.
For the estimation of {it:alpha} a confidence interval is therefore more informative.{p_end}

{p 4 4}The calculation of the standard error of alpha follows the equation B47, Section VI of 
the online appendix of BKP, available 
{browse "https://onlinelibrary.wiley.com/doi/abs/10.1002/jae.2476":here}:{p_end}

{col 10} sigma(alpha) = [1/T V(q) + 4/N^(alpha) S]^(1/2) * 1/2 * 1/ln(N)

{p 4 4}{it:V(q)} is the regression standard error over the square of the sum of 
q coefficients of an AR(q) process of the square of the deviation of standardized 
cross-sectional averages. q is the third root of T. {it:S} is the squared sum divided by 
N^(alpha-1) of OLS coefficients of x(it) on standardized cross-sectional averages 
sorted according to their absolute value.{p_end} 

{p 4 4}In the case of estimating the exponent of cross-sectional dependence in residuals
Bailey, Kapetanios and Pesaran (2019) propose to use pair-wise correlations to estimate 
the exponent. For the calculation, only significant correlations are taken into account. 
The exponent is estiamted according to (Eq 25 in BKP 2019):{p_end}

{col 10} alpha = ln(tau' delta tau) / [2 ln(N)]

{p 4 4}where tau is a Nx1 vector of ones and delta is a matrix which contains 
the significant pair-wise correlations. 
For the significance, the size of the test and a tuning parameter need to be 
set a priori. 
{cmd:xtcse2} uses a size of 10% and a tuning parameter of 0.5 as a default.
Both can be changed with the options {cmd:size()} and {cmd:tuning()}.{p_end}

{p 4 4}In the case of a panel with weakly exogenous regressors, the pair-wise correlations
are based on recursive residuals, see BKP 2019, section 5.2. 
{cmd:xtcse2} allows for this if the option {cmd:lags()} is used.{p_end}

{p 4 4}BKP 2019 do not derive a closed form solution for standard errors. 
Therefore standard errors and confidence intervals are calculated using a simple bootstrap, 
where the cross-sectional units are replaced with replacement. This approach is
outlined in BKP 2019 section 5.3.{p_end}

{marker saved_vales}{title:Saved Values}

{cmd:xtcse2} stores the following in {cmd:r()}:

{col 4} Matrices
{col 8}{cmd: r(alpha)}{col 27} matrix with estimated alphas
{col 8}{cmd: r(alphaSE)}{col 27} matrix with estimated standard errors of alphas
{col 8}{cmd: r(alphas)}{col 27} matrix with estimated alpha tilde, alpha hat and alpha
{col 8}{cmd: r(N_g)}{col 27} matrix with number of cross-sectional units
{col 8}{cmd: r(T)}{col 27} matrix with number of time periods
{col 8}{cmd: r(CD)}{col 27} matrix with values of CD test statistic (if requested)
{col 8}{cmd: r(CDp)}{col 27} matrix values of p value of CD test statistic (if requested)

{marker examples}{title:Examples}

{p 4 4}An example dataset of the Penn World Tables 8 is available for download {browse "https://drive.google.com/open?id=1mL4s0X_pUjvTLTccmLbGNtfVBQ63Mon2":here}.
The dataset contains yearly observations from 1960 until 2007 and is already tsset.
To estimate a growth equation the following variables are used:
log_rgdpo (real GDP), log_hc (human capital), log_ck (physical capital) and log_ngd (population growth + break even investments of 5%).{p_end}

{p 4 4}Before running the growth regression the exponent of the cross-sectional 
dependence for the variables is estimated:{p_end}

{p 8}{stata xtcse2 d.log_rgdpo L.log_rgdpo log_hc log_ck log_ngd}.{p_end}

{p 4 4}All variables are highly cross-sectional dependent with alphas close or even 
above 1. Therefore an 
estimation method taking cross-sectional dependence is required. 
{help xtdcce2} is uses such an estimation method by adding cross-sectional averages 
to the model. After running {cmd:xtdcce2} it is possible to use {cmd:xtcse2} to estimate
the strength of the exponent of the residual using the option {cmd:residuals}.{p_end}

{p 8}{stata xtdcce2 log_rgdpo L.log_rgdpo log_ck log_ngd  log_hc , cr(log_rgdpo log_ck log_ngd  log_hc)  }.{p_end}
{p 8}{stata xtcse2, res}{p_end}

{p 4 4}{cmd:xtcse2} automatically predicts the residuals using {help predict} 
({help xtdcce2#postestimation:predict after xtdcce2}). 
The CD statistic is still in a rejection region, therefore the residuals 
exhibit strong cross-sectional dependence. {p_end}

{p 4 4}The estimated model above is mis-specified as it is a dynamic model, but no lags
of the cross-sectional averages are added. The number of lags should be in the
region of T^(1/3), so with 47 periods 3 lags are added. Then {cmd:xtcse2} is used 
to estimate alpha again, this time the CD test is omitted:{p_end}

{p 8}{stata xtdcce2 log_rgdpo L.log_rgdpo log_ck log_ngd  log_hc , cr(log_rgdpo log_ck log_ngd  log_hc) cr_lags(3) }.{p_end}
{p 8}{stata xtcse2 ,nocd residual lags(3) reps(200)}{p_end}

{p 4 4}The value of the CD test statistic is 1.32 and in a non-rejection region.
The estimate of {it:alpha} is considerably small the confidence interval does not 
overlap with 0.5{p_end}

{p 4 4}As a second exercise the first row of Table 1. in BKP is reproduced. 
The data is available on Pesaran's 
{browse "http://www.econ.cam.ac.uk/people/emeritus/mhp1/published-articles#2016":webpage}
and for download {browse "http://www.econ.cam.ac.uk/people-files/emeritus/mhp1/fp15/BKP_GAUSS_procedures.zip":here}.{p_end}

{p 4 4}After the data is loaded, reshaped (it comes in a matrix) and renamed as variable gdp,
the option {cmd:standardize} is used to standardize the variable as done in BKP:{p_end}

{p 8}{stata xtcse2 gdp , standardize}.{p_end}
 
{marker references}{title:References}

{p 4 8}Bailey, N., G. Kapetanios and M. H. Pesaran. 2016.
Exponent of cross-sectional dependence: estimation and inference.
Journal of Applied Econometrics 31: 929-960.{p_end}

{p 4 8}Bailey, N., G. Kapetanios and M. H. Pesaran. 2019.
Exponent of Cross-sectional Dependence for Residuals.
Sankhya B. The Indian Journal of Statistics: forthcoming.{p_end}

{p 4 8}Chudik, A., M. H. Pesaran and E. Tosetti. 2011. 
Weak and strong cross-section dependence and estimation of large panels.
The Econometrics Journal 14(1):C45–C90.{p_end}

{p 4 8}Chudik, A., and M. H. Pesaran. 2015.
Common correlated effects estimation of heterogeneous dynamic panel data models with weakly exogenous regressors.
Journal of Econometrics 188(2): 393-420.{p_end}

{p 4 8}Ditzen, J. 2018. Estimating Dynamic Common Correlated Effcts in Stata. The Stata Journal, 18:3, 585 - 617.{p_end}

{p 4 8}Ditzen, J. 2019. Estimating long run effects in models with cross-sectional dependence using xtdcce2. 
CEERP Working Paper Series: 7.{p_end}

{p 4 8}Pesaran, M. H. 2006.
Estimation and inference in large heterogeneous panels with a multifactor error structure.
Econometrica 74(4): 967-1012.{p_end}

{p 4 8}Pesaran, M. H. 2015. 
Testing Weak Cross-Sectional Dependence in Large Panels. 
Econometric Reviews 34(6-10):1089–1117.{p_end}

{marker about}{title:Author}

{p 4}Jan Ditzen (Heriot-Watt University){p_end}
{p 4}Email: {browse "mailto:j.ditzen@hw.ac.uk":j.ditzen@hw.ac.uk}{p_end}
{p 4}Web: {browse "www.jan.ditzen.net":www.jan.ditzen.net}{p_end}

{p 4 8}I am grateful to Sean Holly for the suggestion to implement the test 
to {help xtdcce2} and {help xtcd2} and Hashem Pesaran and Natalia Bailey for their 
help.
Part of the code is taken from BKP and transferred from Gauss to Stata.
The original Gauss code is available on Pesaran's 
{browse "http://www.econ.cam.ac.uk/people/emeritus/mhp1/published-articles#2016":webpage}
and for download {browse "http://www.econ.cam.ac.uk/people-files/emeritus/mhp1/fp15/BKP_GAUSS_procedures.zip":here}.
 All remaining errors are my own.{p_end}

{p 4 4}In the fashion of {cmd:xtdcce2} and {cmd:xtcd2} {cmd:xtcse2} has a {cmd:2} as a suffix.
There is no such program called {help xtcsee}.{p_end}

{p 4 4}Small discrepancies to BKP 2018 and 2019 can arise due to different programs
Stata vs. Gauss.{p_end}

{p 4 8}Please cite as follows:{break}
Ditzen, J. 2019. xtcse2: Estimating Exponent of Cross-Sectional Dependence in large panels.
{p_end}

{p 4 8}The latest versions can be obtained as a part of {cmd:xtdcce2} via {stata "net from https://github.com/JanDitzen/xtdcce2"} 
.{p_end}

{marker ChangLog}{title:Changelog}
{p 4 8}This version: 1.0 - 13. July 2019{p_end}

{title:Also see}
{p 4 4}See also: {help xtdcce2}, {help xtcd2}{p_end} 
