{smcl}
help {hi:xtcd2}
{hline}
{title:Title}

{p 4 4}{cmd:xtcd2} - testing for weak cross sectional dependence. 

{title:Syntax}

{p 4 13}{cmd:xtcd2} [{varname}(max=1)] [if] [{cmd:,}{cmdab:noest:imation} {cmd:rho} {cmdab:kden:sity} {cmd:name({it:string})} ]{p_end}

{p 4 4}{varname} is the name of residuals or variable to be tested for weak cross sectional dependence. 
{varname} is optional if the command is performed after an estimation (postestimation).{p_end}


{title:Contents}

{p 4}{help xtcd2##description:Description}{p_end}
{p 4}{help xtcd2##options:Options}{p_end}
{p 4}{help xtcd2##saved_vales:Saved Values}{p_end}
{p 4}{help xtcd2##examples:Examples}{p_end}
{p 4}{help xtcd2##references:References}{p_end}
{p 4}{help xtcd2##about:About}{p_end}


{marker description}{title:Description}

{p 4 4}{cmd:xtcd2} tests residuals or a variable for weak cross sectional dependence in a panel data.
Cross sectional dependence in the error term occurs if dependence between cross sectional units in a regression is not accounted for.
The dependence between units violates the basic OLS assumption of an independent and identically distributed error term.
In the worst case cross sectional dependence in the error term can lead to endogeneity and therefore to inconsistent estimates.
Cross sectional dependence can be measured as the correlation between units.
For example the correlation of the errors of unit i and unit j can be calculated.
Obviously, if the correlation is large, cross sectional dependence is present.{break}
Pesaran (2015) develops a test for weak cross sectional dependence based on this principle.
Weak cross sectional dependence means that the correlation between units at each point in time converges to zero as the number of cross section goes to infinity.
Under strong dependence the correlation converges to a constant.
The null hypothesis of the test is, that the error term (or variable) is weakly cross sectional dependent.
This means that correlation between an observation of unit i in time t and unit j in time t is zero.
The hypothesis is:{p_end}

{p 8} H0: errors are weakly cross sectional dependent.{p_end}

{p 4 4}Pesaran (2015) derives a test statistic, which sums the correlation coefficients of the different units.
The test statistic for a balanced panel is:{p_end}

{p 8} CD = [2*T / (N*(N-1))]^(1/2) * sum(i=1,N-1) sum(j=i+1,N) rho(ij),{p_end}

{p 4 4}and for an unbalanced panel (see Chudik, Pesaran, 2015):

{p 8} CD = [2 / (N*(N-1))]^(1/2) * sum(i=1,N-1) sum(j=i+1,N) [T(ij)^(1/2) * rho(ij)],{p_end}

{p 4 4}where rho(ij) is the correlation coefficient of unit i and j and T(ij) the number of common observations between i and j.
Under the null hypothesis the statistic is asymptotically{p_end}

{p 8} CD ~ N(0,1){p_end}

{p 4 4}distributed. 

{p 4 4}{cmd:xtcd2} calculates the CD test statistic for a given variable, or if run after an estimation command which supports {cmd:predict} and {cmd:e(sample)}.
In the latter case {cmd:xtcd2} calculates the error term using {cmd:predict, residuals} and then applies the CD test from above.
In the former case and if the option {cmd:noestimation} is set, {cmd:e(sample)} is not needed and any variable can be tested for cross sectional dependence.
In this setting, all available observations are used. {cmd:xtcd2} supports balanced as well as unbalanced panels.
Furthermore by specifying the {cmd:kdensity} option, a kernel density plot with the distribution of the cross correlations is drawn.{p_end}

{p 4 4}If {cmd:xtcd2} is used after {help xtreg}, then the residuals are calculated using {cmd: predict, e} rather than {cmd: predict, res}.
That is the residuals including the fixed- or random-error componen, see {help xtreg postestimation##predict: xtreg postestimation}.
In all other cases {cmd: predict, residuals} is used to calculate the residuals.{p_end}

{marker options}{title:Options}

{p 4 4}{cmdab:noest:imation} if specified, then{cmd:xtcd2} is not run as a postestimation command and does not require {cmd:e(sample)} to be set.
This option allows to test any variable.
If not set, then {cmd:xtcd2} uses either the variable specified in {varname} or predicts the residuals using {cmd:predict, residuals}.
In both cases the sample is restricted to {cmd:e(sample)}.{p_end}

{p 4 4}{cmd:rho} saves the matrix with the cross correlations in {cmd:r(rho)}.{p_end}

{p 4 4}{cmdab:kden:sity} plots a kernel density plot of the cross correlations, see {help twoway kdensity}.
The number of observations, the mean, percentiles, minimum and maximum of the cross correlations are reported.
If {cmd:name({it:string})} is set, then the histogram is saved and not drawn.{p_end}

{p 4 4}{cmd:name({it:string})} saves the kdensity.{p_end}


{marker saved_vales}{title:Saved Values}

{cmd:xtcd2} stores the following in {cmd:r()}:

{col 4} Scalars
{col 8}{cmd: r(cd)}{col 27} CD test statistic
{col 8}{cmd: r(p)}{col 27} p-value of CD test statistic

{col 4} Matrices
{col 8}{cmd: r(rho)}{col 27} cross correlation matrix, if requested


{marker examples}{title:Examples}
{p 4 4}An example dataset of the Penn World Tables 8 is available for download {browse "https://drive.google.com/open?id=1mL4s0X_pUjvTLTccmLbGNtfVBQ63Mon2":here}.
The dataset contains yearly observations from 1960 until 2007 and is already tsset.
Estimating a simple panel version of the Solow model and run the CD test afterwards:{p_end}

{p 8}{stata reg d.log_rgdpo log_hc log_ck log_ngd}{p_end}
{p 8}{stata xtcd2}{p_end}

{p 4 4}Predicting the error terms after {cmd:reg}, leads to the same result:{p_end}

{p 8}{stata reg d.log_rgdpo log_hc log_ck log_ngd}{p_end}
{p 8}{stata predict res, residuals}{p_end}
{p 8}{stata xtcd2 res}{p_end}

{p 4 4}The test statistic is 36.34 and the p-value is 0,
therefore rejecting the null hypothesis of weak cross sectional dependence.{p_end}

{p 4 4}To draw a density plot with the cross correlations the {cmd:kdensity} option is used:{p_end}

{p 8}{stata xtcd2 res, kdensity}{p_end}

{p 4 4}Testing the variable {it:log_rgdpo} for cross sectional dependence reads:{p_end}

{p 8}{stata xtcd2 log_rgdpo, noestimation}{p_end}


{marker references}{title:References}

{p 4 8}Feenstra, R. C., R. Inklaar, and M. Timmer. 2015. The Next Generation of the Penn World Table.
American Economic Review . www.ggdc.net/pwt{p_end}

{p 4 8}Chudik, A., Pesaran, M. H. 2015. Large Panel Data Models with Cross-Sectional Dependence A Survey.
Oxford Handbook of Panel Data. Edition 1. Editor Badi H. Baltagi.

{p 4 8}Pesaran, M. H. 2015. Testing Weak Cross-Sectional Dependence in Large Panels.
Econometric Reviews 34(6-10): 1089-1117.{p_end}

{marker about}{title:Author}

{p 4}Jan Ditzen (Heriot-Watt University){p_end}
{p 4}Email: {browse "mailto:j.ditzen@hw.ac.uk":j.ditzen@hw.ac.uk}{p_end}
{p 4}Web: {browse "www.jan.ditzen.net":www.jan.ditzen.net}{p_end}

{p 4}Thanks to Achim Ahrens for providing many helpful comments to the code and an anonymous reviewer for many helpful comments.{p_end}

{title:Also see}
{p 4 4}See also: {help xtdcce2} {help xtcse2}
