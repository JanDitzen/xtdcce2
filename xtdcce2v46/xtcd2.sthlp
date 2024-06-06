{smcl}
{hline}
{hi:help xtcd2}{right: v. 4.2 - May 2023}
{right:SJ18-3: st0536}
{hline}
{title:Title}

{p 4 4}{cmd:xtcd2} - testing for weak cross sectional dependence. 

{title:Syntax}

{p 4 13}{cmd:xtcd2} [{varlist}] [if] [{cmd:,}{cmd:pesaran}  {cmd:cdw} {cmd:pea} {cmd:cdstar}
{cmd:rho}
{cmd:pca(integer)}
{cmd:reps(integer)} 
{cmdab:kden:sity} {cmd:name({it:string})} 
{cmd:heatplot[(}{cmdab:abs:olute} {it:options_heatplot}{cmd:)] contour[(}{cmdab:abs:olute} {it:options_contour}{cmd:) {cmdab:noadju:st}]}
{cmd:seed(integer)}
]{p_end}

{p 4 4}{varlist} is the name of residuals or variables to be tested for weak cross sectional dependence. 
{it:varlist} may contain time-series operators, see {help tsvarlist}. 
{varlist} is optional if the command is performed after an estimation (postestimation).{p_end}


{title:Contents}

{p 4}{help xtcd2##description:Description}{p_end}
{p 4}{help xtcd2##options:Options}{p_end}
{p 4}{help xtcd2##saved_vales:Saved Values}{p_end}
{p 4}{help xtcd2##examples:Examples}{p_end}
{p 4}{help xtcd2##references:References}{p_end}
{p 4}{help xtcd2##about:About}{p_end}


{marker description}{title:Description}

{p 4 4}{cmd:xtcd2} tests residuals or a variables for weak cross sectional dependence in a panel data model. 
It implements the tests by Pesaran ({help xtcd2##Pesaran2015:2015}, {help xtcd2##Pesaran2021:2021}),
the weighted CD test (CDw) by
Juodis & Reese ({help xtcd2##JR2021:2022}) including the power enhancement (Fang et al., {help xtcd2##Fang2015:2015}).
It also implements the CD* from Pesaran & Xie ({help xtcd2##PesaranXie2021:2021}).
As a default all four test statistics are calculated and presented next to each other.
p-values are displayed in parenthesis.{p_end}

{p 4 4}Cross sectional dependence in the error term occurs if dependence between cross sectional units in a regression is not accounted for.
The dependence between units violates the basic OLS assumption of an independent and identically distributed error term.
In the worst case cross sectional dependence in the error term can lead to omitted variable bias or endogeneity and therefore to inconsistent estimates.
Cross sectional dependence can be measured as the correlation between units.
For example the correlation of the errors of unit i and unit j can be calculated.
Obviously, if the correlation is large, cross sectional dependence is present.{p_end}

{marker oCD}{p 4 4}{ul:{bf:The "original" CD Test (Pesaran, {help xtcd2##Pesaran2015:2015})}}{break}
Pesaran ({help xtcd2##Pesaran2015:2015}) 
develops a test for weak cross sectional dependence based on this principle.
Weak cross sectional dependence means that the correlation between units at each point in time converges to zero as the number of cross section goes to infinity.
Under strong dependence the correlation converges to a constant.
The null hypothesis of the test is, that the error term (or variable) is weakly cross sectional dependent.
This means that correlation between an observation of unit i in time t and unit j in time t is zero.
The hypothesis is:{p_end}

{p 8} H0: errors are weakly cross sectional dependent.{p_end}

{p 4 4}Pesaran ({help xtcd2##Pesaran2015:2015}) derives a test statistic, which sums the correlation coefficients of the different units.
The test statistic for a balanced panel is:{p_end}

{p 8} CD = [2*T / (N*(N-1))]^(1/2) * sum(i=1,N-1) sum(j=i+1,N) rho(ij),{p_end}

{p 4 4}and for an unbalanced panel (see Chudik, Pesaran, 2015):

{p 8} CD = [2 / (N*(N-1))]^(1/2) * sum(i=1,N-1) sum(j=i+1,N) [T(ij)^(1/2) * rho(ij)],{p_end}

{p 4 4}where rho(ij) is the correlation coefficient of unit i and j and T(ij) the number of common observations between i and j.
Under the null hypothesis the statistic is asymptotically{p_end}

{p 8} CD ~ N(0,1){p_end}

{p 4 4}distributed.{p_end}

{marker CDw}{p 4 4}{ul:{bf:Weighted CD test (Juodis and Reese, {help xtcd2##JR2021:2022})}}{break}
{cmd:xtcd2} further implements three alternatives to test for weak cross-sectional dependence.
It includes the weighted CD (CDw) test proposed by Juodis and Reese ({help xtcd2##JR2021:2022}).
Juodis and Reese ({help xtcd2##JR2021:2022}) show that the CD test diverges if the time dimension grows and 
the test is applied to residuals after a CCE or FE regression. The CDw test weights each observation by cross-section specific Rademacher weights. 
The pair wise correlations are calculated as:{p_end}

{p 8}rho(ij) = sum(t=1,T) w(i)eps(i,t)eps(j,t)w(j){p_end}

{p 4 4}where w(i) and w(j) are the Rademacher weights 
which take on the values 1 or -1 with equal probability. 
To reduce the dependence on the random Rademacher weights,
the draw can be repeated using the {cmd:reps()} option.{p_end} 

{marker pea}{p 4 4}{ul:{bf:Power Enhanced CD test }}{ul:{bf:(Juodis and Reese, {help xtcd2##JR2021:2022} and }}{ul:{bf:Fan et al., {help xtcd2##Fan2015:2015})}}{break}
A second alternative proposed by Juodis and Reese ({help xtcd2##JR2021:2022}) is the 
Power Enhancement Approach (PEA) by Fan et al. ({help xtcd2##Fan2015:2015}). 
The power of the CD test is improved by calculating the CD test as:{p_end}

{p 8} CD = [2*T / (N*(N-1))]^(1/2) * sum(i=1,N-1) sum(j=i+1,N) rho(ij) + sum(i=2,N)sum(j=1,N-1}|rho(ij)|*(|rho(ij)>2 log(N)^(1/2)T^(-1){p_end}

{p 4 4}Fan et al. ({help xtcd2##Fan2015:2015}) show that the PEA works if the number of cross-sectional units (N) is very large. 
Therefore it is advisable to use the PEA method only for such datasets.{p_end} 

{marker cdstar}{p 4 4}{ul:{bf:CD Star (Pesaran & Xie, {help xtcd2##PesaranXie:2021})}}{break}
As forth test {cmd:xtcd2} implements the bias corrected CD* test from
Pesaran & Xie ({help xtcd2##PesaranXie:2021}). 
The bias corrected test statistic is based on the following:{p_end}

{p 8}CD* = (CD + (T/2*Theta)^(1/2))/(1-Theta){p_end}

{p 4 4}where {it:Theta} is the bias correction and a function of the estimated factor loadings.
The factor loadings are estimated using the first {it:p} principal components as factors.
Option {cmd:pca()} specifies the number of principal components. Default is 4.
In case of unbalanced panels an Expected Maximisation algorithm taken from
{help xtnumfac} is used.{p_end}

{p 4 4}{cmd:xtcd2} calculates the CD test statistic for given variables, or if run after an estimation command which supports {cmd:predict} and {cmd:e(sample)}.
In the latter case {cmd:xtcd2} calculates the error term using {cmd:predict, residuals} and then applies the CD test from above.
{cmd:xtcd2} supports balanced as well as unbalanced panels.
Furthermore by specifying the {cmd:kdensity} option, a kernel density plot with the distribution of the cross correlations is drawn.{p_end}

{p 4 4}If {cmd:xtcd2} is used after {help xtreg}, then the residuals are calculated using {cmd: predict, e} rather than {cmd: predict, res}.
That is the residuals including the fixed- or random-error component, see {help xtreg postestimation##predict: xtreg postestimation}.
In all other cases {cmd: predict, residuals} is used to calculate the residuals.{p_end}

{p 4 4}{cmd:xtcd2} can draw {help heatplot:heatplots} and {help twoway contour:contour} plots 
of the cross-correlations. 
To draw heatplots Ben Jann's {help heatplot} is required.
Contour plots are drawn using Stata's {help twoway contour}.{p_end}

{marker options}{title:Options}

{p 4 4}{cmd:pesaran} calculates the original CD test by Pesaran ({help xtcd2##Pesaran2015:2015}),
see {help xtcd2##oCD:Description of Pesaran (2015)}.{p_end}

{p 4 4}{cmd:cdw} calculates the weighted CD test following Juodis and Reese (2019), see {help xtcd2##CDw:Description of Juodis and Reese (2021)}.
Results vary if {it:seed} not specified.{p_end}

{p 4 4}{cmd:pea} uses the Power Enhancement Approach (PEA) by Fan et al. (2015), see {help xtcd2##pea:Description of Fan et al. (2015)}.
This method is designed for large panel panel datasets.{p_end}

{p 4 4}{cmd:cdstar} calculates the bias corrected CD test following Pesaran & Xie ({help xtcd2##PesaranXie2021:2021}),
see {help xtcd2##cdstar:Description of Pesaran & Xie (2021)}.{p_end}

{p 4 4}{cmd:reps(integer)} number of repetitions for the weighted CD test. 
Implies option {cmd:cdw}. Default is 30.{p_end}

{p 4 4}{cmd:pca(integer)} number of Principal Components when using the bias corrected CD test.
Requires option {cmd:cdstar}.
Default is 4.{p_end}

{p 4 4}{cmd:rho} saves the matrix with the cross correlations in {cmd:r(rho)}.{p_end}

{p 4 4}{cmdab:kden:sity} plots a kernel density plot of the cross correlations, see {help twoway kdensity}.
The number of observations, the mean, percentiles, minimum and maximum of the cross correlations are reported.
If {cmd:name({it:string})} is set, then the histogram is saved and not drawn.{p_end}

{p 4 4}{cmd:name({it:string})} saves the kdensity.{p_end}

{p 4 4}{cmd:heatplot[(}{cmdab:abs:olute} {it:options_heatplot}{cmd:)]} draws a {help heatplot} of the cross-correlations.
{it: options_heatplot} are options to be passed to {help heatplot.}
{cmdab:abs:olute} uses the absolute values of the cross-correlations.{p_end}

{p 4 4}{cmd:contour[(}{cmdab:abs:olute} {it:options_contour}{cmd:)]} draws a {help contour} plot of the cross-correlations.
{it: options_contour} are options to be passed to {help twoway contour.}
{cmdab:abs:olute} uses the absolute values of the cross-correlations.{p_end}

{p 4 4}{cmdab:noadj:ust} do not remove cross-section specific means for calculation of cross-correlations. 
This was the default in versions prior 2.3.{p_end}

{p 4 4}{cmd:seed(integer)} sets the {help set seed:seed} for the weighted CD test.{p_end}

{marker saved_vales}{title:Saved Values}

{cmd:xtcd2} stores the following in {cmd:r()}:

{col 4} Matrices
{col 8}{cmd: r(CD)}{col 27} matrix of CD test statistics
{col 8}{cmd: r(p)}{col 27} matrix of p-values of CD test statistic
{col 8}{cmd: r(rho)}{col 27} cross correlation matrix, if requested and one variable used. 
{col 8}{cmd: r(rho_varname)}{col 27} cross correlation matrix, if requested and more than one variable is used.

{marker examples}{title:Examples}
{p 4 4}An example dataset of the Penn World Tables 8 is available for download {browse "https://github.com/JanDitzen/xtdcce2/raw/master/xtdcce2_sample_dataset.dta":here}.
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

{p 4 4}The CD test statistic is known to diverge if many periodic
specific parameters are used (Juodis, Reese, 2021). 
Unit specific rademacher weights can be applied to prevent this behaviour
by using the option {cmd:cdw}:{p_end}

{p 8}{stata xtcd2 res, cdw}{p_end}

{p 4 4}To reduce the dependence of the weighted CD test statistic,
the test can be repeatedly performed with different weights using
the {cmd:reps()} option:{p_end}

{p 8}{stata xtcd2 res, cdw reps(20)}{p_end}

{p 4 4}To improve the power of the weighted CD test, the Power Enhancement
Approach can be applied by using the {cmd:pea} option:{p_end}

{p 8}{stata xtcd2 res, pea}{p_end}

{p 4 4}Testing the variable {it:log_rgdpo} for cross sectional dependence reads:{p_end}

{p 8}{stata xtcd2 log_rgdpo, noestimation}{p_end}


{marker references}{title:References}

{p 4 8}Chudik, A., Pesaran, M. H. 2015. Large Panel Data Models with Cross-Sectional Dependence A Survey.
Oxford Handbook of Panel Data. Edition 1. Editor Badi H. Baltagi.{p_end}

{marker JR2021}{p 4 8}Juodis, A., & Reese, S. 2022. The Incidental Parameters Problem in Testing for Remaining Cross-section Correlation. Journal of Business Economics and Statistics. 40(3).{p_end}

{marker Fan2015}{p 4 8}Fan, J., Y. Liao & J. Yao. 2015. Power Enhancement in High-Dimensional Cross-Sectional Tests. Econometrica(83): 1497–1541.{p_end}

{p 4 8}Feenstra, R. C., R. Inklaar, and M. Timmer. 2015. The Next Generation of the Penn World Table.
American Economic Review . www.ggdc.net/pwt{p_end}

{marker Pesaran2015}{p 4 8}Pesaran, M. H. 2015. Testing Weak Cross-Sectional Dependence in Large Panels.
Econometric Reviews 34(6-10): 1089-1117.{p_end}

{marker Pesaran2021}{p 4 8}Pesaran, M. H. 2021. 
General diagnostic tests for cross-sectional dependence in panels.
Empirical Economics 60: 13-50.{p_end}

{marker PesaranXie2021}{p 4 8}Pesaran, M. H. & Xie, Y. 2021. 
A Bias-Corrected CD Test for Error Cross-
Sectional Dependence in Panel Data Models
with Latent Factors.
Cambridge Working Papers in Economics 2158.{p_end}

{marker about}{title:Author}

{p 4}Jan Ditzen (Free University of Bozen-Bolzano){p_end}
{p 4}Email: {browse "mailto:jan.ditzen@unibz.it":jan.ditzen@unibz.it}{p_end}
{p 4}Web: {browse "www.jan.ditzen.net":www.jan.ditzen.net}{p_end}

{p 4 4}Thanks to Achim Ahrens and Simon Reese for providing many helpful comments to the code and an anonymous reviewer for many helpful comments.
The code for the bias corrected CD test was provided by H. Pesaran and Y. Xie.
The code for the EM algorithm is taken from {help xtnumfac}.
Thanks to Simon Reese for his help with it.{p_end}

{title:Also see}
{p 4 4}See also: {help xtdcce2}, {help xtcse2}
