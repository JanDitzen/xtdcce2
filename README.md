# xtdcce2

## A package for model specification and estimation for linear panel time series models with cross-sectional dependence

![version](https://img.shields.io/github/v/release/janditzen/xtdcce2)  ![release](https://img.shields.io/github/release-date/janditzen/xtdcce2) 


The `xtdcce2` package contains
1. `xtdcce2` - Estimating heterogeneous coefficient models using common correlated effects in a dynamic panel with a large number of observations over groups and time periods.
2. `xtcd2` - Testing for weak cross-sectional dependence.
3. `xtcse2` - Estimation of the exponent of cross-sectional dependence.

An introduction into the topic can be found in my slides of the 2021 Stata
Economics Virtual Symposium [here](https://ideas.repec.org/p/boc/econ21/2.html).

__Table of Contents__
1. [Syntax](#1-syntax)
2. [Description](#2-description)
3. [Options](#3-options)
4. [Econometric and Empirical Model](#4-econometric-and-empirical-model)
	1. [Mean Group](#41-mean-group)
	2. [Common Correlated Effects](#42-common-correlated-effects)
	3. [Dynamic Common Correlated Effects](#43-dynamic-common-correlated-effects)
	4. [Pooled Estimations](#44-pooled-estimations)
	5. [Instrumental Variables](#45-instrumental-variables)
	6. [Error Correction Models (ECM/PMG)](#46-error-correction-models-ecmpmg)
	7. [Cross-Section Augmented Distributed Lag (CS-DL)](#47-cross-section-augmented-distributed-lag-cs-dl)
	8. [Cross-Section Augmented ARDL(CS-ARDL)](#48-cross-section-augmented-ardl-cs-ardl)
	9. [Regularized CCE (rCCE)](#49-regularized-cce)
5. [Saved Values](#5-saved-values)
6. [Postestimation Commands](#6-postestimation-commands)
	1. [predict](#61-predict)
	2. [estat](#62-estat)
	3. [Bootstrap](#63-bootstrap)
7. [Examples](#7-examples)
	1. [Mean Group](#71-mean-group-estimation)
	2. [Common Correlated Effects](#72-common-correlated-effects)
	3. [Dynamic Common Correlated Effects](#73-dynamic-common-correlated-effects)
	4. [Pooled Estimation](#74-pooled-estimations)
	5. [Instrumental Variables](#75-instrumental-variables)
	6. [Error Correction Model (ECM/PMG)](#76-error-correction-models-ecmpmg)
	7. [Cross-Section Augmented Distributed Lag (CS-DL)](#77-cross-section-augmented-ardl-cs-dl)
	8. [Cross-Section Augmented ARDL(CS-ARDL)](#78-cross-section-augmented-distributed-lag-cs-ardl)
	9. [Regularized CCE (rCCE)](#79-regularized-cce)
	10. [Bootstrapping](710-bootstrapping)
8. [Testing for Cross-sectional Dependence](#8-testing-for-cross-sectional-dependence)
	1. [Description](#81-description)
	2. [Options](#82-options)
	3. [Examples](#83-examples)
9. [Estimating the exponent of Cross-sectional Dependence](#9-estimating-the-exponent-of-cross-sectional-dependence)
	1. [Description](#91-description)
	2. [Options](#92-options)
	3. [Examples](#93-examples)
10. [References](#10-references)
11. [About](#11-about)
12. [How to install](#12-installation)
13. [Changelog](#13-change-log)

# 1. Syntax

```
xtdcce2 _depvar_ [_indepvars_] [_varlist2_ = _varlist_iv_] [ifin] , 
crosssectional(_varlist_[,cr_lags(_numlist_) rcce[(criterion(er/gr) scale npc(integer))]]) 
[clustercrosssectional(_varlist_, clustercr(_varlist_) [cr_lags(_numlist_)]) 
globalcrosssectional(_varlist_[,cr_lags(_numlist_)]) pooled(_varlist_) cr_lags(_numlist_) 
NOCRosssectional ivreg2options(_string_) e_ivreg2_ ivslow noisily lr(_varlist_) lr_options(_string_) 
pooledconstant reportconstant pooledvce(_string_) noconstant trend 
pooledtrend jackknife recursive nocd exponent xtcse2options(_string_) 
showindividual fullsample fast fast2 blockdiaguse nodimcheck useqr useinvsym noomitted mgmissing]
```

and for an optimized version for speed and large datasets:

```
xtdcce2fast _depvar_ [_indepvars_] [ifin] , crosssectional(_varlist_[,cr_lags(_numlist_) rcce[(criterion(er/gr) scale npc(integer))]]) 
[clustercrosssectional(_varlist_, clustercr(_varlist_) [cr_lags(_numlist_)]) 
globalcrosssectional(_varlist_[,cr_lags(_numlist_)]) cr_lags(_string_) 
NOCRosssectional lr(_varlist_) lr_options(_string_) 
reportconstant noconstant cd fullsample notable cd postframe nopost ]
```

where _varlist2_ are endogenous variables and _varlist_iv_ the instruments. Data has to be `xtset` before using `xtdcce2`; see `tssst`.
_varlists_ may contain time-series operators, see `tsvarlist`, or factor variables, see `fvvarlist`.
`xtdcce2` requires the  [moremata](https://ideas.repec.org/c/boc/bocode/s455001.html) package.

# 2. Description

`xtdcce2` estimates a heterogeneous coefficient model in a large panel with dependence between cross sectional units. A panel is large if the number of cross-sectional units (or groups) and the number of time periods are going to infinity.

It fits the following estimation methods:

i) The Mean Group Estimator (MG, Pesaran and Smith 1995).

ii) The Common Correlated Effects Estimator (CCE, Pesaran 2006),

iii) The Dynamic Common Correlated Effects Estimator (DCCE, Chudik and Pesaran 2015)

iv) The regularized CCE Estimator (rCCE, Juodis 2022), and

For a dynamic model, several methods to estimate long run effects are possible:

a) The Pooled Mean Group Estimator (PMG, Shin et. al 1999) based on an Error Correction Model,

b) The Cross-Sectional Augmented Distributed Lag (CS-DL, Chudik et. al 2016) estimator which directly estimates the long run coefficients
from a dynamic equation, and

c) The Cross-Sectional ARDL (CS-ARDL, Chudik et. al 2016) estimator using an ARDL model.
For a further discussion see Ditzen (2018b).

Additionally `xtdcce2` tests for cross sectional dependence (see `xtcd2`) and estimates the exponent of the
    cross sectional dependence alpha (see `xtcse2`). It also supports instrumental variable estimations (see [ivreg2](http://www.stata-journal.com/software/sj5-4/)).

`xtdcce2fast` is an optimized version for speed and large datasets. In comparison to xtdcce2 it does not perform any collinearity checks does not support pooled estimations and instrumental variable regressions. It also stores some estimation results in mata rather than e() to circumvent some restrictions on matrix dimensions in Stata


# 3. Options

Option | Description
--- | ---
**crosssectional(_varlist_, [cr_lags(_numlist_)])** | defines the variables which are added as cross sectional averages to the equation. Variables in **crosssectional()** may be included in **pooled()**, **exogenous_vars()**, **endogenous_vars()** and **lr()**. Variables in **crosssectional()** are partialled out, the coefficients not estimated and reported. **crosssectional(_all_)** adds all variables as cross sectional averages. No cross sectional averages are added if **crosssectional(_none_)** is used, which is equivalent to **nocrosssectional**. **crosssectional()** is a required option but can be substituted by **nocrosssectional**. If **cr(..., cr_lags())** is used, then the global option **cr_lags()** (see below) is ignored.
**rcce[(criterion(er/gr) scale npc(integer))]** | implements the regularized CCE estimator from Juodis (2022). **criterion()** sets the er or gr criterion from Ahn and Horenstein (2023).  **scale** scales cross-section averages, see Juodis (2022). **npc(real)** specifies number of eigenvectors without estimating it. Cannot be combined with criterion.
**globalcrosssectional(varlistcr1 [,cr_lags(_numlist_)])** | define global cross-section averages. global cross-section averages are cross-section averages based on observeations which are excluded using if statements. If **cr(..., cr_lags())** is used, then the global option **cr_lags()** (see below) is ignored.
**clusterosssectional(varlistcr1 [,cr_lags(_numlist_)] clustercr(varlist))** | are clustered or local cross-section averages.  That is, the cross-section averages are the same for each realisation of the variables defined in clustercr().  For example, we have data observations regions of multiple countries, defined by variable country Now we want to add cross-section averages for each country.  We can define those by using the option clustercr(varlist , clustercr(country)). If **cr(..., cr_lags())** is used, then the global option **cr_lags()** (see below) is ignored.
**pooled(_varlist_)** | specifies variables which estimated coefficients are constrained to be equal across all cross sectional units. Variables may occur in _indepvars_. Variables in **exogenous_vars()**, **endogenous_vars()** and **lr()** may be pooled as well.
**cr_lags(_integers_)** | sets the number of lags of the cross sectional averages. If not defined but **crosssectional()** contains a _varlist_, then only contemporaneous cross sectional averages are added but no lags. **cr_lags(0)** is the equivalent. The number of lags can be different for different variables, where the order is the same as defined in **cr()**. For example if **cr(y x)** and only contemporaneous cross-sectional averages of y but 2 lags of x are added, then **cr_lags(0 2)**.
**nocrosssectional** | suppresses adding any cross sectional averages Results will be equivalent to the Mean Group estimator.
**pooledconstant** | restricts the constant term to be the same across all cross sectional units.
**reportconstant** | reports the constant term. If not specified the constant is partialled out.
**noconstant** | suppresses the constant term.
**trend** |  adds a linear unit specific trend. May not be combined with **pooledtrend**.
**pooledtrend** | adds a linear common trend. May not be combined with **trend**.
**jackknife** | applies the jackknife bias correction method. May not be combined with **recursive**.
**recursive** | applies the recursive mean adjustment method. May not be combined with **jackknife**.
**nocd** | suppresses calculation of CD test. For details about the CD test see LINK TO XTCD2.
**exponent** | uses `xtcse2` to estimate the exponent of the cross-sectional dependence of the residuals. A value above 0.5 indicates cross-sectional dependence, see `xtcse2`.
**showindividual** | reports unit individual estimates in output.
**mgmissing** | if it is not possible to estimate individual coefficient for a cross-section because of missing data or perfect collinearity, individual coefficient is excluded for MG estimation. Coefficient will still be displayed as zero in e(bi).
**fullsample** | uses entire sample available for calculation of cross sectional averages. Any observations which are lost due to lags will be included calculating the cross sectional averages (but are not included in the estimation itself).
**fast** | omit calculation of unit specific standard errors.
**fast2** | use xtdcce2fast instead of xtdcce2.
**blockdiaguse** | uses **mata blockdiag** rather than an alternative algorithm. **mata blockdiag** is slower, but might produce more stable results.
**nodimcheck** | Does not check for dimension. Before estimating a model, `xtdcce2` automatically checks if the time dimension within each panel is long enough to run a mean group regression. Panel units with an insufficient number are automatically dropped.
**notable** |do not display output (only `xtdcce2fast`).
**cd** | calculate CD test statistic, see xtcd2 (only `xtdcce2fast`).
**postframe** | save predicted values to frame. Speeds up predict (only `xtdcce2fast`).
**nopost** | do not save/post predicted values (only `xtdcce2fast`).

xtdcce2 checks for collinearity in three different ways.  It checks if matrix of the cross-sectional averages is of full rank.  After partialling out the cross-sectional averages, it checks if the entire model across all cross-sectional units exhibits multicollinearity.  The final check is on a cross-sectional level.  The outcome of the checks influence which method is used to invert matrices.  If a check fails xtdcce2 posts a warning message.  The default is cholinv and invsym if a matrix is of rank-deficient.  For a further discussion see   [collinearity issues](#410-collinearity-issues)) . 

The following options are available to alter the behaviour of xtdcce2 with respect to matrices of not full rank:

Option | Description
--- | ---
**useqr** | calculates the generalized inverse via QR decomposition. This was the default for rank-deficient matrices for xtdcce2 pre version 1.35.
**useinvsym** |  calculates the generalized invers via mata invsym.
**showomitted** |  displays a cross-sectional unit - variable breakdown of omitted coefficients.
**noomitted** | no omitted variable checks on the entire model.

`xtdcce2` supports IV regressions using `ivreg2`. The IV specific options are:

Option | Description
--- | ---
**ivreg2options(_string_)** | passes further options to `ivreg2`, see `ivreg2, options`
**e_ivreg2** | posts all available results from `ivreg2` in **e()** with prefix _ivreg2_, see ivreg2, macros.
**noisily** | displays output of `ivreg2`.
**ivslow** | For the calculation of standard errors for pooled coefficients an auxiliary regressions is performed. In case of an IV regression, xtdcce2 runs a simple IV regression for the auxiliary regressions. This is faster. If option is used **ivslow**, then xtdcce2 calls ivreg2 for the auxiliary regression. This is advisable as soon as ivreg2 specific options are used.

`xtdcce2` is able to estimate long run coefficients.
Three models are supported:

The pooled mean group models (Shin et. al 1999), similar to [xtpmg](http://www.stata-journal.com/software/sj7-2/) (see [xtdcce2, ecm](#46-error-correction-modelsecmpmg)), the CS-DL (see [xtdcce2, CSDL](#4.7-cross-sectional-augmented-distributed-lag-cs-dl)) and CS-ARDL method (see [xtdcce2, ardl](#4.8-cross-section-augmented-ardl-cs-ardl)) as developed in Chudik et. al 2016. No options for the CS-DL model are necessary.

Options | Description
--- | ---
**lr(_varlist_)** | specifies the variables to be included in the long-run cointegration vector. The first variable(s) is/are the error-correction speed of adjustment term. The default is to use the pmg model. In this case each estimated coefficient is divided by the negative of the long-run cointegration vector (the first variable). If the option **ardl** is used, then the long run coefficients are estimated as the sum over the coefficients relating to a variable, 	divided by the sum of the coefficients of the dependent variable.
**lr_options(_string_)** | options for the long run coefficients. Options are:
**ardl** |  estimates the CS-ARDL estimator. For further details see [xtdcce2, ardl](#4.8-cross-section-augmented-ardl-(cs-ardl)).
**nodivide** | coefficients are not divided by the error correction speed of adjustment vector. Equation (7) is estimated, see [xtdcce2, ecm](#4.6-error-correction-models(pooled-mean-group-estimator)).
**xtpmgnames** | coefficient names in **e(b_p_mg)** (or **e(b_full)**) and **e(V_p_mg)** (or **e(V_full)**) match the name convention from `xtpmg`.

# 4. Econometric and Empirical Model

### Econometric Model

Assume the following dynamic panel data model with heterogeneous coefficients:

```
(1) y(i,t) = b0(i) + b1(i)*y(i,t-1) + x(i,t)*b2(i) + x(i,t-1)*b3(i) + u(i,t) u(i,t) = g(i)*f(t) + e(i,t)
```

where f(t) is an unobserved common factor loading, g(i) a heterogeneous factor loading, x(i,t) is a (1 x K) vector and b2(i) and b3(i) the coefficient vectors. The error e(i,t) is iid and the heterogeneous coefficients b1(i), b2(i) and b3(i) are randomly distributed around a common mean. It is assumed that x(i,t) is strictly exogenous. In the case of a static panel model (b1(i) = 0) Pesaran (2006) shows that mean of the coefficients 0, b2 and b3 (for example for b2(mg) = 1/N sum(b2(i))) can be consistently estimated by adding cross sectional means of the dependent and all independent variables. The cross sectional means approximate the unobserved factors. In a dynamic panel data model (b1(i) <> 0) pT lags of the cross sectional means are added to achieve consistency (Chudik and Pesaran 2015). The mean group estimates for b1, b2 and b3 are consistently estimated as long as N,T and pT go to infinity. This implies that the number of cross sectional units and time periods is assumed to grow with the same rate.

In an empirical setting this can be interpreted as N/T being constant. A dataset with one dimension being large in comparison to the other would lead to inconsistent estimates, even if both dimension are large in numbers. For example a financial dataset on stock markets returns on a monthly basis over 30 years (T=360) of 10,000 firms would not be sufficient. While individually both dimension can be interpreted as large, they do not grow with the same rate and the ratio would not be constant. Therefore an estimator relying on fixed T asymptotics and large N would be appropriate. On the other hand a dataset with lets say N = 30 and T = 34 would qualify as appropriate, if N and T grow with the same rate.

The variance of the mean group coefficient b1(mg) is estimated as:

```
var(b(mg)) = 1/N sum(i=1,N) (b1(i) - b1(mg))^2
```

or if the vector pi(mg) = (b0(mg),b1(mg)) as:

```
var(pi(mg)) = 1/N sum(i=1,N) (pi(i) - pi(mg))(p(i)-pi(mg))'
```
### Empirical Model

The empirical model of equation (1) without the lag of variable x is:

```
(2) y(i,t) = b0(i) + b1(i)*y(i,t-1) + x(i,t)*b2(i) + sum[d(i)*z(i,s)] + e(i,t),
```

where z(i,s) is a (1 x K+1) vector including the cross sectional means at time s and the sum is over s=t...t-pT. `xtdcce2` supports several different specifications of equation (2).

`xtdcce2` partials out the cross sectional means internally. For consistency of the cross sectional specific estimates, the matrix z = (z(1,1),...,z(N,T)) has to be of full column rank. This condition is checked for each cross section. `xtdcce2` will return a warning if z is not full column rank. It will, however, continue estimating the cross sectional specific coefficients and then calculate the mean group estimates.
The mean group estimates will be consistent. For further reading see, Chudik, Pesaran (2015, Journal of Econometrics), Assumption 6 and page 398.

## 4.1 Mean Group

If no cross sectional averages are added (d(i) = 0), then the estimator is the Mean Group Estimator as proposed by Pesaran and Smith (1995).

The estimated equation is:

```
(3) y(i,t) = b0(i) + b1(i)*y(i,t-1) + x(i,t)*b2(i) + e(i,t).
```

Equation (3) can be estimated by using the **nocross** option of `xtdcce2`. The model can be either sta#tic (b(1) = 0) or dynamic (b(1) <> 0).

See [example](#71-mean-group-estimation)

## 4.2 Common Correlated Effects

The model in equation (3) does not account for unobserved common factors between units. To do so, cross sectional averages are added in the fashion of Pesaran (2006):

```
(4) y(i,t) = b0(i) + x(i,t)*b2(i) + d(i)*z(i,t) + e(i,t).
```

Equation (4) is the default equation of `xtdcce2`. Including the dependent and independent variables in **crosssectional()** and setting **cr_lags(0)** leads to the same result. **crosssectional()** defines the variables to be included in z(i,t). Important to notice is, that b1(i) is set to zero.

See [example](#72-common-correlated-effects)

## 4.3 Dynamic Common Correlated Effects

If a lag of the dependent variable is added, endogeneity occurs and adding solely contemporaneous cross sectional averages is not sufficient any longer to achieve consistency.

Chudik and Pesaran (2015) show that consistency is gained if pT lags of the cross sectional averages are added:

```
(5) y(i,t) = b0(i) + b1(i)*y(i,t-1) + x(i,t)*b2(i) + sum [d(i)*z(i,s)] + e(i,t).
```

where s = t,...,t-pT. Equation (5) is estimated if the option **cr_lags()** contains a positive number.

See [example](#73-dynamic-common-correlated-effects)

## 4.4 Pooled Estimations

Equations (3) - (5) can be constrained that the parameters are the same across units. Hence the equations become:

```
(3-p) y(i,t) = b0 + b1*y(i,t-1) + x(i,t)*b2 + e(i,t),
(4-p) y(i,t) = b0 + x(i,t)*b2 + d(i)*z(i,t) + e(i,t),
(5-p) y(i,t) = b0 + b1*y(i,t-1) + x(i,t)*b2 + sum [d(i)*z(i,s)] + e(i,t).
```
Variables with pooled (homogenous) coefficients are specified using the **pooled(_varlist_)** option. The constant is pooled by using the option **pooledconstant**. In case of a pooled estimation, the standard errors are obtained from a mean group regression. This regression is performed in the background. See Pesaran (2006).

See [example](#74-pooled-estimations)

## 4.5 Instrumental Variables

**xtdcce2** supports estimations of instrumental variables by using the [ivreg2](http://www.stata-journal.com/software/sj5-4/) package. Endogenous variables (to be instrumented) are defined in _varlist_2_ and their instruments are defined in _varlist__iv_.

See [example](#75-instrumental-variables)

## 4.6 Error Correction Models (ECM/PMG)

As an intermediate between the mean group and a pooled estimation, Shin et. al (1999) differentiate between homogenous long run and heterogeneous short run effects. Therefore the model includes mean group as well as pooled coefficients. Equation (1) (without the lag of the explanatory variable x and for a better readability without the cross sectional averages) is transformed into an ARDL model:

```
(6)y(i,t) = phi(i)*(y(i,t-1) - w0(i) - x(i,t)*w2(i)) + g1(i)*[y(i,t)-y(i,t-1)] + [x(i,t) - x(i,t-1)] * g2(i) + e(i,t),
```

where phi(i) is the cointegration vector, w(i) captures the long run effects and g1(i) and g2(i) the short run effects. Shin et. al estimate the long run coefficients by ML and the short run coefficients by OLS.

`xtdcce2` estimates a slightly different version by OLS:

```
(7)	y(i,t) = o0(i) + phi(i)*y(i,t-1) + x(i,t)*o2(i) + g1(i)*[y(i,t)-y(i,t-1)] + [x(i,t) - x(i,t-1)] * g2(i) + e(i,t),
```

where w2(i) = - o2(i) / phi(i) and w0(i) = - o0(i)/phi(i).

Equation (7) is estimated by including the levels of y and x as long run variables using the **lr(_varlist_)** and **pooled(_varlist_)** options and adding the first differences as independent variables.

`xtdcce2` estimates equation (7) but automatically calculates estimates for w(i) = (w0(i),...,wk(i)). The advantage estimating equation (7) by OLS is that it is possible to use IV regressions and add cross sectional averages to account for dependencies between units. The variance/covariance matrix is calculated using the delta method,
for a further discussion, see Ditzen (2018).

See [Example](#76-error-correction-models-ecmpmg)

## 4.7 Cross-Section Augmented Distributed Lag (CS-DL)

Chudik et. al (2016) show that the long run effect of variable x on variable y in equation (1) can be directly estimated.

Therefore they fit the following model, based on equation (1):

```
(8)  y(i,t) = w0(i) + x(i,t) * w2(i)  + delta(i) * (x(i,t) - x(i,t-1)) + sum [d(i)*z(i,s)] + e(i,t)
```

where w2(i) is the long effect and sum [d(i) z(i,s)] the cross-sectional averages with
an appropriate number of lags. To account for the lags of the dependent variable, the corresponding number of first differences are added.

If the model is an ARDL(1,1), then only the first difference of the explanatory variable is added. In the case of an ARDL(1,2) model, the first and the second difference are added. The advantage of the CS-DL approach is, that no short run coefficients need to be estimated.

A general ARDL(py,px) model is estimated by:

```
(8) y(i,t) = w0(i) + x(i,t) * w2(i)  + sum(l=1,px) delta(i,l) * (x(i,t-l) - x(i,t-l-1))  + sum [d(i)*z(i,s)] + e(i,t)
```

The mean group coefficients are calculated as the unweighted averages of all cross-sectional specific coefficient estimates. The variance/covariance matrix is estimated as in the case of a Mean Group Estimation.

See [Example](#77-cross-section-augmented-distributed-lag-cs-dl)

## 4.8 Cross-Section Augmented ARDL (CS-ARDL)
As an alternative approach the long run coefficients can be estimated by first estimating the short run coefficients and then the long run coefficients.

For a general ARDL(py,px) model including cross-sectional averages such as:

```
(9) y(i,t) = b0(i) + sum(l=1,py) b1(i,l) y(i,t-l) + sum(l=0,px) b2(i,l) x(i,t-l) +  sum [d(i)*z(i,s)] + e(i,t),
```

the long run coefficients for the independent variables are calculated as:

```
(10) w2(i) = sum(l=0,px) b2(i,l) / ( 1 - sum(l=1,py) b1(i,l))
```

and for the dependent variable as:

```
(11) w1(i) = 1 - sum(l=1,py) b1(i,l).
```

This is the CS-ARDL estimator in Chudik et. al (2016).

The variables belonging to w(1,i) need to be enclosed in parenthesis, or  _tsvarlist_ need to be used. For example coding **lr(y x L.x)** is equivalent to **lr(y (x lx))**, where lx is a variable containing the first lag of x (lx = L.x).

The disadvantage of this approach is, that py and px need to be known. The variance/covariance matrix is calculated using the delta method, see Ditzen (2018b).

See [Example](#78-cross-section-augmented-ardl-cs-ardl)

## 4.9 Regularized CCE (rCCE)

The CCE approach can involve a large number of cross-section averages which is larger than the number of factors and can lead to a non-trivial bias for the pooled and mean group estimator, see Karabiyik et. al.     (2017).  Juodis (2022) propose a solution for linear static panels which uses singular value decomposition to remove redundant singular values in the cross-section averages.  The so-called rCCE method involves the following steps:

            1. Calculate cross-sectional averages.
            2. Estimate number of common factors using the ER or GR criterion from Ahn and Horenstein (2013).
            3. Replace the cross-sectional averages with eigenvectors from the cross-section averages. 
            The eigenvectors are the eigenvectors of the largest eigenvalues and the number is obtained in step 2.

The method requires bootstrapped standard errors, see bootstrapping. 

See [Example](#79-regularized-cce)

## 4.10 Coefficient of Determination (R2)
`xtdcce2` calculates up to three different coefficients of determination (R2).  It calculates the standard un-adjusted R2 and the adjusted R2 as common in the literature.  If all coefficients are either pooled or heterogeneous, xtdcce2 calculates an adjusted R2 following Holly et. al (2010); Eq. 3.14 and 3.15.  The R2 and adjusted R2 are calculated even if the pooled or mean group adjusted R2 is calculated.  However the pooled or mean group adjusted R2 is displayed instead of the adjusted R2 if calculated.

In the case of a pure homogenous model, the adjusted R2 is calculated as:

```
R2(CCEP) = 1 - s(p)^2 / s^2
```

where s(p)^2 is the error variance estimator from the pooled regressions and s^2 the overall error variance estimator. They are defined as

```
s(p)^2 = sum(i=1,N) e(i)'e(i) / [N ( T - k - 2) - k],
s^2 = 1/(N (T -1)) sum(i=1,N) sum(t=1,T) (y(i,t) - ybar(i) )^2.
```

k is the number of regressors, e(i) is a vector of residuals and ybar(i) is the cross sectional specific mean of the dependent variable.

For mean group regressions the adjusted R2 is the mean of the cross-sectional individual R2 weighted by the overall error variance:

```
R2(CCEMG) = 1 - s(mg)^2 / s^2
s(mg)^2 = 1/N sum(i=1,N) e(i)'e(i) / [T - 2k - 2].
```

## 4.11 Collinearity Issues
(Multi-)Collinearity in a regression models means that two or more explanatory variables are linearly dependent.  The individual effect of a collinear explanatory variable on the dependent variable cannot be differentiated from the effect of another collinear explanatory variable.  This implies it is impossible to estimate the individual coefficient of the collinear explanatory variables.  If the explanatory variables are stacked into matrix X, one or more variables (columns) in x are collinear, then X'X is rank deficient.  Therefore it cannot be inverted and the OLS estimate of beta = inverse(X'X)X'Y does not exist.

In a model in which cross-sectional dependence in which dependence is approximated by cross-sectional averages, collinearity can easily occur.  The empirical model (2) can exhibit collinearity in four ways:
1. In the cross-sectional averages (z(i,s)) stacked in Z are collinear.
2. The cross-sectional averages and the explanatory variables are collinear.
3. In the global set of model of explanatory variables (the constant, y(i,t-1), x(i,t), x(i,t-1) stacked in X) are collinear for all i.
4. In a cross-sectional unit specific model of explanatory variables (the constant, y(i,t-1), x(i,t), x(i,t-1) stacked in X(i)) are collinear for some i.

xtdcce2 checks all types of collinearity and according to the prevalent type decides how to continue and invert (X'X).  It uses as a default cholinv. If a matrix is rank deficient it uses invsym, where variables (columns) are removed from the right.  If X = (X1 X2 X3 X4) and X1 and X4 are collinear, then X4 will be removed.  This is done by invsym, specifying the order in which columns are dropped. Older versions of xtdcce2 used qrinv for rank deficient matrices.  However results can be unstable and no order of which columns to be dropped can be specified.  The use of qrinv for rank deficient matrices can be enforced with the option useqr.

xtdcce2 takes the following steps if:
1. Z'Z is not of full rank
Before partialling out xtdcce2 checks of Z'Z is of full rank.  In case Z'Z is rank deficient, then xtdcce2 will return a warning.  Cross-section unit specific estimates are not consistent, however the mean group estimates are.  See Chudik, Pesaran (2015, Journal of Econometrics), Assumption 6 and page 398.

2. The cross-sectional averages and the explanatory variables are collinear.
In this case regressors from the right are dropped, this means the cross-sectional averages are dropped.  This case corresponds to the first because the cross-sectional averages are regressors for the partialling out.

3. X'X is collinear for all i.
xtdcce2 uses `_rmcoll` to remove any variables which are collinear on the global level.  A message with the list of omitted variables will be posted. A local of omitted variables is posted in e(omitted_var) and the number in e(K_omitted).

4. X(i)'X(i) is collinear for some i.
xtdcce2 automatically drops variables (columns) from the right for those cross-sectional units with collinear variables (columns).  An error message appears. More details can be obtained using the option showomitted by showing a matrix with a detailed break down on a cross-section - variable level.  The matrix is stored in e(omitted_var_i) as well.

Results obtained with xtdcce2 can differ from those obtained with reg or xtmg.  The reasons are that xtdcce2, partialles out the cross-sectional averages and enforces the use of doubles, both is not done in xtmg.  In addition it use as a default a different alogorithm to invert matrices.

# 5. Saved Values

`xtdcce2` stores the following in **e()**:

Scalars | Description
--- | ---
**e(N)** | number of observations
**e(N_g)** | number of groups (cross sectional units)
**e(T)** | number of time periods
**e(K_mg)** | number of regressors (excluding variables partialled out)
**e(N_partial)** | number of partialled out variables
**e(N_omitted)** | number of omitted variables
**e(N_pooled)** | number of pooled (homogenous) coefficients
**e(mss)** | model sum of squares
**e(rss)** | residual sum of squares
**e(F)** | F statistic
**e(rmse)** | root mean squared error
**e(df_m)** | model degrees of freedom
**e(df_r)** | residual degree of freedom
**e(r2)** | R-squared
**e(r2_a)** | R-squared adjusted
**e(cd)** | CD test statistic
**e(cdp)** | p-value of CD test statistic
**e(Tmin)** | minimum time (only unbalanced panels)
**e(Tbar)** | average time (only unbalanced panels)
**e(Tmax)** | maximum time (only unbalanced panels)
**e(cr_lags)** | number of lags of cross sectional averages

Macros | Description
--- | ---
**e(tvar)** | name of time variable
**e(idvar)** | name of unit variable
**e(depvar)** | name of dependent variable
**e(indepvar)** | name of independent variables
**e(omitted)** | omitted variables
**e(lr)** | variables in long run cointegration vector
**e(pooled)** | pooled (homogenous) coefficients
**e(cmd)** | command line
**e(cmdline)** | command line including options
**e(insts)** | instruments (exogenous) variables (only IV)
**e(istd)** | instrumented (endogenous) variables (only IV)
**e(version)** | xtdcce2 version, if `stata xtdcce2, version` used.

Matrices | Description
--- | ---
**e(b)** | coefficient vector
**e(V)** | variance-covariance matrix
**e(bi)** | coefficient vector of individual and pooled coefficients
**e(Vi)** | variance-covariance matrix of individual and pooled coefficients
**e(alpha)** | estimates of the exponent of cross-sectional dependence
**e(alpha)** | estimates of the standard error exponent of cross-sectional dependence

Estimated long run coefficients of the ARDL model are marked with the prefix _lr_.

Functions | Description
--- |---
**e(sample)** | marks estimation sample

# 6. Postestimation Commands

`predict` and `estat` can be used after `xtdcce2`.

## 6.1 Predict
The syntax for **predict** is:

```
predict [type] _newvar_ _ifin_ [ options ]
```

Options | Description
--- | ---
**xb** | calculate linear prediction on partialled out variables
**xb2** | calculate linear prediction on non partialled out variables
**stdp** | calculate standard error of the prediction
**residuals** | calculate residuals (e(i,t))
**cfresiduals** | calculate residuals including the common factors (u(i,t))
**coefficients** | a variable with the estimated cross section specific values for all coefficients is created. The name of the new variable is _newvar_varname_.
**se** | as **coefficient**, but with standard error instead.
**partial** | create new variables with the partialled out values.
**replace** | replace the variable if existing.

Option **xb2** is equivalent to calculate the coefficients and then multiply
the explanatory variables with it, while **xb** first partialles out
the cross sectional averages and then multiplies the coefficients.

The following Table summarizes the differences with the command line **xtdcce2 y x , nocross**:

**xb** | **xb2**
--- | ---
1. `predict coeff, coeff` | 1. `predict coeff, coeff`
2. `predict partial, partial`| 2. `gen xb2 = coeff_x * x`
3. `gen xb = coeff_x * partial_x` |

`xtdcce2` is able to calculate both residuals from equation (1). `predict _newvar_` , **eesiduals** calculates e(i,t). That is, the residuals of the regression with the cross sectional averages partialled out.

`predict _newvar , cfresiduals` calculates u(i,t) = g(i)f(g) + e(i,t). That is, the error including the cross-sectional averages. Internally, the fitted values are calculated and then subtracted from the dependent variable. Therefore it is important to note, that if a constant is used, the constant needs to be reported using the `xtdcce2` option **reportconstant**. Otherwise the u(i,t) includes the constant as well (u(i,t) = b0(i) + g(i)f(g) + e(i,t)).


## 6.2 estat
`estat` can be used to create a box, bar or range plot. The syntax is:

```
estat _graphtype_ [_varlist_] {ifin} [,combine(_string_) individual(_string_)} nomg cleargraph]
```

_graphtype_ | Description
--- | ---
_box_ | box plot
_bar_ | bar plot
_rcap_ | range plot

**Options** | Description
--- | ---
**individual(_string_)** | passes options for individual graphs (only bar and rcap)
**combine(_string)** | passes options for combined graphs
**nomg** | mean group point estimate and confidence interval are not included in bar and range plot graphs
**cleargraph** | clears the option of the graph command and is best used in combination with the **combine()** and **individual()** options
**dropzero** | does not display coefficients with zeros in bar or rcap graphs.

The name of the combined graph is saved in **r(graph_name)**.

## 6.3 Bootstrap

`xtdcce2` can bootstrap confidence intervals and standard errors. It supports two types of bootstraps: the wild bootstrap and the cross-section bootstrap.  The syntax is:

```
estat bootstrap , [options]
```

Options | Description
--- | ---
reps(integer) | Number of repetitions. Default 100.
seed(string) | Set seed, see seed.
wild | Use wild bootstrap rather than cross-section bootstrap.
cfresdiduals | Use residuals including common factors for wild bootstrap.
percentile | Bootstrap confidence intervals.
showindividual | show unit specific results.

estat bootstrap implements two types of bootstraps, the wild bootstrap and the cross-section bootstrap.  The cross-section bootstrap is the default.

The cross-section bootstrap draws with replacement from the cross-sectional dimension.  That is it draws randomly cross-sectional units with their entire time series.  It then estimates the model using xtdcce2. The cross-section bootstrap has been proposed in Westerlund et. al. (2019) or Goncalves and Perron (2014).

The wild bootstrap is a slower from of the wild bootstrap implemented in boottest (Roodman et. al. 2019).  It reweighs the residuals with Rademacher weights from the initial regression, recalculates the dependent variable and then runs xtdcce2.

The default is to bootstrap standard errors and then use the bootstrapped standard errors to calculate the confidence intervals.  Option percentile directly bootstraps confidence intervals.


# 7 Examples

An example dataset of the Penn World Tables 8 is available for download [here](https://github.com/JanDitzen/xtdcce2/raw/master/xtdcce2_sample_dataset.dta). The dataset contains yearly observations from 1960 until 2007 and is already tsset.

To estimate a growth equation the following variables are used: log_rgdpo (real GDP), log_hc (human capital), log_ck (physical capital) and log_ngd (population growth + break even investments of 5%).

## 7.1 Mean Group Estimation

To estimate equation (3), the option **nocrosssectional** is used.
In order to obtain estimates for the constant, the option **reportconstant** is enabled.

```
xtdcce2 d.log_rgdpo L.log_rgdpo log_hc log_ck log_ngd , nocross reportc
```

Omitting **reportconstant** leads to the same result, however the constant is partialled out:

```
xtdcce2 d.log_rgdpo L.log_rgdpo log_hc log_ck log_ngd , nocross
```


## 7.2 Common Correlated Effects

Common Correlated effects (static) models can be estimated in several ways. The first possibility is without any cross sectional averages related options:

```
xtdcce2 d.log_rgdpo log_hc log_ck log_ngd , cr(_all) reportc
```

Note, that as this is a static model, the lagged dependent variable does not occur and only contemporaneous cross sectional averages are used. Defining all independent and dependent variables in **crosssectional(_varlist_)** leads to the same result:

```
xtdcce2 d.log_rgdpo log_hc log_ck log_ngd , reportc cr(log_rgdpo log_hc log_ck log_ngd)
```

The default for the number of cross sectional lags is zero, implying only contemporaneous cross sectional averages are used. Finally the number of lags can be specified as well using the **cr_lags** option.

```
xtdcce2 d.log_rgdpo log_hc log_ck log_ngd , reportc cr(log_rgdpo log_hc log_ck log_ngd) cr_lags(0)
```

All three command lines are equivalent and lead to the same estimation results.

## 7.3 Dynamic Common Correlated Effects

The lagged dependent variable is added to the model again. To estimate the mean group coefficients consistently, the number of lags is set to 3:

```
xtdcce2 d.log_rgdpo L.log_rgdpo log_hc log_ck log_ngd , reportc cr(log_rgdpo  log_hc log_ck log_ngd) cr_lags(3)
```

### Using predict

`predict, _[options]_` can be used to predict the linear prediction, the residuals, coefficients and the partialled out variables. To predict the residuals, options **residuals** is used:

```
predict residuals, residuals
```

The residuals do not contain the partialled out factors, that is they are e(i,t) in equation (1) and (2). To estimate u(i,t), the error term containing the common factors, option **cfresiduals** is used:

```
predict uit, cfresiduals
```

In a similar fashion, the linear prediction (option **xb**, the default) and the standard error of the prediction can be obtained. The unit specific estimates for each variable and the standard error can be obtained using options **coefficients** and **se**.

For example, obtain the coefficients for log_hc from the regression above and calculate the mean, which should be the same as the
mean group estimate:

```
predict coeff, coefficients

sum coeff_log_hc.
```

The partialled out variables can be obtained using

```
predict partial, partial
```

Then a regression on the variables would lead to the same results as above.{break}
If the option **replace** is used, then the _newvar_ is replaced if it exists.


## 7.4 Pooled Estimations

All coefficients can be pooled by including them in **pooled(_varlist_)**. The constant is pooled by using the **pooledconstant** option:

```
xtdcce2 d.log_rgdpo L.log_rgdpo log_hc log_ck log_ngd , reportc cr(log_rgdpo  log_hc log_ck log_ngd) pooled(L.log_rgdpo  log_hc log_ck log_ngd) cr_lags(3) pooledconstant
```

## 7.5 Instrumental Variables

Endogenous variables can be instrumented by using options **endogenous_vars(_varlist_)** and **exogenous_vars(_varlist_)**. Internally `ivreg2` estimates the individual coefficients. Using the lagged level of physical capital as an instrument for the contemporaneous level, leads to:

```
xtdcce2 d.log_rgdpo L.log_rgdpo log_hc log_ck log_ngd  (log_ck = L.log_ck), reportc cr(log_rgdpo  log_hc log_ck log_ngd) cr_lags(3) ivreg2options(nocollin noid)
```

Further `ivreg2` options can be passed through using **ivreg2options**. Stored values in **e()** from **ivreg2options** can be posted using the option **fulliv**.


## 7.6 Error Correction Models (ECM/PMG)

Variables of the long run cointegration vector are defined in **lr(_varlist_)**, where the first variable is the error correction speed of adjustment term. To ensure homogeneity of the long run effects, the corresponding variables have to be included in the **pooled(_varlist_)** option. Following the example from Blackburne and Frank (2007) with the _jasa2_ dataset
(the dataset is available at [here](www.econ.cam.ac.uk/people-files/emeritus/mhp1/jasa.exe) from [Pesaran's webpage](www.econ.cam.ac.uk/people/emeritus/mhp1/published-articles#1999):

```
xtdcce2 d.c d.pi d.y if year >= 1962 , lr(L.c pi y) p(L.c pi y) nocross
```

`xtdcce2` internally estimates equation (7) and then recalculates the long run coefficients, such that estimation results for equation (8) are obtained. Equation (7) can be estimated adding **nodivide** to **lr_options()**.

A second option is **xtpmgnames** in order to match the naming convention from `xtpmg`.

```
xtdcce2 d.c d.pi d.y if year >= 1962 , lr(L.c pi y) p(L.c pi y) nocross lr_options(nodivide)
xtdcce2 d.c d.pi d.y if year >= 1962 , lr(L.c pi y) p(L.c pi y) nocross lr_options(xtpmgnames)
```

## 7.7 Cross-Section Augmented Distributed Lag (CS-DL)

Chudik et. al (2013) estimate the long run effects of public debt on output growth (the data is available [here](www.econ.cam.ac.uk/people-files/faculty/km418/CMPR_Data.zip) on [Kamiar Mohaddes' personal webpage](www.econ.cam.ac.uk/people-files/faculty/km418/research.html).

In the dataset, the dependent variable is _d.y_ and the independent variables are the inflation rate (_dp_) and debt to GDP ratio (_d.gd_).

For an ARDL(1,1,1) only the first difference of _dp_ and _d.gd_ are added as further covariates. Only a contemporaneous lag of the cross-sectional averages (i.e. **cr_lags(0)** of the dependent variable and 3 lags of the independent variables are added. The lag structure is implemented by defining a _numlist_ rather than a number in **cr_lags()**. For the example here **cr_lags(0 3 3)** is used, where the first number refers to the first variable defined in **cr()**, the second to the second etc.

To replicate the results in Table 18, the following command line is used:

```
xtdcce2 d.y dp d.gd d.(dp d.gd), cr(d.y dp d.gd) cr_lags(0 3 3) fullsample
```

For an ARDL(1,3,3) model the first and second lag are of the first differences are added by putting **L(0/2)** in front of the **d.(dp d.gd)**:

```
xtdcce2 d.y dp d.gd L(0/2).d.(dp d.gd), cr(d.y dp d.gd) cr_lags(0 3 3) fullsample
```

Note, the **fullsample** option is used to reproduce the results in Chudik et. al (2013).

## 7.8 Cross-Section Augmented ARDL (CS-ARDL)

Chudik et. al (2013) estimate besides the CS-DL model a CS-ARDL model. To estimate this model all variables are treated as long run coefficients and thus added to _varlist_ in **lr(_varlist_)**. `xtdcce2` first estimates the short run coefficients and the calculates then long run coefficients, following Equation 10. The option **lr_options(ardl)** is used to invoke the estimation of the long run coefficients. Variables with the same base (i.e. forming the same long run coefficient) need to be either enclosed in parenthesis or _tsvarlist_ operators need to be used. In Table 17 an ARDL(1,1,1) model is estimated with three lags of the cross-sectional averages:

```
xtdcce2 d.y , lr(L.d.y dp L.dp d.gd L.d.gd) lr_options(ardl) cr(d.y dp d.gd) cr_lags(3) fullsample
```

`xtdcce2` calculates the long run effects identifying the variables by their base. For example it recognizes that _dp_ and _L.dp_ relate to the same variable. If the lag of _dp_ is called _ldp_, then the variables need to be enclosed in parenthesis.

Estimating the same model but as an ARDL(3,3,3) and with enclosed parenthesis reads:

```
xtdcce2 d.y , lr((L(1/3).d.y) (L(0/3).dp) (L(0/3).d.gd) ) lr_options(ardl) cr(d.y dp d.gd) cr_lags(3) fullsample
```

which is equivalent to coding without parenthesis:

```
xtdcce2 d.y , lr(L(1/3).d.y L(0/3).dp L(0/3).d.gd) lr_options(ardl) cr(d.y dp d.gd) cr_lags(3) fullsample
```

## 7.9 Regularized CCE (rCCE)

The regularized CCE approach is only possible for static models.  To estimate a static model of growth on human, physical captial and population growth, we can use:

```
xtdcce2 log_rgdpo log_hc log_ck log_ngd , cr(log_rgdpo log_hc log_ck log_ngd, rcce)
```

xtdcce2 selects the first and second eigenvector of the cross-section averages and adds it as a variable.  The selection criterion is the ER criterion from Ahn and Horenstein (2013).  To use the GR criterion instead, the option criterion(gr) is used:

```
xtdcce2 log_rgdpo log_hc log_ck log_ngd , cr(log_rgdpo log_hc log_ck log_ngd, rcce(criterion(gr)))
```

Three regularized cross-section averages are added. Instead of specifying the criteria to estimate the number of eigenvectors of the rcce approach, we can hard set it using the option npc():

```
xtdcce2 log_rgdpo log_hc log_ck log_ngd , cr(log_rgdpo log_hc log_ck log_ngd, rcce(npc(3)))
```

## 7.10 Bootstrapping

To bootstrap standard errors with a fixed seed:

```
estat bootstrap, seed(123)
```

To run a wild bootstrap and bootstrap confidence intervals, the options wild and percentile are added:

```
estat bootstrap, seed(123) wild percentile
```

# 8. Testing for cross-sectional dependence

The `xtdcce2` package includes `xtcd2` which tests for weak cross-sectional dependence. The syntax is:

```
xtcd2 [varlist] [if] [,peasaran cdw pea cdstar rho 
pca(integer) reps(integer) seed(integer)
kdensity name(string) heatplot[(absolute options_heatplot)] 
contour[(absolute options_contour) noadjust] ]
```

`varlist` is the name of residuals or variables to be tested for weak cross sectional dependence. `varlist` may contain time-series operators, see tsvarlist.  `varlist` is optional if the command is performed after     an estimation (postestimation).

# 8.1 Description

xtcd2 tests residuals or a variables for weak cross sectional dependence in a panel data model.  It implements the tests by Pesaran (2015, 2021), the weighted CD test (CDw) by Juodis & Reese (2021) including the power enhancement (Fang et. al., 2015).  It also implements the CD* from Pesaran & Xie (2021). As a default all four test statistics are calculated and presented next to each other.  p-values are displayed in parenthesis.

Cross sectional dependence in the error term occurs if dependence between cross sectional units in a regression is not accounted for.  The dependence between units violates the basic OLS assumption of an independent and identically distributed error term.  In the worst case cross sectional dependence in the error term can lead to omitted variable bias or endogeneity and therefore to inconsistent estimates. Cross sectional dependence can be measured as the correlation between units.  For example the correlation of the errors of unit i and unit j can be calculated.  Obviously, if the correlation is large, cross sectional dependence is present.

### The CD Test (Pesaran 2015)

Pesaran (2015) develops a test for weak cross sectional dependence based on this principle.  Weak cross sectional dependence means that the correlation between units at each point in time converges to zero as the number of cross section goes to infinity.  Under strong dependence the correlation converges to a constant.  The null hypothesis of the test is, that the error term (or variable) is weakly cross sectional dependent. This means that correlation between an observation of unit i in time t and unit j in time t is zero.  The hypothesis is:

```
H0: errors are weakly cross sectional dependent.
```

Pesaran (2015) derives a test statistic, which sums the correlation coefficients of the different units.  The test statistic for a balanced panel is:

```
CD = [2*T / (N*(N-1))]^(1/2) * sum(i=1,N-1) sum(j=i+1,N) rho(ij),
```

and for an unbalanced panel (see Chudik, Pesaran, 2015):

```
CD = [2 / (N*(N-1))]^(1/2) * sum(i=1,N-1) sum(j=i+1,N) [T(ij)^(1/2) * rho(ij)],
```

where rho(ij) is the correlation coefficient of unit i and j and T(ij) the number of common observations between i and j.  Under the null hypothesis the statistic is asymptotically 

```
CD ~ N(0,1)
```
distributed.


### Weighted CD test (Juodis and Reese, 2021)

xtcd2 further implements three alternatives to test for weak cross-sectional dependence.  It includes the weighted CD (CDw) test proposed by Juodis and Reese (2021).  Juodis and Reese (2021) show that the CD test diverges if the time dimension grows and the test is applied to residuals after a CCE or FE regression. The CDw test weights each observation by cross-section specific Rademacher weights.  The pair wise correlations are calculated as:

```
rho(ij) = sum(t=1,T) w(i)eps(i,t)eps(j,t)w(j)
```

where w(i) and w(j) are the Rademacher weights which take on the values 1 or -1 with equal probability.  To reduce the dependence on the random Rademacher weights, the draw can be repeated using the reps() option.
 

### Power Enhanced CD test (Juodis and Reese, 2021 and Fan et. al., 2015)

A second alternative proposed by Juodis and Reese (2021) is the Power Enhancement Approach (PEA) by Fan et. al. (2015).  The power of the CD test is improved by calculating the CD test as:

```
CD = [2*T / (N*(N-1))]^(1/2) * sum(i=1,N-1) sum(j=i+1,N) rho(ij) + sum(i=2,N)sum(j=1,N-1}|rho(ij)|*(|rho(ij)>2 log(N)^(1/2)T^(-1)
```

Fan et. al. (2015) show that the PEA works if the number of cross-sectional units (N) is very large.  Therefore it is advisable to use the PEA method only for such datasets.
 

### CD Star (Pesaran & Xie, 2021)

As forth test xtcd2 implements the bias corrected CD* test from Pesaran & Xie (2021).  The bias corrected test statistic is based on the following:

```
CD* = (CD + (T/2*Theta)^(1/2))/(1-Theta)
```

where Theta is the bias correction and a function of the estimated factor loadings.  The factor loadings are estimated using the first p principal components as factors.  Option pca() specifies the number of principal components. Default is 4.  In case of unbalanced panels an Expected Maximisation algorithm taken from xtnumfac is used.

xtcd2 calculates the CD test statistic for given variables, or if run after an estimation command which supports predict and e(sample).  In the latter case xtcd2 calculates the error term using predict, residuals and then applies the CD test from above.  xtcd2 supports balanced as well as unbalanced panels.  Furthermore by specifying the kdensity option, a kernel density plot with the distribution of the cross correlations is drawn.

If xtcd2 is used after xtreg, then the residuals are calculated using predict, e rather than predict, res.  That is the residuals including the fixed- or random-error component, see  xtreg postestimation.  In all other cases predict, residuals is used to calculate the residuals. xtcd2 can draw heatplots and contour plots of the cross-correlations.  To draw heatplots Ben Jann's heatplot is required.  Contour plots are drawn using Stata's twoway contour.

# 8.2 Options

Options | Description
--- | ---
**pesaran** |  calculates the original CD test by Pesaran (2015), see Description of Pesaran (2015).
**cdw** | calculates the weighted CD test following Juodis and Reese (2022), see Description of Juodis and Reese (2022).  Results vary if seed not specified.
**pea** | uses the Power Enhancement Approach (PEA) by Fan et. al. (2015), see Description of Fan et. al. (2015).  This method is designed for large panel panel datasets.
**cdstar** | calculates the bias corrected CD test following Pesaran & Xie (2021), see Description of Pesaran & Xie (2021).
**reps(integer)** | number of repetitions for the weighted CD test.  Implies option cdw. Default is 30.
**pca(integer)** | number of Principal Components when using the bias corrected CD test.  Requires option cdstar.  Default is 4.
**rho** | saves the matrix with the cross correlations in r(rho).
**kdensity** | plots a kernel density plot of the cross correlations, see twoway kdensity.  The number of observations, the mean, percentiles, minimum and maximum of the cross correlations are reported.  If name(string) is set, then the histogram is saved and not drawn.
**name(string)** | saves the kdensity.
**heatplot[(absolute options_heatplot)]** | draws a heatplot of the cross-correlations.  options_heatplot are options to be passed to heatplot.  absolute uses the absolute values of the cross-correlations.
**contour[(absolute options_contour)]** | draws a contour plot of the cross-correlations.  options_contour are options to be passed to twoway contour.  absolute uses the absolute values of the cross-correlations.
**noadjust** | do not remove cross-section specific means.  This was the default in versions prior 2.3.
**seed(integer)** | sets the seed for the weighted CD test.

# 8.3 Examples
An example dataset of the Penn World Tables 8 is available for download [here](https://github.com/JanDitzen/xtdcce2/raw/master/xtdcce2_sample_dataset.dta).  The dataset contains yearly observations from 1960 until 2007 and is already tsset.  Estimating a simple panel version of the Solow model and run the CD test afterwards:

```
reg d.log_rgdpo log_hc log_ck log_ngd
xtcd2
```

Predicting the error terms after reg, leads to the same result:

```
reg d.log_rgdpo log_hc log_ck log_ngd
predict res, residuals
xtcd2 res
```

The test statistic is 36.34 and the p-value is 0, therefore rejecting the null hypothesis of weak cross sectional dependence.

To draw a density plot with the cross correlations the kdensity option is used:

```
xtcd2 res, kdensity
```

The CD test statistic is known to diverge if many periodic specific parameters are used (Juodis, Reese, 2021).  Unit specific rademacher weights can be applied to prevent this behaviour by using the option cdw:

```
xtcd2 res, cdw
```

To reduce the dependence of the weighted CD test statistic, the test can be repeatedly performed with different weights using the reps() option:

```
xtcd2 res, cdw reps(20)
```

To improve the power of the weighted CD test, the Power Enhancement Approach can be applied by using the pea option:

```
xtcd2 res, pea
```

Testing the variable log_rgdpo for cross sectional dependence reads:

```
xtcd2 log_rgdpo, noestimation
```

# 9. Estimating the exponent of cross-sectional dependence

The `xtdcce2` package includes `xtcse2` which estimates the exponent of cross-sectional dependence. The syntax is:

```
xtcse2 [varlist] [if] [, pca(integer) standardize nocenter nocd 
RESsidual Reps(integer) size(real) tuning(real) lags(integer) ]
```

Data has to be xtset before using xtcse2; see tsset.  varlist may contain time-series operators, see tsvarlist.  If varlist if left empty, xtcse2 predicts residuals from the last estimation command, see predict.  xtcse2 uses an expectation–maximization (EM) algorithm to impute missing values if the panel is unbalanced.

# 9.1 Description

xtcse2 estimates the exponent of cross-sectional dependence in a panel with a large number of observations over time (T) and cross-sectional units (N).  The estimation method follows Bailey, Kapetanios, Pesaran (2016,2019) (henceforth BKP). A variable or a residual is cross-sectional dependent if it inhibits an across cross-sectional units common factor.

xtcse2 estimates the strength of the factor, for a residual or one or more variables.  It outputs a standard error and confidence interval in the usual estimation output fashion, however it does not show a t or z statistic and p-value. Generally speaking strong cross-sectional dependence occurs if alpha is above 0.5.  Testing this is done by a separate test of weak cross-sectional dependence.  Therefore a confidence interval is more informative when estimating alpha.

xtcse2 is intend to support the decision whether to include cross-sectional averages when using xtdcce2 and accompanies xtcd2 in testing for weak cross-sectional dependence. As a default it uses xtcd2 to test for weak cross-sectional dependence.  For a discussion of xtdcce2 and xtcd2 see Ditzen (2018,2019).

In case of unbalanced panels an Expected Maximisation algorithm taken from xtnumfac is used. xtcd2 imputes values independently from xtcse2 and therefore results can differ.

### Econometric Model

For the following assume a general factor model with m factors:

```
x(i,t) = sum(j=1,m) b(j,i) f(j,t) + u(i,t)
i = 1,...,N and t = 1,...,T
```

where x(i,t) depends on unobserved m common factors f(j,t) with loading b(j,i) and a cross sectionally independent error term u(i,t).  The time dimension (T) and the number of cross-sectional units (N) increases to infinity; (N,T) -> infinity.

Chudik et al (2011) specify the factors as weak or strong using a constant 0<=alpha<=1 such that:

```
lim N^(-alpha) sum(j=1,m) abs(b(j,i)) = K < infinity.
```

The type of dependence of the factors and thus the series then depends on the characteristics of b(j,i):

alpha | dependence
--- | ---
alpha = 0 | weak
0 < alpha < 0.5 | semi weak
0.5 <= alpha < 1 | semi strong
alpha = 1 | strong

Weak cross-sectional dependence can be thought of as the following: Even if the number of cross-sectional units increases to infinity, the sum of the effect of the common factors on the dependent variable remain constant. In the case of strong cross-sectional dependence, the sum of the effect of the common factors becomes stronger with an increase in the number of cross-sectional units.

In an estimation ignoring (semi-) strong dependence in the dependent or independent variables can cause an omitted variable bias and therefore lead to inconsistent estimates.  Pesaran (2015) proposes a test to test for weak cross-sectional dependence, see xtcd2.  Pesaran (2006) and Chudik, Peasaran (2015) develop a method to estimate models with cross-sectional dependence by adding time averages of the dependent and independent variables (cross-sectional averages).  This estimator is implemented in Stata by xtdcce2.

xtcse2 estimates alpha in the equation above.  An alpha above 0.5 implies strong cross-sectional dependence and the appropriate when using a variable is required.

### Exponent Estimation (alpha)

Bailey, Kapetanios and Pesaran (2016) [BKP] propose a method for the estimation of the exponent. This section summarizes their approach, a careful reading of the assumptions and theorems is strongly encouraged.

BKP derive a bias-adjusted estimator for alpha in a panel with N_g cross-sectional units (see Eq. 13):

```
alpha = 1 + 1/2 ln(sigma_x^2)/ln(N_g) - 1/2 ln(mu^2)/ln(N_g) - 1/2 cn / [N_g * ln(N_g) * sigma_x^2]  
```

where sigma_x^2 is the variance of the cross-sectional averages.  mu^2 is average variance of significant regression coefficients of x(i,t) on standardized cross-sectional averages with a pre specified size of the test.  cn is the variance of scaled errors from a regression of the x(i,t) on its first K(PC) principle components.  The number of principle components can be set using the option pca(integer). The default is to use the first 4 principle components.

xtcse2 outputs a standard error for alpha and a confidence interval in the usual Stata estimation fashion.  A t- or z-test statistic with p-value is however omitted, because the test is done by the test for weak cross-sectional dependence (CD-test), see xtcd2.  xtcse2 automatically calculates the CD-test statistic and posts its results.  For the estimation of alpha a confidence interval is therefore more informative.

The calculation of the standard error of alpha follows the equation B47, Section VI of the online appendix of BKP, available here:

```
sigma(alpha) = [1/T V(q) + 4/N^(alpha) S]^(1/2) * 1/2 * 1/ln(N)
```

V(q) is the regression standard error over the square of the sum of q coefficients of an AR(q) process of the square of the deviation of standardized cross-sectional averages. q is the third root of T. S is the squared sum divided by N^(alpha-1) of OLS coefficients of x(it) on standardized cross-sectional averages sorted according to their absolute value.

In the case of estimating the exponent of cross-sectional dependence in residuals Bailey, Kapetanios and Pesaran (2019) propose to use pair-wise correlations to estimate the exponent. For the calculation, only significant correlations are taken into account.  The exponent is estimated according to (Eq 25 in BKP 2019):

```
alpha = ln(tau' delta tau) / [2 ln(N)]
```

where tau is a Nx1 vector of ones and delta is a matrix which contains the significant pair-wise correlations.  For the significance, the size of the test and a tuning parameter need to be set a priori.  xtcse2 uses a size of 10% and a tuning parameter of 0.5 as a default.  Both can be changed with the options size() and tuning().

In the case of a panel with weakly exogenous regressors, the pair-wise correlations are based on recursive residuals, see BKP 2019, section 5.2.  xtcse2 allows for this if the option lags() is used.

BKP 2019 do not derive a closed form solution for standard errors.  Therefore standard errors and confidence intervals are calculated using a simple bootstrap, where the cross-sectional units are replaced with replacement. This approach is outlined in BKP 2019 section 5.3.

# 9.2 Options

Options | Description
--- | ---
**pca(integer)** | sets the number of principle components for the calculation of cn. Default is to use the first 4 components.
**standardize** | standardizes variables.
**nocenter** | do not center variables.
**nocd** | suppresses test for cross-sectional dependence using xtcd2.
**size(real)** | size of the test. Default is 10% (0.1).
**ressidual** | estimates the exponent of cross-sectional depdendence in residuals, following BKP 2019.
**tuning(real)** | tuning parameter for estimation of the exponent in residuals.  Default is 0.5.
**reps(integer)** | number of repetitions for bootstrap for calculation of standard error and confidence interval for exponent in residuals. Default is 0.
**lags(integer)** | number of lags (or training period) for calculation of recursive residuals when estimating the exponent after a regression with weakly exogenous regressors.

# 9.3 Examples

An example dataset of the Penn World Tables 8 is available for download [here](https://github.com/JanDitzen/xtdcce2/raw/master/xtdcce2_sample_dataset.dta).  The dataset contains yearly observations from 1960 until 2007 and is already tsset.  To estimate a growth equation the following variables are used:  log_rgdpo (real GDP), log_hc (human capital), log_ck (physical capital) and log_ngd (population growth + break even investments of 5%).

Before running the growth regression the exponent of the cross-sectional dependence for the variables is estimated:

```
xtcse2 d.log_rgdpo L.log_rgdpo log_hc log_ck log_ngd.
```

All variables are highly cross-sectional dependent with alphas close or even above 1. Therefore an estimation method taking cross-sectional dependence is required.  xtdcce2 is uses such an estimation method by adding cross-sectional averages to the model. After running xtdcce2 it is possible to use xtcse2 to estimate the strength of the exponent of the residual using the option residuals.

```
xtdcce2 log_rgdpo L.log_rgdpo log_ck log_ngd log_hc , cr(log_rgdpo log_ck log_ngd log_hc) .
xtcse2, res
```

xtcse2 automatically predicts the residuals using predict (predict after xtdcce2).  The CD statistic is still in a rejection region, therefore the residuals exhibit strong cross-sectional dependence.

The estimated model above is mis-specified as it is a dynamic model, but no lags of the cross-sectional averages are added. The number of lags should be in the region of T^(1/3), so with 47 periods 3 lags are added. Then xtcse2 is used to estimate alpha again, this time the CD test is omitted:

```
xtdcce2 log_rgdpo L.log_rgdpo log_ck log_ngd log_hc , cr(log_rgdpo log_ck log_ngd log_hc) cr_lags(3) .
xtcse2 ,nocd residual lags(3) reps(200)
```

The value of the CD test statistic is 1.32 and in a non-rejection region.  The estimate of alpha is considerably small the confidence interval does not overlap with 0.5

As a second exercise the first row of Table 1. in BKP is reproduced.  The data is available on Pesaran's [webpage](https://www.econ.cam.ac.uk/people/emeritus/mhp1/published-articles#2016) and for download [here](www.econ.cam.ac.uk/people-files/emeritus/mhp1/fp15/BKP_GAUSS_procedures.zip) .

After the data is loaded, reshaped (it comes in a matrix) and renamed as variable gdp, the option standardize is used to standardize the variable as done in BKP:

```
xtcse2 gdp , standardize.
```


# 10. References

Ahn, S. C., & Horenstein, A. R. 2013. 
Eigenvalue ratio test for the number of factors. 
Econometrica, 81(3), 1203–1227.


Bailey, N., G. Kapetanios and M. H. Pesaran. 2016.
Exponent of cross-sectional dependence: estimation and inference.
Journal of Applied Econometrics 31: 929-960.

Bailey, N., G. Kapetanios and M. H. Pesaran. 2019.
Exponent of Cross-sectional Dependence for Residuals.
Sankhya B. The Indian Journal of Statistics: 81(4) p. 46-102.

Baum, C. F., M. E. Schaffer, and S. Stillman 2007.
Enhanced routines for instrumental variables/generalized method of moments estimation and testing.
Stata Journal 7(4): 465-506

Blackburne, E. F., and M. W. Frank. 2007.
Estimation of nonstationary heterogeneous panels.
Stata Journal 7(2): 197-208.

Chudik, A., M. H. Pesaran and E. Tosetti. 2011. 
Weak and strong cross-section dependence and estimation of large panels.
The Econometrics Journal 14(1):C45–C90.

Chudik, A., K. Mohaddes, M. H. Pesaran, and M. Raissi. 2013.
Debt, Inflation and Growth: Robust Estimation of Long-Run Effects in Dynamic Panel Data Model.

Chudik, A., and M. H. Pesaran. 2015. Common correlated effects estimation of heterogeneous dynamic panel data models with weakly exogenous regressors.
Journal of Econometrics 188(2): 393-420.

Chudik, A., K. Mohaddes, M. H. Pesaran, and M. Raissi. 2016.
Long-Run Effects in Large Heterogeneous Panel Data Models with Cross-Sectionally Correlated Errors
Essays in Honor of Aman Ullah. 85-135.

Ditzen, J. 2018. Estimating Dynamic Common Correlated Effcts in Stata. The Stata Journal, 18:3, 585 - 617.

Ditzen, J. 2021. Estimating long run effects and the exponent of cross-sectional dependence: an update to xtdcce2. The Stata Journal 21:3.

Ditzen, J. 2021. Panel-data models with large N and large T: An overview. Economics Virtual Symposium 2021 2, Stata Users Group. [Slides](https://ideas.repec.org/p/boc/econ21/2.html).

Eberhardt, M. 2012.
Estimating panel time series models with heterogeneous slopes.
Stata Journal 12(1): 61-71.

Holly, S., Pesaran, M. H., Yamagata, T. 2010.
A spatio-temporal model of house prices in the USA. 
Journal of Econometrics 158: 160 - 172.

Fan, J., Y. Liao & J. Yao. 2015. Power Enhancement in High-Dimensional Cross-Sectional Tests. Econometrica(83): 1497–1541.

Feenstra, R. C., R. Inklaar, and M. Timmer. 2015.
The Next Generation of the Penn World Table. American Economic Review. www.ggdc.net/pwt

Goncalves, S., & Perron, B. 2014. 
Bootstrapping factor-augmented regression models. 
Journal of Econometrics, 182(1), 156–173.

Jann, B. 2005.
moremata: Stata module (Mata) to provide various functions.
Available from http://ideas.repec.org/c/boc/bocode/s455001.html.

Juodis, A. 2022. 
A regularization approach to common correlated effects estimation.
Journal of Applied Econometrics, 37(4), 788– 810.

Juodis, A., & Reese, S. 2022. The Incidental Parameters Problem in Testing for Remaining Cross-section Correlation. Journal of Business Economics and Statistics. 40(3).

Karabıyık, H., Reese, S., & Westerlund, J. 2017. 
On the role of the rank condition in cce estimation of factor-augmented panel regressions.
Journal of Econometrics, 197(1), 60–64.

Pesaran, M. 2006.
Estimation and inference in large heterogeneous panels with a multifactor error structure.
Econometrica 74(4): 967-1012.

Pesaran, M. H. 2021. 
General diagnostic tests for cross-sectional dependence in panels.
Empirical Economics 60: 13-50.

Pesaran, M. H., and R. Smith. 1995.
Econometrics Estimating long-run relationships from dynamic heterogeneous panels.
Journal of Econometrics 68: 79-113.

Pesaran, M. H. & Xie, Y. 2021. 
A Bias-Corrected CD Test for Error Cross-
Sectional Dependence in Panel Data Models
with Latent Factors.
Cambridge Working Papers in Economics 2158.

Roodman, D., Nielsen, M. Ø., MacKinnon, J. G., & Webb, M. D. 2019. 
Fast and wild: Bootstrap inference in Stata using boottest. 
The Stata Journal, 19(1), 4–60.

Shin, Y., M. H. Pesaran, and R. P. Smith. 1999.
Pooled Mean Group Estimation of Dynamic Heterogeneous Panels.
Journal of the American Statistical Association 94(446): 621-634.

Westerlund, J., Perova, Y., Norkute, M. 2019. CCE in fixed-T panels. 
Journal of Applied Econometrics: 1-6.

# 11. About

### Author
Jan Ditzen (Free University of Bolzano-Bozen)

Email: [jan.ditzen@unibz.it](mailto:jan.ditzen@unibz.it)

Web: [www.jan.ditzen.net](http://www.jan.ditzen.net)

### Acknowledgments

I am grateful to Achim Ahrens, Arnab Bhattacharjee, David M. Drukker, Markus Eberhardt, Tullio Gregori, Sebastian Kripfganz, Erich Gundlach, Sean Holly, Kyle McNabb and Mark Schaffer, to the participants of the
2016 and 2018 Stata Users Group meeting in London and Zuerich, and two anonymous referees of The Stata Journal for many valuable comments and suggestions. All remaining errors are my own.

The routine to check for  positive definite or singular matrices was provided by Mark Schaffer, Heriot-Watt University, Edinburgh, UK.

`xtdcce2` was formally called `xtdcce`.

### Citation

Please cite as follows:

Ditzen, J. 2018. xtdcce2: Estimating dynamic common correlated effects in Stata. The Stata Journal, 18:3, 585 - 617.

or 

Ditzen, J. 2021. Estimating long run effects and the exponent of cross-sectional dependence: an update to xtdcce2. The Stata Journal 21:3.

# 12. Installation
The latest versions can be obtained via
```
net install xtdcce2 , from("https://janditzen.github.io/xtdcce2/")
```
or including beta versions
```
net from https://janditzen.github.io/xtdcce2/
```

and a full history of xtdcce2, pre version 1.34 from

```
net from http://www.ditzen.net/Stata/xtdcce2_beta
```

`xtdcce2` is available on SSC as well:

```
ssc install xtdcce2
```

# 13. Change log
Version 4.0 - February 2023
- bootstrap support
- added option mgmissing
- added option rcce
- added option fast2
- fixed error when pooled and ardl used.

Version 3.0 - August 2021
- several small bug fixes
- improved support for factor variables
- fix for mm_which2
- message for large panels
- error in calculation for variances of cross-sectional unit specific coefficients
- fix predict program: partial now only in-sample and bug fixed when xb2 and reportc was used (thanks to Tullio Gregori for the pointers).
- added global and local cross-sectional averages
- improved output
- xtdcce2fast
- speed improvements

Version 1.35 to Version 2.0
- Bug fix in calculation of minimal T dimension, added option nodimcheck.
- Speed improvements (thanks to Achim Ahrens for the suggestions).
- Bug fixes for jackknife (thanks to Collin Rabe for the pointer).
- Bug fix in predict and if (thanks for Deniey A. Purwanto and Tullio Gregoi for the pointers).
- Bug fix if binary variable used and constant partialled out.
- Bug fixed in calculation of R2, added adjusted R2 for pooled and MG regressions.
- Newey West and Westerlund, Petrova, Norkute standard errors for pooled regressions.
- invsym for rank deficient matrices.
- Added xtcse2 support.

Version 1.33 to Version 1.34
- small bug fixes in code and help file.

Version 1.32 to Version 1.33
- bug in if statements fixed.
- noomitted added, bug in cr(_all_) fixed.
- added option "replace" and "cfresiduals" to predict.
- CS-DL and CS-ARDL method added.
- Output as in Stata Journal Version.

Version 1.31 to Version 1.32
- bug number of groups fixed
- predict, residual produced different results within xtdcce2 and after if panel unbalanced or trend used (thanks to Tullio Gregori for the pointer).
- check for rank condition.
- several bugs fixed.

Version 1.2 to Version 1.31
- code for regression in Mata
- corrected standard errors for pooled coefficients, option cluster not necessary any longer. Please rerun estimations if used option pooled()
- Fixed errors in unbalanced panel
- option post_full removed, individual estimates are posted in e(bi) and e(Vi)
- added option ivslow.
- legacy control for endogenous_var(), exogenous_var() and residuals().
