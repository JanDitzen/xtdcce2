# xtdcce2

## Estimating heterogeneous coefficient models using common correlated effects in a dynamic panel with a large number of observations over groups and time periods.

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
5. [Saved Values](#5-saved-values)
6. [Postestimation Commands](#6-postestimation-commands)
7. [Examples](#7-examples)
	1. [Mean Group](#71-mean-group-estimation)
	2. [Common Correlated Effects](#72-common-correlated-effects)
	3. [Dynamic Common Correlated Effects](#73-dynamic-common-correlated-effects)
	4. [Pooled Estimation](#74-pooled-estimations)
	5. [Instrumental Variables](#75-instrumental-variables)
	6. [Error Correction Model (ECM/PMG)](#76-error-correction-models-ecmpmg)
	7. [Cross-Section Augmented Distributed Lag (CS-DL)](#78-cross-section-augmented-ardl-cs-ardl)
	8. [Cross-Section Augmented ARDL(CS-ARDL)](#77-cross-section-augmented-distributed-lag-cs-dl)
8. [References](#8-references)
9. [About](#9-about)

# 1. Syntax

```
xtdcce2 _depvar_ [_indepvars_] [_varlist2_ = _varlist_iv_] [ifin] , crosssectional(_varlist_) [pooled(_varlist_) cr_lags(_string_) NOCRosssectional ivreg2options(_string_) e_ivreg2_ ivslow noisily lr(_varlist_) lr_options(_string_) pooledconstant reportconstant noconstant trend pooledtrend jackknife recursive nocd showindividual fullsample fast blockdiaguse nodimcheck NOOMITted]
```

where _varlist2_ are endogenous variables and _varlist_iv_ the instruments. Data has to be `xtset` before using `xtdcce2`; see `tssst`.
_varlists_ may contain time-series operators, see `tsvarlist`, or factor variables, see `fvvarlist`.
`xtdcce2` requires the  [moremata](https://ideas.repec.org/c/boc/bocode/s455001.html) package.

# 2. Description

`xtdcce2` estimates a heterogeneous coefficient model in a large panel with dependence between cross sectional units. A panel is large if the number of cross-sectional units (or groups) and the number of time periods are going to infinity.

It fits the following estimation methods:

i) The Mean Group Estimator (MG, Pesaran and Smith 1995).

ii) The Common Correlated Effects Estimator (CCE, Pesaran 2006),

iii) The Dynamic Common Correlated Effects Estimator (DCCE, Chudik and Pesaran 2015), and

For a dynamic model, several methods to estimate long run effects are possible:

a) The Pooled Mean Group Estimator (PMG, Shin et. al 1999) based on an Error Correction Model,

b) The Cross-Sectional Augmented Distributed Lag (CS-DL, Chudik et. al 2016) estimator which directly estimates the long run coefficients
from a dynamic equation, and

c) The Cross-Sectional ARDL (CS-ARDL, Chudik et. al 2016) estimator using an ARDL model.
For a further discussion see Ditzen (2018b).

Additionally `xtdcce2` tests for cross sectional dependence (see `xtcd2`) and supports instrumental variable estimations (see [ivreg2](http://www.stata-journal.com/software/sj5-4/)).


# 3. Options

Option | Description
--- | ---
**crosssectional(_varlist_)** | defines the variables which are added as cross sectional averages to the equation. Variables in **crosssectional()** may be included in **pooled()**, **exogenous_vars()**, **endogenous_vars()** and **lr()**. Variables in **crosssectional()** are partialled out, the coefficients not estimated and reported. **crosssectional(_all_)** adds all variables as cross sectional averages. No cross sectional averages are added if **crosssectional(_none_)** is used, which is equivalent to **nocrosssectional**. **crosssectional()** is a required option but can be substituted by **nocrosssectional**.
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
**showindividual** | reports unit individual estimates in output.
**fullsample** | uses entire sample available for calculation of cross sectional averages. Any observations which are lost due to lags will be included calculating the cross sectional averages (but are not included in the estimation itself).
**fast** | omit calculation of unit specific standard errors.
**blockdiaguse** | uses **mata blockdiag** rather than an alternative algorithm. **mata blockdiag** is slower, but might produce more stable results.
**nodimcheck** | Does not check for dimension. Before estimating a model, `xtdcce2` automatically checks if the time dimension within each panel is long enough to run a mean group regression. Panel units with an insufficient number are automatically dropped.
**noomitted** | no omitted variable checks.

`xtdcce2` supports IV regressions using `ivreg2`. The IV specific options are:

Option | Description
--- | ---
`ivreg2options(_string_)` | passes further options to `ivreg2`, see `ivreg2, options`
`e_ivreg2` | posts all available results from `ivreg2` in **e()** with prefix _ivreg2_, see ivreg2, macros.
`noisily` | displays output of `ivreg2`.
`ivslow` | For the calculation of standard errors for pooled coefficients an auxiliary regressions is performed. In case of an IV regression, xtdcce2 runs a simple IV regression for the auxiliary regressions. This is faster. If option is used **ivslow**, then xtdcce2 calls ivreg2 for the auxiliary regression.	This is advisable as soon as ivreg2 specific options are used.

`xtdcce2` is able to estimate long run coefficients.
Three models are supported:

The pooled mean group models (Shin et. al 1999), similar to [xtpmg](http://www.stata-journal.com/software/sj7-2/) (see [xtdcce2, ecm](#46-error-correction-modelsecmpmg)), the CS-DL (see [xtdcce2, CSDL](#4.7-cross-sectional-augmented-distributed-lag-cs-dl)) and CS-ARDL method (see [xtdcce2, ardl](#4.8-cross-section-augmented-ardl-cs-ardl)) as developed in Chudik et. al 2016. No options for the CS-DL model are necessary.

Options | Description
--- | ---
`lr(_varlist_)` | specifies the variables to be included in the long-run cointegration vector. The first variable(s) is/are the error-correction speed of adjustment term.
	The default is to use the pmg model. In this case each estimated coefficient is divided by the negative of the long-run cointegration vector (the first variable).
	If the option **ardl** is used, then the long run coefficients are estimated as the sum over the coefficients relating to a variable, 	divided by the sum of the coefficients of the dependent variable.
	`lr_options(_string)`| options for the long run coefficients. Options are:
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

The name of the combined graph is saved in **r(graph_name)**.


# 7 Examples

An example dataset of the Penn World Tables 8 is available for download [here](https://www.dropbox.com/s/0087vh8brhid5ws/xtdcce2_sample_dataset.dta?dl=0). The dataset contains yearly observations from 1960 until 2007 and is already tsset.

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
xtdcce2 d.log_rgdpo log_hc log_ck log_ngd , reportc cr(d.log_rgdpo log_hc log_ck log_ngd)
```

The default for the number of cross sectional lags is zero, implying only contemporaneous cross sectional averages are used. Finally the number of lags can be specified as well using the **cr_lags** option.

```
xtdcce2 d.log_rgdpo log_hc log_ck log_ngd , reportc cr(d.log_rgdpo log_hc log_ck log_ngd) cr_lags(0)
```

All three command lines are equivalent and lead to the same estimation results.

## 7.3 Dynamic Common Correlated Effects

The lagged dependent variable is added to the model again. To estimate the mean group coefficients consistently, the number of lags is set to 3:

```
xtdcce2 d.log_rgdpo L.log_rgdpo log_hc log_ck log_ngd , reportc cr(d.log_rgdpo L.log_rgdpo  log_hc log_ck log_ngd) cr_lags(3)
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
xtdcce2 d.log_rgdpo L.log_rgdpo log_hc log_ck log_ngd , reportc cr(d.log_rgdpo L.log_rgdpo  log_hc log_ck log_ngd) pooled(L.log_rgdpo  log_hc log_ck log_ngd) cr_lags(3) pooledconstant
```

## 7.5 Instrumental Variables

Endogenous variables can be instrumented by using options **endogenous_vars(_varlist_)** and **exogenous_vars(_varlist_)**. Internally `ivreg2` estimates the individual coefficients. Using the lagged level of physical capital as an instrument for the contemporaneous level, leads to:

```
xtdcce2 d.log_rgdpo L.log_rgdpo log_hc log_ck log_ngd  (log_ck = L.log_ck), reportc cr(d.log_rgdpo L.log_rgdpo  log_hc log_ck log_ngd) cr_lags(3) ivreg2options(nocollin noid)
```

Further `ivreg2` options can be passed through using **ivreg2options**. Stored values in **e()** from **ivreg2options** can be posted using the option **fulliv**.


## 7.6 Error Correction Models (ECM/PMG)

Variables of the long run cointegration vector are defined in **lr(_varlist_)**, where the first variable is the error correction speed of adjustment term. To ensure homogeneity of the long run effects, the corresponding variables have to be included in the **pooled(_varlist_)** option. Following the example from Blackburne and Frank (2007) with the _jasa2_ dataset
(the dataset is available at [here](www.econ.cam.ac.uk/people-files/emeritus/mhp1/jasa.exe) from [Pesaran's webpage](www.econ.cam.ac.uk/people/emeritus/mhp1/published-articles#1999):

```
xtdcce2 d.c d.pi d.y if year >= 1962 , lr(L.c pi y) p(L.c pi y) cr(_all) cr_lags(2)}
```

`xtdcce2` internally estimates equation (7) and then recalculates the long run coefficients, such that estimation results for equation (8) are obtained. Equation (7) can be estimated adding **nodivide** to **lr_options()**.

A second option is **xtpmgnames** in order to match the naming convention from `xtpmg`.

```xtdcce2 d.c d.pi d.y if year >= 1962 , lr(L.c pi y) p(L.c pi y) cr(_all) cr_lags(2) lr_options(nodivide)
xtdcce2 d.c d.pi d.y if year >= 1962 , lr(L.c pi y) p(L.c pi y) cr(_all) cr_lags(2) lr_options(xtpmgnames)
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

# 8. References

Baum, C. F., M. E. Schaffer, and S. Stillman 2007.
Enhanced routines for instrumental variables/generalized method of moments estimation and testing.
Stata Journal 7(4): 465-506

Chudik, A., K. Mohaddes, M. H. Pesaran, and M. Raissi. 2013.
Debt, Inflation and Growth: Robust Estimation of Long-Run Effects in Dynamic Panel Data Model.
Chudik, A., and M. H. Pesaran. 2015.

Common correlated effects estimation of heterogeneous dynamic panel data models with weakly exogenous regressors.
Journal of Econometrics 188(2): 393-420.

Chudik, A., K. Mohaddes, M. H. Pesaran, and M. Raissi. 2016.
Long-Run Effects in Large Heterogeneous Panel Data Models with Cross-Sectionally Correlated Errors
Essays in Honor of Aman Ullah. 85-135.

Ditzen, J. 2018. Estimating Dynamic Common Correlated Effcts in Stata. The Stata Journal, 18:3, 585 - 617.

Ditzen, J. 2018b. Estimating long run effects in models with cross-sectional dependence using xtdcce2.

Blackburne, E. F., and M. W. Frank. 2007.
Estimation of nonstationary heterogeneous panels.
Stata Journal 7(2): 197-208.

Eberhardt, M. 2012.
Estimating panel time series models with heterogeneous slopes.
Stata Journal 12(1): 61-71.

Feenstra, R. C., R. Inklaar, and M. Timmer. 2015.
The Next Generation of the Penn World Table. American Economic Review. www.ggdc.net/pwt

Jann, B. 2005.
moremata: Stata module (Mata) to provide various functions.
Available from http://ideas.repec.org/c/boc/bocode/s455001.html.

Pesaran, M. 2006.
Estimation and inference in large heterogeneous panels with a multifactor error structure.
Econometrica 74(4): 967-1012.

Pesaran, M. H., and R. Smith. 1995.
Econometrics Estimating long-run relationships from dynamic heterogeneous panels.
Journal of Econometrics 68: 79-113.

Shin, Y., M. H. Pesaran, and R. P. Smith. 1999.
Pooled Mean Group Estimation of Dynamic Heterogeneous Panels.
Journal of the American Statistical Association 94(446): 621-634.

# 9. About

### Author
Jan Ditzen (Heriot-Watt University)

Email: [j.ditzen@hw.ac.uk](mailto:j.ditzen@hw.ac.uk)

Web: [www.jan.ditzen.net](http://www.jan.ditzen.net)

### Acknowledgments

I am grateful to Achim Ahrens, Arnab Bhattacharjee, David M. Drukker, Markus Eberhardt, Tullio Gregori, Erich Gundlach and Mark Schaffer, to the participants of the
2016 and 2018 Stata Users Group meeting in London and Zuerich, and two anonymous referees of The Stata Journal for many valuable comments and suggestions.

The routine to check for  positive definite or singular matrices was provided by Mark Schaffer, Heriot-Watt University, Edinburgh, UK.

`xtdcce2` was formally called `xtdcce`.

### Citation

Please cite as follows:

Ditzen, J. 2018. xtdcce2: Estimating dynamic common correlated effects in Stata. The Stata Journal, 18:3, 585 - 617.

The latest versions can be obtained via
```
net from https://github.com/JanDitzen/xtdcce2
```

and beta versions including a full history of
xtdcce2 from
```
net from http://www.ditzen.net/Stata/xtdcce2_beta
```

### Changelog
This version: 1.35 - 31. January 2019
 - Bug fix in calculation of minimal T dimension, added option nodimcheck.
 - Speed improvements (thanks to Achim Ahrens for the suggestions).
 - Bug fix when if statements used and jackknife (thanks to Collin Rabe for the pointer).

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
