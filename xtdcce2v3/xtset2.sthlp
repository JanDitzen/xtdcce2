{smcl}
{hline}
{hi:help xtset2}{right: v. 1.1 - 03. October 2020}
{hline}
{title:Title}

{p 4 4}{cmd:xtset2} - advanced routines for {cmd: xtset} and {cmd: _xt}.{p_end}

{title:Syntax}

{p 4 13}{cmd:xtset2} [{it:panelvar} {it:timevar}]  [{it:if}]
[{cmd:,}
{cmd:checkvars({varlist})}
{cmd:matrix}
{cmd:showxtset}
{cmd:version}
{cmd:tsoptions}]
{p_end}

{p 4 4}{it:panelvar} is the panel identifier (cross-section) and {it:timevar} the time identifier.
{cmd:xtset2} without the panel and time identifier displays the current setting.
{cmd:tsoptions} are options passed on to {help xtset}.{p_end}


{title:Contents}

{p 4}{help xtset2##description:Description}{p_end}
{p 4}{help xtset2##options:Options}{p_end}
{p 4}{help xtset2##about:About}{p_end}
{p 4}{help xtset2##about:Saved Values}{p_end}


{marker description}{title:Description}
{p 4 4} {cmd:xtset2} extends {help xtset} and {help tsset}. 
{cmd:xtset2} checks if a panel is balanced with respect to the time, panel identifier and if 
defined with resepect to non missings in a {it:variablelist}.
{cmd: xtset2} was developed for {help xtdcce2}.
{p_end}

{marker options}{title:Options}

{p 4 4}{cmd:checkvars({varlist})} defines the variables the sample is restricted to. 
If a variable in {it:varlist} has a {help missing} value, it is removed from the sample.{p_end}

{p 4 4}{cmd:matrix} saves a matrix with Panelid, T, Tmin, Tmax, Tbar and missings.{p_end}

{p 4 4}{cmd:showxtset} display output of {help xtset}.{p_end}

{p 4 4}{cmd:version} returns version number.{p_end}

{p 4 4}{cmd:tsoptions}passes further options to {help xtset}.{p_end}

{marker return}{title:Saved Values}

{cmd:xtdcce2} stores the following in {cmd:r()}:

{col 4} Scalars
{col 8}{cmd: r(N)}{col 27} number of observations
{col 8}{cmd: r(Tmax)}{col 27} maximum of time periods
{col 8}{cmd: r(Tmean)}{col 27} mean of time periods
{col 8}{cmd: r(Tmin)}{col 27} minimum of time periods
{col 8}{cmd: r(NumGaps)}{col 27} number of gaps
{col 8}{cmd: r(NumMissing)}{col 27} number of missings
{col 8}{cmd: r(balancedN)}{col 27} number of balanced cross-sections
{col 8}{cmd: r(N_g)}{col 27} number of groups

{col 4} Macros
{col 8}{cmd: r(timevar)}{col 27} name of time identifer
{col 8}{cmd: r(panelvar)}{col 27} name of panel identifer
{col 8}{cmd: r(balanced)}{col 27} balanced

{col 4} Matrices
{col 8}{cmd: r(PanelMatrix)}{col 27} table with details for each panel

{marker about}{title:Author}

{p 4}Jan Ditzen (Free University of Bozen-Bolzano){p_end}
{p 4}Email: {browse "mailto:jan.ditzen@unibz.it":jan.ditzen@unibz.it}{p_end}
{p 4}Web: {browse "www.jan.ditzen.net":www.jan.ditzen.net}{p_end}

{title:Also see}
{p 4 4}See also: {help xtset}, {help _xt}, {help tsset}{p_end}
