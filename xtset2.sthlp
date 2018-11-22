{smcl}
{hline}
{hi:help xtdcce2}{right: v. 1.0 - 16. January 2018}
{hline}
{title:Title}

{p 4 4}{cmd:xtset2} - advanced routines for {cmd: xtset} and {cmd: _xt}.{p_end}

{title:Syntax}

{p 4 13}{cmd:xtcd2} [{it:panelvar} {it:timevar}]  [{it:if}]
[{cmd:,}
{cmd:checkvars({varlist})}
{cmd:matrix}
{cmd:showxtset}
{cmd:version}
{cmd:tsoptions}]
{p_end}

{p 4 4}{it:panelvar} is the panel identifier (cross-section) and {it:timevar} the time identifier.
{cmd:xtset2} without the panel and time identifier displays the current setting.{p_end}


{title:Contents}

{p 4}{help xtset2##description:Description}{p_end}
{p 4}{help xtset2##about:About}{p_end}


{marker description}{title:Description}
{p 4 4} {cmd:xtset2} extends {help xtset} and help {tsset}. 
It is possible to define a balanced panel with respect to variables, similar to the {it:touse} option of {cmd:_xt}.
{p_end}

{marker about}{title:Author}

{p 4}Jan Ditzen (Heriot-Watt University){p_end}
{p 4}Email: {browse "mailto:j.ditzen@hw.ac.uk":j.ditzen@hw.ac.uk}{p_end}
{p 4}Web: {browse "www.jan.ditzen.net":www.jan.ditzen.net}{p_end}

{title:Also see}
{p 4 4}See also: {help xtset}, {help _xt}, {help tsset}{p_end}
