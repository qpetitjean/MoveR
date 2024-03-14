
<!-- README.md is generated from README.Rmd. Please edit that file -->

# MoveR <img src="man/figures/hexsticker.png" height="120" align="right"/>

<!-- badges: start -->

<img alt="GitHub" src="https://img.shields.io/github/license/qpetitjean/MoveR"><br />
[![CRAN
status](https://www.r-pkg.org/badges/version/MoveR)](https://CRAN.R-project.org/package=MoveR)<br />
<img alt="GitHub release (latest by date including pre-releases)" src="https://img.shields.io/github/v/release/qpetitjean/MoveR?include_prereleases">
<img alt="GitHub repo size" src="https://img.shields.io/github/repo-size/qpetitjean/MoveR"><br />
<img alt="GitHub issues" src="https://img.shields.io/github/issues-raw/qpetitjean/MoveR"> 
<img alt="GitHub closed issues" src="https://img.shields.io/github/issues-closed-raw/qpetitjean/MoveR"><br />
<img alt="GitHub last commit (branch)" src="https://img.shields.io/github/last-commit/qpetitjean/MoveR/Main">
<!-- badges: end -->

The `MoveR`
<img src="man/figures/fa-icon-9b00320707d42527dde67262afb33ded.svg"
style="width:1.13em;height:1em" /> package aims to help users analyze
the output of automated video-tracking solutions in a reproducible,
reliable and open framework.

> Edited version of the article published in SoftwareX (Open Access) is
> available here:<br />
> <https://www.softxjournal.com/article/S2352-7110(24)00045-1/fulltext><br />
> Preprint is available
> here:<br /><https://biorxiv.org/cgi/content/short/2023.11.08.566216v1><br />
> Documentation website is available
> here:<br /><https://qpetitjean.github.io/MoveR/>

## Dependencies

In a sack of flexibility, `MoveR` is mainly coded using base
<img src="man/figures/fa-icon-9b00320707d42527dde67262afb33ded.svg"
style="width:1.13em;height:1em" />.<br /> However it still relies on a
few dependencies:<br />

<ul>
<li>
<a href="https://www.rdocumentation.org/packages/graphics">graphics</a>,
<a href="https://www.rdocumentation.org/packages/grDevices">grDevices</a>
and <a href="https://www.rdocumentation.org/packages/hexbin">hexbin</a>
to display graphical elements.
</li>
<br />
<li>
<a href="https://github.com/r-lib/progress">progress</a> to display a
progress bar showing the advancement of the computations.
</li>
<br />
<li>
<a href="https://www.rdocumentation.org/packages/reticulate">reticulate</a>
and
<a href="https://www.rdocumentation.org/packages/R.matlab">R.matlab</a>
to import <a href="https://https://www.python.org/">Python</a> and
<a href="https://mathworks.com/products/matlab.html">Matlab</a>
formatted data.
</li>
<br />
<li>
<a href="https://www.rdocumentation.org/packages/stats">stats</a> and
<a href="https://www.rdocumentation.org/packages/pracma">pracma</a> to
perform basic and advanced calculations.
</li>
<br />
<li>
<a href="https://www.rdocumentation.org/packages/trajr">trajr</a> to
load R functions to compute basic movement descriptors (e.g., speed,
sinuosity).
</li>
<br />
</ul>

## Installation

You can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("remotes")
remotes::install_github("qpetitjean/MoveR")
```

Then you can attach the package `MoveR`:

``` r
library("MoveR")
```

To update with the latest version, first remove the package:

``` r
detach("package:MoveR", unload=TRUE)
remove.packages("MoveR")
```

and repeat the above installation commands.

## Overview

`MoveR` is an
<img src="man/figures/fa-icon-9b00320707d42527dde67262afb33ded.svg"
style="width:1.13em;height:1em" /> package allowing to import,
clean/filter and analyze raw particles/animal movement data obtained
from video-tracking softwares.<br />

### Main workflow steps

More particularly, `MoveR` provides tools to:<br />

<ul>
<li>
<strong>IMPORT</strong> the raw data from various video-tracking
software such as <a href="https://trex.run">TRex</a>,
<a href="https://swarm-lab.github.io/trackR">trackR</a>,
<a href="https://ctrax.sourceforge.net/">Ctrax</a>,
<a href="https://idtrackerai.readthedocs.io/en/latest/">idtracker.ai</a>
and <a href="https://vchiara.eu/index.php/animalta">AnimalTA</a> (for
now) as a tracklets class object (S3).
</li>
<br />
<li>
<strong>VISUALIZE</strong> the tracklets of all or specified particles
over time.
</li>
<br />
<li>
<strong>MANAGE ROIs (regions of interest)</strong> by importing the
location of area edges or specifying it from scratch and assign ROIs
identity to particle’s position over trajectories.
</li>
<br />
<li>
<strong>CLEAN/FILTER</strong> the data according to custom functions
specified by the user. For instance, it can easily remove suspected
tracking errors based on expected particles’ size or speed. It is also
possible to sample particles’ tracklets (i.e., a fragment of particle’s
trajectory) according to a specified time step or split tracklets’ and
remove parts that are detected outside an arena or a given area.
</li>
<br />
<li>
<strong>EVALUATE</strong> the quantity of data removed over the
cleaning/filtering process and check the amount of true and false
detection compared to manual annotations.
</li>
<br />
<li>
<strong>ANALYSE</strong> the data over tracklets, time, or space (e.g.,
across areas). It is possible to compute basics or high (advanced) level
descriptors over each tracklet and further investigate temporal trends
and/or identify spatial patterns (e.g., sequences of area visited). In a
nutshell, Basic and High-level descriptors return the results of a given
computation over each tracklet, while temporal trends functions return
the results of a given calculation over time by averaging the value of
each tracklet. It is also possible to compute a studentized 95%
confidence interval by bootstrapping over the tracklets. Also, it is
possible to identify specified spatial patterns.
</li>
<br />
</ul>

### Breakthroughs

Besides the high flexibility and unified environment, the primary
innovations provided by `MoveR` are the possibility to:<br />

<ul>
<li>
<strong>Characterize behavioral states</strong> (i.e., active
vs. inactive states) using unsupervised learning methods (i.e.,
density-based clustering - see activity2 function).
</li>
<br />
<li>
<strong>Compute the expected diffusion coefficient D</strong>, a proxy
of population dispersal, assuming a correlated random walk model (from
Turchin’s 2015 result; see Dcoef function)
</li>
<br />
<li>
<strong>Identify and extract arbitrary patterns</strong>, in terms of
changes among behavioral states, spatial regions or areas of interest,
patch crosses, or any other patterns, using a very versatile
regular-expression syntax.
</li>
</ul>

<img src="man/figures/WorkFlowMoveR.png" height="600" text-align="center"/>

### Roadmap/future development

<table border="1">
<tr>
<th>
What
</th>
<th>
When
</th>
</tr>
<tr>
<td>
Publish the three missing “How to” - parallel computation, sequences
identification, performance detection.
</td>
<td>
mid-2024
</td>
</tr>
<tr>
<td>
Providing functions to convert “tracklets” objects to other formats used
in other libraries such as “sf”.
</td>
<td>
mid-2024
</td>
</tr>
<tr>
<td>
speed up MoveR’s computation.
</td>
<td>
Continuous implementation
</td>
</tr>
</table>

## Citation

Please cite this package as:

> Petitjean, Q., Lartigue, S., Cointe, M., Ris, N., Calcagno, V., 2024.
> MoveR: An R package for easy processing and analysis of animal
> video-tracking data. SoftwareX 26, 101674.
> <https://doi.org/10.1016/j.softx.2024.101674>

## Code of Conduct

Please note that the `MoveR` project is released with a [Contributor
Code of
Conduct](https://contributor-covenant.org/version/2/0/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.
