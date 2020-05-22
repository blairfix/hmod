# hmod

`hmod` is an R package containing tools for modeling and analyzing hierarchies.  I use the package mainly for testing and visualizing models written in C++. 

For a description of these models, see: 

*  *Personal Income and Hierarchical Power*. Journal of Economic Issues. 2019; 53(4): 928-945. [SocArXiv preprint](https://osf.io/preprints/socarxiv/pb475/).

* *[Energy, hierarchy and the origin of inequality](https://journals.plos.org/plosone/article?id=10.1371/journal.pone.0215692)*. PLOS ONE. 2019; 14(4):e0215692.

* *Hierarchy and the Power-Law Income Distribution Tail*. Journal of Computational Social Science. 2018; 1(2):471–91. [SocArXiv preprint](https://osf.io/u95dk/)

* Fix, B. (2020). Economic Development and the Death of the Free Market. SocArXiv. https://doi.org/10.31235/osf.io/g86am

### Functions

Many of the functions in `hmod` have their own repositories with associated README files. Check them out:

* [boot_mean](https://github.com/blairfix/boot_mean): calculates the confidence interval of the mean using the bootstrap method

* [fit_power_law](https://github.com/blairfix/fit_power_law): fits a power-law to the tail of a distribution of income

* [gini](https://github.com/blairfix/gini): calculates the Gini index of raw data

* [grc](https://github.com/blairfix/grc): calculates the global reaching centrality of a network

* [grid](https://github.com/blairfix/grid): a function for plotting a size distribution as a grid of squares

* [hierarchy](https://github.com/blairfix/hierarchy): generates a hierarchy with a fixed span of control

* [hierarchical_power](https://github.com/blairfix/hierarchical_power): calculates the hierarchical power of individuals in a hierarchy

* [hp_mod](https://github.com/blairfix/hp_mod): simulates the distribution of hierarchical power in a population of firms

* [lorenz](https://github.com/blairfix/lorenz): calculates the Lorenz curve for a distribution of income

* [manage_frac](https://github.com/blairfix/manage_frac): models the management fraction of employment in a society

* [project](https://github.com/blairfix/project): projects 3D data onto a 2D plane

* [rpld](https://github.com/blairfix/rpld): generates a discrete power-law distribution

* [string_replace](https://github.com/blairfix/string_replace): a fully-vectorized function for finding and replacing strings

* [top_frac](https://github.com/blairfix/top_frac): calculates the income share of the top n% of incomes, given raw data

* [weighted_mean](https://github.com/blairfix/weighted_mean): calculates the weighted mean of a vector of numbers, given another vector of weights

* [world_bank](https://github.com/blairfix/world_bank): downloads and manipulates World Bank data



### Installation

To install and use the functions in `hmod`, first download this repository. To build the `hmod` package, you'll need R [devtools](https://cran.r-project.org/web/packages/devtools/index.html) installed. Once you've got that, install the following packages:

* [BH](https://cran.r-project.org/web/packages/BH/index.html)
* [data.table](https://cran.r-project.org/web/packages/data.table/index.html)
* [Rcpp](https://cran.r-project.org/web/packages/Rcpp/index.html)
* [RcppArmadillo](https://cran.r-project.org/web/packages/RcppArmadillo/index.html)
* [RcppZiggurat](https://cran.r-project.org/web/packages/RcppZiggurat/index.html)


Then  open the file `hmod.Rproj` in Rstudio. In the R terminal, enter this command:

```R
 Sys.setenv("PKG_CXXFLAGS"="-std=c++11 -O3 -march=native")
```

Next, the top right panel, click the "Build" tab. Clock "Install and Restart". If everything works, you'll then be able to load `hmod` like any other library:

```R
library(hmod)
```

If you have problem installing, or if you find a bug, please let me know.





