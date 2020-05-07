# hmod

`hmod` is an R package containing tools for modeling and analyzing hierarchies.  I use the package mainly for testing and visualizing models written in C++. 

For a description of these models, see: 

*  *Personal Income and Hierarchical Power*. Journal of Economic Issues. 2019; 53(4): 928-945. [SocArXiv preprint](https://osf.io/preprints/socarxiv/pb475/).

* *[Energy, hierarchy and the origin of inequality](https://journals.plos.org/plosone/article?id=10.1371/journal.pone.0215692)*. PLOS ONE. 2019; 14(4):e0215692.

* *Hierarchy and the Power-Law Income Distribution Tail*. Journal of Computational Social Science. 2018; 1(2):471–91. [SocArXiv preprint](https://osf.io/u95dk/)

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
library(`hmod`)
```

If you have problem installing, or if you find a bug, please let me know.





