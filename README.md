
# phoneigen

[![Build Status](https://travis-ci.org/patrickreidy/phoneigen.svg?branch=master)](https://travis-ci.org/patrickreidy/phoneigen)
[![Coverage Status](https://coveralls.io/repos/github/patrickreidy/phoneigen/badge.svg?branch=master&bust=1)](https://coveralls.io/github/patrickreidy/phoneigen?branch=master)

Functions for applying manifold-and-eigenmapping methods to
high-dimensional speech data to learn low-dimensional representations of
speech for phonetic analysis. Includes a data set that comprises productions 
of the sibilant fricatives /s, S/ by native American English-speaking adults, 
productions by native American English-acquiring two- and three-year-old 
children, and ratings of the children's productions by native American 
English-speaking adult listeners. Includes a vignette on how to construct
a manifold from speech data and then eigenmap it to a low-dimensional
representation on which phonetic analysis can proceed. Accompanies the 
manuscript: A. R. Plummer and P. F. Reidy (submitted). "Computing 
low-dimensional representations of speech from socio-auditory structures for 
phonetic analyses". __Journal of Phonetics__.


## Installation

`phoneigen` may be installed directly from GitHub:

```r
devtools::install_github("patrickreidy/phoneigen", build_vignettes = TRUE)
```



## Vignette

After the package has been installed on your machine, you can access the 
vignette that accompanies the manuscript submitted to __Journal of Phonetics__
by calling `vignette("socio-auditory-manifolds", "phoneigen")`.
