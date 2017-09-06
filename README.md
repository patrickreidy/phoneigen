
# phoneigen

[![Build Status](https://travis-ci.org/patrickreidy/phoneigen.svg?branch=master)](https://travis-ci.org/patrickreidy/phoneigen)
[![Coverage Status](https://coveralls.io/repos/github/patrickreidy/phoneigen/badge.svg?branch=master&bust=1)](https://coveralls.io/github/patrickreidy/phoneigen?branch=master)

Functions for applying Laplacian Eigenmaps to acoustic data for
the purpose of phonetic analysis. Includes data sets of adult speakers'
productions of American English /s, S/ and of productions of /t, k/.
Vignettes apply Laplacian Eigenmaps to high-dimensional spectral
representations so as to learn low-dimensional representations for the
/s/-vs-/S/ and the /t/-vs-/k/ contrasts, respectively.
Accompanies the manuscript: A. R. Plummer and P. F. Reidy (submitted).
"Computing low-dimensional representations of speech from socio-auditory
structures for phonetic analyses". _Journal of Phonetics_.


## Installation

`phoneigen` may be installed directly from GitHub:

```r
devtools::install_github("patrickreidy/phoneigen")
```


## Data sets

The vignettes apply Laplacian Eigenmaps to spectral representations computed
from adults' productions of American English /s, S/ and /t, k/. For these
demonstrations, the vignettes make use of two package-internal data sets,
`sibilantFricatives` and `stopBursts`, which are `tibble`s that each comprise
a list-column named `ExcitationPattern`, whose elements are 361-component 
numeric vectors that represent the values of a psychoacoustic spectra computed
from the consonant productions. These data sets are available upon package 
installation, and can be accessed by calling `phoneigen::SibilantFricatives()`
and `phoneigen::StopBursts()`.

The audio recordings of the consonant productions are available from the source
package on GitHub. Each production was extracted, from a longer recording, with
a rectangular window that began 20 ms prior to the onset of sibilant frication
or the stop burst, and that ended 60 ms after the onset of voicing for the
vowel following the consonant. Each extracted production was exported to WAV
format (44.1 kHz sampling rate, 16 bit resolution) and named according to the
following template `{Participant}_{Session}_{Trial}_{Orthography}.WAV`. Because 
the total size of these WAV files is close to 45 MB, they are ignored when `R`
builds the package from source; hence, they will not be available in the 
installed version of the package. To access the WAV files, you must either
download them from GitHub or clone the repository for the source package.



