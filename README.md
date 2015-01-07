# easyVerification

This package provides functions to simplify application of forecast verification metrics to large datasets of ensemble forecasts. The design goals of `easyVerification` are:

* __Flexibility:__ a variety of data structures are supported
* __Ease of use:__ Absolute forecasts and observations are converted to category and probability forecasts based on the threshold or probability (e.g. terciles) provided, ouputs are reformatted to fit the input
* __Convenience and flexibility over speed:__ R's built-in vectorisation is used where possible but more importantly, new metrics should be easy to implement

The forecast metrics are imported from the `SpecsVerification` package. Additional verification metrics not available through `SpecsVerification` are implemented directly. At the time of publication, the package offers functionality to compute the following deterministic and probabilitistic scores and skill scores:

1. Mean error (`EnsMe`), mean absolute error(`EnsMae`), mean squared error (`EnsMse`), and root mean squared error (`EnsRmse`) of the ensemble mean and their skill scores (e.g. `EnsRmsess`)
2. Correlation with the ensemble mean (`EnsCorr`)
3. Spread to error ratio (`EnsSprErr` and `FairSprErr`)
4. Area under the ROC curve (`EnsRoca`) and its skill score (`EnsRocss`)
5. Fair (`FairRps`) and standard (`EnsRps`) rank probability scores and skill scores (e.g. `FairRpss`)
6. Fair (`FairCrps`) and standard (`EnsCrps`) continuous ranked probability scores and skill scores (e.g. `FairCrpss`)
7. Dressed scores (`DressIgn`, `DressCrps`) and their skill scores (`DressIgnSs`, `DressCrpss`) with default ensemble dressing method ("silverman")
8. The generalized discrimination score for ensembles (`Ens2AFC`)

Additional forecast verification metrics can be added by the user following the examples above.

## Installation 
You can get the latest version using

```{.r}
devtools::install_github("MeteoSwiss/easyVerification", build_vignettes=TRUE)
```

## Getting started

You can find out more about the package and its functionality in the vignette.


```{.r}
vignette('easyVerification')
```

The following example illustrates how to compute the continous ranked probability skill score of an ensemble forecast:


```{.r}
suppressPackageStartupMessages(library(easyVerification))

## check out what is included in easyVerification
ls(pos="package:easyVerification")
#>  [1] "convert2prob" "Ens2AFC"      "EnsCorr"      "EnsError"    
#>  [5] "EnsErrorss"   "EnsMae"       "EnsMaess"     "EnsMe"       
#>  [9] "EnsMess"      "EnsMse"       "EnsMsess"     "EnsRmse"     
#> [13] "EnsRmsess"    "EnsRoca"      "EnsRocss"     "EnsSprErr"   
#> [17] "FairSprErr"   "veriApply"

## set up the forecast and observation data structures
## assumption: we have 100 spatial instances, 15 forecast times and 
## 51 ensemble members
fcst <- array(rnorm(100*15*51), c(100, 15, 51))
obs <- array(rnorm(100*15), c(100, 15))
fo.crpss <- veriApply("EnsCrpss", fcst=fcst, obs=obs)

## if the data were to be organised differently, this has to be indicated
## e.g. ensemble members first, 10x10 spatial domain
fcst <- array(aperm(fcst, c(3,2,1)), c(51, 15, 10, 10))
obs <- array(t(obs), c(15, 10, 10))
fo2.crpss <- veriApply("EnsCrpss", fcst=fcst, obs=obs, 
                       ensdim=1, tdim=2)

## The forecast evaluation metrics are the same, but the 
## data structure is different in the two cases
dim(fo.crpss$crpss)
#> [1] 100
dim(fo2.crpss$crpss)
#> [1] 10 10
range(fo.crpss$crpss - c(fo2.crpss$crpss))
#> [1] 0 0
```

To get additional help and examples please see the vignette `vignette('easyVerification')` or the help pages of the functions in `easyVerification` (e.g. `help(veriApply)`).
