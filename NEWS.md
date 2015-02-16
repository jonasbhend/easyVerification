## easyVerification 0.1.5.1

* updated documentation for elementary skill functions

## easyVerification 0.1.5

* probability and absolute thresholds for conversion of continuous forecasts to category forecasts can now be supplied to be forecast specific (e.g. different thresholds for different lead times and spatial locations)
* bug fix in `veriApply` with minimal forecast, observation examples

## easyVerification 0.1.4.2

* Fixed bug in missing value treatment with `convert2prob`. This will not affect functions called using `veriApply` as of yet as scores in `veriApply` are only computed for complete forecast and observation pairs

## easyVerification 0.1.4.1

* Bug fix (scaling) of standard error provided in `EnsRocss`

## easyVerification 0.1.4

* Support for dressed metrics from `SpecsVerification`. Only the standard dressing method ("silverman") is supported so far.

* Significance for `EnsRocss`

* `FairSprErr` Fair spread error ratio

* `EnsRocss` allow for arbitrary reference forecasts in ROC area skill score (no significance for reference forecasts with ROC area != 0.5)

## easyVerification 0.1.3

* `Ens2AFC` Added the generalized discrimination score for ensembles.

## easyVerification 0.1.2

* Moved repository to new location on github.com.

* Added vignette documenting the basic functionality of the package.

* Bugfix for ensembles of size 1.

* Removed redundant checks on ensemble size.
