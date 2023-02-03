# PROsetta 0.4.1

* Now requires `TestDesign (>= 1.5.1)`.
* Response probabilities are now computed faster using cpp functions.
* Deprecated `guiPROsetta()` is removed. Use `PROsetta()` instead.
* Updated documentation.
* Made minor updates to the Shiny app in `PROsetta()`.
* Removed `ncat` column in anchor parameters. This is now inferred from the number of parameters.
* Added cpp routine for EAP computation after Lord-Wingersky recursion. This improves the speed of `runRSSS()`.
* `getRSSS()` now nudges the user if the prior mean input looks like a T-score, which should be entered in the theta metric.
* Updated CITATION to use `bibentry()` to meet CRAN requirements.

# PROsetta 0.3.5

* `runRSSS()` output now includes linear approximation betas when the `CPLA` method is used.
* `runLinking()` output now includes the latent mean and variance when the `FIXEDPAR` method is used.
* `runCalibration()`, `runLinking()`, `runEquateObserved()`, `getCompleteData()` gains `verbose` argument for printing status messages. Status messages that were used to be printed in previous versions are now suppressed by default.

# PROsetta 0.3.4

* Removed unused columns (`min_score`, `reverse`, `scores`) in example datasets for clarity. The package functions do not use these columns.
* `loadData()` now warns if there is a variable that may need reverse coding. This is triggered by a negative correlation value.

# PROsetta 0.3.2

* Fixed where `runLinking(method = "FIXEDPAR")` was not working when the anchor instrument ID was not 1 in item map.
* Fixed where `runLinking(method = "FIXEDPAR")` was not working when the anchor and target instruments had different numbers of categories in response data.
* Fixed where `runFrequency()` was not sorting categories correctly when the number of categories was 10 or above.
* Fixed where `runCalibration(fixedpar = TRUE)` was not reading anchor parameters correctly when an integer value existed in anchor parameters.
* Fixed where item parameters for dichotomous items were triggering an error while being parsed.
* For compatibility with R < 4.0, `loadData()` now sanitizes input data when a data frame is supplied.

# PROsetta 0.3.0

## New features
* `runLinking()` now supports `method = 'CPFIXEDDIM'` to perform two-dimensional calibration, for use in performing calibrated projection ([Thissen et al., 2015](https://doi.org/10.1007/978-3-319-19977-1_1)). The difference with `method = 'CP'` is that `'CPFIXEDDIM'` constrains the mean and the variance of the latent anchor dimension, instead of constraining anchor item parameters. For this purpose, a unidimensional fixed parameter calibration using only the anchor response data is performed to obtain the mean and the variance.
* `getRSSS()` for computing a single raw-score to standard-score table is now exposed.

# PROsetta 0.2.1

## QoL updates
* Added `getResponse()` for extracting scale-wise response data from a `PROsetta_data` object.
* Added `getItemNames()` for extracting scale-wise item names from a `PROsetta_data` object.

# PROsetta 0.2.0

## New features
* `runLinking()` now supports `method = 'CP'` to perform two-dimensional calibration, for use in performing calibrated projection ([Thissen et al., 2011](https://doi.org/10.1007/s11136-011-9874-y)).
* `runLinking()` now supports `method = 'CPLA'` to perform two-dimensional calibration, for use in performing linear approximation of calibrated projection ([Thissen et al., 2015](https://doi.org/10.1007/978-3-319-19977-1_1)).
* `runRSSS()` now performs two-dimensional Lord-Wingersky recursion with numerical integration, when the output from `runLinking(method = 'CP')` is supplied.
* `runRSSS()` now performs linear approximation of calibrated projection, when the output from `runLinking(method = 'CPLA')` is supplied.
* Shiny application `PROsetta()` now supports calibrated projection and its linear approximation.

## Bug fixes
* `runEquateObserved(type_to = "theta")` now works.
* `loadData()` now checks for a valid `@scale_id`.

# PROsetta 0.1.4

* First public release.

## Structural changes
* `PROsetta_config` class and `createConfig()` are now deprecated. The functionalities are merged to `PROsetta_data` class and `loadData()`.
* `run*()` functions now require `PROsetta_data` objects instead of `PROsetta_config` objects.
* `runLinking()` now has `method` argument to specify the type of linking to perform. Accepts `MM`, `MS`, `HB`, `SL`, and `FIXEDPAR`.

## Updates
* `runLinking()` is now capable of performing fixed calibration.
* `runCalibration()` now performs free calibration by default.
* `runCalibration()` and `runLinking()` now errors when iteration limit is reached, without returning results.
* `runRSSS()` now returns thetas in addition to T-scores, and also expected scores in each scale.
* `runEquateObserved()` now has `type_to` argument to specify direct raw -> T-score equating or regular raw -> raw equating.
* Functions are now more verbose.
* Added PROMIS Depression - CES-D linking dataset `data_dep`.
* Added `plot()` for drawing raw score distribution histograms.
* Added `plotInfo()` for drawing scale information plots.
* Added several helper functions.
* Made cosmetic improvements on Shiny app.

## Bug fixes
* Fixed where Shiny app was displaying SL method linear transformation constants regardless of specified linking method.

# PROsetta 0.0.4

* Added `scalewise` argument to `runClassical()` and `runCFA()`. When `TRUE`, analysis is performed for each scale.
* `runEquateObserved()` now removes missing values to produce correct raw sums.
* `loadData()` now retains missing values.

# PROsetta 0.0.3

* `loadData()` now removes missing values.
