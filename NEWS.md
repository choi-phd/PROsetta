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
