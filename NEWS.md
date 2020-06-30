# PROsetta 0.1.4

* Initial public release.

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

## Fixes
* Fixed where Shiny app was displaying SL method linear transformation constants regardless of specified linking method.

# PROsetta 0.0.4

* Added `scalewise` argument to `runClassical()` and `runCFA()`. When `TRUE`, analysis is performed for each scale.
* `runEquateObserved()` now removes missing values to produce correct raw sums.
* `loadData()` now retains missing values.

# PROsetta 0.0.3

* `loadData()` now removes missing values.
