# fabletools (development version)

## Improvements

* Fixed handling of transformed distributions which accept a parameter from the
  dataset.
* `.` in a model formula for `xreg` implemented with `special_xreg()` will now
  include all measured variables (excluding the index and key variables).
* Improved handling of transformations with forecast sample distributions.
* Added support for reconciling sample paths.
* `accuracy(<fbl_ts>)` can now summarise accuracy over key variables. This is
  done by specifying the accuracy `by` argument and not including some (or all)
  of the fable's key variables (#341).
* Like `forecast()`, `generate()` will now keep exogenous regressors in the
  output table.

# fabletools 0.3.2

## New features

* Added `hypothesize()` generic for running statistical tests on a trained model.
* Added `combination_weighted()` function for producing a combination model with
  arbitrary weights.

## Improvements

* The fallback residuals() method now handles transformations when 
  `type = "innovation"`.
* Improved supported expressions for producing combination models. The 
  appropriate response variable is now simplified for all functions that produce
  that original response variable. This notably includes `0.7*mdl1 + 0.3*mdl2` -
  if `mdl1` and `mdl2` are models with the same response variables, then the 
  resulting combination model will also have the same response variable.
* Documentation improvements.

## Bug fixes

* Fixed issue with exogenous regressors (`xreg`) in reconciliation methods that 
  partially forecast the hierarchy.
* Fixed issue with keys being dropped when several `mdl_df` (mable) objects were
  combined.

# fabletools 0.3.1

## New features

* Added `outliers()` generic for identifying the outliers of a fitted model.
* Added `special_xreg()` special generator, for producing a model matrix of
  exogenous regressors. It supports an argument for controlling the default 
  inclusion of an intercept.
* Migrated `common_xregs` helper from fable to fabletools for providing a
  common and consistent interface for common time series exogenous regressors.
* Added experimental support for passing the tsibble index to `features()`
  functions if the `.index` argument is used in the function.

## Improvements

* Added transformation support for fallback `fitted(h > 1)` method (#302).
* Documentation improvements.

# fabletools 0.3.0

## New features

* Added `scenarios()` function for providing multiple scenarios to the 
  `new_data` argument. This allows different sets of future exogenous regressors
  to be provided to functions like `forecast()`, `generate()`, and 
  `interpolate()` (#110).
* Added `quantile_score()`, which is similar to `percentile_score()` except it
  allows a set of quantile `probs` to be provided (#280).
* Added distribution support for `autoplot(<dable>)`. If the decomposition 
  provides distributions for its components, then the uncertainty of the 
  components will be plotted with interval ribbons.
* Added block bootstrap option for bootstrapping innovations in `generate()`.
* Added multiple step ahead fitted values support via `fitted(<mable>, h > 1)`.
* Added `as_fable(<forecast>)` for converting older `forecast` class objects to
  `fable` data structures.
* Added `top_down(method = "forecast_proportion")` for reconciliation using the
  forecast proportions techniques.
* Added `middle_out()` forecast reconciliation method.
* Added directional accuracy measures, including `MDA()`, `MDV()` and `MDPV()`
  (#273, @davidtedfordholt).
* Added `fill_gaps(<fable>)`.

## Improvements

* The `pinball_loss()` and `percentile_score()` accuracy measures are now scaled
  up by 2x for improved meaning. The loss at 50% equals absolute error and the
  average loss equals CRPS (#280).
* Automatic transformation functions formals are now named after the response
  variable and not converted to `.x`, preventing conflicts with values named `.x`.
* `box_cox()` and `inv_box_cox()` are now vectorised over the transformation
  parameter `lambda`.
* `RMSSE()` accuracy measure is now included in default `accuracy()` measures.
* Specifying a different `response` variable in `as_fable()` will no longer
  error, it now sets the provided `response` value as the distribution's new 
  response.
* Minor vctrs support improvements.

## Bug fixes

* Data lines in fable `autoplot()` are now always grouped by the data's key.
* Fixed `bottom_up()` aggregation mismatch for redundant leaf nodes (#266).
* Fixed `min_trace()` reconciliation for degenerate hierarchies (#267).
* Fixed `select(<mable>)` not keeping required key variables (#297).
* Fixed `...` not being passed through in `report()`.

# fabletools 0.2.1

## New features

* Added `bottom_up()` forecast reconciliation method.
* Added the `skill_score()` accuracy measure modifier.
* Added `agg_vec()` for manually producing aggregation vectors.

## Improvements

* Fixed some inconsistencies in key ordering of model accessors (such as 
  `augment()`, `tidy()` and `glance()`) with model methods (such as `forecast()`
  and `generate()`).
* Improved equality comparison of `agg_vec` classes, aggregated values will now
  always match regardless of the value used.
* Using `summarise()` with a fable will now retain the fable class if the 
  distribution still exists under the same variable name.
* Added `as_fable.forecast()` to convert forecast objects from the forecast
  package to work with fable.
* Improved `CRPS()` performance when using sampling distributions (#240).
* Reconciliation now works with hierarchies containing aggregate leaf nodes,
  allowing unbalanced hierarchies to be reconciled.
* Produce unique names for unnamed features used with `features()` (#258).
* Documentation improvements
* Performance improvements, including using `future.apply()` to parallelize 
  `forecast()` when the `future` package is attached (#268).

## Breaking changes

* The residuals obtained from the `augment()` function are no longer controlled
  by the `type` argument. Response residuals (`y - yhat`) are now always found
  in the `.resid` column, and innovation residuals (the model's error) are now
  found in the `.innov` column. Response residuals will differ from innovation
  residuals when transformations are used, and if the model has non-additive
  residuals.
* `dist_*()` functions are now removed, and are completely replaced by the 
  distributional package. These are removed to prevent masking issues when
  loading packages.
* `fortify(<fable>)` will now return a tibble with the same structure as the 
  fable, which is more useful for plotting forecast distributions with the 
  ggdist package. It can no longer be used to extract intervals from the 
  forecasts, this can be done using `hilo()`, and numerical values from a 
  `<hilo>` can be extracted with `unpack_hilo()` or `interval$lower`.

## Bug fixes

* Fixed issue with aggregated date vectors (#230).
* Fixed display of models in `View()` panel.
* Fixed issue with combination models not inheriting vctrs functionality (#237).
* `aggregate_key()` can now be used with non-syntactic variable names.
* Added tsibble cast methods for fable and dable objects, fixing issues with
  tidyverse functionality between datasets of different column orders (#247).
* Fixed `refit()` dropping reconciliation attributes (#251).

# fabletools 0.2.0

## New features

* Distributions are now provided by the distributional package, which is more
  space efficient and allows calculation of distributional statistics including
  the `mean()`, `median()`, `variance()`, `quantile()`, `cdf()` and `density()`.
* `autoplot.fbl_ts()` and `autolayer.fbl_ts()` now accept the `point_forecast`
  argument, which is a named list of functions that describe the method used to
  obtain the point forecasts. If multiple are specified, each method will be
  identified using the `linetype`.
* Added accuracy measures: `RMSSE()`, `pinball_loss()`, `scaled_pinball_loss()`.
* Added accessor functions for column names (or metadata) of interest. This 
  includes models in a mable (`mable_vars()`), response variables 
  (`response_vars()`) and distribution variables (`distribution_var()`).
* Added support for combinations of non-normal forecasts, which produces mean
  point forecasts only.
* Added support for reconciling non-normal forecasts, which produces reconciled
  point forecasts only.
  
## Improvements

* Improved dplyr support. You can now use `bind_*()` and `*_join()` operations
  on mables, dables, and fables. More verbs are supported by these extension
  data classes, and so behaviour should work closer to what is expected.
* Progress reporting is now handled by the progressr package. This allows you to
  decide if, when, and how progress is reported. To show progress, wrap your 
  code in the `progressr::with_progress()` function. Progress will no longer be
  displayed automatically during lengthy calculations.
* Improved support for streaming data to models with transformed response
  variables.
* `hilo.fbl_ts()` now keeps existing columns of a fable.
* `forecast()` will now return an empty fable instead of erroring when no
  forecasts are requested.
* `is_aggregated()` now works for non-aggregated data types.
* Documentation improvements.

## Breaking changes

* The fable returned by `forecast()` now stores the distribution in the column
  named the response variable (previously, this was the point forecast). Point
  forecasts are now stored in the `.mean` column, which can be customised using
  the `point_forecast` argument.
* The `bias_adjust` option for forecast() is replaced by `point_forecast`,
  allowing you to specify which point forecast measures to display (fable/#226).
  This has been done to reduce confusion around the argument's usage, 
  disambiguate the returned point forecast's meaning, and also allow users 
  to specify which (if any) point forecasts to provide.
* The data coercion functions `as_mable`, `as_dable`, and `as_fable` have been
  changed to accept character vectors for specifying common attributes (such as
  response variables, and distributions).
* The `models` argument for `mable` and `as_mable` has been replaced with `model`
  for consistency with the lack of plural in `key`.
* Intervals from multivariate distributions are now returned as data frames of 
  `hilo` intervals. The columns are the response variables. Similar structures 
  are returned when computing other distributional statistics like the `mean`.
* `hilo` intervals can no longer be unnested as they are now stored more 
  efficiently as a vctrs record type. The `unpack_hilo()` function will continue
  to function as expected, and you can now obtain the components of the interval
  with `x$lower`, `x$upper`, and `x$level`,
* `rbind()` methods are deprecated in favour of `bind_rows()`
* The row order of wide to long mable operations (such as `accuracy()`) has 
  changed (due to shift to `pivot_longer()` from `gather()`). Model column name
  values are now nested within key values, rather than key values nested in
  model name values.

## Bug fixes

* Fixed `show_gap` option not working when more than one forecast is plotted.
* Fixed `autolayer()` plotting issues due to inherited aesthetics.
* `aggregate_key()` no longer drops keys, instead they are kept as <aggregated>.
* Forecast reconciliation now works with historical data that is not temporally
  aligned.
* Fixed `forecast()` producing forecasts via `h` when `new_data` does not 
  include a given series (#202).

# fabletools 0.1.3

## Improvements

* Better support for tidyverse packages using vctrs.
* Performance improvements for reconciliation and parsing.
* `xreg()` can now be called directly as a special.

## Bug fixes

* Fixed `accuracy.fbl_ts()` error when certain names were used in the fable.

# fabletools 0.1.2

## Improvements

* Added MAAPE accuracy measure.
* Added support for exogenous regressors in decomposition models.
* Added support for generating data from combination models.
* Forecast plots via `autoplot.fbl_ts()` and `autolayer.fbl_ts()` now support
  the `show_gap` argument. This can be used to connect the historical observations
  to the forecasts (#113).
  
## Breaking changes

* Decompositions are now treated as models. 
  To access the decomposed values, you will now have to use `components()`.
  For example, `tourism %>% STL(Trips)` is now `tourism %>% model(STL(Trips)) %>% components()`.
  This change allows for more flexible decomposition specifications, and better interfaces for decomposition modelling.

## Bug fixes

* Fixed `select.mdl_df()` usage with negative select values (#120).
* Fixed `features()` for a tsibble with key variables but only one series.
* Fixed interpolated values not being back transformed (tidyverts/fable#202).
* Fixed `stream()` causing issues with subsequent methods (#144).

# fabletools 0.1.1

## Breaking changes

* Updated method names available for `min_trace()` reconciliation (@GeorgeAthana).

## Improvements

* Improved error messaging for failing features.
* Added Continuous Ranked Probability Score (`CRPS()`) accuracy measure.
* Transformations of features are now computed for separately for each key, allowing transformations such as `scale(value)` to be used.
* Added structural scaling method for MinT (`min_trace(method = "wls_struct")`) forecast reconciliation (@GeorgeAthana).
* Performance improvements.
* Documentation improvements.

## Bug fixes

* Added failure condition for disjoint reconciliation graphs.

# fabletools 0.1.0

* First release.

## New features
### Data structures
* Added the mable (model table) data class (`mdl_df`) which is a tibble-like data structure for applying multiple models to a dataset. Each row of the mable refers to a different time series from the data (identified by the key columns). A mable must contain at least one column of time series models (`mdl_ts`), where the list column itself (`lst_mdl`) describes how these models are related.
* Added the fable (forecast table) data class (`fbl_ts`) which is a tsibble-like data structure for representing forecasts. In extension to the key and index from the tsibble (`tbl_ts`) class, a fable (`fbl_ts`) must contain columns of point forecasts for the response variable(s), and a single distribution column (`fcdist`).
* Added the dable (decomposition table) data class (`dcmp_ts`) which is a tsibble-like data structure for representing decompositions. This data class is useful for representing decompositions, as its print method describes how its columns can be combined to produce the original data, and has a more appropriate `autoplot()` method for displaying decompositions. Beyond this, a dable (`dcmp_ts`) behaves very similarly to a tsibble (`tbl_ts`).

### Modelling
* Support for model (`new_model_class()`, `new_model_definition()`) and decomposition definitions (`new_decomposition_class()`, `new_decomposition_definition()`).
* Added parsing tools to compactly specify models using a formula interface. Transformations specified on left hand side, where the response variable is determined by object length. In case of a conflict in object length, such as `GDP/CPI`, the response will be the ratio of the pair. To transform a variable by some other data variable, the response can be specified using `resp()`, giving `resp(GDP)/CPI`. Multiple variables (and separate transformations for each), can be specified using `vars()`: `vars(log(GDP), CPI)`. The inputs to the model are specified on the right hand side, and are handled using model defined specials (`new_specials()`).
* Added methods to train a model definition to a dataset. `model()` is the recommended interface, which can fit many model definitions to each time series in the input dataset returning a mable (`mdl_df`). The lower level interface for model estimation is accessible using `estimate()` which will return a time series model (`mdl_ts`), however using this interface is discouraged.

### Forecasting
* Added `forecast()`, which allows you to produce future predictions of a time series from fitted models. The methods provided in fabletools handle the application of new data (such as the future index or exogenous regressors) to model specials, giving a simple and consistent interface to forecasting any model. The forecast methods will automatically backtransform and bias adjust any transformations specified in the model formula. This function returns a fable (`fbl_ts`) object.
* Added a forecast distribution class (`fcdist`) which is used to describe the distribution of forecasts. Common forecast distributions have been added to the package, including the normal distribution (`dist_normal()`), multivariate normal (`dist_mv_normal()`) and simulated/sampled distributions (`dist_sim()`). In addition to this, `dist_unknown()` is available for methods that don't support distributional forecasts. A new distribution can be added using the `new_fcdist()` function. The forecast distribution class handles transformations on the distribution, and is used to create forecast intervals of the `hilo` class using the `hilo()` function. Mathematical operations on the normal distribution are supported.
* Added tools for working with transformations in models, including automatic back-transformation, transformation classes (`new_transformation()`), and bias adjustment (`bias_adjust()`) methods.
* Added `aggregate_key()`, which is used to compute all levels of aggregation in a specified key structure. It supports nested structures using `parent / key` and crossed structures using `keyA * keyB`.
* Added support for forecast reconciliation using `reconcile()`. This function modifies the way in which forecasts from a model column are combined to give coherent forecasts. In this version the MinT (`min_trace()`) reconciliation technique is available. This is commonly used in combination with `aggregate_key()`.

### Generics
* Added broom package functionality for `augment()`, `tidy()`, and `glance()`.
* Added `components()`, which returns a dable (`dcmp_ts`) that describes how the fitted values of a model were obtained from its components. This is commonly used to visualise the states of a state space model.
* Added `equation()`, which returns a formatted display of a fitted model's equation. This is commonly used to conveniently add model equations to reports, and to better understand the structure of the model.
* Added accessors to common model data elements: fitted values with `fitted()`, model residuals with `residuals()`, and the response variable with `response()`. These functions return a tsibble (`tbl_ts`) object.
* Added `refit()`, which allows an estimated model to be applied to a new dataset.
* Added `report()`, which provides a detailed summary of an estimated model.
* Added `generate()` support, which is used to simulate future paths from an estimated model.
* Added `stream()`, which allows an estimated model to be extended using newly available data.
* Added `interpolate()`, which allows missing values from a dataset to be interpolated using an estimated model (and model appropriate interpolation strategy).
* Added `features()`, along with scoped variants `features_at()`, `features_if()` and `features_all()`. These functions make it easy to compute a large collection of features for each time series in the input dataset.
* Added `feature_set()`, which allows a collection of registered features from loaded packages to be accessed using a tagging system.

### Models
* Added `decomposition_model()`, which allows the components from any decomposition method that returns a dable (`dcmp_ts`) to be modelled separately and have their forecasts combined to give forecasts on the original response variable.
* Added `combination_model()`, which allows any model to be combined with any other. This function accepts a function which describes how the models are combined (such as `combination_ensemble()`). A combination model can also be obtained by using mathematical operations on model definitions or estimated models.
* Added `null_model()`, which can be used as a empty model in a mable (`mdl_df`). This is most commonly used as a substitute for models which encountered an error, preventing the successfully estimated models from being lost.

### Evaluation
* Added `accuracy()`, which allows the accuracy of a model to be evaluated. This function can be used to summarise model performance on the training data (`accuracy.mdl_df()`, `accuracy.mdl_ts()`), or to evaluate the accuracy of forecasts over a test dataset (`accuracy.fbl_ts()`). Several accuracy measures are supported, including `point_accuracy_measures` (`ME`, `MSE`, `RMSE`, `MAE`, `MPE`, `MAPE`, `MASE`, `ACF1`), `interval_accuracy_measures` (`winkler_score`) and `distribution_accuracy_measures` (`percentile_score`). These accuracy functions can be used in conjunction with the rolling functions in the tsibble package (`stretch_tsibble()`, `slide_tsibble()`, `tile_tsibble()`) to computed time series cross-validated accuracy measures.
