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
