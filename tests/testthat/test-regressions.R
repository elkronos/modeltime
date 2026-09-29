context("TEST regressions: engine, print and control fixes")

# SETUP ----

m750 <- timetk::m4_monthly %>% dplyr::filter(id == "M750")

splits <- rsample::initial_time_split(m750, prop = 0.8)


# SMOOTH ES ----

test_that("smooth_es uses external regressors at predict time", {

    skip_on_cran()

    set.seed(123)
    train_tbl <- rsample::training(splits) %>%
        dplyr::mutate(x = stats::rnorm(dplyr::n()))
    test_tbl  <- rsample::testing(splits) %>%
        dplyr::mutate(x = stats::rnorm(dplyr::n()))

    model_fit <- exp_smoothing() %>%
        parsnip::set_engine("smooth_es") %>%
        parsnip::fit(value ~ date + x, data = train_tbl)

    expect_false(is.null(model_fit$fit$extras$xreg_recipe))

    pred_1 <- predict(model_fit, new_data = test_tbl)$.pred
    pred_2 <- predict(model_fit, new_data = dplyr::mutate(test_tbl, x = x + 100))$.pred

    # If the regressor were ignored, both forecasts would be identical
    expect_false(isTRUE(all.equal(pred_1, pred_2)))

})


# ARIMA BOOST ----

test_that("arima_xgboost respects a user supplied lambda", {

    skip_on_cran()

    model_fit <- arima_boost(
        seasonal_period          = 12,
        non_seasonal_ar          = 1,
        non_seasonal_differences = 1,
        non_seasonal_ma          = 1,
        seasonal_ar              = 0,
        seasonal_differences     = 1,
        seasonal_ma              = 1
    ) %>%
        parsnip::set_engine("arima_xgboost", lambda = 0) %>%
        parsnip::fit(value ~ date, data = rsample::training(splits))

    expect_equal(as.numeric(model_fit$fit$models$model_1$lambda), 0)

})

test_that("arima_boost print shows the xgboost model call", {

    skip_on_cran()

    model_fit <- arima_boost(
        seasonal_period          = 12,
        non_seasonal_ar          = 1,
        non_seasonal_differences = 1,
        non_seasonal_ma          = 1,
        seasonal_ar              = 0,
        seasonal_differences     = 1,
        seasonal_ma              = 1,
        trees                    = 10
    ) %>%
        parsnip::set_engine("arima_xgboost") %>%
        parsnip::fit(value ~ date + as.numeric(date), data = rsample::training(splits))

    out <- utils::capture.output(print(model_fit$fit))

    expect_true(any(grepl("XGBoost Errors", out)))
    expect_false(any(out == "NULL"))

})


# CONTROL ----

test_that("control_nested_forecast has its own print method", {

    expect_output(print(control_nested_forecast()), "nested forecast control object")
    expect_output(print(control_nested_refit()), "nested refit control object")

})
