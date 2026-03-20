# =============================================================================
# R - Modelado ARIMA para Forecasting
# Autor: Isaac Esteban Haro Torres
# =============================================================================

library(forecast)

# -----------------------------------------------------------------------------
# 1. AUTO ARIMA
# -----------------------------------------------------------------------------

fit_auto_arima <- function(ts_data, seasonal = TRUE, max_p = 5, max_q = 5, 
                            max_d = 2, max_P = 2, max_Q = 2) {
  
  cat("\n🔍 Ejecutando Auto ARIMA...\n")
  
  # Configurar búsqueda
  cat("   Búsqueda de parámetros:\n")
  cat("   - p (AR): 0-", max_p, "\n")
  cat("   - d (differencing): 0-", max_d, "\n")
  cat("   - q (MA): 0-", max_q, "\n")
  if (seasonal) {
    cat("   - P (seasonal AR): 0-", max_P, "\n")
    cat("   - Q (seasonal MA): 0-", max_Q, "\n")
  }
  
  # Auto ARIMA
  set.seed(42)
  arima_model <- auto.arima(
    ts_data,
    seasonal = seasonal,
    max.p = max_p,
    max.q = max_q,
    max.d = max_d,
    max.P = max_P,
    max.Q = max_Q,
    max.D = 1,
    stepwise = FALSE,  # Búsqueda completa
    approximation = FALSE,
    trace = FALSE,
    information.criterion = "aicc",  # AICc para muestras pequeñas
    allowdrift = TRUE
  )
  
  # Mostrar mejor modelo
  cat("\n   ✅ Mejor modelo encontrado:", 
      paste0("ARIMA(", arima_model$model$arma[c(1, 6, 2)], 
             if (seasonal) paste0(")(", arima_model$model$arma[c(3, 7, 4)], ")[", 
                                   arima_model$model$arma[5], "]") 
             else ")", "\n")
  
  cat("   AICc:", round(arima_model$aicc, 4), "\n")
  cat("   BIC:", round(arima_model$bic, 4), "\n")
  cat("   AIC:", round(arima_model$aic, 4), "\n")
  
  return(arima_model)
}

# -----------------------------------------------------------------------------
# 2. ENTRENAMIENTO DE ARIMA MANUAL
# -----------------------------------------------------------------------------

fit_arima_manual <- function(ts_data, order = c(1, 1, 1), 
                              seasonal_order = c(1, 1, 1)) {
  
  cat("\n🔧 Entrenando ARIMA manual...\n")
  cat("   Orden:", paste(order, collapse = ", "), "\n")
  cat("   Orden estacional:", paste(seasonal_order, collapse = ", "), "\n")
  
  # Entrenar modelo
  arima_model <- Arima(
    ts_data,
    order = order,
    seasonal = seasonal_order,
    include.drift = TRUE
  )
  
  cat("   AICc:", round(arima_model$aicc, 4), "\n")
  
  return(arima_model)
}

# -----------------------------------------------------------------------------
# 3. FORECAST CON ARIMA
# -----------------------------------------------------------------------------

forecast_arima <- function(model, h = 30, level = c(80, 95)) {
  
  cat("\n📈 Generando forecast con ARIMA...\n")
  cat("   Horizonte:", h, "días\n")
  cat("   Niveles de confianza:", paste(level, collapse = "%, "), "%\n")
  
  # Generar forecast
  forecast_result <- forecast(model, h = h, level = level)
  
  # Resumen
  cat("\n   Resumen del forecast:\n")
  cat("   - Media del forecast:", round(mean(forecast_result$mean), 2), "\n")
  cat("   - Límite inferior (95%):", round(min(forecast_result$lower[, 2]), 2), "\n")
  cat("   - Límite superior (95%):", round(max(forecast_result$upper[, 2]), 2), "\n")
  
  return(forecast_result)
}

# -----------------------------------------------------------------------------
# 4. EVALUACIÓN DEL MODELO
# -----------------------------------------------------------------------------

evaluate_forecast <- function(forecast_result, actual) {
  
  cat("\n📊 Evaluando forecast...\n")
  
  # Asegurar que las longitudes coincidan
  n <- min(length(forecast_result$mean), length(actual))
  predictions <- forecast_result$mean[1:n]
  actual <- actual[1:n]
  
  # Calcular métricas
  errors <- actual - predictions
  
  mae <- mean(abs(errors))
  rmse <- sqrt(mean(errors^2))
  mape <- 100 * mean(abs(errors / actual))
  
  # MASE (Mean Absolute Scaled Error)
  # Comparar con naive forecast (y[t] = y[t-1])
  naive_errors <- abs(diff(actual))
  mase <- mae / mean(naive_errors)
  
  cat("   MAE:", round(mae, 4), "\n")
  cat("   RMSE:", round(rmse, 4), "\n")
  cat("   MAPE:", round(mape, 2), "%\n")
  cat("   MASE:", round(mase, 4), "\n")
  
  if (mase < 1) {
    cat("   ✅ Mejor que Naive forecast!\n")
  } else {
    cat("   ⚠️  Peor que Naive forecast\n")
  }
  
  return(list(
    mae = mae,
    rmse = rmse,
    mape = mape,
    mase = mase,
    errors = errors,
    predictions = predictions,
    actual = actual
  ))
}

# -----------------------------------------------------------------------------
# 5. VALIDACIÓN CRUZADA TEMPORAL
# -----------------------------------------------------------------------------

time_series_cv <- function(ts_data, h = 30, n_folds = 5) {
  
  cat("\n🔄 Validación cruzada temporal (", n_folds, "-folds)...\n", sep = "")
  
  n <- length(ts_data)
  fold_size <- floor(n / (n_folds + 1))
  
  cv_results <- data.frame()
  
  for (i in 1:n_folds) {
    train_end <- fold_size * i
    test_start <- train_end + 1
    test_end <- min(train_end + h, n)
    
    train_data <- ts(ts_data[1:train_end], frequency = frequency(ts_data))
    test_data <- ts_data[test_start:test_end]
    
    # Auto ARIMA en fold
    tryCatch({
      model <- auto.arima(train_data, seasonal = TRUE, 
                          stepwise = TRUE, approximation = TRUE)
      fc <- forecast(model, h = length(test_data))
      
      # Evaluar
      errors <- test_data - fc$mean
      mae <- mean(abs(errors))
      rmse <- sqrt(mean(errors^2))
      mape <- 100 * mean(abs(errors / test_data))
      
      cv_results <- rbind(cv_results, data.frame(
        fold = i,
        train_size = train_end,
        test_size = length(test_data),
        mae = mae,
        rmse = rmse,
        mape = mape,
        stringsAsFactors = FALSE
      ))
      
      cat("   Fold", i, "- MAE:", round(mae, 2), "\n")
      
    }, error = function(e) {
      cat("   Fold", i, "- Error:", e$message, "\n")
    })
  }
  
  if (nrow(cv_results) > 0) {
    cat("\n   Promedio CV:\n")
    cat("   - MAE:", round(mean(cv_results$mae), 2), "\n")
    cat("   - RMSE:", round(mean(cv_results$rmse), 2), "\n")
    cat("   - MAPE:", round(mean(cv_results$mape), 2), "%\n")
  }
  
  return(cv_results)
}

# -----------------------------------------------------------------------------
# 6. FUNCIÓN PRINCIPAL DE MODELADO ARIMA
# -----------------------------------------------------------------------------

run_arima_modeling <- function(ts_data, h = 30, output_dir = "outputs") {
  
  cat("\n🤖 MODELADO ARIMA\n")
  cat("===============\n\n")
  
  # 1. Auto ARIMA
  arima_model <- fit_auto_arima(ts_data, seasonal = TRUE)
  
  # 2. Análisis de residuos
  residual_analysis <- analyze_residuals(arima_model, ts_data)
  
  # 3. Forecast
  forecast_result <- forecast_arima(arima_model, h = h)
  
  # 4. Validación cruzada
  cv_results <- time_series_cv(ts_data, h = h, n_folds = 5)
  
  # Guardar modelo
  if (!dir.exists("models")) {
    dir.create("models")
  }
  
  saveRDS(arima_model, "models/arima_model.rds")
  
  # Guardar forecast
  forecast_df <- data.frame(
    date = seq(Sys.Date(), by = "day", length.out = h),
    forecast = round(as.numeric(forecast_result$mean), 2),
    lower_80 = round(as.numeric(forecast_result$lower[, 1]), 2),
    upper_80 = round(as.numeric(forecast_result$upper[, 1]), 2),
    lower_95 = round(as.numeric(forecast_result$lower[, 2]), 2),
    upper_95 = round(as.numeric(forecast_result$upper[, 2]), 2),
    stringsAsFactors = FALSE
  )
  
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  write.csv(forecast_df, file.path(output_dir, "arima_forecast.csv"), row.names = FALSE)
  
  # Guardar métricas de CV
  if (nrow(cv_results) > 0) {
    write.csv(cv_results, file.path(output_dir, "arima_cv_results.csv"), 
              row.names = FALSE)
  }
  
  cat("\n✅ Modelado ARIMA completado!\n")
  
  return(list(
    model = arima_model,
    forecast = forecast_result,
    residual_analysis = residual_analysis,
    cv_results = cv_results
  ))
}

# Función auxiliar para análisis de residuos (si no está cargada)
analyze_residuals <- function(model, ts_data) {
  residuals <- residuals(model)
  
  if (length(residuals) <= 5000) {
    shapiro_test <- shapiro.test(residuals)
    is_normal <- shapiro_test$p.value > 0.05
  } else {
    is_normal <- FALSE
    shapiro_test <- NULL
  }
  
  ljung_box <- Box.test(residuals, lag = 20, type = "Ljung-Box")
  no_autocorr <- ljung_box$p.value > 0.05
  
  return(list(
    is_normal = is_normal,
    no_autocorrelation = no_autocorr,
    shapiro_p = if (!is.null(shapiro_test)) shapiro_test$p.value else NA,
    ljung_box_p = ljung_box$p.value
  ))
}
