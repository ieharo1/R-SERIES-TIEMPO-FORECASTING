# =============================================================================
# R - Modelado con Prophet para Forecasting
# Autor: Isaac Esteban Haro Torres
# =============================================================================

library(prophet)
library(lubridate)

# -----------------------------------------------------------------------------
# 1. PREPARACIÓN DE DATOS PARA PROPHET
# -----------------------------------------------------------------------------

prepare_data_for_prophet <- function(data, date_col = "date", value_col = "sales") {
  
  cat("\n📊 Preparando datos para Prophet...\n")
  
  # Prophet requiere columnas: ds (date) y y (value)
  prophet_data <- data.frame(
    ds = as.POSIXct(data[[date_col]]),
    y = data[[value_col]]
  )
  
  # Eliminar NAs
  prophet_data <- prophet_data[!is.na(prophet_data$y), ]
  
  cat("   Registros:", nrow(prophet_data), "\n")
  cat("   Rango de fechas:", min(prophet_data$ds), "a", max(prophet_data$ds), "\n")
  cat("   Media:", round(mean(prophet_data$y), 2), "\n")
  
  return(prophet_data)
}

# -----------------------------------------------------------------------------
# 2. ENTRENAMIENTO DE PROPHET
# -----------------------------------------------------------------------------

fit_prophet <- function(prophet_data, daily_seasonality = TRUE, 
                         weekly_seasonality = TRUE, yearly_seasonality = TRUE,
                         changepoint_prior_scale = 0.05,
                         seasonality_prior_scale = 10) {
  
  cat("\n🔮 Entrenando Prophet...\n")
  
  # Configurar modelo
  model <- prophet(
    daily.seasonality = daily_seasonality,
    weekly.seasonality = weekly_seasonality,
    yearly.seasonality = yearly_seasonality,
    changepoint.prior.scale = changepoint_prior_scale,
    seasonality.prior.scale = seasonality_prior_scale,
    interval.width = 0.95  # 95% intervalo de confianza
  )
  
  # Entrenar
  cat("   Entrenando modelo...\n")
  model <- fit_prophet(model, prophet_data)
  
  cat("   ✅ Modelo entrenado exitosamente!\n")
  
  # Información del modelo
  cat("   Número de changepoints:", nrow(model$changepoints), "\n")
  
  return(model)
}

# -----------------------------------------------------------------------------
# 3. FORECAST CON PROPHET
# -----------------------------------------------------------------------------

forecast_prophet <- function(model, prophet_data, periods = 30, freq = "day") {
  
  cat("\n📈 Generando forecast con Prophet...\n")
  cat("   Horizonte:", periods, freq, "\n")
  
  # Crear dataframe futuro
  future <- make_future_dataframe(model, periods = periods, freq = freq)
  
  # Generar forecast
  forecast_result <- predict(model, future)
  
  # Seleccionar columnas relevantes
  forecast_cols <- c("ds", "yhat", "yhat_lower", "yhat_upper", 
                     "trend", "weekly", "yearly")
  
  if ("daily" %in% names(forecast_result)) {
    forecast_cols <- c(forecast_cols, "daily")
  }
  
  forecast_df <- forecast_result[, forecast_cols]
  
  # Resumen
  future_forecast <- forecast_df[forecast_df$ds > max(prophet_data$ds), ]
  
  if (nrow(future_forecast) > 0) {
    cat("\n   Resumen del forecast:\n")
    cat("   - Media:", round(mean(future_forecast$yhat), 2), "\n")
    cat("   - Límite inferior (95%):", round(min(future_forecast$yhat_lower), 2), "\n")
    cat("   - Límite superior (95%):", round(max(future_forecast$yhat_upper), 2), "\n")
  }
  
  return(list(
    forecast = forecast_df,
    future = future,
    components = forecast_result
  ))
}

# -----------------------------------------------------------------------------
# 4. EVALUACIÓN DE PROPHET
# -----------------------------------------------------------------------------

evaluate_prophet <- function(forecast_result, prophet_data, h = 30) {
  
  cat("\n📊 Evaluando forecast de Prophet...\n")
  
  # Separar train/test
  train_size <- nrow(prophet_data) - h
  
  if (train_size <= 0) {
    cat("   No hay suficientes datos para evaluación\n")
    return(NULL)
  }
  
  # Datos reales de test
  actual <- prophet_data$y[(train_size + 1):nrow(prophet_data)]
  
  # Predicciones
  forecast_subset <- forecast_result$forecast[(train_size + 1):nrow(forecast_result$forecast), ]
  predictions <- forecast_subset$yhat
  
  # Asegurar longitudes iguales
  n <- min(length(actual), length(predictions))
  actual <- actual[1:n]
  predictions <- predictions[1:n]
  
  # Calcular métricas
  errors <- actual - predictions
  
  mae <- mean(abs(errors))
  rmse <- sqrt(mean(errors^2))
  mape <- 100 * mean(abs(errors / actual))
  
  # MASE
  naive_errors <- abs(diff(actual))
  mase <- mae / mean(naive_errors)
  
  cat("   MAE:", round(mae, 4), "\n")
  cat("   RMSE:", round(rmse, 4), "\n")
  cat("   MAPE:", round(mape, 2), "%\n")
  cat("   MASE:", round(mase, 4), "\n")
  
  if (mase < 1) {
    cat("   ✅ Mejor que Naive forecast!\n")
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
# 5. ANÁLISIS DE COMPONENTES
# -----------------------------------------------------------------------------

analyze_prophet_components <- function(model, forecast_result) {
  
  cat("\n📊 Analizando componentes de Prophet...\n")
  
  # Extraer componentes del último forecast
  last_forecast <- forecast_result$forecast[nrow(forecast_result$forecast), ]
  
  cat("   Tendencia:", round(last_forecast$trend, 2), "\n")
  cat("   Estacionalidad semanal:", round(last_forecast$weekly, 2), "\n")
  cat("   Estacionalidad anual:", round(last_forecast$yearly, 2), "\n")
  
  # Calcular contribución porcentual
  total <- abs(last_forecast$trend) + abs(last_forecast$weekly) + abs(last_forecast$yearly)
  
  cat("\n   Contribución relativa:\n")
  cat("   - Tendencia:", round(100 * abs(last_forecast$trend) / total, 1), "%\n")
  cat("   - Semanal:", round(100 * abs(last_forecast$weekly) / total, 1), "%\n")
  cat("   - Anual:", round(100 * abs(last_forecast$yearly) / total, 1), "%\n")
  
  return(list(
    trend = last_forecast$trend,
    weekly = last_forecast$weekly,
    yearly = last_forecast$yearly
  ))
}

# -----------------------------------------------------------------------------
# 6. FUNCIÓN PRINCIPAL DE MODELADO CON PROPHET
# -----------------------------------------------------------------------------

run_prophet_modeling <- function(data, date_col = "date", value_col = "sales", 
                                  periods = 30, output_dir = "outputs") {
  
  cat("\n🔮 MODELADO CON PROPHET\n")
  cat("======================\n\n")
  
  # 1. Preparar datos
  prophet_data <- prepare_data_for_prophet(data, date_col, value_col)
  
  # 2. Entrenar modelo
  prophet_model <- fit_prophet(prophet_data)
  
  # 3. Generar forecast
  forecast_result <- forecast_prophet(prophet_model, prophet_data, periods = periods)
  
  # 4. Evaluar modelo
  eval_result <- evaluate_prophet(prophet_model, prophet_data, h = 30)
  
  # 5. Analizar componentes
  components <- analyze_prophet_components(prophet_model, forecast_result)
  
  # Guardar modelo
  if (!dir.exists("models")) {
    dir.create("models")
  }
  
  saveRDS(prophet_model, "models/prophet_model.rds")
  
  # Guardar forecast
  forecast_df <- forecast_result$forecast
  forecast_df$ds <- as.Date(forecast_df$ds)
  
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  write.csv(forecast_df, file.path(output_dir, "prophet_forecast.csv"), row.names = FALSE)
  
  # Guardar métricas de evaluación
  if (!is.null(eval_result)) {
    eval_df <- data.frame(
      mae = eval_result$mae,
      rmse = eval_result$rmse,
      mape = eval_result$mape,
      mase = eval_result$mase,
      stringsAsFactors = FALSE
    )
    write.csv(eval_df, file.path(output_dir, "prophet_metrics.csv"), row.names = FALSE)
  }
  
  cat("\n✅ Modelado con Prophet completado!\n")
  
  return(list(
    model = prophet_model,
    forecast = forecast_result,
    evaluation = eval_result,
    components = components
  ))
}
