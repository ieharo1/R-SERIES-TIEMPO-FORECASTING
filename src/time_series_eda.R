# =============================================================================
# R - Análisis Exploratorio de Series de Tiempo
# Forecasting con ARIMA y Prophet
# Autor: Isaac Esteban Haro Torres
# =============================================================================

library(forecast)
library(lubridate)

# -----------------------------------------------------------------------------
# 1. CREACIÓN DE OBJETO DE SERIE TEMPORAL
# -----------------------------------------------------------------------------

create_time_series <- function(data, value_col = "sales", frequency = 7) {
  
  cat("📊 Creando objeto de serie temporal...\n")
  
  # Convertir a time series
  ts_data <- ts(data[[value_col]], frequency = frequency)
  
  cat("   Frecuencia:", frequency, "(semanal)\n")
  cat("   Número de observaciones:", length(ts_data), "\n")
  cat("   Media:", round(mean(ts_data), 2), "\n")
  cat("   Desviación estándar:", round(sd(ts_data), 2), "\n")
  
  return(ts_data)
}

# -----------------------------------------------------------------------------
# 2. TEST DE ESTACIONARIEDAD (ADF)
# -----------------------------------------------------------------------------

test_stationarity <- function(ts_data) {
  
  cat("\n🧪 Test de Estacionariedad (Augmented Dickey-Fuller)...\n")
  
  # ADF test
  adf_test <- urca::ur.df(ts_data, type = "drift", selectlags = "AIC")
  adf_summary <- summary(adf_test)
  
  # Extraer estadístico y p-value
  test_stat <- adf_summary@teststat[1]
  critical_5pct <- adf_summary$cval[1]
  is_stationary <- test_stat < critical_5pct
  
  cat("   Estadístico ADF:", round(test_stat, 4), "\n")
  cat("   Valor crítico (5%):", round(critical_5pct, 4), "\n")
  cat("   ¿Estacionaria?:", is_stationary, "\n")
  
  if (!is_stationary) {
    cat("   → Se requiere diferenciación (d > 0)\n")
  }
  
  return(list(
    test_statistic = test_stat,
    critical_value = critical_5pct,
    is_stationary = is_stationary
  ))
}

# -----------------------------------------------------------------------------
# 3. DESCOMPOSICIÓN DE SERIE TEMPORAL
# -----------------------------------------------------------------------------

decompose_time_series <- function(ts_data) {
  
  cat("\n📊 Descomponiendo serie temporal...\n")
  
  # Descomposición clásica
  decomposition <- decompose(ts_data, type = "additive")
  
  # Calcular fuerza de estacionalidad
  var_seasonal <- var(decomposition$seasonal, na.rm = TRUE)
  var_residual <- var(decomposition$random, na.rm = TRUE)
  var_total <- var_seasonal + var_residual
  
  seasonal_strength <- max(0, 1 - var_residual / var_total)
  
  # Calcular fuerza de tendencia
  var_trend <- var(decomposition$trend, na.rm = TRUE)
  trend_strength <- max(0, 1 - var_residual / (var_trend + var_residual))
  
  cat("   Fuerza de estacionalidad:", round(seasonal_strength, 4), "\n")
  cat("   Fuerza de tendencia:", round(trend_strength, 4), "\n")
  
  return(list(
    decomposition = decomposition,
    seasonal_strength = seasonal_strength,
    trend_strength = trend_strength
  ))
}

# -----------------------------------------------------------------------------
# 4. FUNCIONES DE AUTOCORRELACIÓN (ACF/PACF)
# -----------------------------------------------------------------------------

analyze_acf_pacf <- function(ts_data, max_lag = 40) {
  
  cat("\n📊 Analizando ACF y PACF...\n")
  
  # Calcular ACF y PACF
  acf_values <- Acf(ts_data, lag.max = max_lag, plot = FALSE)
  pacf_values <- Pacf(ts_data, lag.max = max_lag, plot = FALSE)
  
  # Encontrar lags significativos
  confidence_bound <- 1.96 / sqrt(length(ts_data))
  
  # Lags significativos en ACF
  acf_sig <- which(abs(acf_values$acf) > confidence_bound) - 1
  acf_sig <- acf_sig[acf_sig > 0]
  
  # Lags significativos en PACF
  pacf_sig <- which(abs(pacf_values$acf) > confidence_bound) - 1
  pacf_sig <- pacf_sig[pacf_sig > 0]
  
  cat("   Lags significativos en ACF:", paste(head(acf_sig, 5), collapse = ", "), "\n")
  cat("   Lags significativos en PACF:", paste(head(pacf_sig, 5), collapse = ", "), "\n")
  
  # Sugerencias para parámetros ARIMA
  suggested_p <- if (length(pacf_sig) > 0) min(pacf_sig) else 1
  suggested_q <- if (length(acf_sig) > 0) min(acf_sig) else 1
  
  cat("   Sugerencia: p =", suggested_p, ", q =", suggested_q, "\n")
  
  return(list(
    acf = acf_values,
    pacf = pacf_values,
    significant_acf_lags = acf_sig,
    significant_pacf_lags = pacf_sig,
    suggested_p = suggested_p,
    suggested_q = suggested_q
  ))
}

# -----------------------------------------------------------------------------
# 5. ANÁLISIS DE RESIDUOS
# -----------------------------------------------------------------------------

analyze_residuals <- function(model, ts_data) {
  
  cat("\n📊 Analizando residuos del modelo...\n")
  
  # Extraer residuos
  residuals <- residuals(model)
  
  # Test de normalidad (Shapiro-Wilk)
  if (length(residuals) <= 5000) {
    shapiro_test <- shapiro.test(residuals)
    is_normal <- shapiro_test$p.value > 0.05
  } else {
    is_normal <- FALSE
    shapiro_test <- NULL
  }
  
  # Test de autocorrelación en residuos (Ljung-Box)
  ljung_box <- Box.test(residuals, lag = 20, type = "Ljung-Box")
  no_autocorr <- ljung_box$p.value > 0.05
  
  # Homocedasticidad (varianza constante)
  var_first_half <- var(residuals[1:floor(length(residuals)/2)])
  var_second_half <- var(residuals[floor(length(residuals)/2):length(residuals)])
  is_homoscedastic <- abs(log(var_second_half / var_first_half)) < 0.5
  
  cat("   Media de residuos:", round(mean(residuals), 6), "\n")
  cat("   Desviación estándar:", round(sd(residuals), 4), "\n")
  cat("   ¿Normalidad?:", is_normal, "\n")
  cat("   ¿Sin autocorrelación?:", no_autocorr, "\n")
  cat("   ¿Homocedasticidad?:", is_homoscedastic, "\n")
  
  return(list(
    mean = mean(residuals),
    sd = sd(residuals),
    is_normal = is_normal,
    no_autocorrelation = no_autocorr,
    is_homoscedastic = is_homoscedastic,
    shapiro_p = if (!is.null(shapiro_test)) shapiro_test$p.value else NA,
    ljung_box_p = ljung_box$p.value
  ))
}

# -----------------------------------------------------------------------------
# 6. FUNCIÓN PRINCIPAL DE EDA
# -----------------------------------------------------------------------------

run_time_series_eda <- function(data, value_col = "sales", frequency = 7, 
                                 output_dir = "outputs") {
  
  cat("\n📊 ANÁLISIS EXPLORATORIO DE SERIES DE TIEMPO\n")
  cat("============================================\n\n")
  
  # 1. Crear serie temporal
  ts_data <- create_time_series(data, value_col, frequency)
  
  # 2. Test de estacionariedad
  stationarity <- test_stationarity(ts_data)
  
  # 3. Descomposición
  decomposition <- decompose_time_series(ts_data)
  
  # 4. ACF/PACF
  acf_pacf <- analyze_acf_pacf(ts_data)
  
  # Guardar resultados
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  # Guardar información de estacionariedad
  stationarity_info <- data.frame(
    test = "ADF",
    statistic = stationarity$test_statistic,
    critical_value = stationarity$critical_value,
    is_stationary = stationarity$is_stationary,
    stringsAsFactors = FALSE
  )
  
  write.csv(stationarity_info, file.path(output_dir, "stationarity_test.csv"), 
            row.names = FALSE)
  
  cat("\n✅ EDA de series de tiempo completado!\n")
  
  return(list(
    ts_data = ts_data,
    stationarity = stationarity,
    decomposition = decomposition,
    acf_pacf = acf_pacf
  ))
}
