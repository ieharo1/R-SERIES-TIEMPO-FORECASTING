# =============================================================================
# 📈 R - SERIES DE TIEMPO Y FORECASTING
# =============================================================================
# Proyecto: Series de tiempo (Forecast)
# Técnicas: ARIMA, Auto ARIMA, Prophet, ETS
# Autor: Isaac Esteban Haro Torres
# Fecha: 2026
# =============================================================================

# Limpiar entorno
rm(list = ls())

# Configurar opciones
options(stringsAsFactors = FALSE)
options(scipen = 999)
options(warn = -1)

# Mensaje de inicio
cat("\n")
cat("╔════════════════════════════════════════════════════════════════╗\n")
cat("║                                                                ║\n")
cat("║     📈 R - SERIES DE TIEMPO Y FORECASTING                      ║\n")
cat("║                                                                ║\n")
cat("║     ARIMA + Prophet para Predicción de Ventas y Tráfico        ║\n")
cat("║     Desarrollado por: Isaac Esteban Haro Torres                ║\n")
cat("║                                                                ║\n")
cat("╚════════════════════════════════════════════════════════════════╝\n")
cat("\n")

# =============================================================================
# 1. CARGA DE LIBRERÍAS
# =============================================================================

cat("📦 Cargando librerías...\n")

suppressPackageStartupMessages({
  library(tidyverse)
  library(ggplot2)
  library(dplyr)
  library(lubridate)
  library(forecast)
  library(prophet)
  library(zoo)
  library(urca)
})

cat("   ✅ Librerías cargadas exitosamente!\n\n")

# =============================================================================
# 2. GENERACIÓN DE DATOS SIMULADOS
# =============================================================================

cat("📊 GENERANDO DATOS SIMULADOS\n")
cat("============================\n\n")

# Ejecutar script de generación de datos
source("src/data_generation.R")

# Generar datos
set.seed(42)
time_series_data <- generate_all_data(n_days = 730, start_date = "2024-01-01", 
                                       output_dir = "data")

sales_data <- time_series_data$sales
traffic_data <- time_series_data$traffic

cat("\n")

# =============================================================================
# 3. ANÁLISIS EXPLORATORIO DE SERIES DE TIEMPO (EDA)
# =============================================================================

cat("📊 ANÁLISIS EXPLORATORIO DE SERIES DE TIEMPO\n")
cat("============================================\n\n")

# Ejecutar script de EDA
source("src/time_series_eda.R")

# Ejecutar EDA para ventas
eda_result <- run_time_series_eda(sales_data, value_col = "sales", frequency = 7)

ts_data <- eda_result$ts_data

cat("\n")

# =============================================================================
# 4. MODELADO CON ARIMA
# =============================================================================

cat("🔍 MODELADO CON ARIMA\n")
cat("====================\n\n")

# Ejecutar script de ARIMA
source("src/arima_modeling.R")

# Ejecutar modelado ARIMA
arima_result <- run_arima_modeling(ts_data, h = 30, output_dir = "outputs")

cat("\n")

# =============================================================================
# 5. MODELADO CON PROPHET
# =============================================================================

cat("🔮 MODELADO CON PROPHET\n")
cat("======================\n\n")

# Ejecutar script de Prophet
source("src/prophet_modeling.R")

# Ejecutar modelado Prophet
prophet_result <- run_prophet_modeling(
  sales_data, 
  date_col = "date", 
  value_col = "sales", 
  periods = 30,
  output_dir = "outputs"
)

cat("\n")

# =============================================================================
# 6. VISUALIZACIÓN DE RESULTADOS
# =============================================================================

cat("📈 GENERANDO VISUALIZACIONES\n")
cat("============================\n\n")

# Ejecutar script de visualización
source("src/visualization.R")

# Generar visualizaciones
generate_all_visualizations(
  ts_data = ts_data,
  decomposition = eda_result$decomposition,
  acf_pacf_result = eda_result$acf_pacf,
  arima_forecast = arima_result$forecast,
  prophet_data = prophet_result$model$data,
  prophet_forecast = prophet_result$forecast,
  arima_eval = arima_result$cv_results,
  prophet_eval = prophet_result$evaluation,
  output_dir = "plots"
)

cat("\n")

# =============================================================================
# 7. RESUMEN FINAL
# =============================================================================

cat("╔════════════════════════════════════════════════════════════════╗\n")
cat("║                    ✅ PROYECTO COMPLETADO                      ║\n")
cat("╠════════════════════════════════════════════════════════════════╣\n")
cat("║                                                                ║\n")
cat("║  📊 Datos procesados:                                          ║\n")
cat("║     •", nrow(sales_data), "días de ventas históricas                       ║\n")
cat("║     •", nrow(traffic_data), "días de tráfico web                           ║\n")
cat("║     • Período:", min(sales_data$date), "a", max(sales_data$date), "                  ║\n")
cat("║                                                                ║\n")
cat("║  🤖 Modelos entrenados:                                        ║\n")
cat("║     • ARIMA:", deparse(substitute(arima_result$model$model$arma)), "                            ║\n")
cat("║     • Prophet (con estacionalidad semanal y anual)            ║\n")
cat("║                                                                ║\n")
cat("║  📈 Forecast a", 30, "días:\n", sep = "")

# Resumen del forecast
arima_mean <- round(mean(arima_result$forecast$mean), 2)
prophet_mean <- round(mean(prophet_result$forecast$forecast$yhat), 2)

cat("║     • ARIMA - Ventas promedio pronosticadas: $", arima_mean, "              ║\n")
cat("║     • Prophet - Ventas promedio pronosticadas: $", prophet_mean, "            ║\n")
cat("║                                                                ║\n")
cat("║  📁 Archivos generados:                                        ║\n")
cat("║     • data/daily_sales.csv, daily_traffic.csv                 ║\n")
cat("║     • outputs/arima_forecast.csv                              ║\n")
cat("║     • outputs/prophet_forecast.csv                            ║\n")
cat("║     • outputs/arima_cv_results.csv                            ║\n")
cat("║     • plots/*.png (5 visualizaciones)                         ║\n")
cat("║     • models/*.rds (2 modelos)                                ║\n")
cat("║                                                                ║\n")
cat("║  👨‍💻 Desarrollado por Isaac Esteban Haro Torres                 ║\n")
cat("║                                                                ║\n")
cat("╚════════════════════════════════════════════════════════════════╝\n")
cat("\n")

# =============================================================================
# FIN DEL PROGRAMA
# =============================================================================
