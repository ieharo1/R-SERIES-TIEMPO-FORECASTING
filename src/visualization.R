# =============================================================================
# R - Visualización de Series de Tiempo y Forecasts
# Autor: Isaac Esteban Haro Torres
# =============================================================================

library(ggplot2)
library(tidyr)
library(forecast)

# -----------------------------------------------------------------------------
# 1. DESCOMPOSICIÓN DE SERIE TEMPORAL
# -----------------------------------------------------------------------------

plot_time_series_decomposition <- function(data, decomposition, output_dir = "plots") {
  
  cat("📊 Generando gráfico de descomposición...\n")
  
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  # Preparar datos
  n <- length(decomposition$observed)
  dates <- seq(as.Date("2024-01-01"), by = "day", length.out = n)
  
  decomp_df <- data.frame(
    date = dates,
    observed = as.numeric(decomposition$observed),
    trend = as.numeric(decomposition$trend),
    seasonal = as.numeric(decomposition$seasonal),
    random = as.numeric(decomposition$random)
  )
  
  # Crear gráfico facetado
  decomp_long <- pivot_longer(decomp_df, cols = c(observed, trend, seasonal, random),
                               names_to = "component", values_to = "value")
  
  p <- ggplot(decomp_long, aes(x = date, y = value)) +
    geom_line(linewidth = 0.5) +
    facet_wrap(~component, ncol = 1, scales = "free_y") +
    labs(
      title = "Descomposición de Serie Temporal",
      subtitle = "Observado = Tendencia + Estacionalidad + Residuo",
      x = "Fecha",
      y = "Valor",
      caption = "Fuente: Análisis de series de tiempo"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 11, hjust = 0.5, color = "gray50"),
      axis.title = element_text(size = 11, face = "bold"),
      axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
      strip.text = element_text(size = 11, face = "bold"),
      panel.grid.minor = element_blank()
    )
  
  ggsave(file.path(output_dir, "time_series_decomposition.png"), 
         plot = p, width = 12, height = 10, dpi = 300)
  
  cat("   Guardado en:", file.path(output_dir, "time_series_decomposition.png"), "\n")
  
  return(p)
}

# -----------------------------------------------------------------------------
# 2. ACF Y PACF
# -----------------------------------------------------------------------------

plot_acf_pacf <- function(acf_pacf_result, output_dir = "plots") {
  
  cat("📊 Generando gráficos ACF y PACF...\n")
  
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  # ACF
  p1 <- ggplot(acf_pacf_result$acf, aes(x = Lag, y = ACF)) +
    geom_hline(yintercept = 0, color = "black", linewidth = 0.5) +
    geom_segment(aes(xend = Lag, yend = 0), color = "#2166AC", linewidth = 1) +
    geom_point(color = "#B2182B", size = 2) +
    geom_hline(yintercept = c(-1.96, 1.96) / sqrt(acf_pacf_result$acf$n), 
               linetype = "dashed", color = "gray") +
    labs(
      title = "Función de Autocorrelación (ACF)",
      x = "Lag",
      y = "ACF",
      caption = "Líneas punteadas = límite de significancia (95%)"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
      axis.title = element_text(size = 10, face = "bold"),
      panel.grid.minor = element_blank()
    )
  
  # PACF
  p2 <- ggplot(acf_pacf_result$pacf, aes(x = Lag, y = PACF)) +
    geom_hline(yintercept = 0, color = "black", linewidth = 0.5) +
    geom_segment(aes(xend = Lag, yend = 0), color = "#2166AC", linewidth = 1) +
    geom_point(color = "#B2182B", size = 2) +
    geom_hline(yintercept = c(-1.96, 1.96) / sqrt(acf_pacf_result$pacf$n), 
               linetype = "dashed", color = "gray") +
    labs(
      title = "Función de Autocorrelación Parcial (PACF)",
      x = "Lag",
      y = "PACF",
      caption = "Líneas punteadas = límite de significancia (95%)"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
      axis.title = element_text(size = 10, face = "bold"),
      panel.grid.minor = element_blank()
    )
  
  # Guardar combinados
  library(gridExtra)
  ggsave(file.path(output_dir, "acf_pacf.png"), 
         plot = grid.arrange(p1, p2, ncol = 2), 
         width = 14, height = 6, dpi = 300)
  
  cat("   Guardado en:", file.path(output_dir, "acf_pacf.png"), "\n")
  
  return(list(acf = p1, pacf = p2))
}

# -----------------------------------------------------------------------------
# 3. FORECAST ARIMA
# -----------------------------------------------------------------------------

plot_arima_forecast <- function(ts_data, forecast_result, h = 30, 
                                 output_dir = "plots") {
  
  cat("📊 Generando gráfico de forecast ARIMA...\n")
  
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  # Preparar datos
  n historical <- length(ts_data)
  dates_historical <- seq(as.Date("2024-01-01"), by = "day", length.out = n_historical)
  dates_forecast <- seq(max(dates_historical) + 1, by = "day", length.out = h)
  
  # Dataframe combinado
  plot_df <- data.frame(
    date = c(dates_historical, dates_forecast),
    value = c(as.numeric(ts_data), rep(NA, h)),
    forecast = c(rep(NA, n_historical), as.numeric(forecast_result$mean)),
    lower_80 = c(rep(NA, n_historical), as.numeric(forecast_result$lower[, 1])),
    upper_80 = c(rep(NA, n_historical), as.numeric(forecast_result$upper[, 1])),
    lower_95 = c(rep(NA, n_historical), as.numeric(forecast_result$lower[, 2])),
    upper_95 = c(rep(NA, n_historical), as.numeric(forecast_result$upper[, 2])),
    type = c(rep("Historical", n_historical), rep("Forecast", h))
  )
  
  p <- ggplot(plot_df, aes(x = date)) +
    # Historical
    geom_line(aes(y = value), color = "#2166AC", linewidth = 1) +
    # Forecast
    geom_line(aes(y = forecast), color = "#B2182B", linewidth = 1) +
    # Intervalos de confianza
    geom_ribbon(aes(ymin = lower_95, ymax = upper_95), 
                fill = "#B2182B", alpha = 0.2) +
    geom_ribbon(aes(ymin = lower_80, ymax = upper_80), 
                fill = "#B2182B", alpha = 0.3) +
    labs(
      title = "Forecast con ARIMA",
      subtitle = "Línea azul = Histórico | Línea roja = Pronóstico | Sombras = Intervalos de confianza",
      x = "Fecha",
      y = "Ventas",
      caption = "Fuente: Modelo ARIMA"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 11, hjust = 0.5, color = "gray50"),
      axis.title = element_text(size = 12, face = "bold"),
      axis.text.x = element_text(angle = 45, hjust = 1, size = 9),
      panel.grid.minor = element_blank()
    )
  
  ggsave(file.path(output_dir, "arima_forecast.png"), 
         plot = p, width = 12, height = 6, dpi = 300)
  
  cat("   Guardado en:", file.path(output_dir, "arima_forecast.png"), "\n")
  
  return(p)
}

# -----------------------------------------------------------------------------
# 4. FORECAST PROPHET
# -----------------------------------------------------------------------------

plot_prophet_forecast <- function(prophet_data, forecast_result, 
                                   output_dir = "plots") {
  
  cat("📊 Generando gráfico de forecast Prophet...\n")
  
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  # Preparar datos
  forecast_df <- forecast_result$forecast
  forecast_df$ds <- as.Date(forecast_df$ds)
  
  p <- ggplot() +
    # Datos históricos
    geom_point(data = prophet_data, aes(x = as.Date(ds), y = y), 
               color = "#2166AC", alpha = 0.5, size = 1) +
    # Forecast
    geom_line(data = forecast_df, aes(x = ds, y = yhat), 
              color = "#B2182B", linewidth = 1) +
    # Intervalos
    geom_ribbon(data = forecast_df, aes(x = ds, ymin = yhat_lower, ymax = yhat_upper),
                fill = "#B2182B", alpha = 0.2) +
    labs(
      title = "Forecast con Prophet",
      subtitle = "Puntos azules = Histórico | Línea roja = Pronóstico | Sombra = IC 95%",
      x = "Fecha",
      y = "Ventas",
      caption = "Fuente: Modelo Prophet (Facebook)"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 11, hjust = 0.5, color = "gray50"),
      axis.title = element_text(size = 12, face = "bold"),
      axis.text.x = element_text(angle = 45, hjust = 1, size = 9),
      panel.grid.minor = element_blank()
    )
  
  ggsave(file.path(output_dir, "prophet_forecast.png"), 
         plot = p, width = 12, height = 6, dpi = 300)
  
  cat("   Guardado en:", file.path(output_dir, "prophet_forecast.png"), "\n")
  
  return(p)
}

# -----------------------------------------------------------------------------
# 5. COMPARACIÓN DE MODELOS
# -----------------------------------------------------------------------------

plot_model_comparison <- function(arima_eval, prophet_eval, output_dir = "plots") {
  
  cat("📊 Generando comparación de modelos...\n")
  
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  # Preparar datos
  if (!is.null(arima_eval) && !is.null(prophet_eval)) {
    comparison_df <- data.frame(
      Metrica = c("MAE", "RMSE", "MAPE (%)", "MASE"),
      ARIMA = c(arima_eval$mae, arima_eval$rmse, arima_eval$mape, arima_eval$mase),
      Prophet = c(prophet_eval$mae, prophet_eval$rmse, prophet_eval$mape, prophet_eval$mase),
      stringsAsFactors = FALSE
    )
    
    comparison_long <- pivot_longer(comparison_df, cols = c(ARIMA, Prophet),
                                     names_to = "Modelo", values_to = "Valor")
    
    p <- ggplot(comparison_long, aes(x = Metrica, y = Valor, fill = Modelo)) +
      geom_bar(stat = "identity", position = "dodge") +
      scale_fill_manual(values = c("ARIMA" = "#2166AC", "Prophet" = "#B2182B")) +
      geom_text(aes(label = round(Valor, 2)), position = position_dodge(width = 0.9),
                vjust = -0.5, size = 4, fontface = "bold") +
      labs(
        title = "Comparación de Modelos de Forecasting",
        subtitle = "Métricas de evaluación en datos de test",
        x = "Métrica",
        y = "Valor",
        caption = "Valores más bajos = Mejor desempeño"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 11, hjust = 0.5, color = "gray50"),
        axis.title = element_text(size = 12, face = "bold"),
        legend.position = "top",
        legend.title = element_blank(),
        panel.grid.minor = element_blank()
      )
    
    ggsave(file.path(output_dir, "model_comparison.png"), 
           plot = p, width = 10, height = 6, dpi = 300)
    
    cat("   Guardado en:", file.path(output_dir, "model_comparison.png"), "\n")
    
    return(p)
  }
}

# -----------------------------------------------------------------------------
# 6. FUNCIÓN DE VISUALIZACIÓN COMPLETA
# -----------------------------------------------------------------------------

generate_all_visualizations <- function(ts_data, decomposition, acf_pacf_result,
                                         arima_forecast, prophet_data, 
                                         prophet_forecast, arima_eval, prophet_eval,
                                         output_dir = "plots") {
  
  cat("\n📈 GENERANDO VISUALIZACIONES\n")
  cat("============================\n\n")
  
  plot_time_series_decomposition(ts_data, decomposition, output_dir)
  plot_acf_pacf(acf_pacf_result, output_dir)
  plot_arima_forecast(ts_data, arima_forecast, h = 30, output_dir)
  
  if (!is.null(prophet_data) && !is.null(prophet_forecast)) {
    plot_prophet_forecast(prophet_data, prophet_forecast, output_dir)
  }
  
  if (!is.null(arima_eval) && !is.null(prophet_eval)) {
    plot_model_comparison(arima_eval, prophet_eval, output_dir)
  }
  
  cat("\n✅ Todas las visualizaciones generadas!\n")
}
