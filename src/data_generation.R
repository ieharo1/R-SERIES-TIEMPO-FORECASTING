# =============================================================================
# R - Generación de Datos de Series de Tiempo
# Forecasting de Ventas y Tráfico
# Autor: Isaac Esteban Haro Torres
# =============================================================================

library(lubridate)

# -----------------------------------------------------------------------------
# 1. GENERACIÓN DE VENTAS DIARIAS
# -----------------------------------------------------------------------------

generate_daily_sales <- function(n_days = 730, start_date = "2024-01-01") {
  
  cat("📊 Generando", n_days, "días de ventas simuladas...\n")
  
  set.seed(42)
  
  # Crear secuencia de fechas
  dates <- seq(as.Date(start_date), by = "day", length.out = n_days)
  
  # Inicializar vectores
  sales <- data.frame(
    date = dates,
    stringsAsFactors = FALSE
  )
  
  # ---------------------------------------------------------------------------
  # COMPONENTE DE TENDENCIA
  # ---------------------------------------------------------------------------
  
  # Tendencia creciente con algo de ruido
  trend <- 5000 + (1:n_days) * 5 + rnorm(n_days, 0, 200)
  
  # ---------------------------------------------------------------------------
  # COMPONENTE ESTACIONAL
  # ---------------------------------------------------------------------------
  
  # Estacionalidad semanal (más ventas fines de semana)
  day_of_week <- wday(dates, week_start = 1)  # 1 = Monday, 7 = Sunday
  
  weekly_pattern <- case_when(
    day_of_week == 1 ~ -800,   # Monday
    day_of_week == 2 ~ -600,   # Tuesday
    day_of_week == 3 ~ -500,   # Wednesday
    day_of_week == 4 ~ -400,   # Thursday
    day_of_week == 5 ~ 200,    # Friday
    day_of_week == 6 ~ 800,    # Saturday
    day_of_week == 7 ~ 1000    # Sunday
  )
  
  # Estacionalidad mensual (más ventas a mitad y fin de mes)
  day_of_month <- mday(dates)
  monthly_pattern <- if_else(day_of_month %in% c(15, 16, 28, 29, 30, 31), 500, 0)
  
  # Estacionalidad anual (temporada alta en Q4)
  month <- month(dates)
  yearly_pattern <- case_when(
    month %in% c(1, 2) ~ -500,      # Enero, Febrero (bajo)
    month %in% c(3, 4, 5) ~ 200,    # Primavera
    month %in% c(6, 7, 8) ~ 400,    # Verano
    month %in% c(9, 10) ~ 300,      # Otoño
    month %in% c(11, 12) ~ 1500     # Q4 (alto - holidays)
  )
  
  # ---------------------------------------------------------------------------
  # EVENTOS ESPECIALES
  # ---------------------------------------------------------------------------
  
  events <- rep(0, n_days)
  
  # Black Friday (día 330 aprox)
  black_friday <- which(month == 11 & day_of_week == 5)[4]
  if (!is.na(black_friday)) events[black_friday] <- 5000
  
  # Cyber Monday
  cyber_monday <- black_friday + 3
  if (cyber_monday <= n_days) events[cyber_monday] <- 3000
  
  # Navidad
  christmas <- which(month == 12 & day_of_month == 24)
  if (length(christmas) > 0) events[christmas] <- 4000
  
  # Día de San Valentín
  valentines <- which(month == 2 & day_of_month == 14)
  if (length(valentines) > 0) events[valentines] <- 2000
  
  # ---------------------------------------------------------------------------
  # RUIDO ALEATORIO
  # ---------------------------------------------------------------------------
  
  noise <- rnorm(n_days, 0, 400)
  
  # ---------------------------------------------------------------------------
  # COMBINAR COMPONENTES
  # ---------------------------------------------------------------------------
  
  sales$sales <- round(trend + weekly_pattern + monthly_pattern + 
                        yearly_pattern + events + noise, 2)
  
  # Asegurar valores positivos
  sales$sales <- pmax(500, sales$sales)
  
  # ---------------------------------------------------------------------------
  # GENERAR ÓRDENES Y VALOR PROMEDIO
  # ---------------------------------------------------------------------------
  
  # Número de órdenes (correlacionado con ventas)
  avg_order_value <- round(rnorm(n_days, 75, 15), 2)
  avg_order_value <- pmax(40, pmin(150, avg_order_value))
  
  sales$orders <- round(sales$sales / avg_order_value)
  sales$orders <- pmax(5, sales$orders)
  
  sales$avg_order_value <- round(sales$sales / sales$orders, 2)
  
  cat("   Ventas generadas exitosamente!\n")
  cat("   Rango de fechas:", min(sales$date), "a", max(sales$date), "\n")
  cat("   Ventas promedio: $", round(mean(sales$sales), 2), "\n")
  cat("   Ventas máximas: $", round(max(sales$sales), 2), "\n")
  cat("   Ventas mínimas: $", round(min(sales$sales), 2), "\n")
  
  return(sales)
}

# -----------------------------------------------------------------------------
# 2. GENERACIÓN DE TRÁFICO WEB
# -----------------------------------------------------------------------------

generate_daily_traffic <- function(n_days = 730, start_date = "2024-01-01") {
  
  cat("\n🌐 Generando", n_days, "días de tráfico web simulado...\n")
  
  set.seed(43)
  
  # Crear secuencia de fechas
  dates <- seq(as.Date(start_date), by = "day", length.out = n_days)
  
  traffic <- data.frame(
    date = dates,
    stringsAsFactors = FALSE
  )
  
  # ---------------------------------------------------------------------------
  # COMPONENTE DE TENDENCIA
  # ---------------------------------------------------------------------------
  
  trend <- 10000 + (1:n_days) * 15 + rnorm(n_days, 0, 500)
  
  # ---------------------------------------------------------------------------
  # COMPONENTE ESTACIONAL
  # ---------------------------------------------------------------------------
  
  day_of_week <- wday(dates, week_start = 1)
  
  # Tráfico web diferente al de ventas (más tráfico weekdays)
  weekly_pattern <- case_when(
    day_of_week == 1 ~ 1500,   # Monday (alto)
    day_of_week == 2 ~ 1200,   # Tuesday
    day_of_week == 3 ~ 1000,   # Wednesday
    day_of_week == 4 ~ 800,    # Thursday
    day_of_week == 5 ~ 500,    # Friday
    day_of_week == 6 ~ -1000,  # Saturday (bajo)
    day_of_week == 7 ~ -500    # Sunday
  )
  
  # Estacionalidad anual
  month <- month(dates)
  yearly_pattern <- case_when(
    month %in% c(1, 2) ~ 1000,      # Enero, Febrero (alto - año nuevo)
    month %in% c(6, 7, 8) ~ -1500,  # Verano (vacaciones)
    month %in% c(11, 12) ~ 2000,    # Q4 (holiday shopping)
    TRUE ~ 0
  )
  
  # ---------------------------------------------------------------------------
  # RUIDO
  # ---------------------------------------------------------------------------
  
  noise <- rnorm(n_days, 0, 800)
  
  # ---------------------------------------------------------------------------
  # COMBINAR COMPONENTES
  # ---------------------------------------------------------------------------
  
  traffic$visitors <- round(trend + weekly_pattern + yearly_pattern + noise)
  traffic$visitors <- pmax(5000, traffic$visitors)
  
  # ---------------------------------------------------------------------------
  # GENERAR MÉTRICAS ADICIONALES
  # ---------------------------------------------------------------------------
  
  # Page views por visitante
  pages_per_visitor <- round(rnorm(n_days, 3.5, 0.8), 1)
  pages_per_visitor <- pmax(1, pmin(8, pages_per_visitor))
  
  traffic$page_views <- round(traffic$visitors * pages_per_visitor)
  
  # Bounce rate (porcentaje)
  traffic$bounce_rate <- round(rnorm(n_days, 42, 8), 1)
  traffic$bounce_rate <- pmax(20, pmin(70, traffic$bounce_rate))
  
  # Conversion rate (porcentaje) - correlacionado con visitantes
  traffic$conversion_rate <- round(rnorm(n_days, 2.5, 0.5), 2)
  traffic$conversion_rate <- pmax(0.5, pmin(5, traffic$conversion_rate))
  
  cat("   Tráfico generado exitosamente!\n")
  cat("   Visitantes promedio:", round(mean(traffic$visitors), 0), "\n")
  cat("   Page views promedio:", round(mean(traffic$page_views), 0), "\n")
  cat("   Bounce rate promedio:", round(mean(traffic$bounce_rate), 1), "%\n")
  
  return(traffic)
}

# -----------------------------------------------------------------------------
# 3. FUNCIÓN PRINCIPAL
# -----------------------------------------------------------------------------

generate_all_data <- function(n_days = 730, start_date = "2024-01-01", 
                               output_dir = "data") {
  
  cat("📈 R - Forecasting - Generando datos de series de tiempo...\n\n")
  
  # Crear directorio
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  # Generar datos
  sales <- generate_daily_sales(n_days, start_date)
  traffic <- generate_daily_traffic(n_days, start_date)
  
  # Guardar
  cat("\n💾 Guardando datos en", output_dir, "...\n")
  write.csv(sales, file.path(output_dir, "daily_sales.csv"), row.names = FALSE)
  write.csv(traffic, file.path(output_dir, "daily_traffic.csv"), row.names = FALSE)
  
  # Resumen
  cat("\n📈 RESUMEN DE DATOS GENERADOS:\n")
  cat("================================\n")
  cat("Período:", min(sales$date), "a", max(sales$date), "\n")
  cat("Días totales:", nrow(sales), "\n")
  cat("\nVentas:\n")
  cat("   Total: $", format(sum(sales$sales), big.mark = ","), "\n")
  cat("   Promedio diario: $", round(mean(sales$sales), 2), "\n")
  cat("   Órdenes totales:", sum(sales$orders), "\n")
  cat("\nTráfico:\n")
  cat("   Visitantes totales:", format(sum(traffic$visitors), big.mark = ","), "\n")
  cat("   Page views totales:", format(sum(traffic$page_views), big.mark = ","), "\n")
  
  cat("\n✅ Datos generados exitosamente!\n")
  
  return(list(sales = sales, traffic = traffic))
}

# Ejecutar si es el script principal
if (!interactive()) {
  data <- generate_all_data()
}
