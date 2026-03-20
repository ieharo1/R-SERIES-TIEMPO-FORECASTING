# 📈 R - Series de Tiempo y Forecasting

<p align="center">
  <img src="https://img.icons8.com/color/200/000000/line-chart.png" alt="Time Series Forecasting Logo" width="200"/>
</p>

---

## 📱 Descripción

**R** es un proyecto de **Forecasting y Series de Tiempo** desarrollado en **R** que utiliza modelos estadísticos avanzados como **ARIMA** y **Prophet** para predecir ventas y tráfico de usuarios, permitiendo la planificación estratégica y la toma de decisiones basada en tendencias temporales, estacionalidad y patrones históricos.

> El sistema analiza datos históricos de ventas y tráfico web para generar pronósticos precisos con intervalos de confianza, identificando tendencias, estacionalidad y puntos de cambio.

---

## ✨ Características

### Funcionalidades Implementadas ✅

- ✅ **Generación de Datos Simulados** - Series temporales realistas
- ✅ **Análisis Exploratorio (EDA)** - Descomposición de serie temporal
- ✅ **Estacionariedad** - Tests de Dickey-Fuller (ADF)
- ✅ **ARIMA** - AutoRegressive Integrated Moving Average
- ✅ **Auto ARIMA** - Búsqueda automática de parámetros (p,d,q)
- ✅ **Prophet** - Modelo de Facebook para series con estacionalidad
- ✅ **ETS** - Error, Trend, Seasonal (Exponential Smoothing)
- ✅ **Validación Temporal** - Train/Test split cronológico
- ✅ **Métricas de Forecast** - MAE, RMSE, MAPE, MASE
- ✅ **Intervalos de Confianza** - 80% y 95%
- ✅ **Visualización de Tendencias** - Gráficos de forecast
- ✅ **Exportación de Pronósticos** - Predicciones listas para BI

### Próximamente 🔄

- 🧠 **Deep Learning** - LSTM para series temporales
- 📊 **VAR** - Vector Autoregression para múltiples series
- 🔮 **Prophet Multivariado** - Con variables exógenas
- 📈 **Intervention Analysis** - Impacto de eventos
- 🌐 **Dashboard Shiny** - Visualización interactiva
- ⚡ **Forecast en Tiempo Real** - Actualización automática

---

## 🛠️ Stack Tecnológico

| Componente | Tecnología | Versión |
|------------|------------|---------|
| Lenguaje | R | 4.3.x |
| IDE | RStudio | 2024.x |
| Data Manipulation | tidyverse | 2.0.x |
| Visualización | ggplot2 | 3.5.x |
| Series de Tiempo | forecast | 8.21.x |
| ARIMA | forecast | 8.21.x |
| Prophet | prophet | 1.1.x |
| ETS | forecast | 8.21.x |
| Métricas | Metrics | 0.1.x |

---

## 📁 Estructura del Proyecto

```
R/
├── 📂 data/
│   ├── daily_sales.csv
│   └── daily_traffic.csv
├── 📂 src/
│   ├── data_generation.R
│   ├── time_series_eda.R
│   ├── arima_modeling.R
│   ├── prophet_modeling.R
│   └── visualization.R
├── 📂 plots/
│   ├── time_series_decomposition.png
│   ├── acf_pacf.png
│   ├── arima_forecast.png
│   ├── prophet_forecast.png
│   └── model_comparison.png
├── 📂 models/
│   ├── arima_model.rds
│   └── prophet_model.rds
├── 📂 outputs/
│   ├── forecasts.csv
│   └── model_metrics.csv
├── main.R
├── Dockerfile
├── .dockerignore
└── README.md
```

---

## 🚀 Cómo Ejecutar el Proyecto

### 1. Clonar el Repositorio
```bash
git clone https://github.com/ieharo1/R.git
cd R
```

### 2. Ejecutar con R Local
```bash
Rscript main.R
```

### 3. Ejecutar con Docker
```bash
# Construir la imagen
docker build -t time-series-forecast .

# Ejecutar el contenedor
docker run --rm time-series-forecast
```

### 4. Ejecutar en RStudio
1. Abrir `main.R` en RStudio
2. Ejecutar línea por línea o source completo
3. Revisar outputs en carpetas generadas

---

## 📊 Datos de Series Temporales

### Ventas Diarias
```r
variable | descripción
---------|------------
date | Fecha (YYYY-MM-DD)
sales | Ventas totales del día ($)
orders | Número de órdenes
avg_order_value | Valor promedio de orden
```

### Tráfico Web
```r
variable | descripción
---------|------------
date | Fecha (YYYY-MM-DD)
visitors | Visitantes únicos
page_views | Vistas de página
bounce_rate | Tasa de rebote (%)
```

---

## 🎯 Modelos de Forecasting

### ARIMA (AutoRegressive Integrated Moving Average)

```
ARIMA(p, d, q):
  - p: Orden del componente AutoRegresivo
  - d: Grado de diferenciación (estacionariedad)
  - q: Orden del componente Moving Average

ARIMA con estacionalidad: ARIMA(p,d,q)(P,D,Q)[s]
  donde s = período estacional (7 para semanal, 12 para mensual)
```

### Auto ARIMA
```r
Búsqueda automática:
  - Rango de p: 0-5
  - Rango de d: 0-2
  - Rango de q: 0-5
  - Criterio: AICc (Akaike Information Criterion corrected)
```

### Prophet
```r
Componentes:
  - Trend: Crecimiento no lineal (logistic o linear)
  - Seasonality: Estacionalidad semanal, mensual, anual
  - Holidays: Efectos de días festivos
  - Changepoints: Puntos de cambio de tendencia
```

---

## 📈 Métricas de Evaluación de Forecast

| Métrica | Descripción | Fórmula | Interpretación |
|---------|-------------|---------|----------------|
| **MAE** | Error Absoluto Medio | Σ|y-ŷ|/n | Error promedio en unidades |
| **RMSE** | Raíz Error Cuadrático Medio | √(Σ(y-ŷ)²/n) | Penaliza errores grandes |
| **MAPE** | Error Porcentual Absoluto Medio | 100% × Σ|y-ŷ|/y | Error en porcentaje |
| **MASE** | MAPE Escalado | MAE / MAE(naive) | < 1 = mejor que naive |

---

## 📊 Visualizaciones Generadas

1. **Time Series Decomposition** - Tendencia, estacionalidad, residuo
2. **ACF/PACF** - Funciones de autocorrelación para ARIMA
3. **ARIMA Forecast** - Pronóstico con intervalos de confianza
4. **Prophet Forecast** - Pronóstico con componentes
5. **Model Comparison** - Comparativa de modelos

---

## 👨‍💻 Desarrollado por Isaac Esteban Haro Torres

**Ingeniero en Sistemas · Full Stack Developer · Automatización · Data**

### 📞 Contacto

- 📧 **Email:** zackharo1@gmail.com
- 📱 **WhatsApp:** [+593 988055517](https://wa.me/593988055517)
- 💻 **GitHub:** [ieharo1](https://github.com/ieharo1)
- 🌐 **Portafolio:** [ieharo1.github.io](https://ieharo1.github.io/portafolio-isaac.haro/)

---

## 📄 Licencia

© 2026 Isaac Esteban Haro Torres - Todos los derechos reservados.

---

⭐ Si te gustó el proyecto, ¡dame una estrella en GitHub!
