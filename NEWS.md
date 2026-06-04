# psaLAC 0.1.0

# psaLAC 0.0.1

Primera versión del paquete `psaLAC`, desarrollada en el marco de la
consultoría UNFPA-LACRO para la librería regional del Análisis de
Situación Poblacional (ASP).

## Funciones nuevas

* `psa_context()` — calcula los indicadores macrodemográficos de la
  sección **Contexto País** del ASP:
  - Población total.
  - Tasa anual de crecimiento poblacional (por mil habitantes).
  - Estructura por grupos quinquenales de edad (pirámide poblacional).
  - Razón de dependencia con identificación del bono demográfico.
  - Urbanización (proporción urbana/rural).

  Soporta ejecución a nivel nacional (`var_terr = NULL`) y subnacional
  (cualquier columna territorial provista en `var_terr`). Devuelve un
  objeto `list` con cinco tablas (`data.table`) y cinco gráficos
  (`ggplot`).

## Datos nuevos

* `base_ag` — base de población proyectada del Ecuador (2010-2035),
  desagregada por año, provincia, área de residencia, sexo y edad
  simple. Incluida con `LazyData: true` para facilitar la ejecución de
  los ejemplos.

## Documentación

* Documentación completa en español (roxygen2) para `psa_context()` y
  `base_ag`.
* `README` actualizado con instrucciones de instalación, uso básico y
  estructura de insumos.

## Aseguramiento de calidad

* Validación defensiva de inputs en `psa_context()` (verificación de
  `data`, nombres de columnas y tipos esperados) con mensajes de error
  en español.
* Pruebas unitarias iniciales con `testthat` (camino feliz, casos borde
  y errores controlados).
