# psaLAC 0.2.0

# psaLAC 0.1.0

Segunda versión del paquete, con la función de dinámica demográfica,
nuevos datasets, documentación completa y estandarización del API en español.

## Funciones nuevas

* `psa_dinam_demogra()` — calcula los indicadores de la sección
  **Dinámica Demográfica** del ASP a partir de bases de nacimientos,
  defunciones y migración:
  - Tasa Específica de Fecundidad (TEF) y Tasa Global de Fecundidad (TGF).
  - Tasa Bruta de Mortalidad (TBM), tabla de vida abreviada y esperanza
    de vida al nacer (e₀).
  - Saldo Migratorio Neto (SMN) y Tasa Bruta de Migración (TBMig).

  Los tres bloques son independientes y opcionales. Soporta nivel nacional
  y subnacional (fecundidad y mortalidad). Devuelve una lista con tablas
  y gráficos `ggplot` por bloque.

## Datos nuevos

* `base_nac` — microdatos de nacimientos del Ecuador (2022-2024), con
  año, primera división territorial (`div1`) y edad de la madre.
* `base_def` — microdatos de defunciones del Ecuador (2022-2024), con
  año, `div1`, sexo, edad y unidad de medida de la edad (`cod_edad`).
* `base_mig` — registros de movimientos migratorios del Ecuador
  (2022-2024), con tipo de movimiento, año, sexo y edad.
* `base_pob` — población proyectada del Ecuador (2010-2035), equivalente
  a `base_ag` con nombres de columna en español y las 24 provincias.

## Cambios de API (breaking changes)

* Todos los argumentos de `psa_context()` y `psa_dinam_demogra()` ahora
  están en español: `var_pob`, `var_anio`, `var_sexo`, `var_edad`,
  `var_div`, `var_area`, `anio_piramide`.
* La columna `provincia` de `base_ag` fue renombrada a `div1` para
  consistencia con los demás datasets.
* Los valores por defecto de `psa_dinam_demogra()` coinciden con los
  nombres de columna de los datasets incluidos en el paquete.

## Documentación

* Documentación roxygen completa para `psa_dinam_demogra()` y los cuatro
  nuevos datasets.
* Documentación de `base_ag` y `psa_context()` actualizada para reflejar
  el renombrado de columnas y argumentos.

## Aseguramiento de calidad

* 49 pruebas unitarias nuevas para `psa_dinam_demogra()` (camino feliz,
  casos borde y errores controlados). Total: 80 pruebas, 0 fallos.
* Corrección de deprecaciones de ggplot2 (`label.size`) y tidyselect
  (`all_of()`) en `psa_context()`.

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
