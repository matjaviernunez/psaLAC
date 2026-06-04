#' Base de migración de ejemplo para psaLAC
#'
#' @description
#' Conjunto de datos de registros de movimientos migratorios
#' internacionales del Ecuador, desagregados por tipo de movimiento,
#' año, sexo y edad. Sirve como insumo para calcular los indicadores
#' de migración mediante `psa_dinam_demogra()`.
#'
#' Cada fila representa un movimiento migratorio registrado (entrada
#' o salida). El análisis de migración se realiza siempre a nivel
#' nacional; esta base no incluye desagregación territorial.
#'
#' @format Un `data.frame` con 60,000 filas y 4 columnas:
#' \describe{
#'   \item{tipo_mov}{`character`. Tipo de movimiento migratorio:
#'         `"1"` = entrada (inmigración), `"2"` = salida (emigración).}
#'   \item{anio}{`integer`. Año de registro del movimiento.
#'         Rango: 2022-2024.}
#'   \item{sexo}{`integer`. Sexo del migrante: `1` = Hombre,
#'         `2` = Mujer.}
#'   \item{edad}{`integer`. Edad del migrante en años cumplidos.
#'         Rango: 0-97.}
#' }
#'
#' @source Registros de movimientos migratorios internacionales,
#'   Instituto Nacional de Estadística y Censos (INEC), Ecuador.
#'   Años 2022-2024.
#'
#' @usage data(base_mig)
#'
#' @examples
#' data(base_mig)
#' head(base_mig)
#' # Entradas y salidas por año
#' table(base_mig$anio, base_mig$tipo_mov)
#'
#' @keywords datasets
#' @docType data
#' @name base_mig
NULL
