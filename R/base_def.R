#' Base de defunciones de ejemplo para psaLAC
#'
#' @description
#' Conjunto de datos de registros de defunciones del Ecuador,
#' desagregados por año, primera división territorial (`div1`), sexo,
#' edad y unidad de medida de la edad (`cod_edad`). Sirve como insumo
#' para calcular los indicadores de mortalidad mediante
#' `psa_dinam_demogra()`.
#'
#' Cada fila representa una defunción registrada. La columna
#' `cod_edad` indica la unidad en que se expresa `edad`; los registros
#' con unidad distinta de `"Años"` (p. ej. fallecidos en horas o meses
#' de vida) son reclasificados a edad 0 por la función antes del
#' cálculo de indicadores.
#'
#' @format Un `data.frame` con 132,086 filas y 5 columnas:
#' \describe{
#'   \item{anio}{`integer`. Año de registro de la defunción.
#'         Rango: 2022-2024.}
#'   \item{div1}{`integer`. Código numérico de la primera división
#'         territorial (provincia) del Ecuador (1 a 17), siguiendo
#'         la codificación oficial del INEC.}
#'   \item{sexo}{`integer`. Sexo del fallecido: `1` = Hombre,
#'         `2` = Mujer.}
#'   \item{edad}{`integer`. Edad del fallecido expresada en la unidad
#'         indicada por `cod_edad`. Rango: 0-120.}
#'   \item{cod_edad}{`character`. Unidad de medida de `edad`.
#'         Valores posibles: `"Años"`, `"Meses"`, `"Días"`,
#'         `"Horas"`, `"Sin información"`.}
#' }
#'
#' @source Registros vitales de defunciones, Instituto Nacional de
#'   Estadística y Censos (INEC), Ecuador. Años 2022-2024.
#'
#' @usage data(base_def)
#'
#' @examples
#' data(base_def)
#' head(base_def)
#' # Defunciones por año y sexo
#' table(base_def$anio, base_def$sexo)
#'
#' @keywords datasets
#' @docType data
#' @name base_def
NULL
