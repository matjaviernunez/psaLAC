#' Base de nacimientos de ejemplo para psaLAC
#'
#' @description
#' Conjunto de datos de registros de nacimientos del Ecuador, con la
#' edad de la madre al momento del parto y la primera división
#' territorial (`div1`). Sirve como insumo para calcular los
#' indicadores de fecundidad mediante `psa_dinam_demogra()`.
#'
#' Cada fila representa un nacimiento registrado. La columna `edad`
#' contiene la edad de la madre como texto; los valores no numéricos
#' (p. ej. `"Sin información"`) son imputados internamente por la
#' función antes del cálculo de indicadores.
#'
#' @format Un `data.frame` con 312,726 filas y 3 columnas:
#' \describe{
#'   \item{anio}{`integer`. Año de registro del nacimiento.
#'         Rango: 2022-2024.}
#'   \item{div1}{`integer`. Código numérico de la primera división
#'         territorial (provincia) del Ecuador (1 a 17), siguiendo
#'         la codificación oficial del INEC.}
#'   \item{edad}{`character`. Edad de la madre en años cumplidos.
#'         Puede contener valores numéricos (`"15"`, `"32"`, etc.) o
#'         `"Sin información"` para registros sin dato de edad.}
#' }
#'
#' @source Registros vitales de nacimientos, Instituto Nacional de
#'   Estadística y Censos (INEC), Ecuador. Años 2022-2024.
#'
#' @usage data(base_nac)
#'
#' @examples
#' data(base_nac)
#' head(base_nac)
#' # Nacimientos por año
#' table(base_nac$anio)
#'
#' @keywords datasets
#' @docType data
#' @name base_nac
NULL
