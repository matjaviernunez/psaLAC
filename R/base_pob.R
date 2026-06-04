#' Base de población de referencia para psaLAC
#'
#' @description
#' Conjunto de datos de población proyectada para Ecuador, desagregada
#' por año, primera división territorial (`div1`), área de residencia,
#' sexo y edad simple. Sirve como denominador poblacional en el cálculo
#' de los indicadores de `psa_dinam_demogra()` (tasas de fecundidad,
#' mortalidad y migración).
#'
#' A diferencia de `base_ag`, esta base cubre las 24 provincias del
#' Ecuador y utiliza los mismos nombres de columna que los argumentos
#' por defecto de `psa_dinam_demogra()` (`var_anio = "anio"`,
#' `var_pob = "poblacion"`, etc.), facilitando su uso directo sin
#' necesidad de renombrar columnas.
#'
#' @format Un `data.frame` con 214,656 filas y 6 columnas:
#' \describe{
#'   \item{anio}{`integer`. Año calendario. Rango: 2010-2035.}
#'   \item{div1}{`integer`. Código numérico de la primera división
#'         territorial (provincia) del Ecuador (1 a 24), siguiendo
#'         la codificación oficial del INEC.}
#'   \item{area}{`integer`. Área de residencia: `1` = Urbano,
#'         `2` = Rural.}
#'   \item{sexo}{`integer`. Sexo registrado al nacer: `1` = Hombre,
#'         `2` = Mujer.}
#'   \item{edad}{`integer`. Edad simple en años cumplidos. Rango: 0-85,
#'         donde 85 agrupa a la población de 85 años y más.}
#'   \item{poblacion}{`integer`. Número estimado de personas para la
#'         combinación `anio`-`div1`-`area`-`sexo`-`edad`.}
#' }
#'
#' @source Tabulados provinciales de edad simple 1990-2035, revisión
#'   2024. Instituto Nacional de Estadística y Censos (INEC), Ecuador.
#'
#' @usage data(base_pob)
#'
#' @examples
#' data(base_pob)
#' head(base_pob)
#' # Población total nacional por año
#' aggregate(poblacion ~ anio, data = base_pob, FUN = sum)
#'
#' @keywords datasets
#' @docType data
#' @name base_pob
NULL
