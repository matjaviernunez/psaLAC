#' @import data.table
#' @import dplyr
#' @import tidyr
#' @import ggplot2
#' @import scales
#' @importFrom stats as.formula loess predict
#' @importFrom utils head
#'
#' @title Calcular indicadores de dinámica demográfica para el ASP
#'
#' @description
#' `psa_dinam_demogra()` genera, en una sola ejecución, los indicadores
#' de la sección **Dinámica Demográfica** del Análisis de Situación
#' Poblacional (ASP) propuesto por UNFPA-LACRO. A partir de una base
#' de población (`data_pob`) y, de forma opcional, bases de nacimientos
#' (`data_nac`), defunciones (`data_def`) y migración (`data_mig`), la
#' función produce tablas y gráficos `ggplot2` para los siguientes
#' bloques de indicadores:
#'
#' * **Fecundidad** (si se proporciona `data_nac`): Tasa Específica de
#'   Fecundidad (TEF) y Tasa Global de Fecundidad (TGF).
#' * **Mortalidad** (si se proporciona `data_def`): Tasa Bruta de
#'   Mortalidad (TBM), tabla de vida abreviada y esperanza de vida al
#'   nacer (e₀).
#' * **Migración** (si se proporciona `data_mig`): Saldo Migratorio
#'   Neto (SMN) y Tasa Bruta de Migración (TBMig). El análisis de
#'   migración es siempre a nivel nacional.
#'
#' Los bloques son independientes: se puede ejecutar cualquier
#' combinación de ellos pasando o no cada base opcional.
#'
#' @details
#' **Preprocesamiento de nacimientos (`data_nac`):** la columna de edad
#' de la madre se convierte a numérica; las etiquetas de edad
#' desconocida definidas en `etiq_na_nac` se tratan como `NA` y se
#' redistribuyen uniformemente entre los grupos de edad conocidos
#' mediante el Método de Hamilton (Largest Remainder), preservando el
#' total de nacimientos por grupo.
#'
#' **Preprocesamiento de defunciones (`data_def`):** las defunciones
#' cuya unidad de edad (`var_cod_edad`) corresponde a horas, meses o
#' días (definidas en `cod_edad_a_cero`) se reclasifican a edad 0.
#' Los `NA` en edad se imputan también con el Método de Hamilton.
#'
#' **Cálculos clave:**
#' * **TEF:** \eqn{TEF_x = N_x / P_x^f}, donde \eqn{N_x} son los
#'   nacimientos de madres con edad \eqn{x} y \eqn{P_x^f} es la
#'   población femenina en edad \eqn{x}.
#' * **TGF:** \eqn{TGF = \sum_{x=15}^{49} TEF_x}.
#' * **TBM:** \eqn{TBM = (D / P) \times 1000}, donde \eqn{D} son
#'   defunciones totales y \eqn{P} la población total.
#' * **Tabla de vida:** construida con el método de Coale-Demeny /
#'   Preston. Las tasas de mortalidad por edad (`mx`) se suavizan
#'   opcionalmente con LOESS antes del cálculo.
#' * **SMN:** \eqn{SMN = Entradas - Salidas}.
#' * **TBMig:** \eqn{TBMig = ((Entradas + Salidas) / P) \times 1000}.
#'
#' **Programación defensiva:** antes de iniciar los cálculos se
#' verifica que `data_pob` no sea nulo, que sea un `data.frame`, que
#' no esté vacío y que contenga las columnas requeridas con los tipos
#' esperados. Se aplican validaciones equivalentes a cada base
#' opcional cuando se proporcionan. Los mensajes de error están en
#' español.
#'
#' @param data_pob `data.frame` o `data.table` con la población de
#'   referencia. Debe contener al menos las columnas indicadas por
#'   `var_anio`, `var_sexo`, `var_edad` y `var_pob` (y `var_div` si
#'   se solicita nivel subnacional). Se usa como denominador para el
#'   cálculo de tasas de fecundidad y mortalidad.
#' @param data_nac `data.frame` o `data.table` con microdatos de
#'   nacimientos (un registro por evento) o `NULL` (valor por
#'   defecto). Debe contener las columnas `var_anio`, `var_edad` (edad
#'   de la madre) y, si aplica, `var_div`. Si es `NULL`, el bloque de
#'   fecundidad se omite.
#' @param data_def `data.frame` o `data.table` con microdatos de
#'   defunciones o `NULL` (valor por defecto). Debe contener las
#'   columnas `var_anio`, `var_sexo`, `var_edad`, `var_cod_edad` y,
#'   si aplica, `var_div`. Si es `NULL`, el bloque de mortalidad se
#'   omite.
#' @param data_mig `data.frame` o `data.table` con registros de
#'   movimientos migratorios o `NULL` (valor por defecto). Debe
#'   contener las columnas `var_anio` y `var_tipo_mov`. El análisis
#'   de migración es siempre a nivel nacional. Si es `NULL`, el bloque
#'   de migración se omite.
#' @param var_anio `character` de longitud 1. Nombre de la columna de
#'   año en todas las bases. Por defecto `"anio"`.
#' @param var_div `character` de longitud 1 o `NULL`. Nombre de la
#'   columna de primera división territorial (provincia, departamento,
#'   etc.). Si es `NULL` (valor por defecto), los resultados se
#'   reportan a nivel nacional.
#' @param var_sexo `character` de longitud 1. Nombre de la columna de
#'   sexo en `data_pob` y `data_def`. Por defecto `"sexo"`.
#' @param var_edad `character` de longitud 1. Nombre de la columna de
#'   edad en todas las bases. Por defecto `"edad"`.
#' @param var_pob `character` de longitud 1. Nombre de la columna de
#'   población en `data_pob` (`integer` o `numeric` no negativa). Por
#'   defecto `"poblacion"`.
#' @param var_cod_edad `character` de longitud 1. Nombre de la columna
#'   que indica la unidad de medida de la edad en `data_def`
#'   (p. ej. `"Años"`, `"Meses"`, `"Días"`, `"Horas"`). Por defecto
#'   `"cod_edad"`.
#' @param var_tipo_mov `character` de longitud 1. Nombre de la columna
#'   que identifica el tipo de movimiento migratorio en `data_mig`.
#'   Por defecto `"tipo_mov"`.
#' @param cod_hombre Código que identifica al sexo masculino en la
#'   columna `var_sexo`. Por defecto `1`.
#' @param cod_mujer Código que identifica al sexo femenino en la
#'   columna `var_sexo`. Por defecto `2`.
#' @param cod_entrada Código de entrada (inmigración) en la columna
#'   `var_tipo_mov`. Por defecto `"1"`.
#' @param cod_salida Código de salida (emigración) en la columna
#'   `var_tipo_mov`. Por defecto `"2"`.
#' @param edad_fec_min `integer`. Límite inferior del rango de edad
#'   fértil. Por defecto `15`.
#' @param edad_fec_max `integer`. Límite superior del rango de edad
#'   fértil. Por defecto `49`.
#' @param edad_max `integer`. Edad máxima para agrupar defunciones en
#'   un intervalo abierto. Por defecto `85`.
#' @param radix `integer`. Raíz de la tabla de vida (cohorte
#'   hipotética inicial). Por defecto `100000`.
#' @param loess_span `numeric`. Parámetro de suavizado LOESS para las
#'   tasas de mortalidad por edad (`mx`). Valores más pequeños
#'   producen ajustes más flexibles. Por defecto `0.25`.
#' @param suavizar_mx `logical`. Si `TRUE` (valor por defecto),
#'   suaviza las tasas `mx` con LOESS antes de calcular la tabla de
#'   vida. Si `FALSE`, usa las tasas crudas.
#' @param anios_excluir `integer` vector o `NULL`. Años que se
#'   excluyen del análisis en `data_nac`, `data_def` y `data_mig`
#'   antes de los cálculos. Por defecto `NULL` (ningún año excluido).
#' @param etiq_na_nac `character` vector. Etiquetas en la columna
#'   `var_edad` de `data_nac` que representan edad desconocida. Por
#'   defecto `c("perdido", "Sin información")`.
#' @param cod_edad_a_cero `character` vector. Valores de `var_cod_edad`
#'   en `data_def` que se reclasifican a edad 0 (fallecidos en horas,
#'   meses o días de vida). Por defecto `c("Horas", "Meses", "Días")`.
#'
#' @return Una `list` con hasta tres elementos según las bases
#'   proporcionadas:
#'   \describe{
#'     \item{`$fecundidad`}{`list` (presente si `data_nac` no es
#'       `NULL`) con:
#'       \describe{
#'         \item{`tabla_tef`}{`data.table` con las Tasas Específicas
#'           de Fecundidad por edad (y territorio si aplica).}
#'         \item{`tabla_tgf`}{`data.table` con la Tasa Global de
#'           Fecundidad por año (y territorio si aplica).}
#'         \item{`grafico_tef`}{`ggplot` con las TEF por edad y año.}
#'         \item{`grafico_tgf`}{`ggplot` con la TGF por año.}
#'       }}
#'     \item{`$mortalidad`}{`list` (presente si `data_def` no es
#'       `NULL`) con:
#'       \describe{
#'         \item{`tabla_tbm`}{`data.table` con la Tasa Bruta de
#'           Mortalidad por año y sexo.}
#'         \item{`tabla_vida`}{`data.table` con la tabla de vida
#'           completa (mx, qx, lx, dx, Lx, Tx, ex) por año y sexo.}
#'         \item{`grafico_tbm`}{`ggplot` con la TBM por año y sexo.}
#'         \item{`grafico_e0`}{`ggplot` con la esperanza de vida al
#'           nacer por año y sexo.}
#'       }}
#'     \item{`$migracion`}{`list` (presente si `data_mig` no es
#'       `NULL`) con:
#'       \describe{
#'         \item{`tabla_migracion`}{`data.table` con entradas, salidas,
#'           SMN y TBMig por año.}
#'         \item{`grafico_smn`}{`ggplot` con el Saldo Migratorio Neto
#'           por año.}
#'         \item{`grafico_tbmig`}{`ggplot` con la Tasa Bruta de
#'           Migración por año.}
#'       }}
#'   }
#'
#' @author Ángel Gaibor \email{mat.angel.gaibor@gmail.com}
#' @author Javier Núñez \email{mat.javier.nunez@gmail.com}
#'
#' @references
#' UNFPA-LACRO (2025). *Consultoría para el desarrollo y programación
#' de la librería regional en lenguaje R para la generación del Análisis
#' de la Situación de Población (ASP) — Producto 2*.
#'
#' SNP, INEC, USFQ & UNFPA Ecuador (2025). *Análisis de Situación
#' Poblacional del Ecuador 2024-2025*.
#'
#' Preston, S., Heuveline, P. & Guillot, M. (2001). *Demography:
#' Measuring and Modeling Population Processes*. Blackwell Publishers.
#'
#' @export
#'
#' @examples
#' \donttest{
#' data(base_pob)
#' data(base_nac)
#' data(base_def)
#' data(base_mig)
#'
#' # Solo fecundidad (nivel nacional)
#' res_fec <- psa_dinam_demogra(
#'   data_pob = base_pob,
#'   data_nac = base_nac
#' )
#' res_fec$fecundidad$tabla_tgf
#' res_fec$fecundidad$grafico_tef
#'
#' # Fecundidad y mortalidad (nivel nacional)
#' res_fm <- psa_dinam_demogra(
#'   data_pob = base_pob,
#'   data_nac = base_nac,
#'   data_def = base_def
#' )
#' res_fm$mortalidad$grafico_e0
#'
#' # Los tres bloques a nivel subnacional (por div1)
#' res_comp <- psa_dinam_demogra(
#'   data_pob = base_pob,
#'   data_nac = base_nac,
#'   data_def = base_def,
#'   data_mig = base_mig,
#'   var_div  = "div1"
#' )
#' res_comp$fecundidad$grafico_tgf
#' res_comp$mortalidad$tabla_vida
#' res_comp$migracion$tabla_migracion
#' }

psa_dinam_demogra <- function(
    data_pob,
    data_nac        = NULL,
    data_def        = NULL,
    data_mig        = NULL,

    var_anio        = "anio",
    var_div         = NULL,
    var_sexo        = "sexo",
    var_edad        = "edad",
    var_pob         = "poblacion",

    var_cod_edad    = "cod_edad",
    var_tipo_mov    = "tipo_mov",

    cod_hombre      = 1,
    cod_mujer       = 2,
    cod_entrada     = "1",
    cod_salida      = "2",

    edad_fec_min    = 15,
    edad_fec_max    = 49,
    edad_max        = 85,
    radix           = 100000,
    loess_span      = 0.25,
    suavizar_mx     = TRUE,

    anios_excluir   = NULL,
    etiq_na_nac     = c("perdido", "Sin información"),
    cod_edad_a_cero = c("Horas", "Meses", "Días")
) {

  # ============================================================
  # VALIDACIONES (programación defensiva — Fail Fast)
  # ============================================================

  if (is.null(data_pob))
    stop("psa_dinam_demogra(): el argumento 'data_pob' no puede ser NULL.",
         call. = FALSE)
  if (!is.data.frame(data_pob))
    stop("psa_dinam_demogra(): 'data_pob' debe ser un data.frame o data.table.",
         call. = FALSE)
  if (nrow(data_pob) == 0)
    stop("psa_dinam_demogra(): 'data_pob' no contiene filas; no es posible calcular indicadores.",
         call. = FALSE)

  col_args <- list(var_anio = var_anio, var_sexo = var_sexo,
                   var_edad = var_edad, var_pob  = var_pob)
  if (!is.null(var_div)) col_args$var_div <- var_div
  for (nm in names(col_args)) {
    if (!is.character(col_args[[nm]]) || length(col_args[[nm]]) != 1)
      stop(sprintf(
        "psa_dinam_demogra(): '%s' debe ser un character de longitud 1 (nombre de columna).",
        nm), call. = FALSE)
  }

  cols_req_pob <- c(var_anio, var_sexo, var_edad, var_pob, var_div)
  cols_req_pob <- cols_req_pob[!sapply(cols_req_pob, is.null)]
  miss_pob     <- setdiff(cols_req_pob, names(data_pob))
  if (length(miss_pob) > 0)
    stop(sprintf(
      "psa_dinam_demogra(): las siguientes columnas no existen en 'data_pob': %s",
      paste(miss_pob, collapse = ", ")), call. = FALSE)

  if (!is.numeric(data_pob[[var_pob]]))
    stop(sprintf(
      "psa_dinam_demogra(): la columna '%s' (var_pob) debe ser numérica.", var_pob),
      call. = FALSE)
  if (!is.numeric(data_pob[[var_anio]]))
    stop(sprintf(
      "psa_dinam_demogra(): la columna '%s' (var_anio) debe ser numérica/entera.", var_anio),
      call. = FALSE)
  if (!is.numeric(data_pob[[var_edad]]))
    stop(sprintf(
      "psa_dinam_demogra(): la columna '%s' (var_edad) debe ser numérica/entera.", var_edad),
      call. = FALSE)

  if (!is.null(data_nac)) {
    if (!is.data.frame(data_nac))
      stop("psa_dinam_demogra(): 'data_nac' debe ser un data.frame o data.table.",
           call. = FALSE)
    if (nrow(data_nac) == 0)
      stop("psa_dinam_demogra(): 'data_nac' no contiene filas.", call. = FALSE)
    cols_nac <- c(var_anio, var_div, var_edad)
    cols_nac <- cols_nac[!sapply(cols_nac, is.null)]
    miss_nac <- setdiff(cols_nac, names(data_nac))
    if (length(miss_nac) > 0)
      stop(sprintf(
        "psa_dinam_demogra(): las siguientes columnas no existen en 'data_nac': %s",
        paste(miss_nac, collapse = ", ")), call. = FALSE)
  }

  if (!is.null(data_def)) {
    if (!is.data.frame(data_def))
      stop("psa_dinam_demogra(): 'data_def' debe ser un data.frame o data.table.",
           call. = FALSE)
    if (nrow(data_def) == 0)
      stop("psa_dinam_demogra(): 'data_def' no contiene filas.", call. = FALSE)
    cols_def <- c(var_anio, var_div, var_sexo, var_edad, var_cod_edad)
    cols_def <- cols_def[!sapply(cols_def, is.null)]
    miss_def <- setdiff(cols_def, names(data_def))
    if (length(miss_def) > 0)
      stop(sprintf(
        "psa_dinam_demogra(): las siguientes columnas no existen en 'data_def': %s",
        paste(miss_def, collapse = ", ")), call. = FALSE)
  }

  if (!is.null(data_mig)) {
    if (!is.data.frame(data_mig))
      stop("psa_dinam_demogra(): 'data_mig' debe ser un data.frame o data.table.",
           call. = FALSE)
    if (nrow(data_mig) == 0)
      stop("psa_dinam_demogra(): 'data_mig' no contiene filas.", call. = FALSE)
    cols_mig <- c(var_anio, var_tipo_mov)
    miss_mig <- setdiff(cols_mig, names(data_mig))
    if (length(miss_mig) > 0)
      stop(sprintf(
        "psa_dinam_demogra(): las siguientes columnas no existen en 'data_mig': %s",
        paste(miss_mig, collapse = ", ")), call. = FALSE)
  }

  # ================================================================
  # HELPERS COMPARTIDOS
  # ================================================================

  # Redondeo Hamilton (Largest Remainder Method)
  hamilton_round <- function(weights, total) {
    n  <- length(weights)
    if (n == 0 || total == 0)        return(rep(0L, n))
    sw <- sum(weights, na.rm = TRUE)
    if (!is.finite(sw) || sw == 0)   return(rep(0L, n))

    target    <- total * (weights / sw)
    base      <- floor(target)
    frac      <- target - base
    remainder <- as.integer(total - sum(base))

    if (remainder > 0L) {
      idx <- order(-frac, seq_len(n))[seq_len(remainder)]
      base[idx] <- base[idx] + 1L
    }
    as.integer(base)
  }

  # Imputación de filas con edad NA (preserva total exacto)
  impute_na_age <- function(dt, group_cols, var_e, var_count, metodo) {

    if (metodo == "none") return(dt)
    if (metodo == "drop") return(dt[!is.na(get(var_e))])

    dt[, na_value := sum(get(var_count)[is.na(get(var_e))]),
       by = group_cols]

    dt_known <- dt[!is.na(get(var_e))]

    if (metodo == "uniforme") {
      dt_known[, ajuste := hamilton_round(rep(1, .N), na_value[1L]),
               by = group_cols]
    } else if (metodo == "proporcional") {
      dt_known[, ajuste := hamilton_round(get(var_count), na_value[1L]),
               by = group_cols]
    }

    dt_known[, (var_count) := as.integer(get(var_count)) + ajuste]

    cols_aux <- intersect(c("na_value", "ajuste"), names(dt_known))
    dt_known[, (cols_aux) := NULL]

    return(dt_known)
  }

  # Tema gráfico consistente
  theme_psa <- function() {
    theme(
      plot.title            = element_text(size = 16, hjust = 0.5,
                                           face = "bold.italic",
                                           margin = margin(b = 14)),
      plot.subtitle         = element_text(size = 13, hjust = 0.5,
                                           face = "italic",
                                           margin = margin(b = 12)),
      axis.text             = element_text(size = 12, face = "italic"),
      axis.title            = element_text(size = 14, face = "bold"),
      axis.title.x          = element_text(margin = margin(t = 15)),
      axis.title.y          = element_text(margin = margin(r = 20)),
      axis.text.x           = element_text(vjust = 0.5, hjust = 0.5, angle = 90),
      legend.position       = "bottom",
      legend.box            = "horizontal",
      legend.title          = element_text(size = 16, face = "bold"),
      legend.justification  = "center",
      panel.grid.major      = element_blank(),
      panel.grid.minor      = element_blank(),
      axis.line             = element_line(color = "black", linewidth = 0.5),
      axis.ticks            = element_line(color = "black", linewidth = 0.5),
      axis.ticks.length     = unit(0.15, "cm"),
      plot.background       = element_rect(fill = "transparent", color = NA),
      panel.background      = element_rect(fill = "transparent", color = NA),
      strip.background      = element_rect(fill = "transparent", color = NA),
      strip.text            = element_text(face = "bold", size = 12),
      legend.background     = element_rect(fill = "transparent", color = NA),
      legend.box.background = element_rect(fill = "transparent", color = NA),
      legend.key            = element_rect(fill = "transparent", color = NA))
  }

  # ================================================================
  # FUNCIONES TEMÁTICAS (anidadas)
  # ================================================================

  # ---------------------------------------------------------------
  # prep_nac() — Preprocesamiento de nacimientos
  # ---------------------------------------------------------------
  prep_nac <- function(datos, var_anio, var_div, var_edad,
                       edad_fec_min, edad_fec_max,
                       etiq_na, anios_excluir,
                       metodo_imputacion = "uniforme") {

    dt <- as.data.table(copy(datos))

    if (!is.null(anios_excluir) && length(anios_excluir) > 0)
      dt <- dt[!(get(var_anio) %in% anios_excluir)]

    dt[, age_num := get(var_edad)]
    dt[get(var_edad) %in% etiq_na, age_num := NA]
    dt[, age_num := suppressWarnings(as.numeric(age_num))]

    dt[!is.na(age_num) & age_num <= edad_fec_min, age_num := edad_fec_min]
    dt[!is.na(age_num) & age_num >= edad_fec_max, age_num := edad_fec_max]

    group_full <- c(var_anio, var_div, "age_num")
    nac_agg    <- dt[, .(births = .N), by = group_full]
    setnames(nac_agg, "age_num", var_edad)

    group_cols <- c(var_anio, var_div)
    nac_agg    <- impute_na_age(nac_agg,
                                group_cols = group_cols,
                                var_e      = var_edad,
                                var_count  = "births",
                                metodo     = metodo_imputacion)

    setorderv(nac_agg, c(var_anio, var_div, var_edad))
    return(nac_agg)
  }

  # ---------------------------------------------------------------
  # prep_def() — Preprocesamiento de defunciones
  # ---------------------------------------------------------------
  prep_def <- function(datos, var_anio, var_div, var_sexo, var_edad,
                       var_cod_edad, cod_edad_a_cero, edad_max,
                       anios_excluir,
                       metodo_imputacion = "uniforme") {

    dt <- as.data.table(copy(datos))

    if (!is.null(anios_excluir) && length(anios_excluir) > 0)
      dt <- dt[!(get(var_anio) %in% anios_excluir)]

    dt[, age_clean := ifelse(get(var_cod_edad) %in% cod_edad_a_cero,
                             0,
                             suppressWarnings(as.numeric(get(var_edad))))]

    group_full <- c(var_anio, var_div, var_sexo, "age_clean")
    def_agg    <- dt[, .(deaths = .N), by = group_full]
    setnames(def_agg, "age_clean", var_edad)

    group_cols <- c(var_anio, var_div, var_sexo)
    def_agg    <- impute_na_age(def_agg,
                                group_cols = group_cols,
                                var_e      = var_edad,
                                var_count  = "deaths",
                                metodo     = metodo_imputacion)

    if (!is.null(edad_max)) {
      def_agg[!is.na(get(var_edad)) & get(var_edad) >= edad_max,
              (var_edad) := edad_max]
      def_agg <- def_agg[, .(deaths = sum(deaths)),
                         by = c(var_anio, var_div, var_sexo, var_edad)]
    }

    setorderv(def_agg, c(var_anio, var_div, var_sexo, var_edad))
    return(def_agg)
  }

  # ---------------------------------------------------------------
  # psa_fecundidad() — TEF, TGF y gráficos
  # ---------------------------------------------------------------
  psa_fecundidad <- function(data_nac, data_pob,
                             var_anio, var_div, var_edad, var_sexo, var_pob,
                             cod_mujer, edad_fec_min, edad_fec_max) {

    dt_nac <- as.data.table(copy(data_nac))
    dt_pob <- as.data.table(copy(data_pob))

    group_edad <- if (is.null(var_div)) c(var_anio, var_edad)
                  else                  c(var_anio, var_div, var_edad)

    pop_fem <- dt_pob[get(var_sexo) == cod_mujer &
                      get(var_edad) >= edad_fec_min &
                      get(var_edad) <= edad_fec_max,
                      .(pob_fem = sum(get(var_pob), na.rm = TRUE)),
                      by = group_edad]

    nac_agr <- dt_nac[, .(nacimientos = sum(births, na.rm = TRUE)),
                      by = group_edad]

    tef_dt <- merge(nac_agr, pop_fem, by = group_edad, all.x = TRUE)
    tef_dt[, tef := nacimientos / pob_fem]

    group_anio <- setdiff(group_edad, var_edad)
    tgf_dt     <- tef_dt[, .(tgf = sum(tef, na.rm = TRUE)), by = group_anio]

    min_anio <- min(tef_dt[[var_anio]], na.rm = TRUE)
    max_anio <- max(tef_dt[[var_anio]], na.rm = TRUE)

    graphs <- list()

    if (is.null(var_div)) {

      graphs[[1]] <- ggplot(tef_dt,
                            aes(x = get(var_edad), y = tef,
                                color = as.factor(get(var_anio)),
                                group = get(var_anio))) +
        geom_line(linewidth = 0.7, alpha = 0.7) +
        geom_point(size = 0.7) +
        scale_x_continuous(breaks = sort(unique(tef_dt[[var_edad]]))) +
        scale_y_continuous(expand = expansion(mult = c(0.05, 0.15))) +
        scale_color_viridis_d(option = "C", end = 0.9) +
        labs(title    = "Tasas Específicas de Fecundidad",
             subtitle = paste("Período", min_anio, "-", max_anio),
             x = "Edad", y = "Tasas Específicas de Fecundidad (TEF)",
             color = "Año") +
        theme_psa()

      graphs[[2]] <- ggplot(tgf_dt, aes(x = get(var_anio), y = tgf)) +
        geom_line(linewidth = 0.7, alpha = 0.7, color = "#8B008B") +
        geom_point(size = 1, color = "#8B008B") +
        geom_text(aes(label = round(tgf, 2)), vjust = -0.8, hjust = 0.5) +
        scale_x_continuous(breaks = sort(unique(tgf_dt[[var_anio]]))) +
        scale_y_continuous(expand = expansion(mult = c(0.05, 0.20))) +
        labs(title    = "Tasa Global de Fecundidad - Nacional",
             subtitle = paste("Período", min_anio, "-", max_anio),
             x = "Año", y = "Tasa Global de Fecundidad (TGF)") +
        theme_psa() +
        theme(axis.text.x = element_text(angle = 0))

    } else {

      graphs[[1]] <- ggplot(tef_dt,
                            aes(x = get(var_edad), y = tef,
                                color = as.factor(get(var_anio)),
                                group = get(var_anio))) +
        geom_line(linewidth = 0.7, alpha = 0.7) +
        geom_point(size = 0.7) +
        scale_color_viridis_d(option = "C", end = 0.9) +
        scale_x_continuous(breaks = seq(edad_fec_min, edad_fec_max, by = 5)) +
        scale_y_continuous(expand = expansion(mult = c(0.05, 0.15))) +
        facet_wrap(as.formula(paste("~", var_div)), scales = "free_y") +
        labs(title    = "Tasas Específicas de Fecundidad por Territorio",
             subtitle = paste("Período", min_anio, "-", max_anio),
             x = "Edad", y = "Tasas Específicas de Fecundidad (TEF)",
             color = "Año") +
        theme_psa() +
        theme(axis.text = element_text(size = 10))

      graphs[[2]] <- ggplot(tgf_dt,
                            aes(x = get(var_anio), y = tgf,
                                color = as.factor(get(var_div)),
                                group = as.factor(get(var_div)))) +
        geom_line(linewidth = 0.7, alpha = 0.7) +
        geom_point(size = 1) +
        facet_wrap(as.formula(paste("~", var_div)), scales = "free_y") +
        scale_x_continuous(breaks = seq(min_anio, max_anio, 1)) +
        scale_y_continuous(expand = expansion(mult = c(0.05, 0.20))) +
        scale_color_viridis_d(option = "C", end = 0.9) +
        labs(title    = "Tasa Global de Fecundidad por Territorio",
             subtitle = paste("Período", min_anio, "-", max_anio),
             x = "Año", y = "Tasa Global de Fecundidad (TGF)") +
        theme_psa() +
        theme(legend.position = "none",
              axis.text.x     = element_text(angle = 45))
    }

    tef_out <- tef_dt[, .SD, .SDcols = c(group_edad, "tef")]

    if (is.null(var_div)) {
      setorderv(tef_out, c(var_anio, var_edad))
      tef_out <- pivot_wider(tef_out,
                             names_from  = all_of(var_anio),
                             values_from = "tef") %>%
        setDT() %>%
        setorderv(var_edad)
      setorderv(tgf_dt, var_anio)
    } else {
      tef_out <- pivot_wider(tef_out,
                             names_from  = all_of(var_anio),
                             values_from = "tef") %>%
        setDT() %>%
        setorderv(c(var_div, var_edad))
      tgf_dt  <- pivot_wider(tgf_dt,
                              names_from  = all_of(var_anio),
                              values_from = "tgf") %>%
        setDT() %>%
        setorderv(var_div)
    }

    return(list(tabla_tef   = tef_out,
                tabla_tgf   = tgf_dt,
                grafico_tef = graphs[[1]],
                grafico_tgf = graphs[[2]]))
  }

  # ---------------------------------------------------------------
  # psa_mortalidad() — mx, TBM, tabla de vida y gráficos
  # ---------------------------------------------------------------
  psa_mortalidad <- function(data_def, data_pob,
                             var_anio, var_div, var_sexo, var_edad, var_pob,
                             cod_hombre, cod_mujer,
                             edad_max, radix, loess_span, suavizar_mx) {

    # Tabla de vida (Coale-Demeny / Preston)
    tabla_vida_dt <- function(dt, grupo_sexo, radix) {
      dt <- copy(dt)
      if (var_edad != "edad_tv") setnames(dt, var_edad, "edad_tv")

      setorder(dt, edad_tv)
      edad_max_loc <- dt[, max(edad_tv)]

      dt[, n  := fifelse(edad_tv == edad_max_loc, NA_real_, 1.0)]
      dt[, ax := NA_real_]

      if (grupo_sexo == cod_hombre) {
        dt[edad_tv == 0 & mx <  0.107, ax := 0.045 + 2.684 * mx]
        dt[edad_tv == 0 & mx >= 0.107, ax := 0.330]
      } else {
        dt[edad_tv == 0 & mx <  0.107, ax := 0.053 + 2.800 * mx]
        dt[edad_tv == 0 & mx >= 0.107, ax := 0.350]
      }
      dt[edad_tv > 0 & edad_tv < edad_max_loc, ax := 0.5]

      dt[, qx := fifelse(is.na(n), 1.0, (n * mx) / (1 + (n - ax) * mx))]
      dt[, qx := pmin(qx, 1.0, na.rm = TRUE)]

      dt[, lx := radix * cumprod(c(1, head(1 - qx, -1)))]
      dt[, dx := lx * qx]
      dt[, Lx := fifelse(is.na(n), lx / mx, lx - dx * ax)]
      dt[, Tx := rev(cumsum(rev(Lx)))]
      dt[, ex := Tx / lx]

      if (var_edad != "edad_tv") setnames(dt, "edad_tv", var_edad)
      return(dt)
    }

    dt_def <- as.data.table(copy(data_def))
    dt_pob <- as.data.table(copy(data_pob))

    group_edad    <- if (is.null(var_div)) c(var_anio, var_sexo, var_edad)
                     else                  c(var_anio, var_div, var_sexo, var_edad)
    group_no_edad <- setdiff(group_edad, var_edad)

    pop_edad <- dt_pob[, .(pop = sum(get(var_pob), na.rm = TRUE)),
                       by = group_edad]
    if (!is.null(edad_max)) {
      pop_edad[get(var_edad) >= edad_max, (var_edad) := edad_max]
      pop_edad <- pop_edad[, .(pop = sum(pop)), by = group_edad]
    }

    pop_tot <- dt_pob[, .(pop = sum(get(var_pob), na.rm = TRUE)),
                      by = group_no_edad]

    def_edad <- dt_def[, .(deaths = sum(deaths, na.rm = TRUE)),
                       by = group_edad]
    def_tot  <- dt_def[, .(deaths = sum(deaths, na.rm = TRUE)),
                       by = group_no_edad]

    mx_data <- merge(def_edad, pop_edad, by = group_edad, all.x = TRUE)
    mx_data[, mx := deaths / pop]

    if (isTRUE(suavizar_mx)) {
      form_loess   <- as.formula(paste("log(mx + 1e-9) ~", var_edad))
      smooth_group <- if (is.null(var_div)) c(var_anio, var_sexo)
                      else                   c(var_anio, var_div, var_sexo)

      mx_data[, mx := {
        fit <- loess(form_loess, data = .SD, span = loess_span)
        exp(predict(fit, newdata = .SD))
      }, by = smooth_group, .SDcols = c(var_edad, "mx")]
    }

    tbm_dt  <- merge(def_tot, pop_tot, by = group_no_edad, all.x = TRUE)
    tbm_dt[, tbm := (deaths / pop) * 1000]
    tbm_out <- tbm_dt[, .SD, .SDcols = c(group_no_edad, "tbm")]

    vida_group <- if (is.null(var_div)) c(var_anio, var_sexo)
                  else                   c(var_anio, var_div, var_sexo)

    tabla_vida <- mx_data[, tabla_vida_dt(.SD, .BY[[var_sexo]], radix),
                          by = vida_group,
                          .SDcols = c(var_edad, "mx")]

    e0_out <- tabla_vida[get(var_edad) == 0,
                         .SD, .SDcols = c(vida_group, "ex")]

    min_anio <- min(tbm_dt[[var_anio]], na.rm = TRUE)
    max_anio <- max(tbm_dt[[var_anio]], na.rm = TRUE)

    graphs     <- list()
    sex_colors <- c("#4D6291", "#CA60A7")
    names(sex_colors) <- as.character(c(cod_hombre, cod_mujer))

    if (is.null(var_div)) {

      graphs[[1]] <- ggplot(tbm_dt,
                            aes(x = get(var_anio), y = tbm,
                                color = as.factor(get(var_sexo)))) +
        geom_line(linewidth = 0.7, alpha = 0.7) +
        geom_point(size = 1) +
        geom_text(aes(label = round(tbm, 2)),
                  vjust = -0.8, hjust = 0.5, show.legend = FALSE) +
        scale_x_continuous(breaks = seq(min_anio, max_anio, 1)) +
        scale_y_continuous(expand = expansion(mult = c(0.05, 0.20))) +
        scale_color_manual(values = sex_colors,
                           labels = c("Hombres", "Mujeres")) +
        labs(title    = "Tasa Bruta de Mortalidad - Nacional",
             subtitle = paste("Período", min_anio, "-", max_anio),
             x = "Año", y = "Tasa Bruta de Mortalidad (por mil)",
             color = "Sexo") +
        theme_psa() +
        theme(axis.text.x = element_text(angle = 45))

      graphs[[2]] <- ggplot(e0_out,
                            aes(x = get(var_anio), y = ex,
                                color = as.factor(get(var_sexo)))) +
        geom_line(linewidth = 0.7, alpha = 0.7) +
        geom_point(size = 0.9) +
        geom_text(aes(label = round(ex, 2)),
                  vjust = -0.8, hjust = 0.5, show.legend = FALSE) +
        scale_x_continuous(breaks = seq(min_anio, max_anio, 1)) +
        scale_y_continuous(expand = expansion(mult = c(0.05, 0.20))) +
        scale_color_manual(values = sex_colors,
                           labels = c("Hombres", "Mujeres")) +
        labs(title    = "Esperanza de Vida al Nacer - Nacional",
             subtitle = paste("Período", min_anio, "-", max_anio),
             x = "Año", y = "Esperanza de vida (años)",
             color = "Sexo") +
        theme_psa() +
        theme(axis.text.x = element_text(angle = 45))

    } else {

      graphs[[1]] <- ggplot(tbm_dt,
                            aes(x = get(var_anio), y = tbm,
                                color = as.factor(get(var_sexo)))) +
        geom_line(linewidth = 0.7, alpha = 0.7) +
        geom_point(size = 1) +
        scale_x_continuous(breaks = seq(min_anio, max_anio, 1)) +
        scale_y_continuous(expand = expansion(mult = c(0.05, 0.15))) +
        scale_color_manual(values = sex_colors,
                           labels = c("Hombres", "Mujeres")) +
        facet_wrap(as.formula(paste("~", var_div)), scales = "free_y") +
        labs(title    = "Tasa Bruta de Mortalidad por Territorio",
             subtitle = paste("Período", min_anio, "-", max_anio),
             x = "Año", y = "Tasa Bruta de Mortalidad (por mil)",
             color = "Sexo") +
        theme_psa() +
        theme(axis.text.x = element_text(angle = 45))

      graphs[[2]] <- ggplot(e0_out,
                            aes(x = get(var_anio), y = ex,
                                color = as.factor(get(var_sexo)))) +
        geom_line(linewidth = 0.7, alpha = 0.7) +
        geom_point(size = 0.9) +
        scale_x_continuous(breaks = seq(min_anio, max_anio, 1)) +
        scale_y_continuous(expand = expansion(mult = c(0.05, 0.15))) +
        scale_color_manual(values = sex_colors,
                           labels = c("Hombres", "Mujeres")) +
        facet_wrap(as.formula(paste("~", var_div)), scales = "free_y") +
        labs(title    = "Esperanza de Vida al Nacer por Territorio",
             subtitle = paste("Período", min_anio, "-", max_anio),
             x = "Año", y = "Esperanza de vida (años)",
             color = "Sexo") +
        theme_psa() +
        theme(axis.text.x = element_text(angle = 45))
    }

    if (is.null(var_div)) {
      tbm_out <- pivot_wider(tbm_out,
                             names_from  = all_of(var_anio),
                             values_from = "tbm") %>%
        setDT() %>%
        setorderv(var_sexo)
      setorderv(tabla_vida, c(var_anio, var_sexo, var_edad))
    } else {
      tbm_out <- pivot_wider(tbm_out,
                             names_from  = all_of(var_anio),
                             values_from = "tbm") %>%
        setDT() %>%
        setorderv(c(var_div, var_sexo))
      setorderv(tabla_vida, c(var_anio, var_div, var_sexo, var_edad))
    }

    return(list(tabla_tbm   = tbm_out,
                tabla_vida  = tabla_vida,
                grafico_tbm = graphs[[1]],
                grafico_e0  = graphs[[2]]))
  }

  # ---------------------------------------------------------------
  # psa_migracion() — SMN, TBMig y gráficos (solo nacional)
  # ---------------------------------------------------------------
  psa_migracion <- function(data_mig, data_pob,
                            var_anio, var_tipo_mov, var_pob,
                            cod_entrada, cod_salida,
                            anios_excluir = NULL) {

    dt_mig <- as.data.table(copy(data_mig))
    dt_pob <- as.data.table(copy(data_pob))

    if (!is.null(anios_excluir) && length(anios_excluir) > 0) {
      dt_mig <- dt_mig[!(get(var_anio) %in% anios_excluir)]
      dt_pob <- dt_pob[!(get(var_anio) %in% anios_excluir)]
    }

    mig_count <- dt_mig[, .(entries    = sum(get(var_tipo_mov) == cod_entrada,
                                             na.rm = TRUE),
                            departures = sum(get(var_tipo_mov) == cod_salida,
                                             na.rm = TRUE)),
                        by = var_anio]

    pop_anio <- dt_pob[, .(pop = sum(get(var_pob), na.rm = TRUE)),
                       by = var_anio]

    mig_tab <- merge(mig_count, pop_anio, by = var_anio, all.x = TRUE)
    mig_tab[, smn   := entries - departures]
    mig_tab[, tbmig := ((entries + departures) / pop) * 1000]

    tabla_migracion <- mig_tab[, .SD,
                                .SDcols = c(var_anio, "entries", "departures",
                                            "smn", "tbmig")]
    setorderv(tabla_migracion, var_anio)

    min_anio <- min(tabla_migracion[[var_anio]], na.rm = TRUE)
    max_anio <- max(tabla_migracion[[var_anio]], na.rm = TRUE)

    g_smn <- ggplot(tabla_migracion, aes(x = get(var_anio), y = smn)) +
      geom_line(linewidth = 0.7, alpha = 0.7, color = "#891171") +
      geom_point(size = 0.9, color = "#891171") +
      geom_text(aes(label = round(smn, 2)),
                vjust = -0.8, hjust = 0.5, show.legend = FALSE) +
      scale_x_continuous(breaks = seq(min_anio, max_anio, 1)) +
      scale_y_continuous(expand = expansion(mult = c(0.05, 0.20))) +
      labs(title    = "Saldo Migratorio Neto - Nacional",
           subtitle = paste("Período", min_anio, "-", max_anio),
           x = "Año", y = "Saldo Migratorio Neto") +
      theme_psa() +
      theme(axis.text.x = element_text(angle = 45))

    g_tbmig <- ggplot(tabla_migracion, aes(x = get(var_anio), y = tbmig)) +
      geom_line(linewidth = 0.7, alpha = 0.7, color = "#891171") +
      geom_point(size = 0.9, color = "#891171") +
      geom_text(aes(label = round(tbmig, 2)),
                vjust = -0.8, hjust = 0.5, show.legend = FALSE) +
      scale_x_continuous(breaks = seq(min_anio, max_anio, 1)) +
      scale_y_continuous(expand = expansion(mult = c(0.05, 0.20))) +
      labs(title    = "Tasa Bruta de Migración - Nacional",
           subtitle = paste("Período", min_anio, "-", max_anio),
           x = "Año", y = "Tasa Bruta de Migración (por mil)") +
      theme_psa() +
      theme(axis.text.x = element_text(angle = 45))

    return(list(tabla_migracion = tabla_migracion,
                grafico_smn     = g_smn,
                grafico_tbmig   = g_tbmig))
  }

  # ================================================================
  # LÓGICA DEL ORQUESTADOR
  # ================================================================

  result <- list()

  # ----- 1. FECUNDIDAD -----
  if (!is.null(data_nac)) {
    nac_prep <- prep_nac(
      datos             = data_nac,
      var_anio          = var_anio,
      var_div           = var_div,
      var_edad          = var_edad,
      edad_fec_min      = edad_fec_min,
      edad_fec_max      = edad_fec_max,
      etiq_na           = etiq_na_nac,
      anios_excluir     = anios_excluir,
      metodo_imputacion = "uniforme"
    )

    result$fecundidad <- psa_fecundidad(
      data_nac     = nac_prep,
      data_pob     = data_pob,
      var_anio     = var_anio,
      var_div      = var_div,
      var_edad     = var_edad,
      var_sexo     = var_sexo,
      var_pob      = var_pob,
      cod_mujer    = cod_mujer,
      edad_fec_min = edad_fec_min,
      edad_fec_max = edad_fec_max
    )
  }

  # ----- 2. MORTALIDAD -----
  if (!is.null(data_def)) {
    def_prep <- prep_def(
      datos             = data_def,
      var_anio          = var_anio,
      var_div           = var_div,
      var_sexo          = var_sexo,
      var_edad          = var_edad,
      var_cod_edad      = var_cod_edad,
      cod_edad_a_cero   = cod_edad_a_cero,
      edad_max          = edad_max,
      anios_excluir     = anios_excluir,
      metodo_imputacion = "uniforme"
    )

    result$mortalidad <- psa_mortalidad(
      data_def    = def_prep,
      data_pob    = data_pob,
      var_anio    = var_anio,
      var_div     = var_div,
      var_sexo    = var_sexo,
      var_edad    = var_edad,
      var_pob     = var_pob,
      cod_hombre  = cod_hombre,
      cod_mujer   = cod_mujer,
      edad_max    = edad_max,
      radix       = radix,
      loess_span  = loess_span,
      suavizar_mx = suavizar_mx
    )
  }

  # ----- 3. MIGRACIÓN (siempre nacional) -----
  if (!is.null(data_mig)) {
    result$migracion <- psa_migracion(
      data_mig      = data_mig,
      data_pob      = data_pob,
      var_anio      = var_anio,
      var_tipo_mov  = var_tipo_mov,
      var_pob       = var_pob,
      cod_entrada   = cod_entrada,
      cod_salida    = cod_salida,
      anios_excluir = anios_excluir
    )
  }

  return(result)
}
