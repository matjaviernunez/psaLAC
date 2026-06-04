#' @import data.table
#' @import dplyr
#' @import tidyr
#' @import janitor
#' @import ggplot2
#' @import scales
#' @import paletteer
#' @import shadowtext
#'
#' @title Calcular indicadores de Contexto País del Análisis de Situación Poblacional
#'
#' @description
#' `psa_context()` genera, en una sola ejecución, el conjunto de
#' indicadores macrodemográficos que conforman la sección **Contexto
#' País** del Análisis de Situación Poblacional (ASP) propuesto por
#' UNFPA-LACRO. A partir de un `data.frame` (o `data.table`) con
#' población por año, sexo, edad y área de residencia —y, opcionalmente,
#' un nivel territorial subnacional— la función produce cinco tablas y
#' cinco gráficos `ggplot2` con los siguientes indicadores:
#'
#' 1. Población total (gráfico de barras).
#' 2. Tasa anual de crecimiento poblacional por mil habitantes
#'    (gráfico combinado de barras y líneas).
#' 3. Estructura por grupos quinquenales de edad — pirámide poblacional
#'    (gráfico de barras horizontales).
#' 4. Razón de dependencia, con identificación del período del bono
#'    demográfico (gráfico combinado de barras y líneas).
#' 5. Urbanización: población por área de residencia y proporción urbana
#'    (gráfico combinado de barras y líneas).
#'
#' Cuando se proporciona `var_div`, todas las salidas se desagregan al
#' nivel territorial indicado (provincia, departamento, región u otro),
#' produciendo `facets` por territorio en cada gráfico.
#'
#' @details
#' La función está pensada para ser ejecutada sobre una base de población
#' previamente armonizada. Para cada combinación de los argumentos
#' `var_anio`, `var_sexo`, `var_edad` y `var_area` se espera **una única
#' observación** con el conteo (o estimación) de población en
#' `var_pob`. Si se incluye `var_div`, la unidad de observación
#' adicional es el territorio.
#'
#' Cálculos clave:
#' * **Población total:** suma de `var_pob` por año (y territorio si
#'   aplica).
#' * **Tasa de crecimiento anual:** \eqn{((P_{t+1}-P_{t}) /
#'   ((P_{t+1}+P_{t})/2)) \times 1000}.
#' * **Grupos quinquenales:** la edad simple se agrupa en cortes
#'   `0-4`, `5-9`, ..., `85 y más`.
#' * **Razón de dependencia:** \eqn{((Pob_{<15} + Pob_{\ge 65}) /
#'   Pob_{15-64}) \times 100}. Se considera *bono demográfico* cuando
#'   este valor es inferior a `66.7`.
#' * **Pirámide:** se grafica la proporción de cada grupo quinquenal
#'   respecto del total del año seleccionado (`anio_piramide`).
#' * **Urbanización:** se calcula la proporción de población urbana
#'   sobre el total año a año.
#'
#' Validaciones (programación defensiva): antes de iniciar los cálculos
#' se verifica que `data` no sea nulo, que sea coercible a `data.table`
#' y que existan todas las columnas referidas por `var_pob`, `var_anio`,
#' `var_sexo`, `var_edad`, `var_area` y, si corresponde, `var_div`. Si
#' alguna validación falla, la función se detiene con un mensaje
#' explícito en español.
#'
#' @param data `data.frame` o `data.table` con la información
#'   poblacional. Debe contener al menos las columnas indicadas por
#'   `var_pob`, `var_anio`, `var_sexo`, `var_edad` y `var_area`. La
#'   función no agrega ni imputa registros faltantes: la base debe estar
#'   previamente armonizada.
#' @param var_pob `character` de longitud 1. Nombre de la columna en
#'   `data` que contiene la cantidad de población (`integer` o
#'   `numeric` no negativa).
#' @param var_div `character` de longitud 1 o `NULL`. Nombre de la
#'   columna que identifica el nivel territorial subnacional
#'   (provincia, departamento, etc.). Si es `NULL` (valor por defecto)
#'   los resultados se reportan únicamente a nivel nacional.
#' @param var_anio `character` de longitud 1. Nombre de la columna que
#'   contiene el año (`integer`). Para el cálculo de la tasa de
#'   crecimiento se requieren al menos dos años consecutivos.
#' @param var_sexo `character` de longitud 1. Nombre de la columna que
#'   identifica el sexo. Debe estar codificada como `1` = Hombre,
#'   `2` = Mujer. La pirámide poblacional asume esta codificación.
#' @param var_edad `character` de longitud 1. Nombre de la columna con
#'   la edad simple en años cumplidos (`integer` no negativa). La
#'   función la agrupa internamente en quinquenios.
#' @param var_area `character` de longitud 1. Nombre de la columna que
#'   identifica el área de residencia. Debe estar codificada como
#'   `1` = Urbano, `2` = Rural.
#' @param anio_piramide `integer` de longitud 1 o `NULL`. Año específico
#'   para graficar la pirámide poblacional. Si es `NULL` (valor por
#'   defecto), se utiliza el año máximo presente en `data`.
#'
#' @return Una `list` con diez elementos:
#'   \describe{
#'     \item{`tabla_poblacion`}{`data.table` con la población total por
#'       año (o por año y territorio).}
#'     \item{`tabla_crecimiento`}{`data.table` con la tasa anual de
#'       crecimiento por mil habitantes.}
#'     \item{`tabla_edad`}{`data.table` con la población por año, sexo
#'       y grupo quinquenal de edad.}
#'     \item{`tabla_dependencia`}{`data.table` con los porcentajes de
#'       población 0-14, 15-64, 65+ y la razón de dependencia.}
#'     \item{`tabla_urbanizacion`}{`data.table` con la población urbana,
#'       rural y total por año.}
#'     \item{`grafico_poblacion`}{`ggplot` de la población total.}
#'     \item{`grafico_crecimiento`}{`ggplot` con barras de población y
#'       línea de tasa de crecimiento.}
#'     \item{`grafico_edad`}{`ggplot` con la pirámide poblacional del
#'       año seleccionado.}
#'     \item{`grafico_dependencia`}{`ggplot` con la razón de dependencia
#'       y la identificación del bono demográfico.}
#'     \item{`grafico_urbanizacion`}{`ggplot` de población por área de
#'       residencia.}
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
#' Naciones Unidas, Departamento de Asuntos Económicos y Sociales,
#' División de Población (2024). *World Population Prospects*.
#'
#' @export
#'
#' @examples
#' \donttest{
#' data(base_ag)
#'
#' # Ejecución a nivel nacional, pirámide del año más reciente disponible
#' res_nacional <- psa_context(
#'   data         = base_ag,
#'   var_pob      = "poblacion",
#'   var_div     = NULL,
#'   var_anio     = "anio",
#'   var_sexo      = "sexo",
#'   var_edad      = "edad",
#'   var_area     = "area",
#'   anio_piramide = NULL
#' )
#'
#' # Tablas
#' res_nacional$tabla_poblacion
#' res_nacional$tabla_dependencia
#'
#' # Gráficos
#' res_nacional$grafico_poblacion
#' res_nacional$grafico_edad
#'
#' # Ejecución subnacional (por div1) con pirámide del año 2020
#' res_provincial <- psa_context(
#'   data         = base_ag,
#'   var_pob      = "poblacion",
#'   var_div     = "div1",
#'   var_anio     = "anio",
#'   var_sexo      = "sexo",
#'   var_edad      = "edad",
#'   var_area     = "area",
#'   anio_piramide = 2020
#' )
#' res_provincial$grafico_dependencia
#' }

psa_context <- function(data, var_pob, var_div = NULL, var_anio, var_sexo, var_edad,
                        var_area, anio_piramide = NULL) {

  # ============================================================
  # VALIDACIONES (programación defensiva — Fail Fast)
  # ============================================================
  if (is.null(data)) {
    stop("psa_context(): el argumento 'data' no puede ser NULL.",
         call. = FALSE)
  }
  if (!is.data.frame(data)) {
    stop("psa_context(): 'data' debe ser un data.frame o data.table.",
         call. = FALSE)
  }
  if (nrow(data) == 0) {
    stop("psa_context(): 'data' no contiene filas; no es posible calcular indicadores.",
         call. = FALSE)
  }

  .check_char1 <- function(x, nm) {
    if (!is.character(x) || length(x) != 1 || is.na(x) || !nzchar(x)) {
      stop(sprintf("psa_context(): '%s' debe ser un character de longitud 1 (nombre de columna).", nm),
           call. = FALSE)
    }
  }
  .check_char1(var_pob,  "var_pob")
  .check_char1(var_anio, "var_anio")
  .check_char1(var_sexo,  "var_sexo")
  .check_char1(var_edad,  "var_edad")
  .check_char1(var_area, "var_area")
  if (!is.null(var_div)) .check_char1(var_div, "var_div")

  cols_req <- c(var_pob, var_anio, var_sexo, var_edad, var_area)
  if (!is.null(var_div)) cols_req <- c(cols_req, var_div)
  faltantes <- setdiff(cols_req, names(data))
  if (length(faltantes) > 0) {
    stop(sprintf("psa_context(): las siguientes columnas no existen en 'data': %s",
                 paste(faltantes, collapse = ", ")),
         call. = FALSE)
  }

  if (!is.numeric(data[[var_pob]])) {
    stop(sprintf("psa_context(): la columna '%s' (var_pob) debe ser numérica.", var_pob),
         call. = FALSE)
  }
  if (!is.numeric(data[[var_anio]])) {
    stop(sprintf("psa_context(): la columna '%s' (var_anio) debe ser numérica/entera.", var_anio),
         call. = FALSE)
  }
  if (!is.numeric(data[[var_edad]])) {
    stop(sprintf("psa_context(): la columna '%s' (var_edad) debe ser numérica/entera.", var_edad),
         call. = FALSE)
  }

  if (!is.null(anio_piramide)) {
    if (!is.numeric(anio_piramide) || length(anio_piramide) != 1 || is.na(anio_piramide)) {
      stop("psa_context(): 'anio_piramide' debe ser NULL o un único valor numérico.",
           call. = FALSE)
    }
    if (!(anio_piramide %in% unique(data[[var_anio]]))) {
      stop(sprintf("psa_context(): el año '%s' indicado en 'anio_piramide' no existe en la columna '%s'.",
                   anio_piramide, var_anio),
           call. = FALSE)
    }
  }

    data.table::setDT(data)

  # ============================================================
  # HELPERS
  # ============================================================

  age_labels <- function(dt) {
    dt[, age_q_e := case_when(
      age_q == 0  ~ "0-4",    age_q == 5  ~ "5-9",    age_q == 10 ~ "10-14",
      age_q == 15 ~ "15-19",  age_q == 20 ~ "20-24",  age_q == 25 ~ "25-29",
      age_q == 30 ~ "30-34",  age_q == 35 ~ "35-39",  age_q == 40 ~ "40-44",
      age_q == 45 ~ "45-49",  age_q == 50 ~ "50-54",  age_q == 55 ~ "55-59",
      age_q == 60 ~ "60-64",  age_q == 65 ~ "65-69",  age_q == 70 ~ "70-74",
      age_q == 75 ~ "75-79",  age_q == 80 ~ "80-84",  age_q == 85 ~ "85 y más"
    )]
  }

  calc_dep_ratio <- function(dt, group_cols) {
    dt[get(var_edad) >= 0  & get(var_edad) <= 14,  pop_u15   := get(var_pob), by = group_cols] %>%
      .[get(var_edad) >= 65 & get(var_edad) <= 100, pop_o65   := get(var_pob), by = group_cols] %>%
      .[get(var_edad) >= 15 & get(var_edad) <= 64,  pop_15_64 := get(var_pob), by = group_cols] %>%
      pivot_longer(cols = c("pop_u15", "pop_o65", "pop_15_64"),
                   names_to = "demo_indic", values_to = "value_f") %>%
      setDT() %>%
      .[!is.na(value_f)] %>%
      .[, .(value_f = sum(value_f)), by = c(group_cols, "demo_indic")] %>%
      pivot_wider(names_from = "demo_indic", values_from = "value_f") %>%
      setDT() %>%
      .[, dependency_ratio   := ((pop_u15 + pop_o65) / pop_15_64) * 100] %>%
      .[, pop_total          := pop_u15 + pop_o65 + pop_15_64] %>%
      .[, `:=`(proportion_0_14  = (pop_u15    / pop_total) * 100,
               proportion_65_m  = (pop_o65    / pop_total) * 100,
               proportion_15_64 = (pop_15_64  / pop_total) * 100)] %>%
      .[, .SD, .SDcols = c(group_cols, "proportion_0_14", "proportion_15_64",
                           "proportion_65_m", "dependency_ratio")]
  }

  calc_urbanization <- function(dt, group_cols) {
    dt[, .(pop = sum(get(var_pob))), by = c(group_cols, var_area)] %>%
      pivot_wider(names_from = all_of(var_area), values_from = "pop") %>%
      setDT() %>%
      setnames(c("1", "2"), c("Urban", "Rural")) %>%
      .[, Pop_total := Urban + Rural]
  }

  theme_psa <- function() {
    theme(
      plot.title       = element_text(size = 20, hjust = 0.5, face = "bold.italic"),
      plot.subtitle    = element_text(size = 16, hjust = 0.5, face = "italic"),
      axis.text        = element_text(size = 12, face = "italic"),
      axis.title       = element_text(size = 14, face = "bold"),
      axis.text.x      = element_text(vjust = 0.5, hjust = 0.5, angle = 45),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.line        = element_line(color = "black", linewidth = 0.5),
      axis.ticks       = element_line(color = "black", linewidth = 0.5),
      axis.ticks.length = unit(0.2, "cm"),
      panel.background = element_blank()
    )
  }

  # ============================================================
  # TABLAS
  # ============================================================

  if (is.null(var_div)) {

    tot_pop <- data[, .(tot_pop = sum(get(var_pob), na.rm = TRUE)),
                    by = var_anio] %>% setDT()

    growth_anual_rate <- data[, .(pop_anual = sum(get(var_pob))),
                              by = var_anio] %>%
      .[, pop_growth := (lead(pop_anual) - pop_anual)] %>%
      .[, mean_pop   := (lead(pop_anual) + pop_anual) / 2] %>%
      .[, .(growth_anual_rate = (pop_growth / mean_pop) * 1000), by = var_anio] %>%
      .[!is.na(growth_anual_rate)]

    age_structure <- data[, .(pop = sum(get(var_pob))),
                          by = c(var_anio, var_sexo, var_edad)] %>%
      .[, age_q := get(var_edad) - get(var_edad) %% 5] %>%
      .[, .(pop_f = sum(pop)), by = c(var_anio, var_sexo, "age_q")] %>%
      age_labels() %>%
      setorderv(c(var_anio, var_sexo)) %>%
      .[, .SD, .SDcols = c(var_anio, var_sexo, "age_q_e", "pop_f")]

    dep_ratio    <- calc_dep_ratio(copy(data), var_anio)
    urbanization <- calc_urbanization(data, var_anio)

  } else {

    tot_pop <- data[, .(tot_pop = sum(get(var_pob), na.rm = TRUE)),
                    by = c(var_anio, var_div)] %>%
      pivot_wider(names_from = all_of(var_anio), values_from = "tot_pop") %>%
      setDT() %>%
      adorn_totals("row")

    growth_anual_rate <- data[, .(pop_anual = sum(get(var_pob))),
                              by = c(var_anio, var_div)] %>%
      setorderv(var_div) %>%
      .[, pop_growth := (lead(pop_anual) - pop_anual), by = var_div] %>%
      .[, mean_pop   := (lead(pop_anual) + pop_anual) / 2, by = var_div] %>%
      .[, .(growth_anual_rate = (pop_growth / mean_pop) * 1000),
        by = c(var_anio, var_div)] %>%
      .[!is.na(growth_anual_rate)] %>%
      pivot_wider(names_from = all_of(var_anio), values_from = "growth_anual_rate")

    age_structure <- data[, .(pop = sum(get(var_pob))),
                          by = c(var_anio, var_div, var_sexo, var_edad)] %>%
      .[, age_q := get(var_edad) - get(var_edad) %% 5] %>%
      .[, .(pop_f = sum(pop)), by = c(var_anio, var_div, var_sexo, "age_q")] %>%
      age_labels() %>%
      setorderv(c(var_anio, var_div, var_sexo)) %>%
      .[, .SD, .SDcols = c(var_anio, var_div, var_sexo, "age_q_e", "pop_f")] %>%
      pivot_wider(names_from = all_of(var_anio), values_from = "pop_f")

    dep_ratio    <- calc_dep_ratio(copy(data), c(var_anio, var_div))
    urbanization <- calc_urbanization(data, c(var_anio, var_div))
  }

  # ============================================================
  # GRÁFICOS
  # ============================================================

  min_year <- min(data[[var_anio]], na.rm = TRUE)
  max_year <- max(data[[var_anio]], na.rm = TRUE)
  year_pir <- if (is.null(anio_piramide)) max_year else anio_piramide

  psa_context_graphs <- function() {

    graphs    <- list()
    age_levels <- c("0-4","5-9","10-14","15-19","20-24","25-29","30-34","35-39",
                    "40-44","45-49","50-54","55-59","60-64","65-69","70-74",
                    "75-79","80-84","85 y más")

    if (is.null(var_div)) {

      # -- G1: Población Total Nacional --
      tot_pop_mill <- copy(tot_pop)[, tot_pop_mill := tot_pop / 1000000]

      graphs[[1]] <- ggplot(tot_pop_mill,
                            aes(x = as.factor(get(var_anio)), y = tot_pop_mill,
                                fill = as.factor(get(var_anio)))) +
        geom_col() +
        geom_shadowtext(aes(label = scales::number(tot_pop_mill, accuracy = 0.1)),
                        position = position_stack(vjust = 0.9), angle = 90,
                        size = 6, color = "#C95E78", bg.color = "white",
                        bg.r = 0.15, fontface = "bold") +
        scale_fill_manual(values = paletteer_c("grDevices::Burg",
                                               n = length(unique(tot_pop_mill[[var_anio]])))) +
        scale_y_continuous(expand = c(0, 0)) +
        labs(x = "Año", y = "Población Total (millones)", title = "Población Total",
             subtitle = paste("Período", min_year, "-", max_year)) +
        theme_psa() + theme(legend.position = "none")

      # -- G2: Población + Tasa de Crecimiento Nacional --
      tot_pop_mill <- copy(tot_pop)[, tot_pop_mill := tot_pop / 1000000]
      max_pop      <- max(tot_pop_mill$tot_pop_mill, na.rm = TRUE)
      max_rate     <- max(growth_anual_rate$growth_anual_rate, na.rm = TRUE)
      scale_factor <- (max_pop / max_rate) * 0.3
      offset       <- 50

      # Garantizar que var_anio tenga el nombre correcto en growth_anual_rate
      growth_plot  <- copy(growth_anual_rate)

      graphs[[2]] <- ggplot(tot_pop_mill,
                            aes(x = as.factor(get(var_anio)), y = tot_pop_mill,
                                fill = as.factor(get(var_anio)))) +
        geom_col(width = 0.85) +
        geom_line(data = growth_plot,
                  aes(x = as.factor(get(var_anio)),
                      y = (growth_anual_rate + offset) * scale_factor, group = 1),
                  position = position_nudge(x = 0.5),
                  color = "#33608C", linewidth = 1.2, inherit.aes = FALSE) +
        geom_point(data = growth_plot,
                   aes(x = as.factor(get(var_anio)),
                       y = (growth_anual_rate + offset) * scale_factor),
                   position = position_nudge(x = 0.5),
                   color = "#33608C", size = 3, inherit.aes = FALSE) +
        geom_text(data = growth_plot,
                  aes(x = as.factor(get(var_anio)),
                      y = (growth_anual_rate + offset) * scale_factor,
                      label = scales::number(growth_anual_rate, accuracy = 0.1)),
                  position = position_nudge(x = 0.425), inherit.aes = FALSE,
                  vjust = 0.5, hjust = -0.3, color = "#33608C",
                  angle = 90, size = 4, fontface = "bold") +
        geom_shadowtext(data = tot_pop_mill,
                        aes(label = scales::number(tot_pop_mill, accuracy = 0.1)),
                        position = position_stack(vjust = 0.15), angle = 90,
                        size = 4.5, color = "#C95E78", bg.color = "white",
                        bg.r = 0.15, fontface = "bold") +
        scale_fill_manual(values = paletteer_c("grDevices::Burg",
                                               n = length(unique(tot_pop_mill[[var_anio]])))) +
        scale_y_continuous(limits = c(0, max_pop * 1.25), expand = c(0, 0),
                           sec.axis = sec_axis(~ . / scale_factor - offset,
                                               name = "Tasa de Crecimiento (por cada mil habitantes)")) +
        labs(x = "Año", y = "Población Total (millones)",
             title = "Población Total y Tasa de Crecimiento Anual",
             subtitle = paste("Período", min_year, "-", max_year)) +
        theme_psa() + theme(legend.position = "none",
                            axis.title.y.right = element_text(color = "black", size = 13))

      # -- G3: Pirámide Nacional --
      age_struct_plot       <- copy(age_structure)
      age_struct_plot$age_q_e <- factor(age_struct_plot$age_q_e, levels = age_levels)

      piramid <- age_struct_plot[get(var_anio) == year_pir] %>%
        .[, pop_total    := sum(pop_f), by = var_anio] %>%
        .[, propor_pob   := pop_f / pop_total] %>%
        .[, propor_pob_p := ifelse(get(var_sexo) == 1, -propor_pob, propor_pob)]

      lim <- max(abs(piramid$propor_pob_p))
      gap <- lim * 0.1

      graphs[[3]] <- ggplot(piramid,
                            aes(x = age_q_e, y = propor_pob_p,
                                fill = as.factor(get(var_sexo)))) +
        geom_col(data = piramid[get(var_sexo) == 1], width = 0.9,
                 position = position_nudge(y = -gap)) +
        geom_col(data = piramid[get(var_sexo) == 2], width = 0.9,
                 position = position_nudge(y =  gap)) +
        coord_flip() +
        scale_y_continuous(labels = function(x) scales::percent(abs(x), accuracy = 0.1)) +
        scale_fill_manual(values = c("1" = "#33608C", "2" = "#8867A1"),
                          labels = c("Hombres", "Mujeres")) +
        geom_label(aes(x = age_q_e, y = 0, label = age_q_e),
                   fill = "white", alpha = 0.8, color = "black", size = 5,
                   label.padding = unit(0, "lines"), label.r = unit(0, "lines"),
                   linewidth = 0) +
        annotate("text", x = 17, y = -lim * 0.7, label = "Hombres", size = 5, fontface = "bold") +
        annotate("text", x = 17, y =  lim * 0.7, label = "Mujeres",  size = 5, fontface = "bold") +
        labs(x = "Grupos Quinquenales de Edad", y = "Población (Porcentaje)",
             title = "Población por Grupos Quinquenales de Edad",
             subtitle = paste("Año", year_pir)) +
        theme_psa() + theme(legend.position = "none",
                            axis.text.y = element_blank(),
                            axis.ticks  = element_blank())

      # -- G4: Dependencia Nacional --
      graphs[[4]] <- copy(dep_ratio)[
        , bono := ifelse(dependency_ratio >= 66.7,
                         "Período fuera del Bono Demográfico",
                         "Período dentro del Bono Demográfico")] %>%
        ggplot(aes(x = get(var_anio), y = dependency_ratio, fill = factor(bono))) +
        geom_bar(stat = "identity") +
        geom_line(aes(y = proportion_0_14,  color = "Menores de 15"),  linewidth = 1.5) +
        geom_line(aes(y = proportion_15_64, color = "15 a 64 años"),   linewidth = 1.5) +
        geom_line(aes(y = proportion_65_m,  color = "Mayores de 65"),  linewidth = 1.5) +
        scale_x_continuous(breaks = seq(min_year, max_year, by = 5)) +
        scale_y_continuous(
          name = "Relación de dependencia", limits = c(0, 100), breaks = seq(0, 100, 10),
          sec.axis = sec_axis(~ ., name = "Porcentajes de población (%)",
                              breaks = seq(0, 100, 10),
                              labels = scales::percent_format(scale = 1))) +
        scale_color_manual(name = "Grupos de población",
                           values = c("Menores de 15" = "#B81840",
                                      "15 a 64 años"  = "#4D6291",
                                      "Mayores de 65" = "#583C88")) +
        scale_fill_manual(name = "Bono Demográfico",
                          values = c("Período fuera del Bono Demográfico"  = "#F09574",
                                     "Período dentro del Bono Demográfico" = "#CA60A7")) +
        labs(x = "Año", title = "Relación de Dependencia Nacional",
             subtitle = paste("Período", min_year, "-", max_year)) +
        theme_psa() + theme(legend.position = "bottom", legend.box = "vertical",
                            axis.title.y.right = element_text(color = "black", size = 13))

      # -- G5: Urbanización Nacional --
      urb_data <- copy(urbanization)[, .(Urban = Urban / 1000000,
                                         Rural  = Rural  / 1000000), by = var_anio] %>%
        pivot_longer(cols = c(Urban, Rural), names_to = "Area", values_to = "poblation") %>%
        setDT() %>%
        .[, .(Area, poblation, pop_tot = sum(poblation)), by = var_anio] %>%
        .[, Urban_proportion := ifelse(Area == "Urban", poblation / pop_tot, NA)]

      urban_line <- urb_data[Area == "Urban"]
      max1 <- round(max(urb_data$pop_tot), 0)
      min2 <- min(urban_line$Urban_proportion, na.rm = TRUE)
      max2 <- max(urban_line$Urban_proportion, na.rm = TRUE)
      a    <- max1 / (max2 - min2)
      b    <- -a * min2

      graphs[[5]] <- ggplot(urb_data, aes(x = get(var_anio), y = poblation, fill = Area)) +
        geom_bar(stat = "identity", position = "stack") +
        geom_line(data = urban_line,
                  aes(x = get(var_anio), y = Urban_proportion * a + b,
                      color = "Proporción área urbana", group = 1),
                  inherit.aes = FALSE, linewidth = 1.5) +
        geom_point(data = urban_line,
                   aes(x = get(var_anio), y = Urban_proportion * a + b,
                       color = "Proporción área urbana"),
                   inherit.aes = FALSE, size = 2) +
        geom_shadowtext(aes(label = scales::number(poblation, accuracy = 0.1)),
                        position = position_stack(vjust = 0.85), angle = 90,
                        size = 4, color = "#C95E78", bg.color = "white",
                        bg.r = 0.15, fontface = "bold") +
        scale_y_continuous(
          name = "Población (millones)", limits = c(0, max1),
          sec.axis = sec_axis(~ (. - b) / a, name = "Población Urbana (%)",
                              breaks = scales::pretty_breaks(n = 5),
                              labels = scales::percent_format(accuracy = 0.2))) +
        scale_x_continuous(breaks = seq(min_year, max_year, by = 5)) +
        scale_fill_manual(values  = c("Urban" = "#D64267", "Rural" = "#EA5A4E")) +
        scale_color_manual(name   = "Proporción área urbana",
                           values = c("Proporción área urbana" = "#7B106D")) +
        labs(x = "Año", y = "Población (millones)", fill = "Área",
             title = "Población por Área de Residencia",
             subtitle = paste("Período", min_year, "-", max_year)) +
        guides(fill = guide_legend(order = 1), color = guide_legend(order = 2)) +
        theme_psa() + theme(legend.position = "bottom", legend.box = "vertical",
                            axis.text.x = element_text(angle = 90, size = 12),
                            axis.title  = element_text(size = 16, face = "bold"))

    } else {

      # -- G1: Población Total Provincial --
      n_terr <- nrow(tot_pop) - 1

      graphs[[1]] <- tot_pop[c(1:n_terr), ] %>%
        pivot_longer(cols = -all_of(var_div), names_to = "anio", values_to = "pop") %>%
        setDT() %>%
        .[, pop_mill := pop / 1000] %>%
        .[, anio     := as.numeric(anio)] %>%
        .[, (var_div) := factor(get(var_div),
                                 levels = as.character(sort(unique(get(var_div)))))] %>%
        ggplot(aes(x = anio, y = pop_mill,
                   color = as.factor(get(var_div)),
                   group  = as.factor(get(var_div)))) +
        geom_line(linewidth = 1) +
        facet_wrap(~ get(var_div), scales = "free_y") +
        scale_color_manual(values = paletteer_d("colorBlindness::SteppedSequential5Steps")) +
        labs(title = "Población por Nivel Territorial",
             subtitle = paste("Período", min_year, "-", max_year),
             x = "Año", y = "Población (miles)") +
        theme_psa() + theme(legend.position = "none",
                            panel.grid.major.x = element_blank())

      # -- G2: Tasa de Crecimiento Provincial --
      graphs[[2]] <- pivot_longer(growth_anual_rate, cols = -all_of(var_div),
                                  names_to = "anio", values_to = "growth_anual") %>%
        setDT() %>%
        .[, anio := as.numeric(anio)] %>%
        ggplot(aes(x = anio, y = growth_anual,
                   color = as.factor(get(var_div)),
                   group  = as.factor(get(var_div)))) +
        geom_line(linewidth = 1.5) +
        geom_smooth(aes(linetype = "Línea de tendencia"),
                    method = "lm", col = "red", linewidth = 0.6, se = FALSE) +
        scale_linetype_manual(name = "Línea de tendencia",
                              values = c("Línea de tendencia" = "dashed")) +
        scale_x_continuous(breaks = seq(min_year, max_year, by = 5)) +
        facet_wrap(~ get(var_div), scales = "free_y") +
        guides(color = "none") +
        labs(x = "Año", y = "Tasa de crecimiento anual (por miles de habitantes)",
             title = "Tasa de Crecimiento Anual por Territorio",
             subtitle = paste("Período", min_year, "-", max_year)) +
        theme_psa() + theme(legend.position = "bottom",
                            panel.grid.major.x = element_blank())

      # -- G3: Pirámide Provincial --
      age_struct_plot         <- copy(age_structure)
      age_struct_plot$age_q_e <- factor(age_struct_plot$age_q_e, levels = age_levels)

      piramid_terr <- pivot_longer(age_struct_plot,
                                   cols = -all_of(c(var_div, var_sexo, "age_q_e")),
                                   names_to = "anio", values_to = "pop") %>%
        setDT() %>%
        .[, pop_total    := sum(pop), by = c("anio", var_div)] %>%
        .[, propor_pob   := pop / pop_total] %>%
        .[, propor_pob_p := ifelse(get(var_sexo) == 1, -propor_pob, propor_pob)] %>%
        .[anio == as.character(year_pir)]

      graphs[[3]] <- ggplot(piramid_terr,
                            aes(x = age_q_e, y = propor_pob_p,
                                fill = as.factor(get(var_sexo)))) +
        geom_col(width = 0.9) +
        coord_flip() +
        scale_y_continuous(labels = function(x) scales::percent(abs(x), accuracy = 0.1)) +
        scale_fill_manual(values = c("1" = "#33608C", "2" = "#8867A1"),
                          labels = c("Hombres", "Mujeres")) +
        facet_wrap(~ get(var_div), scales = "free_x") +
        labs(x = "Grupos Quinquenales de Edad", y = "Población (Porcentaje)",
             title = "Población por Grupos Quinquenales de Edad",
             subtitle = paste("Año", year_pir)) +
        theme_psa() +
        theme(legend.position = "bottom",
              axis.text.x   = element_text(vjust = 0.5, hjust = 0.5, size = 6),
              axis.text.y   = element_text(vjust = 0.5, hjust = 0.5, size = 6),
              axis.title.x  = element_text(vjust = -0.8),
              panel.spacing = unit(0.8, "cm"))

      # -- G4: Dependencia Provincial --
      graphs[[4]] <- copy(dep_ratio)[
        , bono := ifelse(dependency_ratio >= 66.7,
                         "Período fuera del Bono Demográfico",
                         "Período dentro del Bono Demográfico"),
        by = c(var_anio, var_div)] %>%
        ggplot(aes(x = get(var_anio), y = dependency_ratio, fill = factor(bono))) +
        geom_bar(stat = "identity") +
        geom_line(aes(y = proportion_0_14,  color = "Menores de 15"),  linewidth = 0.8) +
        geom_line(aes(y = proportion_15_64, color = "15 a 64 años"),   linewidth = 0.8) +
        geom_line(aes(y = proportion_65_m,  color = "Mayores de 65"),  linewidth = 0.8) +
        scale_y_continuous(
          name = "Relación de dependencia", limits = c(0, 100), breaks = seq(0, 100, 10),
          sec.axis = sec_axis(~ ., name = "Porcentajes de población (%)",
                              breaks = seq(0, 100, 10),
                              labels = scales::percent_format(scale = 1))) +
        scale_x_continuous(breaks = seq(min_year, max_year, by = 5)) +
        scale_color_manual(name = "Grupos de población",
                           values = c("Menores de 15" = "#B81840",
                                      "15 a 64 años"  = "#4D6291",
                                      "Mayores de 65" = "#583C88")) +
        scale_fill_manual(name = "Bono Demográfico",
                          values = c("Período fuera del Bono Demográfico"  = "#F09574",
                                     "Período dentro del Bono Demográfico" = "#CA60A7")) +
        facet_wrap(~ get(var_div), scales = "free_y") +
        labs(x = "Año", title = "Relación de Dependencia por Territorio",
             subtitle = paste("Período", min_year, "-", max_year)) +
        theme_light() + theme_psa() +
        theme(legend.position = "bottom", legend.box = "vertical",
              axis.text = element_text(size = 7, face = "italic"),
              axis.title.y.right = element_text(color = "black", size = 13),
              panel.spacing.x    = unit(0.1, "lines"))

      # -- G5: Urbanización Provincial --
      graphs[[5]] <- copy(urbanization)[
        , `:=`(`Urban Proportion` = Urban / Pop_total,
               `Rural Proportion` = Rural / Pop_total),
        by = c(var_anio, var_div)] %>%
        .[, .SD, .SDcols = c(var_anio, var_div, "Urban Proportion", "Rural Proportion")] %>%
        pivot_longer(cols = c("Urban Proportion", "Rural Proportion"),
                     names_to = "Area", values_to = "proportion") %>%
        setDT() %>%
        ggplot(aes(x = get(var_anio), y = proportion, fill = Area)) +
        geom_area(alpha = 0.6, linewidth = 0.5, colour = "grey35", position = "fill") +
        scale_y_continuous(labels = scales::percent) +
        scale_fill_manual(values = c("Urban Proportion" = "#891171",
                                     "Rural Proportion" = "#EA5A4E")) +
        facet_wrap(~ get(var_div), scales = "free_y") +
        labs(x = "Año", y = "Proporción de población", fill = "Área",
             title = "Población de cada territorio por Área de Residencia",
             subtitle = paste("Período", min_year, "-", max_year)) +
        theme_psa() +
        theme(legend.position = "bottom",
              axis.text.x   = element_text(vjust = 0.5, hjust = 0.5, size = 8, angle = 45),
              axis.text.y   = element_text(vjust = 0.5, hjust = 0.5, size = 7),
              panel.spacing = unit(0.8, "cm"))
    }

    return(graphs)
  }

  graphs <- psa_context_graphs()

  if(sum("tot_pop" %in% names(tot_pop))){
    tot_pop = tot_pop %>%
      rename(pob_total = tot_pop)
  }
  if(sum("growth_anual_rate" %in% names(growth_anual_rate))){
    growth_anual_rate = growth_anual_rate %>%
      rename(tasa_crec_anual = growth_anual_rate)
  }
  if(sum("pop_f" %in% names(age_structure))){
    age_structure = age_structure %>%
      rename(pob = pop_f)
  }
  if(sum("proportion_0_14" %in% names(dep_ratio))){
    dep_ratio = dep_ratio %>%
      rename(pob_0_14_porcen = proportion_0_14,
             pob_15_64_porcen = proportion_15_64,
             pob_65_mas_porcen = proportion_65_m,
             rela_dep = dependency_ratio)
  }
  if(sum("Urban" %in% names(urbanization))){
    urbanization = urbanization %>%
      rename(pob_urb = Urban,
             pob_rur = Rural,
             pob_total = Pop_total)
  }

  # ============================================================
  # RESULTADO
  # ============================================================
  result <- list(
    tabla_poblacion      = tot_pop,
    tabla_crecimiento    = growth_anual_rate,
    tabla_edad           = age_structure%>%
      rename(grup_edad_quinq = age_q_e),
    tabla_dependencia    = dep_ratio,
    tabla_urbanizacion   = urbanization,
    grafico_poblacion    = graphs[[1]],
    grafico_crecimiento  = graphs[[2]],
    grafico_edad         = graphs[[3]],
    grafico_dependencia  = graphs[[4]],
    grafico_urbanizacion = graphs[[5]]
  )

  return(result)
}
