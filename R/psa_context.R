#' @import data.table
#' @import dplyr
#' @import tidyr
#' @import janitor
#' @import ggplot2
#' @import scales
#' @import paletteer
#' @import shadowtext
#' @title
#' Título.
#' @description
#' Descripción.
#' @details
#' Detalles. Detalles.
#' @author Angel Gaibor <mat.angel.gaibor at gmail.com>
#' @author Javier Núñez <mat.javier.nunez at gmail.com>
#' @param data p1.
#' @param var_pop p2.
#' @param var_terr p3.
#' @param var_year p4.
#' @param var_sex p5.
#' @param var_age p6.
#' @param var_area p7.
#' @param year_piramid p8.
#'
#' @references
#' Gutierrez, H. A. (2009), \emph{Estrategias de muestreo: Diseno de encuestas y estimacion de parametros}. Editorial Universidad Santo Tomas.
#' Valliant, R, et. al. (2013), \emph{Practical tools for Design and Weighting Survey Samples}. Springer
#' @return ext_pol
#' @export
#'
#' @examples
#' psa_context(data = base_ag, var_pop = "poblacion", var_terr = NULL, var_year = "anio", var_sex = "sexo", var_age = "edad", var_area = "area", year_piramid = NULL)

psa_context <- function(data, var_pop, var_terr = NULL, var_year, var_sex, var_age,
                        var_area, year_piramid = NULL) {

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
    dt[get(var_age) >= 0  & get(var_age) <= 14,  pop_u15   := get(var_pop), by = group_cols] %>%
      .[get(var_age) >= 65 & get(var_age) <= 100, pop_o65   := get(var_pop), by = group_cols] %>%
      .[get(var_age) >= 15 & get(var_age) <= 64,  pop_15_64 := get(var_pop), by = group_cols] %>%
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
    dt[, .(pop = sum(get(var_pop))), by = c(group_cols, var_area)] %>%
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

  if (is.null(var_terr)) {

    tot_pop <- data[, .(tot_pop = sum(get(var_pop), na.rm = TRUE)),
                    by = var_year] %>% setDT()

    growth_anual_rate <- data[, .(pop_anual = sum(get(var_pop))),
                              by = var_year] %>%
      .[, pop_growth := (lead(pop_anual) - pop_anual)] %>%
      .[, mean_pop   := (lead(pop_anual) + pop_anual) / 2] %>%
      .[, .(growth_anual_rate = (pop_growth / mean_pop) * 1000), by = var_year] %>%
      .[!is.na(growth_anual_rate)]

    age_structure <- data[, .(pop = sum(get(var_pop))),
                          by = c(var_year, var_sex, var_age)] %>%
      .[, age_q := get(var_age) - get(var_age) %% 5] %>%
      .[, .(pop_f = sum(pop)), by = c(var_year, var_sex, "age_q")] %>%
      age_labels() %>%
      setorderv(c(var_year, var_sex)) %>%
      .[, .SD, .SDcols = c(var_year, var_sex, "age_q_e", "pop_f")]

    dep_ratio    <- calc_dep_ratio(copy(data), var_year)
    urbanization <- calc_urbanization(data, var_year)

  } else {

    tot_pop <- data[, .(tot_pop = sum(get(var_pop), na.rm = TRUE)),
                    by = c(var_year, var_terr)] %>%
      pivot_wider(names_from = var_year, values_from = "tot_pop") %>%
      setDT() %>%
      adorn_totals("row")

    growth_anual_rate <- data[, .(pop_anual = sum(get(var_pop))),
                              by = c(var_year, var_terr)] %>%
      setorderv(var_terr) %>%
      .[, pop_growth := (lead(pop_anual) - pop_anual), by = var_terr] %>%
      .[, mean_pop   := (lead(pop_anual) + pop_anual) / 2, by = var_terr] %>%
      .[, .(growth_anual_rate = (pop_growth / mean_pop) * 1000),
        by = c(var_year, var_terr)] %>%
      .[!is.na(growth_anual_rate)] %>%
      pivot_wider(names_from = var_year, values_from = "growth_anual_rate")

    age_structure <- data[, .(pop = sum(get(var_pop))),
                          by = c(var_year, var_terr, var_sex, var_age)] %>%
      .[, age_q := get(var_age) - get(var_age) %% 5] %>%
      .[, .(pop_f = sum(pop)), by = c(var_year, var_terr, var_sex, "age_q")] %>%
      age_labels() %>%
      setorderv(c(var_year, var_terr, var_sex)) %>%
      .[, .SD, .SDcols = c(var_year, var_terr, var_sex, "age_q_e", "pop_f")] %>%
      pivot_wider(names_from = var_year, values_from = "pop_f")

    dep_ratio    <- calc_dep_ratio(copy(data), c(var_year, var_terr))
    urbanization <- calc_urbanization(data, c(var_year, var_terr))
  }

  # ============================================================
  # GRÁFICOS
  # ============================================================

  min_year <- min(data[[var_year]], na.rm = TRUE)
  max_year <- max(data[[var_year]], na.rm = TRUE)
  year_pir <- if (is.null(year_piramid)) max_year else year_piramid

  psa_context_graphs <- function() {

    graphs    <- list()
    age_levels <- c("0-4","5-9","10-14","15-19","20-24","25-29","30-34","35-39",
                    "40-44","45-49","50-54","55-59","60-64","65-69","70-74",
                    "75-79","80-84","85 y más")

    if (is.null(var_terr)) {

      # -- G1: Población Total Nacional --
      tot_pop_mill <- copy(tot_pop)[, tot_pop_mill := tot_pop / 1000000]

      graphs[[1]] <- ggplot(tot_pop_mill,
                            aes(x = as.factor(get(var_year)), y = tot_pop_mill,
                                fill = as.factor(get(var_year)))) +
        geom_col() +
        geom_shadowtext(aes(label = scales::number(tot_pop_mill, accuracy = 0.1)),
                        position = position_stack(vjust = 0.9), angle = 90,
                        size = 6, color = "#C95E78", bg.color = "white",
                        bg.r = 0.15, fontface = "bold") +
        scale_fill_manual(values = paletteer_c("grDevices::Burg",
                                               n = length(unique(tot_pop_mill[[var_year]])))) +
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

      # Garantizar que var_year tenga el nombre correcto en growth_anual_rate
      growth_plot  <- copy(growth_anual_rate)

      graphs[[2]] <- ggplot(tot_pop_mill,
                            aes(x = as.factor(get(var_year)), y = tot_pop_mill,
                                fill = as.factor(get(var_year)))) +
        geom_col(width = 0.85) +
        geom_line(data = growth_plot,
                  aes(x = as.factor(get(var_year)),
                      y = (growth_anual_rate + offset) * scale_factor, group = 1),
                  position = position_nudge(x = 0.5),
                  color = "#33608C", linewidth = 1.2, inherit.aes = FALSE) +
        geom_point(data = growth_plot,
                   aes(x = as.factor(get(var_year)),
                       y = (growth_anual_rate + offset) * scale_factor),
                   position = position_nudge(x = 0.5),
                   color = "#33608C", size = 3, inherit.aes = FALSE) +
        geom_text(data = growth_plot,
                  aes(x = as.factor(get(var_year)),
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
                                               n = length(unique(tot_pop_mill[[var_year]])))) +
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

      piramid <- age_struct_plot[get(var_year) == year_pir] %>%
        .[, pop_total    := sum(pop_f), by = var_year] %>%
        .[, propor_pob   := pop_f / pop_total] %>%
        .[, propor_pob_p := ifelse(get(var_sex) == 1, -propor_pob, propor_pob)]

      lim <- max(abs(piramid$propor_pob_p))
      gap <- lim * 0.1

      graphs[[3]] <- ggplot(piramid,
                            aes(x = age_q_e, y = propor_pob_p,
                                fill = as.factor(get(var_sex)))) +
        geom_col(data = piramid[get(var_sex) == 1], width = 0.9,
                 position = position_nudge(y = -gap)) +
        geom_col(data = piramid[get(var_sex) == 2], width = 0.9,
                 position = position_nudge(y =  gap)) +
        coord_flip() +
        scale_y_continuous(labels = function(x) scales::percent(abs(x), accuracy = 0.1)) +
        scale_fill_manual(values = c("1" = "#33608C", "2" = "#8867A1"),
                          labels = c("Hombres", "Mujeres")) +
        geom_label(aes(x = age_q_e, y = 0, label = age_q_e),
                   fill = "white", alpha = 0.8, color = "black", size = 5,
                   label.padding = unit(0, "lines"), label.r = unit(0, "lines"),
                   label.size = 0) +
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
        ggplot(aes(x = get(var_year), y = dependency_ratio, fill = factor(bono))) +
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
                                         Rural  = Rural  / 1000000), by = var_year] %>%
        pivot_longer(cols = c(Urban, Rural), names_to = "Area", values_to = "poblation") %>%
        setDT() %>%
        .[, .(Area, poblation, pop_tot = sum(poblation)), by = var_year] %>%
        .[, Urban_proportion := ifelse(Area == "Urban", poblation / pop_tot, NA)]

      urban_line <- urb_data[Area == "Urban"]
      max1 <- round(max(urb_data$pop_tot), 0)
      min2 <- min(urban_line$Urban_proportion, na.rm = TRUE)
      max2 <- max(urban_line$Urban_proportion, na.rm = TRUE)
      a    <- max1 / (max2 - min2)
      b    <- -a * min2

      graphs[[5]] <- ggplot(urb_data, aes(x = get(var_year), y = poblation, fill = Area)) +
        geom_bar(stat = "identity", position = "stack") +
        geom_line(data = urban_line,
                  aes(x = get(var_year), y = Urban_proportion * a + b,
                      color = "Proporción área urbana", group = 1),
                  inherit.aes = FALSE, linewidth = 1.5) +
        geom_point(data = urban_line,
                   aes(x = get(var_year), y = Urban_proportion * a + b,
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
        pivot_longer(cols = -all_of(var_terr), names_to = "anio", values_to = "pop") %>%
        setDT() %>%
        .[, pop_mill := pop / 1000] %>%
        .[, anio     := as.numeric(anio)] %>%
        .[, (var_terr) := factor(get(var_terr),
                                 levels = as.character(sort(unique(get(var_terr)))))] %>%
        ggplot(aes(x = anio, y = pop_mill,
                   color = as.factor(get(var_terr)),
                   group  = as.factor(get(var_terr)))) +
        geom_line(linewidth = 1) +
        facet_wrap(~ get(var_terr), scales = "free_y") +
        scale_color_manual(values = paletteer_d("colorBlindness::SteppedSequential5Steps")) +
        labs(title = "Población por Nivel Territorial",
             subtitle = paste("Período", min_year, "-", max_year),
             x = "Año", y = "Población (miles)") +
        theme_psa() + theme(legend.position = "none",
                            panel.grid.major.x = element_blank())

      # -- G2: Tasa de Crecimiento Provincial --
      graphs[[2]] <- pivot_longer(growth_anual_rate, cols = -all_of(var_terr),
                                  names_to = "anio", values_to = "growth_anual") %>%
        setDT() %>%
        .[, anio := as.numeric(anio)] %>%
        ggplot(aes(x = anio, y = growth_anual,
                   color = as.factor(get(var_terr)),
                   group  = as.factor(get(var_terr)))) +
        geom_line(linewidth = 1.5) +
        geom_smooth(aes(linetype = "Línea de tendencia"),
                    method = "lm", col = "red", linewidth = 0.6, se = FALSE) +
        scale_linetype_manual(name = "Línea de tendencia",
                              values = c("Línea de tendencia" = "dashed")) +
        scale_x_continuous(breaks = seq(min_year, max_year, by = 5)) +
        facet_wrap(~ get(var_terr), scales = "free_y") +
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
                                   cols = -all_of(c(var_terr, var_sex, "age_q_e")),
                                   names_to = "anio", values_to = "pop") %>%
        setDT() %>%
        .[, pop_total    := sum(pop), by = c("anio", var_terr)] %>%
        .[, propor_pob   := pop / pop_total] %>%
        .[, propor_pob_p := ifelse(get(var_sex) == 1, -propor_pob, propor_pob)] %>%
        .[anio == as.character(year_pir)]

      graphs[[3]] <- ggplot(piramid_terr,
                            aes(x = age_q_e, y = propor_pob_p,
                                fill = as.factor(get(var_sex)))) +
        geom_col(width = 0.9) +
        coord_flip() +
        scale_y_continuous(labels = function(x) scales::percent(abs(x), accuracy = 0.1)) +
        scale_fill_manual(values = c("1" = "#33608C", "2" = "#8867A1"),
                          labels = c("Hombres", "Mujeres")) +
        facet_wrap(~ get(var_terr), scales = "free_x") +
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
        by = c(var_year, var_terr)] %>%
        ggplot(aes(x = get(var_year), y = dependency_ratio, fill = factor(bono))) +
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
        facet_wrap(~ get(var_terr), scales = "free_y") +
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
        by = c(var_year, var_terr)] %>%
        .[, .SD, .SDcols = c(var_year, var_terr, "Urban Proportion", "Rural Proportion")] %>%
        pivot_longer(cols = c("Urban Proportion", "Rural Proportion"),
                     names_to = "Area", values_to = "proportion") %>%
        setDT() %>%
        ggplot(aes(x = get(var_year), y = proportion, fill = Area)) +
        geom_area(alpha = 0.6, linewidth = 0.5, colour = "grey35", position = "fill") +
        scale_y_continuous(labels = scales::percent) +
        scale_fill_manual(values = c("Urban Proportion" = "#891171",
                                     "Rural Proportion" = "#EA5A4E")) +
        facet_wrap(~ get(var_terr), scales = "free_y") +
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
