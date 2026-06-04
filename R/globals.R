#' psaLAC: Análisis de Situación Poblacional para América Latina y el Caribe
#'
#' @description
#' El paquete `psaLAC` sistematiza, en una librería de R, los cálculos y
#' visualizaciones requeridos para elaborar el **Análisis de Situación
#' Poblacional (ASP)** propuesto por el Fondo de Población de las Naciones
#' Unidas (UNFPA) para los países de América Latina y el Caribe. Toma como
#' referencia el ASP Ecuador 2024-2025 y busca estandarizar los
#' procedimientos analíticos, fortalecer las capacidades técnicas
#' nacionales y garantizar comparabilidad regional y transparencia
#' metodológica.
#'
#' @details
#' El ASP regional se estructura en cuatro grandes secciones, cada una
#' implementada como una función especializada del paquete:
#'
#' * **Contexto del país** — [psa_context()] (incluida en esta versión).
#' * **Dinámica demográfica** — [psa_dinam_demogra()] (incluida en esta versión).
#' * **Salud y Salud Sexual y Reproductiva** (`psa_health()`, en
#'   desarrollo).
#' * **Igualdad de género** (`psa_gender()`, en desarrollo).
#'
#' Cada función devuelve un objeto tipo lista con las tablas y los
#' gráficos asociados a sus indicadores, listos para integrarse en
#' informes RMarkdown.
#'
#' @section Datos de ejemplo:
#' El paquete incluye el conjunto [base_ag], con población proyectada
#' por año, primera división territorial (`div1`), área de residencia, sexo y edad simple, útil para
#' probar las funciones sin necesidad de insumos externos.
#'
#' @references
#' UNFPA-LACRO (2025). *Consultoría para el desarrollo y programación de
#' la librería regional en lenguaje R para la generación del Análisis de
#' la Situación de Población (ASP)*.
#'
#' SNP, INEC, USFQ & UNFPA Ecuador (2025). *Análisis de Situación
#' Poblacional del Ecuador 2024-2025*.
#'
#' @keywords internal
"_PACKAGE"

if (getRversion() >= "2.15.1") {
    utils::globalVariables(c(
        ".",
        # Variables creadas dentro de psa_context() por data.table / dplyr
        "pop_growth", "pop_anual", "mean_pop",
        "age_q", "age_q_e", "pop", "pop_f",
        "pop_u15", "pop_o65", "pop_15_64", "pop_total",
        "value_f", "demo_indic", "dependency_ratio",
        "proportion_0_14", "proportion_15_64", "proportion_65_m",
        "Urban", "Rural", "Pop_total", "Area",
        "Urban_proportion", "Urban Proportion", "Rural Proportion",
        "tot_pop", "tot_pop_mill", "growth_anual_rate",
        "propor_pob", "propor_pob_p", "bono", "anio",
        "poblation", "pop_mill", "pop_tot",
        "growth_anual", "proportion",
        # Variables creadas dentro de psa_dinam_demogra() por data.table / dplyr
        # — preprocesamiento
        "age_num", "age_clean",
        "na_value", "ajuste",
        # — fecundidad
        "births", "nacimientos", "pob_fem", "tef", "tgf",
        # — mortalidad
        "deaths", "mx", "tbm",
        "n", "ax", "qx", "lx", "dx", "Lx", "Tx", "ex",
        "edad_tv",
        # — migración
        "entries", "departures", "smn", "tbmig"
    ))
}
