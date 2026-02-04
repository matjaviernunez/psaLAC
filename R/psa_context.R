#' Función contexto de psaLAC
#'
#' Esta funcion calculo el contexto pais usando psaLAC
#'
#' @param data una base de datos de población
#' @param var_pop una variable que denota var_pop
#' @param var_terr una variable que denota var_terr
#' @param var_year una variable que denota var_year
#' @param var_sex una variable que denota var_sex
#' @return una lista con los df calculados a partir de `data`, `var_pop`, `var_terr`, `var_year` y `var_sex`.
#' @importFrom data.table setDT setorderv ':='
#' @importFrom dplyr lead all_of
#' @importFrom tidyr pivot_wider
#' @importFrom janitor adorn_totals
#' @importFrom rlang .data
#' @importFrom magrittr %>%
#' @examples
#' psa_context(base_ag, var_pop = "poblacion", var_year = "anio", var_sex = "sexo")
#' @export



psa_context <- function(data, var_pop, var_terr = NULL, var_year, var_sex) {

    # 1. Carga de librerías
    #pkg::func(data.table)
    #pkg::func(dplyr)
    #pkg::func(tidyr)
    #pkg::func(janitor)

    if(is.null(var_terr)){
        # total population
        tot_pop <- data[, .(tot_pop = sum(get(var_pop), na.rm = TRUE)),
                        by = var_year] %>%
            setDT()
        # population growth rate
        growth_anual_rate <- data[,.(pop_anual = sum (get(var_pop))),
                                  by = var_year] %>%
            .[,pop_growth:=(lead(pop_anual)-pop_anual)] %>%
            .[,mean_pop:=(lead(pop_anual)+pop_anual)/2] %>%
            .[,.(growth_anual_rate = (pop_growth/mean_pop)*1000),.(get(var_year))] %>%
            .[!is.na(growth_anual_rate)]
    }else{
        # total population
        tot_pop <- data[, .(tot_pop = sum(get(var_pop), na.rm = TRUE)),
                        by = c(var_year, var_terr)] %>%
            pivot_wider(names_from = all_of(var_year), values_from = 'tot_pop') %>%
            setDT() %>%
            adorn_totals("row")
        # population growth rate
        growth_anual_rate <- data[,.(pop_anual=sum(get(var_pop))),
                                  by = c(var_year,var_terr)] %>%
            setorderv(var_terr) %>%
            .[,pop_growth:=(lead(pop_anual)-pop_anual), by = var_terr] %>%
            .[,mean_pop:=(lead(pop_anual)+pop_anual)/2, by = var_terr] %>%
            .[,.(growth_anual_rate = (pop_growth/mean_pop)*1000),
              by = c(var_year, var_terr)] %>%
            .[!is.na(growth_anual_rate)] %>%
            pivot_wider(names_from = var_year, values_from = 'growth_anual_rate')
    }

    result <- list(tot_pop, growth_anual_rate)
    #result[[1]] <- tot_pop
    #result[[2]] <- growth_anual_rate
    #return(result)
}
