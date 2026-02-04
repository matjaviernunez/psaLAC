
#' @keywords internal
"_PACKAGE"

if (getRversion() >= "2.15.1") {
    utils::globalVariables(c(
        ".",             # si lo usas en dplyr/data.table
        "pop_growth", "pop_anual", "mean_pop"
    ))
}
