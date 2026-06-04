# Pruebas para psa_context()
#
# Estructura recomendada por UNFPA-LACRO (Producto 2):
#   - Camino feliz: la función devuelve el resultado correcto con
#     inputs estándares.
#   - Casos borde: ejecución a nivel nacional/subnacional,
#     anio_piramide por defecto vs. explícito.
#   - Pruebas de error: la función se detiene con mensaje informativo
#     cuando los inputs son inválidos.

data(base_ag)

# ----------------------------------------------------------------
# 1. Camino feliz: nivel nacional, parámetros estándar
# ----------------------------------------------------------------
test_that("psa_context devuelve una lista con 10 elementos a nivel nacional", {
  res <- psa_context(
    data         = base_ag,
    var_pob      = "poblacion",
    var_div     = NULL,
    var_anio     = "anio",
    var_sexo      = "sexo",
    var_edad      = "edad",
    var_area     = "area",
    anio_piramide = NULL
  )

  expect_type(res, "list")
  expect_length(res, 10)
  expect_named(res, c(
    "tabla_poblacion", "tabla_crecimiento", "tabla_edad",
    "tabla_dependencia", "tabla_urbanizacion",
    "grafico_poblacion", "grafico_crecimiento", "grafico_edad",
    "grafico_dependencia", "grafico_urbanizacion"
  ))
})

test_that("las tablas de salida son data.table no vacías (nivel nacional)", {
  res <- psa_context(
    data    = base_ag, var_pob = "poblacion", var_div = NULL,
    var_anio = "anio", var_sexo = "sexo", var_edad = "edad",
    var_area = "area", anio_piramide = NULL
  )

  expect_s3_class(res$tabla_poblacion,    "data.table")
  expect_s3_class(res$tabla_crecimiento,  "data.table")
  expect_s3_class(res$tabla_edad,         "data.table")
  expect_s3_class(res$tabla_dependencia,  "data.table")
  expect_s3_class(res$tabla_urbanizacion, "data.table")

  expect_gt(nrow(res$tabla_poblacion),    0)
  expect_gt(nrow(res$tabla_crecimiento),  0)
  expect_gt(nrow(res$tabla_edad),         0)
  expect_gt(nrow(res$tabla_dependencia),  0)
  expect_gt(nrow(res$tabla_urbanizacion), 0)
})

test_that("los gráficos son objetos ggplot", {
  res <- psa_context(
    data    = base_ag, var_pob = "poblacion", var_div = NULL,
    var_anio = "anio", var_sexo = "sexo", var_edad = "edad",
    var_area = "area", anio_piramide = NULL
  )

  expect_s3_class(res$grafico_poblacion,    "ggplot")
  expect_s3_class(res$grafico_crecimiento,  "ggplot")
  expect_s3_class(res$grafico_edad,         "ggplot")
  expect_s3_class(res$grafico_dependencia,  "ggplot")
  expect_s3_class(res$grafico_urbanizacion, "ggplot")
})

test_that("tabla_dependencia contiene los porcentajes esperados", {
  res <- psa_context(
    data    = base_ag, var_pob = "poblacion", var_div = NULL,
    var_anio = "anio", var_sexo = "sexo", var_edad = "edad",
    var_area = "area", anio_piramide = NULL
  )

  esperados <- c("pob_0_14_porcen", "pob_15_64_porcen",
                 "pob_65_mas_porcen", "rela_dep")
  expect_true(all(esperados %in% names(res$tabla_dependencia)))

  # Los porcentajes deben sumar ~100 por año
  tot <- with(res$tabla_dependencia,
              pob_0_14_porcen + pob_15_64_porcen + pob_65_mas_porcen)
  expect_true(all(abs(tot - 100) < 1e-6))
})

# ----------------------------------------------------------------
# 2. Casos borde
# ----------------------------------------------------------------
test_that("psa_context se ejecuta a nivel subnacional cuando se entrega var_div", {
  res <- psa_context(
    data    = base_ag, var_pob = "poblacion", var_div = "div1",
    var_anio = "anio", var_sexo = "sexo", var_edad = "edad",
    var_area = "area", anio_piramide = NULL
  )

  expect_type(res, "list")
  expect_length(res, 10)
  # tabla_poblacion en versión subnacional tiene una columna por año
  expect_true("div1" %in% names(res$tabla_poblacion))
})

test_that("anio_piramide explícito es respetado", {
  yr <- 2020
  res <- psa_context(
    data    = base_ag, var_pob = "poblacion", var_div = NULL,
    var_anio = "anio", var_sexo = "sexo", var_edad = "edad",
    var_area = "area", anio_piramide = yr
  )

  # El subtítulo del gráfico debe mencionar el año seleccionado
  sub <- res$grafico_edad$labels$subtitle
  expect_true(grepl(as.character(yr), sub))
})

# ----------------------------------------------------------------
# 3. Pruebas de error
# ----------------------------------------------------------------
test_that("psa_context se detiene si data es NULL", {
  expect_error(
    psa_context(
      data = NULL, var_pob = "poblacion", var_div = NULL,
      var_anio = "anio", var_sexo = "sexo", var_edad = "edad",
      var_area = "area", anio_piramide = NULL
    ),
    "no puede ser NULL"
  )
})

test_that("psa_context se detiene si data no es un data.frame", {
  expect_error(
    psa_context(
      data = "no_es_un_df", var_pob = "poblacion", var_div = NULL,
      var_anio = "anio", var_sexo = "sexo", var_edad = "edad",
      var_area = "area", anio_piramide = NULL
    ),
    "data\\.frame"
  )
})

test_that("psa_context se detiene si data está vacío", {
  expect_error(
    psa_context(
      data = base_ag[0, ], var_pob = "poblacion", var_div = NULL,
      var_anio = "anio", var_sexo = "sexo", var_edad = "edad",
      var_area = "area", anio_piramide = NULL
    ),
    "no contiene filas"
  )
})

test_that("psa_context se detiene si falta una columna requerida", {
  base_sin_area <- base_ag[, setdiff(names(base_ag), "area"), with = FALSE]
  if (!is.data.frame(base_sin_area)) {
    base_sin_area <- subset(base_ag, select = setdiff(names(base_ag), "area"))
  }

  expect_error(
    psa_context(
      data = base_sin_area, var_pob = "poblacion", var_div = NULL,
      var_anio = "anio", var_sexo = "sexo", var_edad = "edad",
      var_area = "area", anio_piramide = NULL
    ),
    "no existen en 'data'"
  )
})

test_that("psa_context se detiene si var_pob no es numérico", {
  base_mod <- data.table::copy(base_ag)
  base_mod$poblacion <- as.character(base_mod$poblacion)

  expect_error(
    psa_context(
      data = base_mod, var_pob = "poblacion", var_div = NULL,
      var_anio = "anio", var_sexo = "sexo", var_edad = "edad",
      var_area = "area", anio_piramide = NULL
    ),
    "debe ser numérica"
  )
})

test_that("psa_context se detiene si anio_piramide no existe en data", {
  expect_error(
    psa_context(
      data = base_ag, var_pob = "poblacion", var_div = NULL,
      var_anio = "anio", var_sexo = "sexo", var_edad = "edad",
      var_area = "area", anio_piramide = 1800
    ),
    "no existe en la columna"
  )
})

test_that("psa_context se detiene si los nombres de columna no son character", {
  expect_error(
    psa_context(
      data = base_ag, var_pob = 1, var_div = NULL,
      var_anio = "anio", var_sexo = "sexo", var_edad = "edad",
      var_area = "area", anio_piramide = NULL
    ),
    "character de longitud 1"
  )
})
