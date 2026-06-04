# Pruebas para psa_dinam_demogra()
#
# Estructura recomendada por UNFPA-LACRO (Producto 2):
#   - Camino feliz: la función devuelve el resultado correcto con
#     inputs estándares.
#   - Casos borde: combinaciones opcionales de bases, nivel
#     subnacional, exclusión de años, suavizado desactivado.
#   - Pruebas de error: la función se detiene con mensaje informativo
#     cuando los inputs son inválidos.

data(base_pob)
data(base_nac)
data(base_def)
data(base_mig)

# Subconjuntos ligeros para acelerar los tests
pob_sub <- base_pob[base_pob$anio %in% c(2022, 2023), ]
nac_sub <- base_nac[base_nac$anio %in% c(2022, 2023), ]
def_sub <- base_def[base_def$anio %in% c(2022, 2023), ]
mig_sub <- base_mig[base_mig$anio %in% c(2022, 2023), ]

# ----------------------------------------------------------------
# 1. Camino feliz
# ----------------------------------------------------------------
test_that("psa_dinam_demogra devuelve $fecundidad con 4 elementos correctos", {
  res <- psa_dinam_demogra(data_pob = pob_sub, data_nac = nac_sub)

  expect_type(res, "list")
  expect_true("fecundidad" %in% names(res))
  expect_named(res$fecundidad,
               c("tabla_tef", "tabla_tgf", "grafico_tef", "grafico_tgf"))
})

test_that("psa_dinam_demogra devuelve $mortalidad con 4 elementos correctos", {
  res <- psa_dinam_demogra(data_pob = pob_sub, data_def = def_sub)

  expect_type(res, "list")
  expect_true("mortalidad" %in% names(res))
  expect_named(res$mortalidad,
               c("tabla_tbm", "tabla_vida", "grafico_tbm", "grafico_e0"))
})

test_that("psa_dinam_demogra devuelve $migracion con 3 elementos correctos", {
  res <- psa_dinam_demogra(data_pob = pob_sub, data_mig = mig_sub)

  expect_type(res, "list")
  expect_true("migracion" %in% names(res))
  expect_named(res$migracion,
               c("tabla_migracion", "grafico_smn", "grafico_tbmig"))
})

test_that("psa_dinam_demogra devuelve los tres bloques cuando se pasan las tres bases", {
  res <- psa_dinam_demogra(
    data_pob = pob_sub,
    data_nac = nac_sub,
    data_def = def_sub,
    data_mig = mig_sub
  )

  expect_type(res, "list")
  expect_length(res, 3)
  expect_named(res, c("fecundidad", "mortalidad", "migracion"))
})

test_that("las tablas de salida son data.table no vacías", {
  res <- psa_dinam_demogra(
    data_pob = pob_sub,
    data_nac = nac_sub,
    data_def = def_sub,
    data_mig = mig_sub
  )

  expect_s3_class(res$fecundidad$tabla_tef,       "data.table")
  expect_s3_class(res$fecundidad$tabla_tgf,       "data.table")
  expect_s3_class(res$mortalidad$tabla_tbm,       "data.table")
  expect_s3_class(res$mortalidad$tabla_vida,      "data.table")
  expect_s3_class(res$migracion$tabla_migracion,  "data.table")

  expect_gt(nrow(res$fecundidad$tabla_tef),      0)
  expect_gt(nrow(res$fecundidad$tabla_tgf),      0)
  expect_gt(nrow(res$mortalidad$tabla_tbm),      0)
  expect_gt(nrow(res$mortalidad$tabla_vida),     0)
  expect_gt(nrow(res$migracion$tabla_migracion), 0)
})

test_that("los gráficos son objetos ggplot", {
  res <- psa_dinam_demogra(
    data_pob = pob_sub,
    data_nac = nac_sub,
    data_def = def_sub,
    data_mig = mig_sub
  )

  expect_s3_class(res$fecundidad$grafico_tef,    "ggplot")
  expect_s3_class(res$fecundidad$grafico_tgf,    "ggplot")
  expect_s3_class(res$mortalidad$grafico_tbm,    "ggplot")
  expect_s3_class(res$mortalidad$grafico_e0,     "ggplot")
  expect_s3_class(res$migracion$grafico_smn,     "ggplot")
  expect_s3_class(res$migracion$grafico_tbmig,   "ggplot")
})

test_that("tabla_migracion contiene las columnas esperadas", {
  res <- psa_dinam_demogra(data_pob = pob_sub, data_mig = mig_sub)

  esperadas <- c("anio", "entries", "departures", "smn", "tbmig")
  expect_true(all(esperadas %in% names(res$migracion$tabla_migracion)))
})

test_that("tabla_vida contiene las columnas de la tabla de vida abreviada", {
  res <- psa_dinam_demogra(data_pob = pob_sub, data_def = def_sub)

  col_vida <- c("mx", "qx", "lx", "dx", "Lx", "Tx", "ex")
  expect_true(all(col_vida %in% names(res$mortalidad$tabla_vida)))
})

# ----------------------------------------------------------------
# 2. Casos borde
# ----------------------------------------------------------------
test_that("psa_dinam_demogra se ejecuta a nivel subnacional con var_div", {
  res <- psa_dinam_demogra(
    data_pob = pob_sub,
    data_nac = nac_sub,
    var_div  = "div1"
  )

  expect_type(res, "list")
  expect_true("fecundidad" %in% names(res))
  expect_true("div1" %in% names(res$fecundidad$tabla_tef))
})

test_that("anios_excluir reduce los años en la tabla de migración", {
  res_completo <- psa_dinam_demogra(data_pob = pob_sub, data_mig = mig_sub)
  res_excluido <- psa_dinam_demogra(
    data_pob      = pob_sub,
    data_mig      = mig_sub,
    anios_excluir = 2023
  )

  anios_completo <- res_completo$migracion$tabla_migracion$anio
  anios_excluido <- res_excluido$migracion$tabla_migracion$anio

  expect_false(2023 %in% anios_excluido)
  expect_true(length(anios_completo) > length(anios_excluido))
})

test_that("suavizar_mx = FALSE ejecuta sin error y devuelve tabla_vida", {
  res <- psa_dinam_demogra(
    data_pob    = pob_sub,
    data_def    = def_sub,
    suavizar_mx = FALSE
  )

  expect_true("mortalidad" %in% names(res))
  expect_s3_class(res$mortalidad$tabla_vida, "data.table")
})

test_that("con solo data_pob la función devuelve una lista vacía sin error", {
  res <- psa_dinam_demogra(data_pob = pob_sub)

  expect_type(res, "list")
  expect_length(res, 0)
})

# ----------------------------------------------------------------
# 3. Pruebas de error
# ----------------------------------------------------------------
test_that("psa_dinam_demogra se detiene si data_pob es NULL", {
  expect_error(
    psa_dinam_demogra(data_pob = NULL),
    "no puede ser NULL"
  )
})

test_that("psa_dinam_demogra se detiene si data_pob no es un data.frame", {
  expect_error(
    psa_dinam_demogra(data_pob = "no_es_df"),
    "data\\.frame"
  )
})

test_that("psa_dinam_demogra se detiene si data_pob está vacío", {
  expect_error(
    psa_dinam_demogra(data_pob = pob_sub[0, ]),
    "no contiene filas"
  )
})

test_that("psa_dinam_demogra se detiene si falta una columna en data_pob", {
  pob_sin_edad <- pob_sub[, setdiff(names(pob_sub), "edad"), with = FALSE]

  expect_error(
    psa_dinam_demogra(data_pob = pob_sin_edad),
    "no existen en 'data_pob'"
  )
})

test_that("psa_dinam_demogra se detiene si var_pob no es numérico", {
  pob_mod <- data.table::copy(pob_sub)
  pob_mod$poblacion <- as.character(pob_mod$poblacion)

  expect_error(
    psa_dinam_demogra(data_pob = pob_mod),
    "debe ser numérica"
  )
})

test_that("psa_dinam_demogra se detiene si data_nac no es un data.frame", {
  expect_error(
    psa_dinam_demogra(data_pob = pob_sub, data_nac = list()),
    "data\\.frame"
  )
})

test_that("psa_dinam_demogra se detiene si data_nac está vacío", {
  expect_error(
    psa_dinam_demogra(data_pob = pob_sub, data_nac = nac_sub[0, ]),
    "no contiene filas"
  )
})

test_that("psa_dinam_demogra se detiene si falta una columna en data_def", {
  def_sin_sexo <- def_sub[, setdiff(names(def_sub), "sexo"), with = FALSE]

  expect_error(
    psa_dinam_demogra(data_pob = pob_sub, data_def = def_sin_sexo),
    "no existen en 'data_def'"
  )
})

test_that("psa_dinam_demogra se detiene si falta una columna en data_mig", {
  mig_sin_tipo <- mig_sub[, setdiff(names(mig_sub), "tipo_mov"), with = FALSE]

  expect_error(
    psa_dinam_demogra(data_pob = pob_sub, data_mig = mig_sin_tipo),
    "no existen en 'data_mig'"
  )
})

test_that("psa_dinam_demogra se detiene si var_pob no es character de longitud 1", {
  expect_error(
    psa_dinam_demogra(data_pob = pob_sub, var_pob = 1),
    "character de longitud 1"
  )
})
