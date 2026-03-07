# tests/test_scoring_engine.R
# Pruebas unitarias para validate_input_integrity usando testthat

library(testthat)

# Mock de la función warn si no está cargada desde el pipeline
if (!exists("warn")) {
  warn <- function(msg, ctx = "") {
    message(sprintf("WARN [%s] %s", ctx, msg))
  }
}

# Cargar la función a probar
# En un paquete R, esto se haría mediante devtools::load_all() o similar.
# Para este script, asumiendo ejecución desde la raíz:
if (file.exists("R/02_scoring_engine.R")) {
  source("R/02_scoring_engine.R")
}

context("Validación de Integridad de Entrada")

test_that("validate_input_integrity detecta DataFrame vacío", {
  empty_df <- data.frame()
  map <- data.frame(FORMA_CODE = "F1", COL_NAME_RAW = "P1", ITEM_ID = "I1", KEY = "A")
  expect_error(validate_input_integrity(empty_df, map), "está vacío")
})

test_that("validate_input_integrity detecta Mapa de Diseño vacío o NULL", {
  raw_df <- data.frame(ID = "1", FORMA = "F1", P1 = "A")
  empty_map <- data.frame()
  expect_error(validate_input_integrity(raw_df, empty_map), "está vacío")
  expect_error(validate_input_integrity(raw_df, NULL), "está vacío")
})

test_that("validate_input_integrity detecta formas faltantes en el mapa", {
  raw_df <- data.frame(ID = "1", FORMA = "F2", P1 = "A")
  map <- data.frame(FORMA_CODE = "F1", COL_NAME_RAW = "P1", ITEM_ID = "I1", KEY = "A")
  expect_error(validate_input_integrity(raw_df, map), "no están definidas en el mapa")
})

test_that("validate_input_integrity detecta columnas faltantes", {
  raw_df <- data.frame(ID = "1", FORMA = "F1", P2 = "A")
  map <- data.frame(FORMA_CODE = "F1", COL_NAME_RAW = "P1", ITEM_ID = "I1", KEY = "A")
  expect_error(validate_input_integrity(raw_df, map), "Faltan 1 columnas requeridas")
})

test_that("validate_input_integrity pasa con entrada válida y retorna TRUE", {
  raw_df <- data.frame(ID = "1", FORMA = "F1", P1 = "A")
  map <- data.frame(FORMA_CODE = "F1", COL_NAME_RAW = "P1", ITEM_ID = "I1", KEY = "A")
  res <- validate_input_integrity(raw_df, map)
  expect_true(res)
})
