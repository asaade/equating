# R/equating_lib/09_ux_helpers.R
# Responsabilidad: Utilidades de UX, Seguridad (Clamping) y Traducción Semántica.
# Dependencias: Ninguna (Base R)
# Versión: v1.0

# -----------------------------------------------------------------------------
# 1. HARDENING: Clamping de Puntajes (OBLIGATORIO)
# -----------------------------------------------------------------------------

#' @title Asegurar rango físico de puntajes
#' @description Fuerza que todos los puntajes equiparados (yx) y bandas de confianza
#' estén dentro de [min_score, max_score]. Previene puntajes negativos o > 100%.
#' Es la última línea de defensa antes de publicar una tabla.
clamp_scores <- function(concordance_df, min_score = 0, max_score = NULL) {
  if (is.null(concordance_df)) {
    return(NULL)
  }

  # Si no se define max_score, intentar inferirlo de la escala cruda (scale)
  # Esto asume que scale representa los puntajes crudos posibles.
  if (is.null(max_score)) {
    if ("scale" %in% names(concordance_df)) {
      max_score <- max(concordance_df$scale, na.rm = TRUE)
    } else {
      # Fallback: Si no hay max_score, usamos infinito para no romper el flujo,
      # pero mantenemos el min_score en 0.
      max_score <- Inf
    }
  }

  # Clamping de la media (yx)
  if ("yx" %in% names(concordance_df)) {
    concordance_df$yx <- pmax(min_score, pmin(concordance_df$yx, max_score))
  }

  # Clamping de bandas de error si existen en columnas comunes
  # Esto evita conos de error que sugieran puntajes imposibles (ej. -2.5)
  cols_to_clamp <- c("upper", "lower", "yx_low", "yx_high")
  for (col in cols_to_clamp) {
    if (col %in% names(concordance_df)) {
      concordance_df[[col]] <- pmax(min_score, pmin(concordance_df[[col]], max_score))
    }
  }

  return(concordance_df)
}

# -----------------------------------------------------------------------------
# 2. SEMÁNTICA: Traductor de Métodos para Reportes
# -----------------------------------------------------------------------------

#' @title Etiquetado Amigable para Reportes
#' @description Traduce los códigos técnicos de R a lenguaje administrativo.
get_method_friendly_label <- function(raw_label) {
  if (is.null(raw_label)) {
    return("Desconocido")
  }
  raw_lower <- tolower(raw_label)

  if (grepl("tucker", raw_lower)) {
    return("Lineal (Tucker) - Optimizado")
  }
  if (grepl("levine", raw_lower)) {
    return("Lineal (Levine) - Varianza Robusta")
  }
  if (grepl("frequency", raw_lower)) {
    return("Equipercentil - Alta Precisión")
  }
  if (grepl("mean", raw_lower)) {
    return("Ajuste de Media (Datos Limitados)")
  }
  if (grepl("identity", raw_lower)) {
    return("Identidad (Datos Insuficientes)")
  }

  # Fallback genérico
  return(paste("Método:", raw_label))
}

# -----------------------------------------------------------------------------
# 3. SEMÁNTICA: Semáforo de Calidad
# -----------------------------------------------------------------------------

#' @title Clasificación de Confianza (Semáforo)
#' @description Genera una calificación cualitativa basada en el error estándar ponderado.
get_confidence_level <- function(weighted_see, scale_range = NULL) {
  if (is.na(weighted_see) || is.infinite(weighted_see)) {
    return("DESCONOCIDO")
  }

  # Umbrales por defecto (Asumiendo escalas típicas 0-30 a 0-60)
  threshold_high <- 0.5 # Medio punto de error promedio es excelente
  threshold_med <- 1.0 # Un punto de error es aceptable

  # Si tenemos el rango (ej. 100 puntos), podemos hacer el umbral relativo
  if (!is.null(scale_range) && scale_range > 0) {
    # Ajustar umbral al 1% del rango total para calidad media
    threshold_med <- scale_range * 0.015
    # Calidad alta es la mitad de eso
    threshold_high <- threshold_med * 0.5
  }

  if (weighted_see < threshold_high) {
    return("EXCELENTE")
  }
  if (weighted_see < threshold_med) {
    return("BUENA")
  }

  return("PRECAUCIÓN") # Error alto, requiere revisión manual
}
