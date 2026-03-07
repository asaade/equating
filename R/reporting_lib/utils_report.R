# ==============================================================================
# UTILERÍAS DE FORMATEO PARA REPORTES PSICOMÉTRICOS
# Responsabilidad: Funciones comunes de texto, tablas ASCII y decoración.
# ==============================================================================

#' Pad a string to a specific width
#' @param x String or vector to pad
#' @param width Total width including padding
#' @param align "left" or "right"
pad_str <- function(x, width, align = "left") {
  x <- as.character(x)
  x[is.na(x) | x == "NA"] <- ""

  eff_width <- width - 1
  if (any(nchar(x) > eff_width)) {
    long <- nchar(x) > eff_width
    x[long] <- paste0(substr(x[long], 1, max(0, eff_width - 2)), "..")
  }

  formatted <- if (align == "right") {
    formatC(x, width = eff_width, flag = "")
  } else {
    formatC(x, width = eff_width, flag = "-")
  }
  paste0(formatted, " ")
}

#' Format numeric vectors with padding
#' @param x Numeric vector
#' @param digits Number of decimal places
#' @param width Total width
fmt_num <- function(x, digits = 4, width = 8) {
  vapply(x, function(val) {
    if (is.null(val) || is.na(val) || !is.numeric(val)) {
      return(pad_str("-", width, "right"))
    }
    txt <- sprintf(paste0("%.", digits, "f"), val)
    pad_str(txt, width, "right")
  }, FUN.VALUE = character(1))
}

#' Draw an ASCII progress or impact bar
#' @param val Value to represent
#' @param limit Reference maximum value
#' @param width Total character width
#' @param center If TRUE, bar starts from center (for bidirectional values)
draw_ascii_bar <- function(val, limit = 3, width = 10, center = FALSE) {
  if (is.na(val)) {
    return(pad_str("", width))
  }
  eff_w <- width - 2
  if (center) {
    norm_val <- max(-1, min(1, val / limit))
    center_idx <- floor(eff_w / 2)
    len <- floor(abs(norm_val) * (eff_w / 2))
    chars <- strrep(" ", eff_w)
    fill_char <- if (val < 0) "<" else ">"
    if (abs(val) > limit) fill_char <- "!"
    left <- center_idx - (if (val < 0) len else 0)
    # Ensure indices are within bounds
    if (len >= 0) {
       substr(chars, left, left + len) <- strrep(fill_char, len + 1)
    }
    substr(chars, center_idx, center_idx) <- "|"
    return(paste0("[", chars, "]"))
  } else {
    mag <- min(abs(val), limit) / limit
    len <- round(mag * eff_w)
    len <- max(0, min(len, eff_w))
    char <- "#"
    if (val > limit * 0.8) char <- "!"
    bar <- strrep(char, len)
    space <- strrep(" ", eff_w - len)
    return(paste0("[", bar, space, "]"))
  }
}

#' Print a major header to console
print_header <- function(title) {
  width <- 100
  cat(paste0("\n", strrep("=", width), "\n"))
  cat(paste0("  ", title, "\n"))
  cat(paste0(strrep("=", width), "\n"))
}

#' Print a section divider
print_section <- function(title) {
  width <- 80
  cat(paste0("\n", strrep("-", width), "\n"))
  cat(paste0(">>> ", title, "\n"))
  cat(paste0(strrep("-", width), "\n"))
}

#' Print an alert box with messages
print_alert_box <- function(msgs, type = "CRITICAL") {
  if (length(msgs) == 0) {
    return()
  }
  width <- 100
  border_char <- if (type == "CRITICAL") "!" else "*"
  cat(paste0("\n", strrep(border_char, width), "\n"))
  cat(paste0("  >>> ALERTA DE AUDITORÍA [", type, "]:\n"))
  for (m in msgs) cat(paste0("      - ", m, "\n"))
  cat(paste0(strrep(border_char, width), "\n"))
}
