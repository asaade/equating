# ==============================================================================
# SCRIPT DE CONFIGURACIÓN DEL ENTORNO
# Descripción: Instala todas las librerías necesarias para el Dashboard PDF
# ==============================================================================

# 1. Lista de paquetes requeridos para el reporte visual
required_packages <- c(
  "rmarkdown", # Motor de generación de reportes
  "ggplot2", # Motor gráfico
  "dplyr", # Manipulación de datos
  "tidyr", # Transformación de datos (pivot_longer)
  "kableExtra", # Tablas avanzadas en LaTeX
  "gridExtra", # Arreglos de gráficos
  "scales", # Formato de ejes (porcentajes, etc.)
  "ggrepel", # Etiquetas de texto que no se superponen
  "tinytex" # Distribución ligera de LaTeX
)

# 2. Instalar paquetes faltantes
new_packages <- required_packages[!(required_packages %in% installed.packages()[, "Package"])]

if (length(new_packages)) {
  message("Instalando paquetes faltantes: ", paste(new_packages, collapse = ", "))
  install.packages(new_packages)
} else {
  message("Todos los paquetes de R están instalados.")
}

# 3. VERIFICACIÓN CRÍTICA DE LATEX
# RMarkdown necesita un compilador LaTeX para crear PDFs.
# La forma más fácil y ligera es usar 'tinytex'.

message("\nVerificando instalación de TinyTeX (LaTeX)...")

if (!tinytex::is_tinytex()) {
  message(">>> TinyTeX no detectado. Instalando ahora (esto puede tardar unos minutos)...")

  # Instala una versión ligera de TeX Live
  tinytex::install_tinytex()

  message(">>> Instalación de TinyTeX completada.")
} else {
  message(">>> Sistema LaTeX detectado correctamente.")
}

# 4. PRUEBA DE ESTADO
message("\n--- RESUMEN DE ESTADO ---")
message("R Packages : OK")
message("LaTeX Core : ", ifelse(tinytex::is_tinytex(), "OK", "PENDIENTE"))
message("Listo para ejecutar el generador de reportes.")
