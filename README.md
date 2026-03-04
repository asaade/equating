# Orquestador Pipeline Psicométrico

Este es el Orquestador Pipeline Psicométrico, un sistema integral para la evaluación psicométrica en producción. Escrito en R, proporciona un pipeline automatizado para el análisis de la Teoría Clásica de los Tests (CTT), la Teoría de Respuesta al Ítem (IRT), y la equiparación (Equating).

## Estructura del Proyecto

El proyecto está organizado de la siguiente manera:

* `R/`: Contiene todos los módulos del pipeline (ingesta de datos, scoring, análisis CTT/IRT, equating, DIF, y generación de reportes).
* `config/`: Archivos de configuración YAML (`config.yaml`, etc.) para controlar la ejecución del pipeline y las exclusiones de ítems.
* `docs/`: Documentación detallada del proyecto (arquitectura, configuración, reportes, etc.).
* `main.R`: El script principal (entry point) que ejecuta todo el pipeline secuencialmente.
* `setup_project.R`: Script para inicializar la estructura de directorios requerida.
* `test.R`: Scripts de pruebas y validación.

## Requisitos Previos

* R (versión reciente recomendada)
* Paquetes de R necesarios, incluyendo `equate` (y otros que pueden ser definidos en los módulos).

## Instalación y Configuración Inicial

1. Clona el repositorio a tu entorno local.
2. Ejecuta el script de configuración para crear la estructura de directorios necesaria (`data/input`, `output/CTT`, etc.):

   ```bash
   Rscript setup_project.R
   ```

3. Coloca tus archivos de datos de entrada (e.g., `.DAT`, `.KEY`, y metadatos en CSV) en el directorio `data/input/`.

## Configuración

El comportamiento del pipeline se controla a través del archivo de configuración `config/config.yaml`.
Ahí puedes definir:

* Rutas de los archivos de entrada (datos, claves, metadatos, datos demográficos).
* Parámetros de exclusión de ítems.
* Configuraciones específicas para las distintas fases del pipeline.

Para obtener más detalles sobre la configuración, consulta la documentación en `docs/config.md`.

## Uso y Ejecución

Para iniciar el proceso completo del pipeline, simplemente ejecuta el orquestador principal:

```bash
Rscript main.R
```

El pipeline ejecutará secuencialmente las siguientes fases y guardará los resultados y reportes en el directorio de salida configurado (por defecto en las carpetas dentro de `output/`):

1. **Ingesta de Datos** (`01_data_ingest.R`)
2. **Scoring Preliminar y Muestreo** (`02_scoring_engine.R`)
3. **Análisis CTT e IRT** (`03a_ctt_analysis.R`, `03b_irt_model.R`)
4. **Equating / Equiparación** (`04_equating.R`)
5. **Análisis de Sesgo (DIF)** (`05_dif_analysis.R`)
6. **Scoring Final** (`06_scoring_final.R`)
7. **Generación de Reportes** (`07*`)
8. **Exportación de Resultados** (`08_export_results.R`)

## Documentación Adicional

Puedes encontrar documentación más detallada sobre módulos específicos en la carpeta `docs/`.
