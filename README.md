# Shiny Dashboard for Multi-Modal 10X Metrics Visualization




https://github.com/user-attachments/assets/95175841-d862-40c9-a9a0-d814fe494fa0





This Shiny application provides an interactive interface for visualizing and exploring sequencing quality metrics from 10X Genomics output directories (e.g., `metrics_summary.csv`, `summary.csv`, `web_summary.html`) across multiple library types (Gene Expression, VDJ, Antibody Capture, ATAC, etc.). It supports flexible file traversal, comparison of multiple directories, dynamic plotting, and downloadable reports.

## Features

-  Directory selection with `shinyFiles` for recursive metrics loading
-  Multiple visualization tabs: bar charts, PCA, correlation, and heatmaps
-  Interactive trend plots across user-selected metrics
-  Sample batch optimization using the OSAT package (if included)
-  Embedded HTML web summaries (e.g., Cell Ranger reports)
-  Exportable tables with `DT` and screenshot functionality

## Directory Structure

The application expects 10X Genomics output folders with paths like:
```

project\_name/
└── sample\_name/
└── outs/
├── metrics\_summary.csv
├── summary.csv
└── web\_summary.html

````

## Installation

Make sure the following R packages are installed:
```r
install.packages(c(
  "shiny", "shinydashboard", "shinyFiles", "shinyscreenshot", "DT",
  "dplyr", "tidyr", "ggplot2", "heatmaply", "plotly", "factoextra", 
  "ggcorrplot", "janitor", "purrr", "readr", "tibble", "stringr"
))
````

Also install any required Bioconductor packages, e.g.:

```r
if (!requireNamespace("BiocManager", quietly = TRUE))
  install.packages("BiocManager")
BiocManager::install("OSAT")
```

## Running the App

Run the application using:

```r
source("run_app.R")
run_app()
```

## 📁 Modules Overview

| Module                 | Purpose                                                 |
| ---------------------- | ------------------------------------------------------- |
| `mod_dataInput`        | Sidebar UI for file selection and control parameters    |
| `mod_dataInput2`       | Main dashboard with tabs for tables and multiple plots  |
| `mod_dataInput3`       | Delta tab for comparing two directories                 |
| `mod_dataInput4`       | Plotting selected metric trends across samples          |
| `mod_dataInput5`       | File list view in a searchable table                    |
| `mod_table` (optional) | Sample batch optimization using OSAT                    |
| `mod_box` (optional)   | Boxplot of log-transformed values from selected columns |

## Screenshots

Users can capture UI states using the `Capture Screenshot` button and download results in multiple formats via DataTables.

## Use Case

This app is ideal for researchers working with:

* Single-cell RNA-seq (GEX)
* Antibody capture (CITE-seq)
* TCR/BCR VDJ libraries
* ATAC-seq and custom feature libraries

##  Development Notes

* Built using the [golem](https://thinkr-open.github.io/golem/) framework
* `run_app()` initializes `app_ui` and `app_server` via `shinyApp()`

## Author

Developed by Foo Cheung 

---

