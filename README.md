# Shiny Dashboard for Multi-Modal 10X Metrics Visualization




https://github.com/user-attachments/assets/95175841-d862-40c9-a9a0-d814fe494fa0





This Shiny application provides an interactive interface for visualizing and exploring sequencing quality metrics from 10X Genomics output directories (e.g., `metrics_summary.csv`, `summary.csv`, `web_summary.html`) across multiple library types (Gene Expression, VDJ, Antibody Capture, ATAC, etc.). It supports flexible file traversal, comparison of multiple directories, dynamic plotting, and downloadable reports.

## Features

-  Directory selection with `shinyFiles` for recursive metrics loading
-  Multiple visualization tabs: bar charts, PCA, correlation, and heatmaps
-  Interactive trend plots across user-selected metrics
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



## Running the App

Run the application using:

```r
source("run_app.R")
run_app()
```


## Screenshots

Users can capture UI states using the `Capture Screenshot` button and download results in multiple formats via DataTables.

## Use Case

This app is ideal for researchers working with:

* Single-cell RNA-seq (GEX)
* Antibody capture (CITE-seq)
* TCR/BCR VDJ libraries
* ATAC-seq and custom feature libraries


## Author

Developed by Foo Cheung 

---

