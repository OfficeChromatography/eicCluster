---
output: html_document
---

## Introduction

This repository contain a shiny app to remove background in HPTLC-HRMS data. Based on clusterisation of the extracted ion chromatograms, this method intends to separate the ions of interest from the background ions in high-performance thin-layer chromatography coupled with high resolution mass spectrometry data.
The only function of the package launch a shiny app where the user can interactively analyse the data.

## Installation

Install R
https://www.r-project.org/

#### From cran

Incomming

#### From github

In the console, install the devtools package with those commands
```r
install.packages("devtools")
library(devtools)
install_github("dimitrif/eicCluster")
```

Then, run this command to launch the application
```r
eicCluster::run_eicCluster()
```

