---
output: html_document
---

## Introduction

This repository contain a shiny app to remove background in HPTLC-HRMS data. Based on clusterisation of the extracted ion chromatograms, this methods intend to separate the ions of interest from the background ions in high-performance thin-layer chromatography coupled with high resolution mass spectrometry data.
The only function of the package launch a shiny app where the user can interactively analyse the data.
Several clusterisation algorithms are supported, e. g. principal component analysis, k-means, t-Distributed Stochastic Neighbor Embedding.

## Installation

Install R
https://www.r-project.org/

In the console, install the devtools and eicCluster packages with those commands
```r
install.packages("devtools")
library(devtools)
install_github("dimitrif/eicCluster")
```

Then, run this command to launch the application
```r
eicCluster::run_eicCluster()
```

## Use

### Data input

### Preparation


### Visualization


