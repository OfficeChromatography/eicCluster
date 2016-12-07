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

## Features

* mzXML loading
* metadata access
* experimental conditions selection
* TIC plotting: general, positive and negative
* interactive mass spectrum selection
* interactive zoom on all plot
* peak table of the selected mass spectrum
* peak difference to base peak and recognition of more common pattern (Cl 35-37 gap for example)
* background substraction
* extracted ion chromatograms
* structure elucidation
* multivariate variable selection for background removal (experimental)
* report download in pdf, html and word format

## Use

### Data input

The RAW file produced with excalibur must be converted in mzXML file first. One possibility is to use MSConvert from proteowizard. An important point is to set the recording to centroid in R otherwise the data couldn't be loaded by R.

It is also important to work with two polarity alternatively because the software take it as convention, having the positive or the negative start is not a problem but two analyser condition are necessary.

### Exploration

To use the zoom, brush the plot to select a zone, then doubleclick to apply the zoom. An other doubleclick without selected zone will reset the zoom. Note that at the opening, there is a zoom applied already, I'll get rid of it in the future.

To select a spectrum either in positive or in negative, click to the corresponding TIC plot. It is then possible to move to the previous or the next spectrum by pressing the X and C key respectively. The generale TIC plot shows a red  verticale line indicating the selected spectrum and can help selecting the best spectrum.

Background substraction should be applied by selecting a blank spectrum and click on the action button named _Background Spectrum_. This manipulation should be made for each polarity. THe way the peak are then substracted is by rounding the masses to the milli dalton and removing all the present masses in the other spectrum.

### Report

The report contain one first page with the TIC plot in both polarity. On those plot is also shown by verticale lines the selected spectrum of interest.

The next pages will contain the spectrum of interest, the full scan and the zoom applied during the selection. To select a spectrum of interest, simply press the _select for report_ button.
