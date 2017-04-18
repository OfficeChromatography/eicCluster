#### License ####
#Copyright (C) {2016}  {Fichou Dimitri}
#{dimitrifichou@laposte.net}

#This program is free software; you can redistribute it and/or modify
#it under the terms of the GNU General Public License as published by
#the Free Software Foundation; either version 2 of the License, or
# any later version.

#This program is distributed in the hope that it will be useful,
#but WITHOUT ANY WARRANTY; without even the implied warranty of
#MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#GNU General Public License for more details.

#You should have received a copy of the GNU General Public License along
#with this program; if not, write to the Free Software Foundation, Inc.,
#51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.

## functions for the app, hoping there will just be one file

getMeta <- function(data,index){
  pol = data[[index]]$metaData$polarity
  if(pol == "-"){pol="negative"}else{pol="positive"}
  MS_level = data[[index]]$metaData$msLevel
  if(MS_level == 1){
    cond = pol
  }else{
    precursorMz = round(data[[index]]$metaData$precursorMz,4)
    collisionEnergy = data[[index]]$metaData$collisionEnergy
    cond = paste0(pol," - MS",MS_level," - precursor ",precursorMz," m/z - ",collisionEnergy," eV")
  }
  return(cond)
}

getTime <- function(data,index){
  return(round(data[[index]]$metaData$retentionTime/60,2))
}

known_pattern <- function(df){
  colnames(df) <- c("m/z","intensity")
  df[,"diff to base peak"] <- df[,1] - df[1,1]
  df[,"Pattern"] <- rep("None",nrow(df))
  df[round(df[,3],3) == 1.003,"Pattern"] <- paste0("C13 ",round(df[1,2]/df[round(df[,3],3) == 1.003,2]/1.07)," of them")
  return(df)
}

standardNormalVariate <- function(X) {
  if (!class(X) %in% c("matrix", "data.frame"))
    stop("X should be a matrix or data.frame")
  X <- sweep(X, 1, rowMeans(X, na.rm = T), "-")
  X <- sweep(X, 1, apply(X, 1, sd, na.rm = T), "/")
  return(X)
}

## CODA stollen from ptw package, full credit to them
coda <- function(x, window = 5, smoothing = c("median", "mean"))
{
  smoothing <- match.arg(smoothing)

  if (is.vector(x)) x <- matrix(x, nrow = 1)

  x.smooth <-
    switch(smoothing,
           mean = t(apply(x, 1, function(xx) rowMeans(embed(xx, window)))),
           median = t(apply(x, 1, runmed, k = window, endrule = "keep")))

  ## cut the first and last couple of variables; with mean smoothing
  ## these are already left out of the smoothed matrix
  nc <- ncol(x)
  noff <- window %/% 2
  if (smoothing == "median")
    x.smooth <- x.smooth[, -c(1:noff, (nc - noff + 1): nc), drop = FALSE]

  x <- x[, -c(1:noff, (nc - noff + 1): nc), drop = FALSE]
  lambda <- sqrt(rowSums(x^2,na.rm = TRUE))
  A.lambda  <- sweep(x, MARGIN=1, STATS=lambda, FUN="/")
  A.s  <- t(scale(t(x.smooth), center=TRUE, scale=TRUE))

  rowSums(A.lambda * A.s) / sqrt(nc - window)
}
