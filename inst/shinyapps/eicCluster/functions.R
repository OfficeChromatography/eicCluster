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

report_plotter = function(data){
  ## need to plot a 7 by 7 inches picture, optimize the par
  ## data must contain all the information to plot the picture and be prepared in parallel in an include to report button
  ## need to plot the full scoreplot and the zoomed score plot
  ## need the tic and eicCluster plot
  ## need the fullfullscan, the fullscan and the eic selected, with and without range_masses
  layout(cbind(c(1,1,2,2),c(1,1,2,2),c(3,4,5,6),c(7,8,9,10))) ## first two are for score plot, then the tic and spectrum with full range, then the eic and zoomed spectrum
}
