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

report_plotter = function(i){
  layout(cbind(c(1,1,2,2),c(3:6)))
  ## first the 2 scoreplot top no zoom, bottom zoom
  par(mar=c(3,3,2,0.5),mgp=c(1.5,0.5,0),yaxs="r", xaxs="r")
  for(j in c(T,F)){
    Int <- apply(data.VarSel$eic,1,sum)
    rbPal <- colorRampPalette(c('blue','red'))
    Col <- rbPal(10)[as.numeric(cut(log10(Int),breaks = 10))]

    if(j){
      plot(data.VarSel$model,type="n",xlab="",ylab="")
      text(data.VarSel$model,labels=rownames(data.VarSel$eic),col=Col,cex = 0.5)
    }else{
      plot(data.VarSel$model,type="n",xlim = i$x, ylim = i$y,xlab="",ylab="")
      text(data.VarSel$model,labels=rownames(data.VarSel$eic),col=Col)
    }
    title(main=data.VarSel$algo,xlab = "dimension 1",ylab="dimension 2")
    ## add the scoreplot_cross if applicable
    # if(!is.null(input$scroreplot_cross)){
    #   text(x=data.VarSel$model[input$scroreplot_cross,1],y=data.VarSel$model[input$scroreplot_cross,2],labels="X",col="darkgreen",cex=3)
    # }
    ## add the contour
    my.cols <- rev(RColorBrewer::brewer.pal(11, "RdYlBu"))
    z <- MASS::kde2d(data.VarSel$model[,1], data.VarSel$model[,2], n=50)
    contour(z, drawlabels=FALSE, nlevels=11, col=my.cols, add=TRUE)

    ## add the selected zone if aplicable
    if(j){
      symbols(x=mean(VarSel_selected$x),y=mean(i$y),
              rectangles = rbind(c(i$x[2]-i$x[1],i$y[2]-i$y[1])),
              add=T,inches = F,fg="darkgreen",lwd=2)
    }
  }

  ## TIC and EIC
  leg = c("tic")
  col = c("black")

  data = data.ls()
  x=range.time$x[1]:range.time$x[2]
  par(mar=c(3, 3, 2, 2))
  plot(x=x,y=apply(data.VarSel$eic,2,sum),type="l",xlab="time (min)",ylab="Intensity",xaxt="n")

  axis(side = 1,at=seq(1,nrow(data.VarSel$eic),length.out = 10),
       labels = round(seq(getTime(data,x[1]*length(cond())),getTime(data,x[length(x)]*length(cond())),length.out = 10),2))
  par(new=T)
  if(length(i$index)>1){
    truc <- apply(data.VarSel$eic[i$index,],2,sum)
  }else{
    truc <- data.VarSel$eic[i$index,]
  }
  plot(x=x,y=truc,type="l",col="red",xaxt="n",yaxt="n",ylab="",xlab="")
  axis(side = 4,at = seq(0,max(truc),length.out = 10),col="red")
  leg = c(leg,"eicCluster")
  col = c(col,"red")

  legend("topright",legend = leg,lty=1,col=col)

  ## Ion cluster
  par(yaxs="i", xaxs="i",mar=c(3, 3, 2, 0.5),mgp=c(1.5,0.5,0))
  df <- data.frame(x = as.numeric(rownames(data.VarSel$eic[i$index,])),y = apply(data.VarSel$eic[i$index,],1,sum))
  df <- df[order(df[,2],decreasing = T),]
  plot(df,type="h",xlim=range.mz_full(),ylab="intensity",xlab="mz",ylim=c(0,df[1,2]*1.2),main="Selected ion cluster")
  text(x=df[1:10,1],y=df[1:10,2],labels=df[1:10,1],pos=3)

  ## Averaged full scan
  df <- data.frame(x = as.numeric(rownames(data.VarSel$eic)),y = apply(data.VarSel$eic,1,sum))
  df <- df[order(df[,2],decreasing = T),]
  plot(df,type="h",xlim=range.mz_full(),ylab="intensity",xlab="mz",ylim=c(0,df[1,2]*1.2),main="Averaged full scan")
  text(x=df[1:10,1],y=df[1:10,2],labels=df[1:10,1],pos=3)

  ## full scan at maximum
  data = data.ls()
  ind = seq(as.numeric(input$VarSel_mode),length(data),by=length(cond()))[which.max(truc)] ## truc refer to the TIC EIC spectrum defined before
  df = data.frame(x=data[[ind]]$spectrum$mass,y=data[[ind]]$spectrum$intensity)
  df <- df[order(df[,2],decreasing = T),]
  plot(df,type="h",xlim=range.mz_full(),ylab="intensity",xlab="mz",ylim=c(0,df[1,2]*1.2),main="Full scan at cluster EIC maaximum")
  text(x=df[1:10,1],y=df[1:10,2],labels=round(df[1:10,1],4),pos=3)
  }
