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

# eicCluster

library(shiny)
library(readMzXmlData)
library(tsne)

options(shiny.maxRequestSize=1000*1024^2)


server <- function(input, output,session) {
  source("functions.R") ## inside to reload faster

  ## reactiveValues
  index <- reactiveValues(index=1,cond=1,last=1)
  observeEvent(input$click.VarSel_eic,{
    data = data.ls()
    index$index = seq(as.numeric(input$VarSel_mode),length(data),by=length(cond()))[round(input$click.VarSel_eic$x)]
    cond.index <- index$index %% length(cond());if(cond.index == 0){cond.index = length(cond())}
    index$cond = cond.index
    index$last = 1
  })

  reported <- reactiveValues(l = list())
  observeEvent(input$VarSel_EIC_report,{
    reported$l[[length(reported$l)+1]] = list(x=VarSel_selected$x,y=VarSel_selected$y,index=VarSel_selected$index)
    print(str(reported$l))
  })

  ## observeEvent
  range.mz <- reactiveValues(x = NULL)
  range.mz_full = reactive({
    c(min(unlist(lapply(data.ls(),function(x){x$metaData$lowMz}))),
      max(unlist(lapply(data.ls(),function(x){x$metaData$highMz})))
    )
  })
  observeEvent(data.ls(), {
    range.mz$x <- c(min(unlist(lapply(data.ls(),function(x){x$metaData$lowMz}))),
                    max(unlist(lapply(data.ls(),function(x){x$metaData$highMz})))
    )
  })
  observeEvent(input$dblclick.VarSel_spectrum, {
    brush <- input$brush.VarSel_spectrum
    if (!is.null(brush)) {
      range.mz$x <- c(brush$xmin, brush$xmax)
    } else {
      range.mz$x <- c(min(unlist(lapply(data.ls(),function(x){x$metaData$lowMz}))),
                      max(unlist(lapply(data.ls(),function(x){x$metaData$highMz})))
      )
    }
  })
  observeEvent(input$dblclick.VarSel_fullscan, {
    brush <- input$brush.VarSel_fullscan
    if (!is.null(brush)) {
      range.mz$x <- c(brush$xmin, brush$xmax)
    } else {
      range.mz$x <- c(min(unlist(lapply(data.ls(),function(x){x$metaData$lowMz}))),
                      max(unlist(lapply(data.ls(),function(x){x$metaData$highMz})))
      )
    }
  })
  observeEvent(input$dblclick.VarSel_fullfullscan, {
    brush <- input$brush.VarSel_fullfullscan
    if (!is.null(brush)) {
      range.mz$x <- c(brush$xmin, brush$xmax)
    } else {
      range.mz$x <- c(min(unlist(lapply(data.ls(),function(x){x$metaData$lowMz}))),
                      max(unlist(lapply(data.ls(),function(x){x$metaData$highMz})))
      )
    }
  })

  data.VarSel <- reactiveValues(eic=NULL,model=NULL,algo=NULL) # this one contain a lot of info so the observeEvent are separated

  observeEvent(data.ls(),{
    range.time$x[1] = 1
    range.time$x[2] = length(data.ls())/length(cond())
  })

  range.time <- reactiveValues(x = NULL)
  observeEvent(input$dblclick.VarSel_tic, {
    data = data.ls()
    brush <- input$brush.VarSel_tic
    if (!is.null(brush)) {
      range.time$x <- c(brush$xmin, brush$xmax)
      if(range.time$x[1] < 1){range.time$x[1] <-1}
      if(range.time$x[2] > data[[1]]$metaData$scanCount/length(cond())){range.time$x[2] <- data[[1]]$metaData$scanCount/length(cond())}
    } else {
      range.time$x <-c(1,data[[1]]$metaData$scanCount/length(cond()))
    }
  })

  VarSel_ranges <- reactiveValues(x = NULL, y = NULL)
  observeEvent(input$dblclick.VarSel_scorePlot, {
    brush <- input$brush.VarSel_scorePlot
    if (!is.null(brush)) {
      VarSel_ranges$x <- c(brush$xmin, brush$xmax)
      VarSel_ranges$y <- c(brush$ymin, brush$ymax)

    } else {
      VarSel_ranges$x <- NULL
      VarSel_ranges$y <- NULL
    }
  })

  VarSel_selected <- reactiveValues(index=c(),x = NULL,y = NULL)
  observeEvent(input$VarSel_EIC_bis, {
    brush <- input$brush.VarSel_scorePlot
    if (!is.null(brush)) {
      score <-  data.VarSel$model
      truc <- which((score[,1] > brush$xmin & score[,1] < brush$xmax & score[,2] > brush$ymin & score[,2] < brush$ymax))
      VarSel_selected$index <- truc

      VarSel_selected$x <- c(brush$xmin, brush$xmax)
      VarSel_selected$y <- c(brush$ymin, brush$ymax)
    }
  })



  ## uiOutput
  output$VarSel_mode <- renderUI({
    selectizeInput("VarSel_mode","select the condition",choices = cond(),select=2)
  })

  ## Input

  data.ls <- reactive({
    validate(
      need(!is.null(input$file_MS),"Upload a mzXML file")
    )
    withProgress(message = "Reading file", value=0, {
      readMzXmlFile(input$file_MS$datapath)
    })
  })

  cond <- reactive({
    data = data.ls()
    ls <- list()
    ind = 1
    while(T){
      cond <- getMeta(data,ind)
      if(ind != 1){
        if(length(ls) >= 2 ){
          if(ls[[2]] == cond & ls[[1]] == ls[[length(ls)]]){ # need change to take into account the strange experiment of manu where he repeat one experiment before doing a new one
            ls[[length(ls)]] <- NULL
            break()
          }
        }
      }
      ls[[ind]] <- cond
      ind <- ind+1
    }
    ls <- unlist(ls)
    truc <- seq(length(ls))
    names(truc) <- ls
    truc
  })

  output$meta <- renderPrint({
    data = data.ls()
    str(data[index$index])
  })


  ## reactive
  tic <- reactive({
    unlist(lapply(data.ls(),function(x){x$metaData$totIonCurrent}))
  })

  data.ls.VarSel <- reactive({
    data = data.ls()
    truc <- lapply(data[seq(as.numeric(input$VarSel_mode),length(data),by=length(cond()))],
                   function(x){
                     keep <- x$spectrum$intensity > input$VarSel_treshold
                     x$spectrum$intensity <- x$spectrum$intensity[keep]
                     x$spectrum$mass <- round(x$spectrum$mass[keep],round(-log10(input$VarSel_increment)))
                     x
                   })
    truc
  })

  sequence.VarSel <- reactive({
    truc <- unique(unlist(lapply(data.ls.VarSel()[range.time$x[1]:range.time$x[2]],
                                 function(x){x$spectrum$mass}
    )))
    truc <- truc[truc < input$VarSel_mz_maxi & truc > input$VarSel_mz_mini]
    truc[order(truc)]
  })

  observeEvent(input$VarSel_EIC,{
    withProgress(message = "preparing matrix", value=0,min=0,max=length(sequence.VarSel()), {
      data.ext <- list()
      # sequence = seq(input$VarSel_mz_mini,input$VarSel_mz_maxi,by=input$VarSel_increment)[seq(input$VarSel_mz_mini,input$VarSel_mz_maxi,by=input$VarSel_increment) %in% sequence.VarSel()]
      for(i in sequence.VarSel()){
        truc <- unlist(
          lapply(data.ls.VarSel()[range.time$x[1]:range.time$x[2]],
                 function(x){sum(x$spectrum$intensity[x$spectrum$mass == i])}))
        data.ext[[as.character(i)]] <- truc
        # print(i)
        incProgress(1,message=i)
      }
    })
    data.VarSel$eic = do.call(rbind,data.ext)
  })

  output$VarSel_EIC_dim <- renderUI({
    validate(
      need(!is.null(data.VarSel$eic),"Please do the bucketting before applying the models")
    )
    h4(paste0(dim(data.VarSel$eic)[1]," different observation (eic) and ",dim(data.VarSel$eic)[2]," time step"))
  })

  observeEvent(input$VarSel_PCA,{
    validate(
      need(dim(data.VarSel$eic)[1] > dim(data.VarSel$eic)[2], "PCA not possible if more variables than observations")
    )
    withProgress(message = "PCA", value=0, {
      data <- data.VarSel$eic
      if("standardNormalVariate" %in% input$VarSel_preprocess){data = standardNormalVariate(data)}
      if("scale" %in% input$VarSel_preprocess){data = scale(data)}
      model = princomp(data)$scores[,1:2]
    })
    data.VarSel$model = model
    rownames(data.VarSel$model) = rownames(data.VarSel$eic)
    data.VarSel$algo = "PCA"
    VarSel_selected$index <- NULL
  })
  observeEvent(input$VarSel_tsne,{
    set.seed(1)
    withProgress(message = "tsne", value=0, {
      data <- data.VarSel$eic
      if("standardNormalVariate" %in% input$VarSel_preprocess){data = standardNormalVariate(data)}
      if("scale" %in% input$VarSel_preprocess){data = scale(data)}
      set.seed(1)
      model = tsne(data,max_iter = input$tsne_max_iter,perplexity = input$tsne_perplexity,whiten = input$tsne_whiten,initial_dims = input$tsne_initial_dims)[,1:2]
    })
    data.VarSel$model = model
    rownames(data.VarSel$model) = rownames(data.VarSel$eic)
    data.VarSel$algo = "TSNE"
    VarSel_selected$index <- NULL
  })
  observeEvent(input$VarSel_kmeans,{
    withProgress(message = "kmeans", value=0, {
      data <- data.VarSel$eic
      if("standardNormalVariate" %in% input$VarSel_preprocess){data = standardNormalVariate(data)}
      if("scale" %in% input$VarSel_preprocess){data = scale(data)}
      set.seed(1)
      model = kmeans(data,centers=input$kmeans_center, iter.max = input$kmeans_iter_max)$cluster
      model <- cbind(model+runif(length(model),min=-0.2,max=0.2),runif(length(model),min=-0.4,max=0.4))
    })
    data.VarSel$model = model
    rownames(data.VarSel$model) = rownames(data.VarSel$eic)
    data.VarSel$algo = "k-means"
    VarSel_selected$index <- NULL
  })

  ## plot
  output$VarSel_tic <- renderPlot({
    data = data.ls()
    ind = seq(as.numeric(input$VarSel_mode),length(data),by=length(cond()))
    x=range.time$x[1]:range.time$x[2]
    plot(x=x,y=tic()[ind][x],type="l",main=paste0("TIC ",names(cond())[as.numeric(input$select_tic_2)]),ylab="tot Ion Current",xaxt="n",xlab="time (min)")
    axis(side = 1,at=seq(x[1],x[length(x)],length.out = 10),
         labels = round(seq(getTime(data,x[1]*length(cond())),getTime(data,x[length(x)]*length(cond())),length.out = 10),2))
  })

  output$scroreplot_cross = renderUI({
    selectizeInput("scroreplot_cross","select the masses to highlight",choices = rownames(data.VarSel$eic),selected = NULL,multiple = T)
  })

  output$VarSel_scorePlot <- renderPlot({
    validate(
      need(!is.null(data.VarSel$model),"Please do the Clusterisation")
    )
    Int <- apply(data.VarSel$eic,1,sum)
    rbPal <- colorRampPalette(c('blue','red'))
    Col <- rbPal(10)[as.numeric(cut(log10(Int),breaks = 10))]

    if(is.null(VarSel_ranges$x)){
      plot(data.VarSel$model,type="n",xlab="",ylab="")
    }else{
      plot(data.VarSel$model,type="n",xlim = VarSel_ranges$x, ylim = VarSel_ranges$y,xlab="",ylab="")
    }
    text(data.VarSel$model,labels=rownames(data.VarSel$eic),col=Col)
    title(main=data.VarSel$algo,xlab = "dimension 1",ylab="dimension 2")
    ## add the scoreplot_cross if applicable
    if(!is.null(input$scroreplot_cross)){
      text(x=data.VarSel$model[input$scroreplot_cross,1],y=data.VarSel$model[input$scroreplot_cross,2],labels="X",col="darkgreen",cex=3)
    }
    ## add the contour
    my.cols <- rev(RColorBrewer::brewer.pal(11, "RdYlBu"))
    z <- MASS::kde2d(data.VarSel$model[,1], data.VarSel$model[,2], n=50)
    contour(z, drawlabels=FALSE, nlevels=11, col=my.cols, add=TRUE)

    ## add the selected zone if aplicable
    if(!is.null(VarSel_selected$x)){
      symbols(x=mean(VarSel_selected$x),y=mean(VarSel_selected$y),
              rectangles = rbind(c(VarSel_selected$x[2]-VarSel_selected$x[1],VarSel_selected$y[2]-VarSel_selected$y[1])),
              add=T,inches = F,fg="darkgreen",lwd=2)
    }
  })

  output$VarSel_eic <- renderPlot({
    leg = c("tic")
    col = c("black")
    validate(
      need(!is.null(data.VarSel$model),"Please do the Clusterisation")
    )
    data = data.ls()
    x=range.time$x[1]:range.time$x[2]
    par(mar=c(5, 4, 3, 5))
    plot(x=x,y=apply(data.VarSel$eic,2,sum),type="l",xlab="time (min)",ylab="Intensity",xaxt="n")

    axis(side = 1,at=seq(range.time$x[1],range.time$x[2],length.out = 10),
         labels = round(seq(getTime(data,x[1]*length(cond())),getTime(data,x[length(x)]*length(cond())),length.out = 10),2))
    if(!is.null(VarSel_selected$index)){
      par(new=T)
      if(length(VarSel_selected$index)>1){
        truc <- apply(data.VarSel$eic[VarSel_selected$index,],2,sum)
      }else{
        truc <- data.VarSel$eic[VarSel_selected$index,]
      }
      if(input$VarSel_eic_normalize){
        plot(x=x,y=truc,type="l",col="red",ylim=c(0,max(apply(data.VarSel$eic,2,sum))),xaxt="n",yaxt="n",ylab="",xlab="")
      }else{
        plot(x=x,y=truc,type="l",col="red",xaxt="n",yaxt="n",ylab="",xlab="")
        axis(side = 4,at = seq(0,max(truc),length.out = 10),col="red")
      }
      leg = c(leg,"eicCluster")
      col = c(col,"red")
    }

    if(range.mz$x != range.mz_full()){
      par(new=T)
      ind = seq(as.numeric(input$VarSel_mode),length(data.ls()),by=length(cond()))
      truc = unlist(lapply(data.ls(),function(x){sum(x$spectrum$intensity[x$spectrum$mass < range.mz$x[2]& x$spectrum$mass > range.mz$x[1]])}))[ind][x]
      if(input$VarSel_eic_normalize){
        plot(x=x,y=truc,type="l",col="darkgreen",ylim=c(0,max(apply(data.VarSel$eic,2,sum))),xaxt="n",yaxt="n",ylab="",xlab="")
      }else{
        plot(x=x,y=truc,type="l",col="darkgreen",xaxt="n",yaxt="n",ylab="",xlab="")
        axis(side = 4,at = seq(0,max(truc),length.out = 10),line=2,col="darkgreen")
      }
      leg = c(leg,"eicClassique")
      col = c(col,"darkgreen")
    }


    legend("topright",legend = leg,lty=1,col=col)
  })

  output$VarSel_spectrum <- renderPlot({
    par(yaxs="i", xaxs="i",mar=c(5, 4, 3, 2))
    validate(
      need(!is.null(VarSel_selected$index),"Please select a cluster")
    )
    if(length(VarSel_selected$index)>1){
      df <- data.frame(x = as.numeric(rownames(data.VarSel$eic[VarSel_selected$index,])),y = apply(data.VarSel$eic[VarSel_selected$index,],1,sum))
      if(!is.null(range.mz$x)){df <- df[df$x <= range.mz$x[2] & df$x >= range.mz$x[1],]}
      validate(
        need(nrow(df) >0,"No selected masses in this range, double click to reset the range")
      )
      df <- df[order(df[,2],decreasing = T),]
      plot(df,type="h",xlim=range.mz$x,ylab="intensity",xlab="mz",ylim=c(0,df[1,2]*1.1),main="selected masses")
      text(x=df[1:10,1],y=df[1:10,2],labels=df[1:10,1],pos=3)
    }
  })

  output$VarSel_fullfullscan <- renderPlot({ ## the fullfullscan means the full scan over the whole time range
    par(yaxs="i", xaxs="i",mar=c(5, 4, 3, 2))
    validate(
      need(!is.null(data.VarSel$model),"Please do the Clusterisation")
    )
    df <- data.frame(x = as.numeric(rownames(data.VarSel$eic)),y = apply(data.VarSel$eic,1,sum))
    if(!is.null(range.mz$x)){df <- df[df$x <= range.mz$x[2] & df$x >= range.mz$x[1],]}
    validate(
      need(nrow(df) >0,"No selected masses in this range, double click to reset the range")
    )
    df <- df[order(df[,2],decreasing = T),]
    plot(df,type="h",xlim=range.mz$x,ylab="intensity",xlab="mz",ylim=c(0,df[1,2]*1.1),main="full scan")
    text(x=df[1:10,1],y=df[1:10,2],labels=df[1:10,1],pos=3)
  })

  spectrum_df <- reactive({
    data = data.ls()
    df = data.frame(x=data[[index$index]]$spectrum$mass,y=data[[index$index]]$spectrum$intensity)
    if(!is.null(range.mz$x)){df <- df[df$x <= range.mz$x[2] & df$x >= range.mz$x[1],]}
    df <- df[order(df[,2],decreasing = T),]
    df
  })

  output$VarSel_fullscan <- renderPlot({
    par(yaxs="i", xaxs="i",mar=c(5, 4, 3, 2))
    data = data.ls()
    df = spectrum_df()
    validate(
      need(nrow(df) >0,"No selected masses in this range, double click to reset the range")
    )
    par(yaxs="i", xaxs="i")
    plot(df,type = "h",
         xlab="m/z",ylab="intensity",xlim=range.mz$x, ylim=c(0,df[1,2]*1.1),
         main=paste0(names(cond())[index$cond],"\nRf = ",getTime(data,index$index)," min ; base peak Mz = ",round(df[1,1],4),
                     " ; base peak intensity = ",df[1,2],"\n",
                     "peaks count = ",nrow(df)," ; totIonCurrent = ",round(sum(df[,2])))
    )
    text(x=df[1:10,1],y=df[1:10,2],labels=round(df[1:10,1],4),pos=3)
  })

  output$report_choices = renderUI({
    validate(need(length(reported$l) > 0 , "select at least a cluster"))
    choices = seq(length(reported$l))
    checkboxGroupInput("report_choices","Report include",choices = choices)
  })
  output$report_preview_choices = renderUI({
    validate(need(length(reported$l) > 0 , "select at least a cluster"))
    choices = seq(length(reported$l))
    selectizeInput("report_preview_choices","Report include",choices = choices)
  })
  output$report_preview = renderPlot({
    i = reported$l[[as.numeric(input$report_preview_choices)]]
    Int <- apply(data.VarSel$eic,1,sum)
    rbPal <- colorRampPalette(c('blue','red'))
    Col <- rbPal(10)[as.numeric(cut(log10(Int),breaks = 10))]
      plot(data.VarSel$model,type="n",xlab="",ylab="")
      text(data.VarSel$model,labels=rownames(data.VarSel$eic),col=Col,cex = 0.5)
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
      symbols(x=mean(i$x),y=mean(i$y),
              rectangles = rbind(c(i$x[2]-i$x[1],i$y[2]-i$y[1])),
              add=T,inches = F,fg="darkgreen",lwd=2)
  })

  output$Report <- downloadHandler(
    filename = function() {
      paste('my-report', sep = '.', switch(
        input$format, PDF = 'pdf', HTML = 'html', Word = 'docx'
      ))
    },
    content = function(file) {
      src <- normalizePath('report.Rmd')

      # temporarily switch to the temp dir, in case you do not have write
      # permission to the current working directory
      # owd <- setwd(tempdir())
      # on.exit(setwd(owd))
      # file.copy(src, 'report.Rmd')

      library(rmarkdown)
      out <- render('report.Rmd', switch(
        input$format,
        PDF = pdf_document(), HTML = html_document(), Word = word_document()
      ))
      file.rename(out, file)
    }
  )
}
