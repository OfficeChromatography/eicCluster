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
library(ggplot2)
library(ggrepel)
library(rhandsontable)

options(shiny.maxRequestSize=1000*1024^2)


server <- function(input, output,session) {
  source("functions.R") ## inside to reload faster

  ## reactiveValues
  index <- reactiveValues(index=1,cond=1,last=1) # use to explore the tic and view a mass spectrum by clicking on it
  observeEvent(input$click.VarSel_eic,{
    data = data.ls()
    index$index = seq(as.numeric(input$mode),length(data),by=length(cond()))[round(input$click.VarSel_eic$x)]
    cond.index <- index$index %% length(cond());if(cond.index == 0){cond.index = length(cond())}
    index$cond = cond.index
    index$last = 1
  })
  observeEvent(input$click.prep_tic,{
    data = data.ls()
    index$index = seq(as.numeric(input$mode),length(data),by=length(cond()))[round(input$click.prep_tic$x)]
    cond.index <- index$index %% length(cond());if(cond.index == 0){cond.index = length(cond())}
    index$cond = cond.index
    index$last = 1
  })



  ## observeEvent Interactive
  range.mz <- reactiveValues(x = NULL) # to zoom inside the mass spectrum, react to a few interaction but easy
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
  observeEvent(input$dblclick.prep_fullscan, {
    brush <- input$brush.prep_fullscan
    if (!is.null(brush)) {
      range.mz$x <- c(brush$xmin, brush$xmax)
    } else {
      range.mz$x <- c(min(unlist(lapply(data.ls(),function(x){x$metaData$lowMz}))),
                      max(unlist(lapply(data.ls(),function(x){x$metaData$highMz})))
      )
    }
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

  data.VarSel <- reactiveValues(eic_before_CODA = NULL,eic=NULL,model=NULL,algo=NULL,keep=NULL,range.time=NULL)
  observeEvent(data.ls(),{
    if(input$Data_2_use == "Your_own_data"){
      range.time$x[1] = 1
      range.time$x[2] = length(data.ls())/length(cond())
    }
  })

  range.time <- reactiveValues(x = NULL) # to zoom inside the TIC
  observeEvent(input$dblclick.prep_tic, {
    data = data.ls()
    brush <- input$brush.prep_tic
    if (!is.null(brush)) {
      range.time$x <- c(brush$xmin, brush$xmax)
      if(range.time$x[1] < 1){range.time$x[1] <-1}
      if(range.time$x[2] > data[[1]]$metaData$scanCount/length(cond())){range.time$x[2] <- data[[1]]$metaData$scanCount/length(cond())}
    } else {
      range.time$x <-c(1,data[[1]]$metaData$scanCount/length(cond()))
    }
  })
  observeEvent(input$dblclick.VarSel_eic, {
    data = data.ls()
    brush <- input$brush.VarSel_eic
    if (!is.null(brush)) {
      range.time$x <- c(brush$xmin, brush$xmax)
      if(range.time$x[1] < 1){range.time$x[1] <-1}
      if(range.time$x[2] > data[[1]]$metaData$scanCount/length(cond())){range.time$x[2] <- data[[1]]$metaData$scanCount/length(cond())}
    } else {
      range.time$x <-c(1,data[[1]]$metaData$scanCount/length(cond()))
    }
  })

  range.scoreplot <- reactiveValues(x = NULL, y = NULL) # to zoom inside the scoreplot
  observeEvent(input$dblclick.VarSel_scorePlot, {
    brush <- input$brush.VarSel_scorePlot
    if (!is.null(brush)) {
      range.scoreplot$x <- c(brush$xmin, brush$xmax)
      range.scoreplot$y <- c(brush$ymin, brush$ymax)

    } else {
      range.scoreplot$x <- NULL
      range.scoreplot$y <- NULL
    }
  })

  VarSel_selected <- reactiveValues(index=c(),x = NULL,y = NULL) # triggered when cluster selected
  observeEvent(input$VarSel_EIC_bis, {
    brush <- input$brush.VarSel_scorePlot
    if (!is.null(brush)) {
      score <-  data.VarSel$model
      truc <- which((score[,1] > brush$xmin & score[,1] < brush$xmax & score[,2] > brush$ymin & score[,2] < brush$ymax & data.VarSel$keep))
      VarSel_selected$index <- truc

      VarSel_selected$x <- c(brush$xmin, brush$xmax)
      VarSel_selected$y <- c(brush$ymin, brush$ymax)
    }
  })
  observeEvent(input$VarSel_EIC_exclude, {
    data.VarSel$keep[VarSel_selected$index] = F
    # reported$l = list()
  })
  observeEvent(input$VarSel_EIC_exclude_reset, {
    data.VarSel$keep = rep(T,nrow(data.VarSel$eic))
    # reported$l = list()
  })

  ## Observe Event data



  ## uiOutput
  output$mode <- renderUI({
    selectizeInput("mode","select the condition",choices = cond(),select=2)
  })

  ## Input

  data.ls <- reactive({
    if(input$Data_2_use == "Check_point_file"){
      checkpoint()$data.ls
    }else if(input$Data_2_use == "Your_own_data"){
      validate(
        need(!is.null(input$file_MS),"Upload a mzXML file")
      )
      withProgress(message = "Reading file", value=0, {
        if(length(input$file_MS$datapath) == 1){
          readMzXmlFile(input$file_MS$datapath)## here need multiple
        }else{
          truc = readMzXmlFile(input$file_MS$datapath[1])
          for(i in 2:length(input$file_MS$datapath)){
            truc = append(truc,readMzXmlFile(input$file_MS$datapath[i]))
          }
          truc
          # truc = lapply(input$file_MS$datapath,readMzXmlFile)
          # append(truc)
        }

      })
    }else if(input$Data_2_use == "Demo_file"){
      checkpoint()$data.ls
    }


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

  observeEvent(input$Bucket,{
    data = data.ls()
    data.ls.VarSel <- lapply(data[seq(as.numeric(input$mode),length(data),by=length(cond()))],
                             function(x){
                               keep <- x$spectrum$intensity > input$Int_treshold
                               x$spectrum$intensity <- x$spectrum$intensity[keep] ## apply the trshold
                               x$spectrum$mass <- round(x$spectrum$mass[keep],round(-log10(input$bucketing_increment))) ## apply the rounding
                               x
                             })

    sequence.VarSel <- unique(unlist(lapply(data.ls.VarSel[range.time$x[1]:range.time$x[2]],
                                            function(x){x$spectrum$mass}
    )))
    sequence.VarSel <- sequence.VarSel[sequence.VarSel < input$range_mz_maxi & sequence.VarSel > input$range_mz_mini]
    sequence.VarSel = sequence.VarSel[order(sequence.VarSel)]

    withProgress(message = "preparing matrix", value=0,min=0,max=length(sequence.VarSel), {
      data.ext <- list()
      for(i in sequence.VarSel){
        truc <- unlist(
          lapply(data.ls.VarSel[range.time$x[1]:range.time$x[2]],
                 function(x){sum(x$spectrum$intensity[x$spectrum$mass == i])}))
        data.ext[[as.character(i)]] <- truc
        # print(i)
        incProgress(1,message=i)
      }
    })
    data.VarSel$eic = do.call(rbind,data.ext)
    data.VarSel$eic_before_CODA = data.VarSel$eic
    data.VarSel$algo = "None"
    data.VarSel$model = NULL
    data.VarSel$keep = rep(T,nrow(data.VarSel$eic))
    data.VarSel$range.time = range.time$x
    meta$Int_treshold=input$Int_treshold;meta$range_mz_mini=input$range_mz_mini;meta$range_mz_maxi=input$range_mz_maxi;meta$bucketing_increment=input$bucketing_increment;meta$mode=input$mode
    meta$window_CODA = input$window_CODA;meta$smoothing_CODA=input$smoothing_CODA;meta$threshold_CODA=input$threshold_CODA;meta$apply_CODA=F

  })
  observeEvent(input$apply_CODA,{
    validate(need(input$window_CODA %% 2 != 0,"Window smoothing for CODA must be odd."))
    data.VarSel$eic = data.VarSel$eic_before_CODA
    truc = coda(data.VarSel$eic_before_CODA,window = input$window_CODA,smoothing = input$smoothing_CODA)
    data.VarSel$eic = data.VarSel$eic[!is.na(truc),] ## will remove the na produce by CODA
    truc = truc[!is.na(truc)]
    data.VarSel$eic = data.VarSel$eic[truc > input$threshold_CODA,]# will keep only eic with values above the threshold
    data.VarSel$keep = rep(T,nrow(data.VarSel$eic))
    meta$window_CODA = input$window_CODA;meta$smoothing_CODA=input$smoothing_CODA;meta$threshold_CODA=input$threshold_CODA;meta$apply_CODA=T
    data.VarSel$model = NULL
  })
  output$Bucket_dim_1 <- renderUI({
    validate(
      need(!is.null(data.VarSel$eic),"Please do the bucketting before applying the models")
    )
    h4(paste0(dim(data.VarSel$eic_before_CODA)[1]," different observation (eic) and ",dim(data.VarSel$eic_before_CODA)[2]," time step"))
  })
  output$Bucket_dim_2 <- renderUI({
    validate(
      need(!is.null(data.VarSel$eic),"Please do the bucketting before applying the models")
    )
    validate(need(input$window_CODA %% 2 != 0,"Window smoothing for CODA must be odd."))
    tagList(
      h4(paste0(dim(data.VarSel$eic)[1]," different observation (eic) and ",dim(data.VarSel$eic)[2]," time step")),
      h4(if(meta$apply_CODA){"CODA applied"}else("CODA not applied"))
    )

  })
  observeEvent(input$PCA,{
    validate(
      need(dim(data.VarSel$eic)[1] > dim(data.VarSel$eic)[2], "PCA not possible if more variables than observations")
    )
    withProgress(message = "PCA", value=0, {
      data <- data.VarSel$eic
      if("standardNormalVariate" %in% input$preprocess){data = standardNormalVariate(data)}
      if("scale" %in% input$preprocess){data = scale(data)}
      model = princomp(data)$scores[,1:2]
    })
    data.VarSel$model = model
    rownames(data.VarSel$model) = rownames(data.VarSel$eic)
    data.VarSel$algo = "PCA"
    VarSel_selected$index <- NULL
    meta$preprocess = input$preprocess
    meta$tsne_initial_dims=input$tsne_initial_dims ;meta$tsne_perplexity=input$tsne_perplexity ;meta$tsne_max_iter=input$tsne_max_iter ;meta$tsne_whiten=input$tsne_whiten ;
    meta$tsne_pca=input$tsne_pca ;meta$tsne_theta=input$tsne_theta
    meta$kmeans_center=input$kmeans_center ;meta$kmeans_iter_max=input$kmeans_iter_max
  })
  observeEvent(input$kmeans,{
    withProgress(message = "kmeans", value=0, {
      data <- data.VarSel$eic
      if("standardNormalVariate" %in% input$preprocess){data = standardNormalVariate(data)}
      if("scale" %in% input$preprocess){data = scale(data)}
      set.seed(1)
      model = kmeans(data,centers=input$kmeans_center, iter.max = input$kmeans_iter_max)$cluster
      model <- cbind(model+runif(length(model),min=-0.2,max=0.2),runif(length(model),min=-0.4,max=0.4))
    })
    data.VarSel$model = model
    rownames(data.VarSel$model) = rownames(data.VarSel$eic)
    data.VarSel$algo = "k-means"
    VarSel_selected$index <- NULL
    meta$preprocess = input$preprocess
    meta$tsne_initial_dims=input$tsne_initial_dims ;meta$tsne_perplexity=input$tsne_perplexity ;meta$tsne_max_iter=input$tsne_max_iter ;meta$tsne_whiten=input$tsne_whiten ;
    meta$tsne_pca=input$tsne_pca ;meta$tsne_theta=input$tsne_theta
    meta$kmeans_center=input$kmeans_center ;meta$kmeans_iter_max=input$kmeans_iter_max
  })
  observeEvent(input$tsne,{
    set.seed(1)
    withProgress(message = "tsne", value=0, {
      data <- data.VarSel$eic
      if("standardNormalVariate" %in% input$preprocess){data = standardNormalVariate(data)}
      if("scale" %in% input$preprocess){data = scale(data)}
      set.seed(1)
      if(data.VarSel$algo == "TSNE"){initial_config = data.VarSel$model}else{initial_config = NULL}
      model = tsne(data,max_iter = input$tsne_max_iter,perplexity = input$tsne_perplexity,whiten = input$tsne_whiten,initial_dims = input$tsne_initial_dims,initial_config = initial_config)[,1:2]
    })
    data.VarSel$model = model
    rownames(data.VarSel$model) = rownames(data.VarSel$eic)
    data.VarSel$algo = "TSNE"
    VarSel_selected$index <- NULL
    meta$preprocess = input$preprocess
    meta$tsne_initial_dims=input$tsne_initial_dims ;meta$tsne_perplexity=input$tsne_perplexity ;meta$tsne_max_iter=input$tsne_max_iter ;meta$tsne_whiten=input$tsne_whiten ;
    meta$tsne_pca=input$tsne_pca ;meta$tsne_theta=input$tsne_theta
    meta$kmeans_center=input$kmeans_center ;meta$kmeans_iter_max=input$kmeans_iter_max
  })
  observeEvent(input$rtsne,{
    set.seed(1)
    withProgress(message = "rtsne", value=0, {
      data <- data.VarSel$eic
      if("standardNormalVariate" %in% input$preprocess){data = standardNormalVariate(data)}
      if("scale" %in% input$preprocess){data = scale(data)}
      set.seed(1)
      if(data.VarSel$algo == "rTSNE"){Y_init = data.VarSel$model}else{Y_init = NULL}
      model = Rtsne::Rtsne(data,dims=2,max_iter = input$tsne_max_iter,perplexity = input$tsne_perplexity,pca = input$tsne_pca,initial_dims = input$tsne_initial_dims,Y_init = Y_init,verbose=T,theta=input$tsne_theta,check_duplicates = F)
      print(str(model)) ## to be removed, just to know what's inside
      model = model$Y
    })
    data.VarSel$model = model
    rownames(data.VarSel$model) = rownames(data.VarSel$eic)
    data.VarSel$algo = "rTSNE"
    VarSel_selected$index <- NULL
    meta$preprocess = input$preprocess
    meta$tsne_initial_dims=input$tsne_initial_dims ;meta$tsne_perplexity=input$tsne_perplexity ;meta$tsne_max_iter=input$tsne_max_iter ;meta$tsne_whiten=input$tsne_whiten ;
    meta$tsne_pca=input$tsne_pca ;meta$tsne_theta=input$tsne_theta
    meta$kmeans_center=input$kmeans_center ;meta$kmeans_iter_max=input$kmeans_iter_max
  })
  observeEvent(input$reset_tsne,{
    data.VarSel$algo = "None"
    meta$tsne_max_iter =NULL
  })

  ## plot
  output$prep_tic <- renderPlot({
    data = data.ls()
    ind = seq(as.numeric(input$mode),length(data),by=length(cond()))
    x=range.time$x[1]:range.time$x[2]
    plot(x=x,y=tic()[ind][x],type="l",main=paste0("TIC ",names(cond())[as.numeric(input$select_tic_2)]),ylab="tot Ion Current",xaxt="n",xlab="time (min)")
    axis(side = 1,at=seq(x[1],x[length(x)],length.out = 10),
         labels = round(seq(getTime(data,x[1]*length(cond())),getTime(data,x[length(x)]*length(cond())),length.out = 10),2))
  })
  output$prep_fullscan <- renderPlot({
    data = data.ls()
    df = data.frame(x=data[[index$index]]$spectrum$mass,y=data[[index$index]]$spectrum$intensity)
    if(!is.null(range.mz$x)){df <- df[df$x <= range.mz$x[2] & df$x >= range.mz$x[1],]}
    df <- df[order(df[,2],decreasing = T),]
    par(yaxs="i", xaxs="i",mar=c(5, 4, 3, 2))
    validate(
      need(nrow(df) >0,"No selected masses in this range, double click to reset the range")
    )
    par(yaxs="i", xaxs="i")
    plot(df,type = "h",
         xlab=expression(italic(m/z)),ylab="Intensity [AU]",xlim=range.mz$x, ylim=c(0,df[1,2]*1.1),
         main=paste0(names(cond())[index$cond],"\nRf = ",getTime(data,index$index)," min ; base peak Mz = ",round(df[1,1],4),
                     " ; base peak intensity = ",df[1,2],"\n",
                     "peaks count = ",nrow(df)," ; totIonCurrent = ",round(sum(df[,2])))
    )
    text(x=df[1:10,1],y=df[1:10,2],labels=round(df[1:10,1],4),pos=3)
  })
  # output$prep_fullscan <- renderPlot({
  #   data = data.ls()
  #   df = data.frame(x=data[[index$index]]$spectrum$mass,y=data[[index$index]]$spectrum$intensity)
  #   if(!is.null(range.mz$x)){df <- df[df$x <= range.mz$x[2] & df$x >= range.mz$x[1],]}
  #   df <- df[order(df[,2],decreasing = T),]
  #   par(yaxs="i", xaxs="i",mar=c(5, 4, 3, 2))
  #   validate(
  #     need(nrow(df) >0,"No selected masses in this range, double click to reset the range")
  #   )
  #   # par(yaxs="i", xaxs="i")
  #   # plot(df,type = "h",
  #   #      xlab=expression(italic(m/z)),ylab="intensity",xlim=range.mz$x, ylim=c(0,df[1,2]*1.1),
  #   #      main=paste0(names(cond())[index$cond],"\nRf = ",getTime(data,index$index)," min ; base peak Mz = ",round(df[1,1],4),
  #   #                  " ; base peak intensity = ",df[1,2],"\n",
  #   #                  "peaks count = ",nrow(df)," ; totIonCurrent = ",round(sum(df[,2])))
  #   # )
  #   ggplot(df,aes(x=x,xend=x,y=0,yend=y))+geom_segment()+#+coord_cartesian(ylim = range(df$y))
  #     geom_text_repel(aes(x=x,y=y, label = round(df[,1],4)))
  #   # text(x=df[1:10,1],y=df[1:10,2],labels=round(df[1:10,1],4),pos=3)
  #
  # })
  output$scroreplot_cross = renderUI({
    selectizeInput("scroreplot_cross","select the masses to highlight",choices = rownames(data.VarSel$eic),selected = NULL,multiple = T)
  })
  output$VarSel_scorePlot <- renderPlot({
    validate(
      need(!is.null(data.VarSel$model),"Please do the Clusterisation")
    )
    if(input$scoreplot_color == "Intensity"){
      Int <- apply(data.VarSel$eic[data.VarSel$keep,],1,sum)
      Int = log10(Int)
      pal <- colorRampPalette(c('blue','red'))
      order_col = findInterval(Int, sort(Int))
      Col = pal(length(Int))[order_col]
      # Col <- rbPal(10)[as.numeric(cut(log10(Int),breaks = 10))]
    }else{
      Int <- coda(data.VarSel$eic,window = input$window_CODA,smoothing = input$smoothing_CODA)
      Int[is.nan(Int)] = 0
      # print(Int)
      pal <- colorRampPalette(c('blue','red'))
      order_col = findInterval(Int, sort(Int))
      Col = pal(length(Int))[order_col]
      # rbPal <- colorRampPalette(c('blue','red'))
      # Col <- rbPal(10)[as.numeric(cut(Int,breaks = 10))]
    }

    if(is.null(range.scoreplot$x)){
      xlim = c(min(data.VarSel$model[data.VarSel$keep,1]),max(data.VarSel$model[data.VarSel$keep,1]))
      ylim = c(min(data.VarSel$model[data.VarSel$keep,2]),max(data.VarSel$model[data.VarSel$keep,2]))
    }else{
      xlim = range.scoreplot$x; ylim = range.scoreplot$y
    }
    if(input$VarSel_eic_pch == "m/z"){
      plot(data.VarSel$model[data.VarSel$keep,],type="n",xlim = xlim, ylim = ylim,xlab="",ylab="")
      text(data.VarSel$model[data.VarSel$keep,],labels=rownames(data.VarSel$eic)[data.VarSel$keep],col=Col)
    }else if(input$VarSel_eic_pch == "punct"){
      plot(data.VarSel$model[data.VarSel$keep,],type="n",xlim = xlim, ylim = ylim,xlab="",ylab="")
      text(data.VarSel$model[data.VarSel$keep,],labels=".",col=Col)
    }else if(input$VarSel_eic_pch == "circles"){
      plot(data.VarSel$model[data.VarSel$keep,],xlim = xlim, ylim = ylim,xlab="",ylab="",col=Col)
    }
    title(main=data.VarSel$algo,xlab = "x",ylab="y")
    ## add the scoreplot_cross if applicable
    if(!is.null(input$scroreplot_cross)){
      text(x=data.VarSel$model[input$scroreplot_cross,1],y=data.VarSel$model[input$scroreplot_cross,2],labels="X",col="darkgreen",cex=3)
    }
    ## add the contour
    my.cols <- rev(RColorBrewer::brewer.pal(11, "RdYlBu"))
    z <- MASS::kde2d(data.VarSel$model[,1], data.VarSel$model[,2], n=50)
    contour(z, drawlabels=FALSE, nlevels=11, col=my.cols, add=TRUE)

    if(length(reported$l) != 0){
      for(i in seq(length(reported$l))){## plot the selected cluster if applicable
        symbols(x=mean(reported$l[[i]]$x),y=mean(reported$l[[i]]$y),
                rectangles = rbind(c(reported$l[[i]]$x[2]-reported$l[[i]]$x[1],reported$l[[i]]$y[2]-reported$l[[i]]$y[1])),
                add=T,inches = F,lwd=2)
        text(x=reported$l[[i]]$x[1],y=reported$l[[i]]$y[2],label=i,pos=2,cex=2)
      }
    }


    ## add the selected zone if aplicable
    if(!is.null(VarSel_selected$x)){
      symbols(x=mean(VarSel_selected$x),y=mean(VarSel_selected$y),
              rectangles = rbind(c(VarSel_selected$x[2]-VarSel_selected$x[1],VarSel_selected$y[2]-VarSel_selected$y[1])),
              add=T,inches = F,fg="darkgreen",lwd=2)
    }
  })
  output$VarSel_eic <- renderPlot({
    leg = c("TIC")
    col = c("black")
    validate(
      need(!is.null(data.VarSel$model),"Please do the Clusterisation")
    )
    data = data.ls()
    x=data.VarSel$range.time[1]:data.VarSel$range.time[2]
    # x2 = range.time$x[1]:range.time$x[2]
    tic.len = 10#round(10*length(x2)/length(x))
    par(mar=c(5, 4, 3, 5),mgp=c(2,0.5,0))
    plot(x=x,y=apply(data.VarSel$eic[data.VarSel$keep,],2,sum),type="l",xlab="time (min)",ylab="Intensity [AU]",xaxt="n",xlim=range.time$x)

    axis(side = 1,at=seq(data.VarSel$range.time[1],data.VarSel$range.time[2],length.out = tic.len),
         labels = round(seq(getTime(data,x[1]*length(cond())),getTime(data,x[length(x)]*length(cond())),length.out = tic.len),2))
    if(!is.null(VarSel_selected$index)){
      par(new=T)
      if(length(VarSel_selected$index)>1){
        truc <- apply(data.VarSel$eic[VarSel_selected$index,],2,sum)
      }else{
        truc <- data.VarSel$eic[VarSel_selected$index,]
      }
      if(input$VarSel_eic_normalize){
        plot(x=x,y=truc,type="l",col="red",ylim=c(0,max(apply(data.VarSel$eic,2,sum))),xaxt="n",yaxt="n",ylab="",xlab="",xlim=range.time$x)
      }else{
        plot(x=x,y=truc,type="l",col="red",xaxt="n",yaxt="n",ylab="",xlab="",xlim=range.time$x)
        axis(side = 4,col="red",col.axis="red")
      }
      leg = c(leg,"eicCluster")
      col = c(col,"red")
    }

    if(sum(range.mz$x) != sum(range.mz_full())){
      par(new=T)
      ind = seq(as.numeric(input$mode),length(data.ls()),by=length(cond()))
      truc = unlist(lapply(data.ls(),function(x){sum(x$spectrum$intensity[x$spectrum$mass < range.mz$x[2]& x$spectrum$mass > range.mz$x[1]])}))[ind][x]
      if(input$VarSel_eic_normalize){
        plot(x=x,y=truc,type="l",col="darkgreen",ylim=c(0,max(apply(data.VarSel$eic,2,sum))),xaxt="n",yaxt="n",ylab="",xlab="",xlim=range.time$x)
      }else{
        plot(x=x,y=truc,type="l",col="darkgreen",xaxt="n",yaxt="n",ylab="",xlab="",xlim=range.time$x)
        axis(side = 4,line=2,col="darkgreen",col.axis="darkgreen")
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
      plot(df,type="h",xlim=range.mz$x,ylab="Intensity [AU]",xlab=expression(italic(m/z)),ylim=c(0,df[1,2]*1.1),main="selected masses")
      text(x=df[1:10,1],y=df[1:10,2],labels=df[1:10,1],pos=3)
    }
  })
  output$Visu_data_table = renderDataTable({
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
      colnames(df) = c("m/z","intensity")
      df
    }
  })
  output$VarSel_fullfullscan <- renderPlot({ ## the fullfullscan means the full scan over the whole time range
    par(yaxs="i", xaxs="i",mar=c(5, 4, 3, 2))
    validate(
      need(!is.null(data.VarSel$model),"Please do the Clusterisation")
    )
    df <- data.frame(x = as.numeric(rownames(data.VarSel$eic)),y = apply(data.VarSel$eic,1,sum))
    df = df[data.VarSel$keep,]
    if(!is.null(range.mz$x)){df <- df[df$x <= range.mz$x[2] & df$x >= range.mz$x[1],]}
    validate(
      need(nrow(df) >0,"No selected masses in this range, double click to reset the range")
    )
    df <- df[order(df[,2],decreasing = T),]
    plot(df,type="h",xlim=range.mz$x,ylab="Intensity [AU]",xlab=expression(italic(m/z)),ylim=c(0,df[1,2]*1.1),main="full scan")
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
         xlab=expression(italic(m/z)),ylab="Intensity [AU]",xlim=range.mz$x, ylim=c(0,df[1,2]*1.1),
         main=paste0(names(cond())[index$cond],"\nRf = ",getTime(data,index$index)," min ; base peak Mz = ",round(df[1,1],4),
                     " ; base peak intensity = ",df[1,2],"\n",
                     "peaks count = ",nrow(df)," ; totIonCurrent = ",round(sum(df[,2])))
    )
    text(x=df[1:10,1],y=df[1:10,2],labels=round(df[1:10,1],4),pos=3)
  })

  # output$report_preview_choices = renderUI({
  #   validate(need(length(reported$l) > 0 , "select at least a cluster"))
  #   choices = seq(length(reported$l))
  #   selectizeInput("report_preview_choices","Report include",choices = choices)
  # })
  # output$report_preview = renderPlot({
  #   i = reported$l[[as.numeric(input$report_preview_choices)]]
  #   Int <- apply(data.VarSel$eic,1,sum)
  #   rbPal <- colorRampPalette(c('blue','red'))
  #   Col <- rbPal(10)[as.numeric(cut(log10(Int),breaks = 10))]
  #     plot(data.VarSel$model,type="n",xlab="",ylab="")
  #     text(data.VarSel$model,labels=rownames(data.VarSel$eic),col=Col,cex = 0.5)
  #   title(main=data.VarSel$algo,xlab = "dimension 1",ylab="dimension 2")
  #   ## add the scoreplot_cross if applicable
  #   # if(!is.null(input$scroreplot_cross)){
  #   #   text(x=data.VarSel$model[input$scroreplot_cross,1],y=data.VarSel$model[input$scroreplot_cross,2],labels="X",col="darkgreen",cex=3)
  #   # }
  #   ## add the contour
  #   my.cols <- rev(RColorBrewer::brewer.pal(11, "RdYlBu"))
  #   z <- MASS::kde2d(data.VarSel$model[,1], data.VarSel$model[,2], n=50)
  #   contour(z, drawlabels=FALSE, nlevels=11, col=my.cols, add=TRUE)
  #
  #   ## add the selected zone if aplicable
  #     symbols(x=mean(i$x),y=mean(i$y),
  #             rectangles = rbind(c(i$x[2]-i$x[1],i$y[2]-i$y[1])),
  #             add=T,inches = F,fg="darkgreen",lwd=2)
  # })

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

  output$checkpoint_download = downloadHandler(
    filename = function(x){"eic2_checkpoint.RData"},
    content = function(con) {
      assign("data",list(index = index$index,cond=index$index,last=index$index,reported=reported$l,#reported.table = reported$table,
                         range.mz.x = range.mz$x,range.time.x = range.time$x,range.scoreplot.x=range.scoreplot$x,range.scoreplot.y=range.scoreplot$y,
                         data.VarSel.eic_before_CODA = data.VarSel$eic_before_CODA,data.VarSel.eic = data.VarSel$eic, data.VarSel.model = data.VarSel$model, data.VarSel.algo = data.VarSel$algo, data.VarSel.keep = data.VarSel$keep,data.VarSel.range.time = data.VarSel$range.time,
                         VarSel_selected.index = VarSel_selected$index, VarSel_selected.x = VarSel_selected$x, VarSel_selected.y = VarSel_selected$y,
                         data.ls = data.ls(),
                         meta.Int_treshold=meta$Int_treshold,meta.range_mz_mini=meta$range_mz_mini,meta.range_mz_maxi=meta$range_mz_maxi,meta.bucketing_increment=meta$bucketing_increment,meta.mode=meta$mode,
                         meta.window_CODA=meta$window_CODA,meta.smoothing_CODA=meta$smoothing_CODA,meta.threshold_CODA=meta$threshold_CODA,meta.apply_CODA=meta$apply_CODA,
                         meta.preprocess=meta$preprocess,
                         meta.tsne_pca=meta$tsne_pca,meta.tsne_theta=meta$tsne_theta,
                         meta.tsne_initial_dims=meta$tsne_initial_dims,meta.tsne_perplexity=meta$tsne_perplexity,meta.tsne_max_iter=meta$tsne_max_iter,meta.tsne_whiten=meta$tsne_whiten,meta.kmeans_center=meta$kmeans_center,meta.kmeans_iter_max=meta$kmeans_iter_max
      ))
      save(list="data", file=con)
    }
  )

  checkpoint = reactive({
    # validate(need(input$Data_2_use != "Your own data","Error"))
    if(input$Data_2_use == "Check_point_file"){
      validate(need(!is.null(input$checkpoint_upload),"Upload a checkpoint file"))
      load(input$checkpoint_upload$datapath)
    }else if(input$Data_2_use == "Demo_file"){
      validate(need(!is.null(input$Demo_file),"Select a demo file"))
      load(paste0("www/",input$Demo_file))
    }
    data
  })

  ## meta: must be change in 4 places: here, in the download, in the observe with all the updates, in the observeEvent(s)
  meta = reactiveValues(Int_treshold=NULL,range_mz_mini=NULL,range_mz_maxi=NULL,bucketing_increment=NULL,mode=NULL,
                        window_CODA = NULL,smoothing_CODA=NULL,threshold_CODA=NULL,apply_CODA=F,
                        preprocess = NULL,
                        tsne_initial_dims=NULL,tsne_perplexity=NULL,tsne_max_iter=NULL,tsne_whiten=NULL,tsne_pca=NULL,tsne_theta=NULL,kmeans_center=NULL,kmeans_iter_max=NULL)

  observeEvent(input$Data_2_use,{
    if(input$Data_2_use == "Your_own_data"){ ## do we need to reboot everybody ??

      index$index=1;index$cond=1;index$last=1
      reported$l = list()
      range.mz$x = NULL
      data.VarSel$eic_before_CODA=NULL;data.VarSel$eic=NULL;data.VarSel$model=NULL;data.VarSel$algo=NULL;data.VarSel$keep=NULL
      range.time$x = NULL # to zoom inside the TIC
      range.scoreplot$x = NULL; range.scoreplot$y = NULL # to zoom inside the scoreplot
      VarSel_selected$index=c();VarSel_selected$x = NULL;VarSel_selected$y = NULL # triggered when cluster selected
    }
  })

  observeEvent(input$checkpoint_upload$datapath,{
    # validate(need(!is.null(checkpoint()),"Error"))
      data = checkpoint()
      data$index -> index$index;data$cond->index$index;data$last->index$index;data$reported->reported$l;
      data$range.mz.x -> range.mz$x; data$range.time.x -> range.time$x; data$range.scoreplot.x->range.scoreplot$x; data$range.scoreplot.y->range.scoreplot$y;
      data$data.VarSel.eic_before_CODA -> data.VarSel$eic_before_CODA;data$data.VarSel.eic -> data.VarSel$eic; data$data.VarSel.model -> data.VarSel$model;
      data$data.VarSel.algo -> data.VarSel$algo; data$data.VarSel.keep -> data.VarSel$keep;data$data.VarSel.range.time -> data.VarSel$range.time;
      data$VarSel_selected.index -> VarSel_selected$index; data$VarSel_selected.x -> VarSel_selected$x; data$VarSel_selected.y -> VarSel_selected$y
      updateNumericInput(session,"Int_treshold",value = data$meta.Int_treshold);data$meta.Int_treshold -> meta$Int_treshold
      updateNumericInput(session,"range_mz_mini",value = data$meta.range_mz_mini);data$meta.range_mz_mini -> meta$range_mz_mini
      updateNumericInput(session,"range_mz_maxi",value = data$meta.range_mz_maxi);data$meta.range_mz_maxi -> meta$range_mz_maxi
      updateNumericInput(session,"bucketing_increment",value = data$meta.bucketing_increment);data$meta.bucketing_increment -> meta$bucketing_increment
      updateSelectizeInput(session,"mode",selected=data$meta.mode);data$meta.mode -> meta$mode
      updateNumericInput(session,"window_CODA",value = data$meta.window_CODA);data$meta.window_CODA -> meta$window_CODA
      updateNumericInput(session,"smoothing_CODA",value = data$meta.smoothing_CODA);data$meta.smoothing_CODA -> meta$smoothing_CODA
      updateNumericInput(session,"threshold_CODA",value = data$meta.threshold_CODA);data$meta.threshold_CODA -> meta$threshold_CODA
      ## spe need change reactive value if needed apply_CODA
      meta$apply_CODA=data$meta.apply_CODA
      updateCheckboxGroupInput(session, "preprocess", selected = data$meta.preprocess);data$meta.preprocess -> meta$preprocess
      updateNumericInput(session,"tsne_initial_dims",value = data$meta.tsne_initial_dims);data$meta.tsne_initial_dims -> meta$tsne_initial_dims
      updateNumericInput(session,"tsne_perplexity",value = data$meta.tsne_perplexity);data$meta.tsne_perplexity -> meta$tsne_perplexity
      updateNumericInput(session,"tsne_max_iter",value = data$meta.tsne_max_iter);data$meta.tsne_max_iter -> meta$tsne_max_iter
      updateCheckboxInput(session, "tsne_whiten", value = data$meta.tsne_whiten);data$meta.tsne_whiten -> meta$tsne_whiten
      updateNumericInput(session,"tsne_theta",value = data$meta.tsne_theta);data$meta.tsne_theta -> meta$tsne_theta
      updateCheckboxInput(session, "tsne_pca", value = data$meta.tsne_pca);data$meta.tsne_pca -> meta$tsne_pca
      updateNumericInput(session,"kmeans_center",value = data$meta.kmeans_center);data$meta.kmeans_center -> meta$kmeans_center
      updateNumericInput(session,"kmeans_iter_max",value = data$meta.kmeans_iter_max);data$meta.kmeans_iter_max -> meta$kmeans_iter_max
      # if(!is.null(data$reported.table)){
      #   data$reported.table -> reported$table
      # }else{
      #     l=length(reported$l);reported$table = data.frame(content=rep("",l),color=rep("black",l),show=rep(T,l),delete=rep(F,l))
      #     # reported$table[,1] = as.character(reported$table[,1])
      #     # reported$table[,2] = as.character(reported$table[,2])
      #     # reported$table[,3] = as.logical(reported$table[,3])
      #     # reported$table[,4] = as.logical(reported$table[,4])
      #     }
  })

  output$Data_2_use = renderUI({
    if(input$Data_2_use == "Check_point_file"){
      fileInput("checkpoint_upload","checkpoint Rdata file")
    }else if(input$Data_2_use == "Your_own_data"){
      fileInput("file_MS","mzXML file",multiple = T)
    }else if(input$Data_2_use == "Demo_file"){
      truc = dir("www/",pattern = ".RData",ignore.case = T)
      tagList(
        selectizeInput("Demo_file","select the demo file to use",choices = truc,selected = truc[[1]]),
        h4("Demo file loaded")
      )

    }
  })

  output$report_choices = renderUI({
    validate(need(length(reported$l) > 0 , "select at least a cluster"))
    choices = seq(length(reported$l))
    selectizeInput("report_choices","Report include",choices = choices,multiple=T,selected=choices)
  })

  output$reported_show = renderUI({
    validate(need(length(reported$l) > 0 , "select at least a cluster"))
      selectizeInput("reported_show","Reported show",choices = c("",seq(length(reported$l))),selected = "")
  })
  observeEvent(input$reported_show,{
    if(input$reported_show != "" && length(reported$l) > 0){
      VarSel_selected$index <- reported$l[[as.numeric(input$reported_show)]]$index

      VarSel_selected$x <- reported$l[[as.numeric(input$reported_show)]]$x
      VarSel_selected$y <- reported$l[[as.numeric(input$reported_show)]]$y
    }
  })
  observeEvent(input$reported_delete,{
    if(input$reported_show != ""){
      reported$l[[as.numeric(input$reported_show)]] = NULL
    }
  })

  reported <- reactiveValues(l = list(),table = data.frame()) # use to store cluster for report
  observeEvent(input$VarSel_EIC_report,{
    truc = list(x=VarSel_selected$x,y=VarSel_selected$y,index=VarSel_selected$index,xlim=range.mz$x,pch = input$VarSel_eic_pch)
    # reported$l[[length(reported$l)+1]] = truc ## else, append
    # if(length(reported$l) == 0){
      reported$l[[length(reported$l)+1]] = truc ## if first time, append truc
    # }else if(reported$l[[length(reported$l)]]$x[1] == truc$x[1] & reported$l[[length(reported$l)]]$x[2] == truc$x[2]){
    #   reported$l = list() ## else if last == truc, reset
    # }else{
    #   reported$l[[length(reported$l)+1]] = truc
    # }
    # reported$table = rbind(
    #   reported$table,
    #   c(
    #     # xmin=VarSel_selected$x[1],xmax=VarSel_selected$x[2],
    #     # ymin=VarSel_selected$y[1],ymax=VarSel_selected$y[2],
    #     "","black",T,F)
    # )
    # colnames(reported$table) = c(#"xmin","xmax","ymin","ymax",
    #                              "content","color","show","delete")
    # reported$table[,1] = as.character(reported$table[,1])
    # reported$table[,2] = as.character(reported$table[,2])
    # reported$table[,3] = as.logical(reported$table[,3])
    # reported$table[,4] = as.logical(reported$table[,4])
  })

  # output$Selected_table = renderRHandsontable({
  #   data = reported$table
  #   rhandsontable(data)
  # })
  # observeEvent(input$Selected_table_update,{
  #   reported$table = hot_to_r(input$Selected_table)
  #   reported$l[which(reported$table$delete)] = NULL
  #   reported$table = reported$table[!reported$table$delete,]
  # })
  outputOptions(output, "mode", suspendWhenHidden = FALSE)
}
