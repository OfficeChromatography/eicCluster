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
## ui.R ##
library(shiny)
library(shinydashboard)
library(shinyBS)
# source("OrbiPrep.R")

dashboardPage(
  dashboardHeader(title = "eicCluster"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Input", tabName = "Input"),
      menuItem("Preparation", tabName = "Preparation"),
      menuItem("Visualization", tabName = "Visualization"),
      menuItem("Report",tabName = "Report"),
      menuItem("About",tabName = "About")
    )
  ),
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "Input",
              fileInput("file_MS","mzXML file"),
              column(6,
                     verbatimTextOutput("meta")
                     ),
              column(6,
                     img(src="pipeline-1.png",width="50%")
                     ),
              # tags$head(tags$style(type="text/css", ".btn {border-radius: 20px; font-size: 30px;}")),
              tags$head(tags$style(type="text/css", "tfoot {display: table-header-group}")),
              tags$head(tags$style(HTML(".shiny-output-error-validation {color: red;font-size: 24px}"))),
              tags$head(tags$style(type="text/css", ".shiny-progress .progress {position: absolute;width: 100%;top: 100px;height: 10px;margin: 0px;}")),
              tags$head(tags$style(type="text/css", ".shiny-progress .progress-text {position: absolute;border-style: solid;
                                                 border-width: 2px;right: 10px;height: 36px;width: 50%;background-color: #EEF8FF;margin: 0px;padding: 2px 3px;opacity: 1;}"))

      ),
      tabItem(tabName = "Preparation",
              numericInput("VarSel_treshold","Intensity treshold",1000000),
              bsTooltip("VarSel_treshold", "This will remove all the samples with intensity lower than the selected value. Used to speed up the extraction", placement = "bottom", trigger = "hover"),
              numericInput("VarSel_mz_mini","Masses minimum",50),
              numericInput("VarSel_mz_maxi","Masses maximum",750),
              numericInput("VarSel_increment","increment",0.01),
              bsTooltip("VarSel_increment", "This will round the values to the given accuracy, we need it to bucket the signal.", placement = "bottom", trigger = "hover"),
              uiOutput("VarSel_mode"),
              plotOutput("VarSel_tic",dblclick = "dblclick.VarSel_tic",
                         brush = brushOpts(id = "brush.VarSel_tic",resetOnNew = TRUE,direction = "x")),
              bsTooltip("VarSel_tic", "You can brush and double click to select a subset of the time range and work only with it.", placement = "bottom", trigger = "hover"),
              actionButton("VarSel_EIC","Do the bucketting"),
              bsTooltip("VarSel_EIC", "Click to extract EIC for each masses in the selected range.", placement = "bottom", trigger = "hover"),
              uiOutput("VarSel_EIC_dim"),
              checkboxGroupInput("VarSel_preprocess","Preprocesses",choices=c("standardNormalVariate","scale"),selected="standardNormalVariate"),
              box(title = "PCA",width = 4,height = "400px",
                  h4("No options here"),
                  actionButton("VarSel_PCA","do PCA"),
                  bsTooltip("VarSel_PCA", "Do not click if there is more variables than masses", placement = "bottom", trigger = "hover")

              ),
              box(title = "tsne",width = 4,height =  "400px",
                  numericInput("tsne_initial_dims","initial_dims",30),
                  bsTooltip("tsne_initial_dims", "The number of dimensions to use in reduction method", placement = "bottom", trigger = "hover"),

                  numericInput("tsne_perplexity","perplexity",30),
                  bsTooltip("tsne_perplexity", "Perplexity parameter. (optimal number of neighbors)", placement = "bottom", trigger = "hover"),

                  numericInput("tsne_max_iter","max_iter",300),
                  bsTooltip("tsne_max_iter", "Maximum number of iterations to perform.", placement = "bottom", trigger = "hover"),

                  checkboxInput("tsne_whiten","whiten",T),
                  bsTooltip("tsne_whiten", "A boolean value indicating whether the matrix data should be whitened.", placement = "bottom", trigger = "hover"),

                  actionButton("VarSel_tsne","do tsne")
              ),
              box(title = "kmeans",width = 4,height =  "400px",
                  numericInput("kmeans_center","center",8),
                  bsTooltip("kmeans_center", "the number of clusters to split the data in.", placement = "bottom", trigger = "hover"),

                  numericInput("kmeans_iter_max","iter.max",10),
                  bsTooltip("kmeans_iter_max", "the maximum number of iterations allowed.", placement = "bottom", trigger = "hover"),

                  actionButton("VarSel_kmeans","do kmeans")
              )
      ),
      tabItem("Visualization",
              actionButton("VarSel_EIC_bis","Plot the selection"),
              actionButton("VarSel_EIC_report","Select for report"),
              checkboxInput("VarSel_eic_normalize","Use same scale for eic and tic",T),
              uiOutput("scroreplot_cross"),
              column(6,
                     plotOutput("VarSel_scorePlot",dblclick = "dblclick.VarSel_scorePlot",
                                brush = brushOpts(id = "brush.VarSel_scorePlot",resetOnNew = TRUE),height = "1200"),
                     bsTooltip("VarSel_scorePlot", "EIC are clusterized with the selected algorithm. Select a zone and click on the Plot the selection button to access the spectrum and EIC, it is also possible to dbl-click to zoom and dezoom.", placement = "bottom", trigger = "hover")
                     ),
              column(6,

                     fluidRow(
                       plotOutput("VarSel_eic",height = "300px",click = "click.VarSel_eic"),
                       plotOutput("VarSel_spectrum",height = "300px",dblclick = "dblclick.VarSel_spectrum",
                                  brush = brushOpts(id = "brush.VarSel_spectrum",resetOnNew = TRUE,direction = "x")),
                       plotOutput("VarSel_fullfullscan",height = "300px",dblclick = "dblclick.VarSel_fullfullscan",
                                  brush = brushOpts(id = "brush.VarSel_fullfullscan",resetOnNew = TRUE,direction = "x")),
                       plotOutput("VarSel_fullscan",height = "300px",dblclick = "dblclick.VarSel_fullscan",
                                  brush = brushOpts(id = "brush.VarSel_fullscan",resetOnNew = TRUE,direction = "x"))
                     )
                     )

      ),
      tabItem("Report",
              column(3,
                     uiOutput("report_choices"),
                     radioButtons('format', 'Document format', c('PDF', 'HTML', 'Word'),
                                  inline = TRUE),
                     downloadButton("Report")
                     ),
              column(9,
                     uiOutput("report_preview_choices"),
                     plotOutput("report_preview")
                     )
              ),
      tabItem("About",
              includeMarkdown("README.md")
      )
    )
  )
)
