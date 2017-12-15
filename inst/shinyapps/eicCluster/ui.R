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

dashboardPage(
  dashboardHeader(title = "eicCluster",titleWidth = 150),
  dashboardSidebar(
    width = 150,
    sidebarMenu(
      menuItem("Input", tabName = "Input"),
      menuItem("Preparation", tabName = "Preparation"),
      menuItem("Visualization", tabName = "Visualization"),
      menuItem("About",tabName = "About")
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "Input",
              radioButtons("Data_2_use","Data to use",choices = c("Your_own_data","Demo_file","Saved_file"),selected = "Your_own_data"),
              uiOutput("Data_2_use"),
              br(),
              column(6,
                     verbatimTextOutput("meta")
                     ),
              column(6,
                     img(src="pipeline-1.png",width="50%")
                     ),
              tags$head(tags$style(type="text/css", "tfoot {display: table-header-group}")),
              tags$head(tags$style(HTML(".shiny-output-error-validation {color: red;font-size: 24px}"))),
              tags$head(tags$style(type="text/css", ".shiny-progress .progress {position: absolute;width: 100%;top: 100px;height: 10px;margin: 0px;}")),
              tags$head(tags$style(type="text/css", ".shiny-progress .progress-text {position: absolute;border-style: solid;
                                                 border-width: 2px;right: 10px;height: 36px;width: 50%;background-color: #EEF8FF;margin: 0px;padding: 2px 3px;opacity: 1;}"))

      ),
      tabItem(tabName = "Preparation",
              column(3,h4("Subsetting/Bucketting"),
                     numericInput("Int_treshold","Intensity treshold",1000000),
                     bsTooltip("Int_treshold", "This will remove all the samples with intensity lower than the selected value. Used to speed up the extraction", placement = "bottom", trigger = "hover"),
                     numericInput("range_mz_mini","Masses minimum",50),
                     numericInput("range_mz_maxi","Masses maximum",750),
                     numericInput("bucketing_increment","increment",0.01),
                     bsTooltip("bucketing_increment", "This will round the values to the given accuracy, we need it to bucket the signal.", placement = "bottom", trigger = "hover"),
                     uiOutput("mode"),
                     actionButton("Bucket","Do the bucketting"),
                     bsTooltip("Bucket", "Click to extract EIC for each masses in the selected range.", placement = "bottom", trigger = "hover"),
                     uiOutput("Bucket_dim_1")
              ),
              column(3,h4("Noise reduction: CODA"),
                     numericInput("window_CODA","window",5),
                     selectizeInput("smoothing_CODA","smoothing",choices=c("median","mean")),
                     numericInput("threshold_CODA","threshold in MCQ",0.85),
                     actionButton("apply_CODA","Apply CODA"),
                     uiOutput("Bucket_dim_2")
              ),
              column(2,h4("Preprocessing"),
                     checkboxGroupInput("preprocess","Preprocesses",choices=c("standardNormalVariate","scale"),selected="standardNormalVariate")
              ),
              column(4,h4("Clusterisation"),
                                numericInput("tsne_initial_dims","initial_dims",30),
                                bsTooltip("tsne_initial_dims", "The number of dimensions to use in reduction method", placement = "bottom", trigger = "hover"),
                                numericInput("tsne_perplexity","perplexity",30),
                                bsTooltip("tsne_perplexity", "Perplexity parameter. (optimal number of neighbors)", placement = "bottom", trigger = "hover"),
                                numericInput("tsne_max_iter","max_iter",300),
                                bsTooltip("tsne_max_iter", "Maximum number of iterations to perform.", placement = "bottom", trigger = "hover"),
                                checkboxInput("tsne_pca","PCA",T),
                                bsTooltip("tsne_PCA", "logical; Whether an initial PCA step should be performed (default: TRUE)", placement = "bottom", trigger = "hover"),
                                numericInput("tsne_theta","theta",0.5),
                                bsTooltip("tsne_theta", "numeric;  Speed/accuracy trade-off (increase for less accuracy),  set to 0.0 for exact TSNE (default: 0.5)", placement = "bottom", trigger = "hover"),
                                actionButton("rtsne","Clusterize"),
                                actionButton("reset_tsne","Reset the model"),
                                bsTooltip("reset_tsne", "Reset to start from scratch, otherwise, each time the buttons are pressed, new iterations are trigerred on top", placement = "bottom", trigger = "hover")
              ),
              column(12,
                     plotOutput("prep_tic",dblclick = "dblclick.prep_tic",click = "click.prep_tic",
                                brush = brushOpts(id = "brush.prep_tic",resetOnNew = TRUE,direction = "x")),
                     bsTooltip("prep_tic", "You can brush and double click to select a subset of the time range and work only with it.", placement = "bottom", trigger = "hover")
                     ),
              column(12,
                     plotOutput("prep_fullscan",dblclick = "dblclick.prep_fullscan",
                                brush = brushOpts(id = "brush.prep_fullscan",resetOnNew = TRUE,direction = "x"))
                     )


      ),
      tabItem("Visualization",
              column(12,
                     div(style="display: inline-block;vertical-align:top; width: 140px;",actionButton("VarSel_EIC_bis","Plot the selection")),
                     div(style="display: inline-block;vertical-align:top; width: 130px;",actionButton("VarSel_EIC_report","Select for report")),
                     bsTooltip("VarSel_EIC_report", "Click to store the current selection for the report, click two times to reset the selection (no double click, just a second time)", placement = "bottom", trigger = "hover"),
                     div(style="display: inline-block;vertical-align:top; width: 150px;",selectizeInput("VarSel_eic_pch","Label in the score-plot",choices = c("m/z","punct","circles"),selected="m/z")),
                     div(style="display: inline-block;vertical-align:top; width: 130px;",selectInput("scoreplot_color","Scoreplot colors",choices=c("Intensity (cps)" = "Intensity","Mass Chromatogram Quality (CODA)" = "CODA"))),
                     div(style="display: inline-block;vertical-align:top; width: 150px;",uiOutput("scroreplot_cross")),
                     div(style="display: inline-block;vertical-align:top; width: 100px;",actionButton("VarSel_EIC_exclude","Exclude")),
                     div(style="display: inline-block;vertical-align:top; width: 120px;",actionButton("VarSel_EIC_exclude_reset","Exclusion reset")),
                     div(style="display: inline-block;vertical-align:top; width: 120px;",uiOutput("reported_show")),
                     div(style="display: inline-block;vertical-align:top; width: 80px;",actionButton("reported_delete","Delete")),
                     div(style="display: inline-block;vertical-align:top; width: 120px;",uiOutput("report_choices")),
                     div(style="display: inline-block;vertical-align:top; width: 120px;",selectizeInput('format', 'Document format', c('PDF', 'HTML', 'Word'))),
                     div(style="display: inline-block;vertical-align:top; width: 100px;",downloadButton("Report","Report",icon=icon("print"))),
                     bsTooltip("VarSel_EIC_exclude", "Click to exclude the current selection from the score-plot, the tic, the eic, the full scan averaged. This will also reset the selection for report.", placement = "bottom", trigger = "hover"),
                     div(style="display: inline-block;vertical-align:top; width: 80px;",downloadButton("checkpoint_download","Save",icon=icon("save")))
                     ),

              column(6,
                     fluidRow(
                           plotOutput("VarSel_scorePlot",dblclick = "dblclick.VarSel_scorePlot",
                                      brush = brushOpts(id = "brush.VarSel_scorePlot",resetOnNew = TRUE),height = "800"),
                           bsTooltip("VarSel_scorePlot", "EIC are clusterized with the selected algorithm. Select a zone and click on the Plot the selection button to access the spectrum and EIC, it is also possible to dbl-click to zoom and dezoom.",
                                     placement = "bottom", trigger = "hover")
                    )
                     ),
              column(6,

                     fluidRow(
                       plotOutput("VarSel_tic",height = "200px",click = "click.VarSel_eic",dblclick = "dblclick.VarSel_eic",
                                  brush = brushOpts(id = "brush.VarSel_eic",resetOnNew = TRUE,direction = "x")),
                       plotOutput("VarSel_eic",height = "200px",click = "click.VarSel_eic",dblclick = "dblclick.VarSel_eic",
                                  brush = brushOpts(id = "brush.VarSel_eic",resetOnNew = TRUE,direction = "x")),
                       plotOutput("VarSel_fullscan",height = "200px",dblclick = "dblclick.VarSel_fullscan",
                                  brush = brushOpts(id = "brush.VarSel_fullscan",resetOnNew = TRUE,direction = "x")),
                       plotOutput("VarSel_spectrum",height = "200px",dblclick = "dblclick.VarSel_spectrum",
                                  brush = brushOpts(id = "brush.VarSel_spectrum",resetOnNew = TRUE,direction = "x"))
                     )
                     ),
              dataTableOutput("Visu_data_table")

      ),
      tabItem("About",
              includeMarkdown("README.md")
      )
    )
  )
)
