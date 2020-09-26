library(shiny)
library(seewave)
library(tuneR)
library(plotly)
library(shinyalert)
library(grid)
library(filesstrings)
library(warbleR)
library(DT)
library(soundgen)
library(dplyr)
library(purrr)
library(reshape2)
library(htmltools)
if(!require(shinyWidgets)) install.packages(shinyWidgets)
library(wavesurfer)
library(htmlwidgets)
library(shinyjs)
library(shinyBS)



shinyUI(navbarPage("Acoustic Analysis",
                   tabPanel("About", uiOutput("aboutInfo")),
                   tabPanel("Exploration",
                            sidebarPanel(width = 4,
                                         h1("Acoustic Analysis"),
                                         br(),
                                         
                                         # Upload Data File
                                         uiOutput('resettableInput'),
                                         br(),
                                         actionButton("resetAll", "Reset Inputs"),
                                         
                                         conditionalPanel("output.filechosen == true",
                                                          # h4("Native Information About .wav File:"),
                                                          br(),
                                                          uiOutput("wavinfo"),
                                                          
                                                          br(),
                                                          
                                                          #uiOutput("audioplay"),
                                                          h3("General Information:"),
                                                          
                                                          # div(style="display:inline-block", uiOutput("mintimelimit")),
                                                          # div(style="display:inline-block", uiOutput("maxtimelimit")),
                                                          # br(),
                                                          # div(style="display:inline-block", uiOutput("minfreqlimit")),
                                                          # div(style="display:inline-block", uiOutput("maxfreqlimit")),
                                                          #br(),
                                                          
                                                          
                                                          ## Spectrum
                                                          #div(style="display:inline-block", uiOutput("spectrumcheck")),
                                                          
                                              
                                                          div(style="display: inline-block;vertical-align:top; width: 100px;", uiOutput("spectrummin")),
                                                          div(style="display: inline-block;vertical-align:top; width: 100px;", uiOutput("spectrummax")),
                                                          #div(style="display: inline-block;vertical-align:top; width: 1px;",HTML("<br>")),
                                                          div(style="display: inline-block;vertical-align:top; width: 100px;", actionButton("specthelp", "", icon = icon("question-circle"))),
                                                          br(),
                                                          
                                                          ## Sampling Rate
                                                          #div(style="display:inline-block", uiOutput("sampcheck")),
                                                          div(style="display: inline-block;vertical-align:top; width: 200px;", uiOutput("samplingrate")),
                                                          #div(style="display: inline-block;vertical-align:top; width: 5px;",HTML("<br>")),
                                                          div(style="display: inline-block;vertical-align:top; width: 200px;", actionButton("samphelp", "", icon = icon("question-circle"))),
                                                          br(),
                                                          
                                                          ## Window Function
                                                          #div(style="display:inline-block", uiOutput("windowcheck")),
                                                          div(style="display: inline-block;vertical-align:top; width: 200px;", uiOutput("window")),
                                                          #div(style="display: inline-block;vertical-align:top; width: 5px;",HTML("<br>")),
                                                          div(style="display: inline-block;vertical-align:top; width: 200px;", actionButton("windowhelp", "", icon = icon("question-circle"))),
  
                                                          br(),
                                                          
                                                          ## Overlapping
                                                          #div(style="display:inline-block", uiOutput("ovlpcheck")),
                                                          div(style="display: inline-block;vertical-align:top; width: 200px;",uiOutput("ovlp")),
                                                          #div(style="display: inline-block;vertical-align:top; width: 5px;",HTML("<br>")),
                                                          div(style="display: inline-block;vertical-align:top; width: 200px;", actionButton("ovlphelp", "", icon = icon("question-circle"))),
                                                          
                                                          br(),
                                                          
                                                          ## Zero Padding
                                                          #div(style="display:inline-block", uiOutput("zpcheck")),
                                                          
                                                          div(style="display: inline-block;vertical-align:top; width: 200px;",uiOutput("zp")),
                                                          #div(style="display: inline-block;vertical-align:top; width: 5px;",HTML("<br>")),
                                                          div(style="display: inline-block;vertical-align:top; width: 200px;", actionButton("zphelp", "", icon = icon("question-circle")))
                                         )
                            ),
                            mainPanel(width = 8,
                                      conditionalPanel("output.filechosen == true",
                                          tags$style(type="text/css",
                                                     ".shiny-output-error { visibility: hidden; }",
                                                     ".shiny-output-error:before { visibility: hidden; }"
                                          ),
                                          plotOutput("spectro",
                                                    width = "auto",
                                                    height = "100px"),
                                          plotlyOutput("plotly",  width = "auto", height = "600px"),
                                          uiOutput("audioplay",),
                                  
                                          # switchInput("minimap", "Minimap", inline = TRUE),
                                          # switchInput("spectrogram", "spectrogram", inline = TRUE),
                                          # switchInput("timeline", "timeline", inline = TRUE),
                                          # switchInput("cursor", "cursor", inline = TRUE),
                                          # actionButton("regions", "annotator", icon = icon("square")),
                                          #tags$br(),
                                          #wavesurferOutput("my_ws"),
                                          #tags$br(),
                                          # actionButton("play", "", icon = icon("play")),
                                          # actionButton("pause", "", icon = icon("pause")),
                                          # actionButton("stop", "", icon = icon("stop")),
                                          # actionButton("skip_backward", "", icon = icon("backward")),
                                          # actionButton("skip_forward", "", icon = icon("forward")),
                                          # actionButton("mute", "", icon = icon("volume-off")),
                                          # sliderInput("volume", "Volume", min = 0, max = 100, value = 50),
                                          
                                          uiOutput("SpecHelpInfo"),
                                          
                                          uiOutput("SampHelpInfo"),
                                          
                                          uiOutput("WindowHelpInfo"),
                                          
                                          uiOutput("OvlpHelpInfo"),
                                          
                                          uiOutput("ZpHelpInfo")
                                          
                                          
                                          # textOutput("file1"),
                                          # textOutput("file2"),
                                          # textOutput("windowchk"),
                                          # textOutput("zpchk"),
                                          # textOutput("zpval")
                                          # textOutput("windowval"),
                                          # textOutput("sampchk"),
                                          # textOutput("samprateval"),
                                          # textOutput("speccheck"),
                                          # textOutput("specmin"),
                                          # textOutput("specmax")
                                      )   
                            )
                   ),
                   tabPanel("Segmentation",
                            sidebarPanel(width = 4,
                                         h3("Segmentation"),
                                         ## Segmentation Help
                                         uiOutput("SegHelpInfo"),
                                         br(),
                                         conditionalPanel("output.filechosen == true",
                                                      
                                                          div(style="display:inline-block",uiOutput("shortestSyl")),
                                                          div(style="display:inline-block", actionButton("segHelp", "", icon = icon("question-circle"))),
                                                          uiOutput("minSegInfo"),
                                                          div(style="display:inline-block",uiOutput("threshold")),
                                                          div(style="display:inline-block", actionButton("thresHelp", "", icon = icon("question-circle"))),
                                                          uiOutput("thresHelpInfo"),
                                                          #actionButton("seghelp", "Segmentation Help")
          
                                                          )
                                         
                            ),
                            mainPanel(width = 8,
                                      conditionalPanel("output.filechosen == true",
                                                      ## Spectrogram 
                                                      plotOutput("spectro_seg"),
                                                      
                                                      ## Segmentation
                                                      plotOutput("segment"),
                                                      
                                                      ## Segments DataTable
                                                      div(style="display:inline-block", div("Segments")),
                                                      img(src = "segment.jpg", width = "5%", height = "5%"),
                                                      div(style="display:inline-block", 
                                                          actionButton("segmentsHelp", "", icon = icon("question-circle"))),
                                                      uiOutput("segmentsHelpInfo"),
                                                      div(DT::dataTableOutput("segments"), style = "font-size: 75%; width: 75%"),
                                                      br(),
                                                      downloadButton("downloadSegments", "Download Segments Data"),
                                                      br()
                                                      
                                                      
                                                      )))
                               
                   
            #tabPanel("About", uiOutput("aboutInfo"))
))



