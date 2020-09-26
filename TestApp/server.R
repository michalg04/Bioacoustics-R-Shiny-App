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
#if(!require(shinyWidgets)) install.packages(shinyWidgets)
#library(wavesurfer)
library(htmlwidgets)
library(shinyjs)
library(shinyBS)



seed = as.numeric(Sys.time())
load("DefaultWavFiles-ForApp.RData")
source("spectro_hg.R")
source("seg_functions.R")
#source("wavesurfer.R")

shinyServer(function(input, output, session) {
    
#### Reactive Values ####
filevalues <- reactiveValues(
    file1 = NULL,
    file2 = 1
)

observeEvent(input$file1, {
    filevalues$file1 <- input$file1
})

observeEvent(input$file2, {
    filevalues$file2 <- input$file2
})

observeEvent(input$resetAll, {
    filevalues$file1 <- NULL
    filevalues$file2 <- 1
})


output$resettableInput <- renderUI({
    times <- input$resetAll
    div(id=letters[(times %% length(letters)) + 1],
        fileInput("file1", "Choose .wav File to Upload", accept = c(".wav")),
        selectInput("file2", label = "Or Choose a Preloaded .wav File",
                    choices = list("None" = 1,
                                   "Sine1" = 2,
                                   "Sine2" = 3,
                                   "Sine3" = 4,
                                   "Square" = 5,
                                   "Triangle" = 6,
                                   "Whale" = 7,
                                   "Dolphin" = 8,
                                   "Noise" = 9),
                    selected = 1))
    
})

output$filechosen <- reactive({
    return(!is.null(filevalues$file1) | filevalues$file2 != 1)
})
outputOptions(output, 'filechosen', suspendWhenHidden = FALSE)


#### UI & Input Elements ####
output$wavinfo <- renderUI({
    
    inFile <- filevalues$file1$datapath
    ### Read .wav file in ###
    if (!is.null(inFile)) {
        wav <- readWave(inFile)
    } else {
        wav <- switch(filevalues$file2,
                      "2" = sine1,
                      "3" = sine2,
                      "4" = sine3,
                      "5" = square,
                      "6" = triangle,
                      "7" = whale,
                      "8" = dolphin,
                      "9" = noise)
    }
    
    if (exists("wav")) {
        sp <- spectro(wav, plot = F)
        
        maxfreq_pos <- which(diff(rev(apply(sp$amp, 1, max) <= -29)) < 0)[1]
        if (length(maxfreq_pos) == 0) {
            maxf <- round(rev(sp$freq)[1], 2)
        } else {
            maxf <- round(min(rev(sp$freq)[maxfreq_pos]+.5, max(sp$freq)), 2)
        }
        
        wavtext <- paste("<p>Sample Rate:", wav@samp.rate, " Hz</p>",
                         "<p>Duration of Signal: ", floor(length(wav@left)/wav@samp.rate*100)/100, " seconds</p>",
                         "<p>Maximum Freqency: ", maxf, " kHz</p>",
                         "<p>Spectrum Duration: ", floor(length(wav@left)/wav@samp.rate*100)/100, " seconds</p>",
                         "<p>Window Function: Hanning</p>",
                         "<p>Overlapping: 0%</p>",
                         "<p>Zero Padding: 0</p>")
        
        tags$span(
            popify(bsButton("wavinfo", "Hover Here for Native Info About .wav File"),
                   "",
                   wavtext)
        )
        
    }
})

# output$mintimelimit <- renderUI({
#     inFile <- filevalues$file1$datapath
#     ### Read .wav file in ###
#     if (!is.null(inFile)) {
#         wav <- readWave(inFile)
# 
#         numericInput("mintime", label = "Min Time (s):",
#                      value = 0, min = 0, max = length(wav@left)/wav@samp.rate)
#     } else {
#         wav <- switch(filevalues$file2,
#                       "2" = sine1,
#                       "3" = sine2,
#                       "4" = sine3,
#                       "5" = square,
#                       "6" = triangle,
#                       "7" = whale,
#                       "8" = dolphin,
#                       "9" = noise)
#         
#         numericInput("mintime", label = "Min Time (s):",
#                      value = 0, min = 0, max = length(wav@left)/wav@samp.rate)
#     }
# })

# output$maxtimelimit <- renderUI({
#     inFile <- filevalues$file1$datapath
#     ### Read .wav file in ###
#     if (!is.null(inFile)) {
#         wav <- readWave(inFile)
#         
#         numericInput("maxtime", label = "Max Time (s):",
#                      value = round(length(wav@left)/wav@samp.rate,3), min = 0, max = length(wav@left)/wav@samp.rate)
#     } else {
#         wav <- switch(filevalues$file2,
#                       "2" = sine1,
#                       "3" = sine2,
#                       "4" = sine3,
#                       "5" = square,
#                       "6" = triangle,
#                       "7" = whale,
#                       "8" = dolphin,
#                       "9" = noise)
#         numericInput("maxtime", label = "Max Time (s):",
#                      value = round(length(wav@left)/wav@samp.rate,3), min = 0, max = length(wav@left)/wav@samp.rate)
#     }
# })



# output$minfreqlimit <- renderUI({
#     
#     inFile <- filevalues$file1$datapath
#     ### Read .wav file in ###
#     if (!is.null(inFile)) {
#         wav <- readWave(inFile)
#         
#         sp <- spectro(wav, plot = F)
#         
#         maxfreq_pos <- which(diff(rev(apply(sp$amp, 1, max) <= -29)) < 0)[1]
#         if (length(maxfreq_pos) == 0) {
#             numericInput("minfreq", label = "Min Frequency (kHz):",
#                          value = 0, min = 0, max = rev(sp$freq)[1])
#         } else {
#             numericInput("minfreq", label = "Min Frequency (kHz):",
#                          value = 0, min = 0, max = min(rev(sp$freq)[maxfreq_pos]+.5, max(sp$freq)))
#         }
#     } else {
#         wav <- switch(filevalues$file2,
#                       "2" = sine1,
#                       "3" = sine2,
#                       "4" = sine3,
#                       "5" = square,
#                       "6" = triangle,
#                       "7" = whale,
#                       "8" = dolphin,
#                       "9" = noise)
#         
#         sp <- spectro(wav, plot = F)
#         
#         maxfreq_pos <- which(diff(rev(apply(sp$amp, 1, max) <= -29)) < 0)[1]
#         if (length(maxfreq_pos) == 0) {
#             numericInput("minfreq", label = "Min Frequency (kHz):",
#                          value = 0, min = 0, max =  rev(sp$freq)[1])
#         } else {
#             numericInput("minfreq", label = "Min Frequency (kHz):",
#                          value = 0, min = 0, max = min(rev(sp$freq)[maxfreq_pos]+.5, max(sp$freq)))
#         }
#     }
# })
# 
# output$maxfreqlimit <- renderUI({
#     
#     inFile <- filevalues$file1$datapath
#     ### Read .wav file in ###
#     if (!is.null(inFile)) {
#         wav <- readWave(inFile)
#         
#         sp <- spectro(wav, plot = F)
#         
#         maxfreq_pos <- which(diff(rev(apply(sp$amp, 1, max) <= -29)) < 0)[1]
#         if (length(maxfreq_pos) == 0) {
#             numericInput("maxfreq", label = "Max Frequency (kHz):",
#                          value = round(rev(sp$freq)[1],3))
#         } else {
#             numericInput("maxfreq", label = "Max Frequency (kHz):",
#                          value = round(min(rev(sp$freq)[maxfreq_pos]+.5, max(sp$freq),
#                                            na.rm = TRUE),3))
#         }
#     } else {
#         wav <- switch(filevalues$file2,
#                       "2" = sine1,
#                       "3" = sine2,
#                       "4" = sine3,
#                       "5" = square,
#                       "6" = triangle,
#                       "7" = whale,
#                       "8" = dolphin,
#                       "9" = noise)
#         
#         sp <- spectro(wav, plot = F)
#         
#         maxfreq_pos <- which(diff(rev(apply(sp$amp, 1, max) <= -29)) < 0)[1]
#         if (length(maxfreq_pos) == 0) {
#             numericInput("maxfreq", label = "Max Frequency (kHz):",
#                          value = round(rev(sp$freq)[1],3))
#             
#         } else {
#             numericInput("maxfreq", label = "Max Frequency (kHz):",
#                          value = round(min(rev(sp$freq)[maxfreq_pos]+.5, max(sp$freq),
#                                            na.rm = TRUE),3))
# 
#         }
#     }
# })

# output$sampcheck <- renderUI({
#     inFile <- filevalues$file1$datapath
#     ### Read .wav file in ###
#     if (!is.null(inFile) | filevalues$file2 > 1) {
#         checkboxInput("sampcheck", label = "Change Sampling Rate?",
#                       value = F)
#     }
# })

output$samplingrate <- renderUI({
  #print(wav@samp.rate/(1:20))
    
#    if (!is.null(input$sampcheck)) {
#        if (input$sampcheck) {
            inFile <- filevalues$file1$datapath
            ### Read .wav file in ###
            if (!is.null(inFile)) {
                wav <- readWave(inFile)
                
                selectInput("samprate", label = "Sampling Rate (Hz):",
                            choices = wav@samp.rate/(1:10),
                            selected = wav@samp.rate)
                
            } else {
                wav <- switch(filevalues$file2,
                              "2" = sine1,
                              "3" = sine2,
                              "4" = sine3,
                              "5" = square,
                              "6" = triangle,
                              "7" = whale,
                              "8" = dolphin,
                              "9" = noise)
                
                selectInput("samprate", label = "Sampling Rate (Hz):",
                            choices = wav@samp.rate/(1:20),
                            selected = wav@samp.rate)
            }
#        }
#    }
})


# output$windowcheck <- renderUI({
#     
#     inFile <- filevalues$file1$datapath
#     ### Read .wav file in ###
#     if (!is.null(inFile) | filevalues$file2 > 1) {
#         checkboxInput("windowcheck", label = "Change Window Function?",
#                       value = F)
#     }
# })

output$window <- renderUI({
    
#    if (!is.null(input$windowcheck)) {
#        if (input$windowcheck) {
            selectInput("window", label = "Choose a Window Function:",
                        choices = list("Rectangular" = 1,
                                       "Bartlett (Triangular)" = 2,
                                       "Hamming" = 3,
                                       "Hanning (Default)" = 4,
                                       "Blackman" = 5,
                                       "Flattop" = 6),
                        selected = 4)
#        }
#    }
})

# output$zpcheck <- renderUI({
#     
#     inFile <- filevalues$file1$datapath
#     ### Read .wav file in ###
#     if (!is.null(inFile) | filevalues$file2 > 1) {
#         checkboxInput("zpcheck", label = "Choose Zero Padding?",
#                       value = F)
#     }
# })

output$zp <- renderUI({
    
#    if (!is.null(input$zpcheck)) {
#        if (input$zpcheck) {
            selectInput("zp", label = "Choose Zero Padding:",
                        choices = c(0, 2^(1:17)),
                        selected = 0)
    #     }
    # }
})

# output$ovlpcheck <- renderUI({
#     
#     inFile <- filevalues$file1$datapath
#     ### Read .wav file in ###
#     if (!is.null(inFile) | filevalues$file2 > 1) {
#         checkboxInput("ovlpcheck", label = "Choose Overlapping?",
#                       value = F)
#     }
# })

output$ovlp <- renderUI({
    
#    if (!is.null(input$zpcheck)) {
#        if (input$ovlpcheck) {
            sliderInput("ovlp", label = "Choose Overlapping (%):",
                        min = 0, max = 99, value = 0, step = 1)
#        }
#    }
})

# output$spectrumcheck <- renderUI({
#     
#     inFile <- filevalues$file1$datapath
#     ### Read .wav file in ###
#     if (!is.null(inFile) | filevalues$file2 > 1) {
#         checkboxInput("spectrumcheck", label = "Change Spectrum Duration?",
#                       value = F)
#     }
# })

output$spectrummin <- renderUI({
    
#    if (!is.null(input$spectrumcheck)) {
#        if (input$spectrumcheck) {
            inFile <- filevalues$file1$datapath
            
            if (!is.null(inFile)) {
                wav <- readWave(inFile)
                
                numericInput("specmin", label = "Spectrum Min Time (s):",
                             value = 0, min = 0, max = length(wav@left)/wav@samp.rate)
            } else {
                wav <- switch(filevalues$file2,
                              "2" = sine1,
                              "3" = sine2,
                              "4" = sine3,
                              "5" = square,
                              "6" = triangle,
                              "7" = whale,
                              "8" = dolphin,
                              "9" = noise)
                
                numericInput("specmin", label = "Spectrum Min Time (s):",
                             value = 0, min = 0, max = length(wav@left)/wav@samp.rate)
#            }
#        }
    }
})

output$spectrummax <- renderUI({
    
#    if (!is.null(input$spectrumcheck)) {
#        if (input$spectrumcheck) {
            inFile <- filevalues$file1$datapath
            
            if (!is.null(inFile)) {
                wav <- readWave(inFile)
                
                numericInput("specmax", label = "Spectrum Max Time (s):",
                             value = round(length(wav@left)/wav@samp.rate,3), min = 0, max = length(wav@left)/wav@samp.rate)
            } else {
                wav <- switch(filevalues$file2,
                              "2" = sine1,
                              "3" = sine2,
                              "4" = sine3,
                              "5" = square,
                              "6" = triangle,
                              "7" = whale,
                              "8" = dolphin,
                              "9" = noise)
                
                numericInput("specmax", label = "Spectrum Max Time (s):",
                             value = round(length(wav@left)/wav@samp.rate,3), min = 0, max = length(wav@left)/wav@samp.rate)
            }
#        }
#    }
})


#### Printed UI Outputs ####
output$file1 <- renderPrint(
    print(filevalues$file1)
)
output$file2 <- renderPrint(
    print(filevalues$file2)
)
output$windowchk <- renderPrint(
    print(input$windowcheck)
)
output$zpchk <- renderPrint(
    print(input$zpcheck)
)
output$zpval <- renderPrint(
    print(input$zp)
)
output$windowval <- renderPrint(
    print(input$window)
)
output$sampchk <- renderPrint(
    print(input$sampcheck)
)
output$samprateval <- renderPrint(
    print(input$samprate)
)
output$speccheck <- renderPrint(
    print(input$spectrumcheck)
)
output$specmin <- renderPrint(
    print(input$specmin)
)
output$specmax <- renderPrint(
    print(input$specmax)
)

#### Window Function Help ####
output$WindowHelpInfo <- renderText({
    times <- input$windowhelp
    if (times %% 2 == 1) {
        return("<p><b>Window Function Help</b></p>
               <p>In order to create a spectrum of an input signal (or
               some subset of the signal) the Discrete Fourier Transform (DFT)
               assumes that the captured input signal is periodic in nature
               with an integer number of periods.</p>
               <p>In reality, not all signals satisfy this assumption and thus
               when the captured input signal is repeated (to meet the criteria
               of periodicity for the DFT) many discontinuities will be present.
               The appearance of these discontinuities causes the energy at
               the frequency of the original signal to leak across other frequencies.
               In other words, the spectrum of a non-periodic signal where
               leakage has occured will display energy (amplitude) at frequencies
               that were not present in the original signal.</p>
               <p><img src='non_periodic.jpg' width= 60% height=60%></p>
               <p>Windowing a signal reduces DFT leakage. To avoid the sharp
               discontinuities that appear by taking a non-periodic signal
               and repeating it, a window function can be be applied to the signal
               in the time domain to create a tapering effect of amplitude at both
               the beginning and end of the sampling window. The windowed signal
               is then repeated with no discontinuities and the DFT is applied.</p>
               <p><img src='windowing.jpg' width= 100% height=100%></p>
               <p>There are several different types of windows that can be used
               to reduce spectral leakage. A few common ones and their spectrum
               can be seen below. The choice of which window to use is usually
               based on a trade off between side lobe effects (i.e. leakage)
               and main lobe width (i.e. loss in frequency resolution).</p>
               <p><img src='WindowFn-Pic.jpeg' width= 100% height=100%></p>
               <p>It is important to remember that the time and frequency resolution 
               of a spectrogram are inversely related and dependent on window 
               length (the number of samples used for the DFT). If the window 
               length is small there will be poor frequency resolution and good 
               time resolution. However, if the window length is large, although 
               the frequency resolution improves, the time resolution becomes 
               poor.</p>")
        
    }
})

#### Sampling Rate Help ####
output$SampHelpInfo <- renderText({
    times <- input$samphelp
    if (times %% 2 == 1) {
        return("<p><b>Sampling Rate Help</b></p>
               <p>Sampling rate (or sampling frequency), f<sub>s</sub>, is 
               the number of sampled points per second taken from a continuous
               signal to create a digital signal. It is typically measured in Hz (cycles per second).</p>
               <p>Perfect reconstruction or representation of a signal is possible
               when the sampling frequency is greater than twice the maximum
               frequency of the signal being sampled (Nyquist Principle).
               This will help avoid aliasing.</p>
               <p>Aliasing is a sampling effect that leads to signal frequencies
               being falsely interpreted as other frequencies. Consider sampling
               of a 7 kHz sine wave. As can be seen in the image, the sample
               values would not change at all if, instead, we were sampling a 1 kHz
               sine wave. The continuous 7 kHz sinusoid is aliased by a 1 kHz
               digitized wave. This will result in a misrepresentation of the frequency
               content of the signal.</p>
                <p>The drop down menu of sampling rates gets calculated by dividing the wave file sample 
                  rate by values from 1 to 20, thus creating 20 different sampling rates to chose from. </p>
               <p><img src='SamplingRateHelpPic.jpeg' width= 80% height=80%></p>")
        
    }
})

#### Spectrum Help ####
output$SpecHelpInfo <- renderText({
    times <- input$specthelp
    if (times %% 2 == 1) {
        return("<p><b>Spectrum Help</b></p>
               <p>The frequency spectrum of a signal is the presentation
               of the signal in the frequency domain based on the Discrete Fourier
               Transform (DFT) of it's time domain function.</p>
               <p>Any signal is comprised of a number of sinusiodal signals
               with varied amplitude, frequency, and phase. The spectrum displays
               the distribution of amplitudes of each frequncy component within
               a specified portion of the signal.</p>")
        
    }
})

#### Overlapping Help ####
output$OvlpHelpInfo <- renderText({
    times <- input$ovlphelp
    if (times %% 2 == 1) {
        return("<p><b>Overlapping Help</b></p>
               <p>The time and frequency resolution of a spectrogram are inversely 
               related and dependent on window length (the number of samples 
               used for the DFT). If the window length is small there will 
               be poor frequency resolution and good time resolution. However, 
               if the window length is large, although the frequency resolution 
               improves, the time resolution becomes poor.</p>
               <p>One way to manage this and to increase the time resolution without 
               reducing the frequency resolution is to apply an overlap of 
               succesive windows. By overlapping windows and summing the amplitudes
               the tappering of the amplitude at the ends of the windowed signal 
               will be cancelled out by the addition of the next successive 
               window. This result is a better approximation of the orginal 
               signal without the negative impacts of spectral leakage.</p>
               <p>As can be seen below, three successive Hanning windows with a 
               50% overlap can be summed together in order to create a more 
               appropriate representation of the input signal. Unfortunately, 
               while useful, the overlap solution also increases the number of
               DFTs to compute by a factor of <b>100/(100-overlap)</b>. This can 
               take a lot of computing power.</p>
               <p><img src='OverlappingHelpPic.jpeg' width= 70% height=70%></p>")
        
    }
})

#### Zero Padding Help ####
output$ZpHelpInfo <- renderText({
    times <- input$zphelp
    if (times %% 2 == 1) {
        return("<p><b>Zero Padding Help</b></p>
               <p>The time and frequency resolution of a spectrogram are inversely 
               related and dependent on window length (the number of samples 
               used for the DFT). If the window length is small there will 
               be poor frequency resolution and good time resolution. However, 
               if the window length is large, although the frequency resolution 
               improves, the time resolution becomes poor.</p>
               <p>One way to manage this and to increase the frequency resolution
               without losing time resolution is to extend a signal (artificially)
               by adding zeros to the end of the signal in the time domain. 
               Although this zero-padding process will not change the result of 
               the DFT, it will increase the density of samples producing 
               a smoother looking spectrum when plotted without further interpolation.</p>")
        
    }
})

#### Spectrogram ####
output$spectro <- renderPlot({
    
    inFile <- filevalues$file1$datapath
    ### Read .wav file in ###
    if (!is.null(inFile)) {
        
        wav <- readWave(inFile)
        sp <- spectro(wav, plot = F)
        
        filetitle <- filevalues$file1$name
        
    } else if (filevalues$file2 != 1) {
        
        ### Assign wav File & Title ###
        wav <- switch(filevalues$file2,
                      "2" = sine1,
                      "3" = sine2,
                      "4" = sine3,
                      "5" = square,
                      "6" = triangle,
                      "7" = whale,
                      "8" = dolphin,
                      "9" = noise)
        sp <- spectro(wav, plot = F)
        filetitle <- switch(filevalues$file2,
                            "2" = "Sine1",
                            "3" = "Sine2",
                            "4" = "Sine3",
                            "5" = "Square",
                            "6" = "Triangle",
                            "7" = "Whale",
                            "8" = "Dolphin",
                            "9" = "Noise")
    }
    
    if (exists("wav")) {
        ### Determine Window Function ###
#        if (!is.null(input$windowcheck)) {
#            if (input$windowcheck) {
                if (!is.null(input$window)) {
                    window_choice <- switch(input$window,
                                            "1" = "rectangle",
                                            "2" = "bartlett",
                                            "3" = "hamming",
                                            "4" = "hanning",
                                            "5" = "blackman",
                                            "6" = "flattop")
                } else {
                    window_choice <- "hanning"
                }
           # } else {
             #   window_choice <- "hanning"
            #}
        #} else {
           # window_choice <- "hanning"
        #}
        
        ### Determine Zero Padding ###
       # if (!is.null(input$zpcheck)) {
       #     if (input$zpcheck) {
                if (!is.null(input$zp)) {
                    zp_choice <- as.numeric(input$zp)
                } else {
                    zp_choice <- 0
                }
           # } else {
             #   zp_choice <- 0
           # }
       # } else {
           # zp_choice <- 0
        #}
        
        
        ### Determine Overlapping ###
#        if (!is.null(input$ovlpcheck)) {
#            if (input$ovlpcheck) {
                if (!is.null(input$ovlp)) {
                    ovlp_choice <- as.numeric(input$ovlp)
                } else {
                    ovlp_choice <- 0
                }
            #} else {
            #    ovlp_choice <- 0
           # }
        #} else {
           # ovlp_choice <- 0
       # }
        
        ### Determine Spectrum Duration ###
       # if (!is.null(input$spectrumcheck)) {
        #    if (input$spectrumcheck) {
                if (!is.null(input$specmin)) {
                    specmin_choice <- input$specmin
                    specmax_choice <- input$specmax
                } else {
                    specmin_choice <- 0
                    specmax_choice <- length(wav@left)/wav@samp.rate
                }
           # } else {
            #    specmin_choice <- 0
            #    specmax_choice <- length(wav@left)/wav@samp.rate
           # }
      #  } else {
           # specmin_choice <- 0
           # specmax_choice <- length(wav@left)/wav@samp.rate
       # }
        
        ### Determine Sampling Rate ###
      #  if (!is.null(input$sampcheck)) {
      #      if (input$sampcheck) {
                if (!is.null(input$samprate)) {
                    samprate_choice <- as.numeric(input$samprate)
                    print(samprate_choice)
                } else {
                    samprate_choice <- wav@samp.rate
                }
      #      } else {
      #          samprate_choice <- wav@samp.rate
      #      }
     #   } else {
      #      samprate_choice <- wav@samp.rate
      #  }
        
        ### Determine Time Limits ###

        # if (!is.null(input$mintime) & !is.null(input$maxtime)) {
        #     mintime_choice <- input$mintime
        #     maxtime_choice <- input$maxtime
        # } else {
        #     mintime_choice <- 0
        #     maxtime_choice <- round(length(wav@left)/wav@samp.rate,3)
        # }
        # 
        # ### Determine Freq Limits ###
        # if (!is.null(input$minfreq) & !is.null(input$maxfreq)) {
        #     minfreq_choice <- input$minfreq
        #     maxfreq_choice <- input$maxfreq
        # } else {
          minfreq_choice <- 0
          maxfreq_pos <- which(diff(rev(apply(sp$amp, 1, max) <= -29)) < 0)[1]
          if (length(maxfreq_pos) == 0) {
              maxfreq_choice <- rev(sp$freq)[1]

          } else {
              maxfreq_choice <- round(min(rev(sp$freq)[maxfreq_pos]+.5, max(sp$freq),na.rm = TRUE),3)

          }
          
      
        #     
        # }
        # 
        ### Plot Spectrogram ###
        myinput <- inputw(wave = wav, f = samprate_choice)
        wave <- myinput$w
        f <- myinput$f
        #wave <- cutw(wave, f = f, from = 0, to =round(length(wav@left)/wav@samp.rate,2))  #, from = mintime_choice, to = maxtime_choice)
        wl <- 512
        ovlp <- ovlp_choice
        wn <- window_choice
        zp <- zp_choice
        fftw <- FALSE
        norm <- TRUE
        complex <- FALSE
        correction <- "none"
        palette <- spectro.colors
        scalefontlab <- 1
        scalelab <- "Amplitude\n(dB)"
        scalecexlab <- 0.85
        collab <- "black"
        colaxis <- "black"
        rm(myinput)
        
        #layout(matrix(c(1,0,2,3,4,0),ncol = 1, byrow = T),widths=c(3,.8), heights = c(.65, 4, 1.5))
        
        ## Amplitude Gradient Legend ##
        n <- nrow(wave)
        step <- seq(1, n + 1 - wl, wl - (ovlp * wl/100))
        z <- stdft(wave = wave, f = samprate_choice, wl = wl, zp = zp, step = step, 
                   wn = wn, fftw = fftw, scale = norm, complex = complex, 
                   correction = correction)
        fl1 <- minfreq_choice * nrow(z) * 2000/f
        fl2 <- maxfreq_choice * nrow(z) * 2000/f
        z <- z[(fl1:fl2) + 1, ]
        z <- 20 * log10(z)
        maxz <- round(max(z, na.rm = TRUE))
        collevels <- seq(maxz - 30, maxz, by = 1)
        par(mar=c(2,4,4,0.5))
        dBscale(collevels = collevels, palette = palette, 
                fontlab = scalefontlab, cexlab = scalecexlab, 
                collab = collab, textlab = scalelab, colaxis = colaxis, side = 1)
        
        # ## Spectrogram ##
        # par(mar=c(4,4,2,0.5))
        # spectro(wav,
        #         tlab = "",
        #         # xaxt = "n",
        #         f = samprate_choice,
        #         tlim = c(mintime_choice, maxtime_choice),
        #         flim = c(minfreq_choice, maxfreq_choice),
        #         main = paste("Spectrogram of", filetitle),
        #         font.main = 1,
        #         cex.main= 1.7,
        #         cex.lab=1.3,
        #         wn = window_choice,
        #         zp = zp_choice,
        #         ovlp = ovlp_choice,
        #         scale = FALSE)
        # abline(v = specmin_choice, col = "red", lty = 2, lwd=3)
        # abline(v = specmax_choice, col = "red", lty = 2, lwd=3)
        # 
        # ## Spectrum ##
        # par(mar=c(4,1,2,0.5))
        # specvals <- spec(wav,
        #                  f = round(samprate_choice),
        #                  col = "red",
        #                  lwd=3,
        #                  plot = 2,
        #                  wn = window_choice,
        #                  flab = "", yaxt = "n",
        #                  from = specmin_choice,
        #                  to = specmax_choice,
        #                  flim = c(minfreq_choice, maxfreq_choice),
        #                  dB = "max0",
        #                  xaxt = "n",
        #                  main = "Spectrum",
        #                  alab = "Amplitude (dB)",
        #                  cex.axis = 1.5
        # )
        # spectcks <- seq(from = round(min(specvals[,2])), to = 0, by = 5)
        # axis(1, at = spectcks, labels = spectcks, tck = -.025, pos = minfreq_choice)
        # axis(side = 1, at = c(0), tck = -.025)
        # #abline(h = input$minfreq, col = "blue", lty = 2, lwd=3)
        # # text(median(spectcks), -.05, "Amplitude (dB)", cex = 1.5)
        # 
        # ## Oscillogram ##
        # par(mar=c(4,4,2,0.5))
        # oscillo(wav, 
        #         f = samprate_choice,
        #         from = input$mintime,
        #         to = input$maxtime,
        #         cexlab = 0.87)
        # abline(v = specmin_choice, col = "red", lty = 2, lwd=3)
        # abline(v = specmax_choice, col = "red", lty = 2, lwd=3)
        
        
        output$plotly <- renderPlotly({
            
            ##Spectrogram
            minf <- minfreq_choice
            maxf <- maxfreq_choice
            maxt <- round(length(wav@left)/wav@samp.rate,2)
            #mint <- input$mintime
            #maxt <- input$maxtime
            
            

            p1 <- ggspectro(wave, f = samprate_choice, ovlp = ovlp_choice, wn = window_choice, zp = zp_choice)
            
            if (filetitle == "Sine1" | filetitle == "Sine2" | filetitle == "Sine3" | filetitle == "Square" | filetitle == "Triangle" ){
              p1 <- p1 + geom_tile(aes(fill = amplitude))
            }
            if (filetitle == "Dolphin" ){
              p1 <- p1  + stat_contour(geom="polygon", aes(fill=..level..), bins=15)
            }
            else{
              p1 <- p1  + stat_contour(geom="polygon", aes(fill=..level..), bins=100)
            }
            p1 <- p1  + scale_fill_gradientn(name="Amplitude\n(dB)\n", 
                limits=c(-28,0), na.value="transparent",
                colours = spectro.colors(30)) + ylim(minf, maxf) + 
                xlim(0, maxt) +
                theme(legend.position="none",
                      panel.grid.major.y = element_line(size=.1, color="grey", linetype=3),
                      axis.text.x = element_text(color = "grey20", size = 6, angle = 90, hjust = .5, vjust = .5, face = "plain"),
                      axis.text.y = element_text(color = "grey20", size = 6, angle = 0, hjust = 1, vjust = 0, face = "plain"),  
                      axis.title.x = element_text(color = "grey20", size = 8, angle = 0, hjust = .5, vjust = 0, face = "plain"),
                      axis.title.y = element_text(color = "grey20", size = 8, angle = 90, hjust = .5, vjust = .5, face = "plain"),
                      panel.border = element_rect(colour = "black", fill=NA, size=0.5),
                      plot.title = element_text(size=9, face = "plain", hjust = .3)) + 
                #ggtitle(paste("Spectrogram of", filetitle)) + 
                geom_vline(xintercept=specmin_choice, linetype="dashed", color = "red", size=.3) + 
                geom_vline(xintercept=specmax_choice, linetype="dashed", color = "red", size=.3)
                                                                                                                                
                        
            
        
            # ## Spectrum ##
            specvals <- spec(wave,
                             f = round(samprate_choice),
                             col = "red",
                             lwd=3,
                             plot = FALSE,
                             wn = window_choice,
                             flab = "", yaxt = "n",
                             from = specmin_choice,
                             alim = c(-30,0),
                             to = specmax_choice,
                             flim = c(minfreq_choice, maxfreq_choice),
                             dB = "max0"
            )


            spec <- round(data.frame(specvals),3) %>%
                mutate("Amplitude (dB)"= y,
                       "Frequency (kHz)" = x) %>%
                select(-x,-y) %>% filter(`Frequency (kHz)` < maxfreq_choice) %>%
                filter(`Amplitude (dB)` > -30)

            min_amp <- min(spec$`Amplitude (dB)`)

            spectcks <- seq(from = round(min(specvals[,2])), to = 0, by = 5)
            p2 <- ggplotly(ggplot(data=spec, aes(x=`Frequency (kHz)`, y=`Amplitude (dB)`, group=1))
                           + geom_line(color="red", size=0.5)
                           +  coord_flip() +
                               theme(axis.text.x = element_text(color = "grey20", size = 6, angle = 0, hjust = .5, vjust = .5, face = "plain"),
                                     axis.text.y = element_text(color = "grey20", size = 6, angle = 0, hjust = 1, vjust = 0, face = "plain"),
                                     axis.title.x = element_text(color = "grey20", size = 8, angle = 0, hjust = .5, vjust = 0, face = "plain"),
                                     axis.title.y = element_text(color = "grey20", size = 8, angle = 90, hjust = .5, vjust = .5, face = "plain"),
                                     panel.border = element_rect(colour = "black", fill=NA, size=0.5)), dynamicTicks = TRUE)

            #axis(1, at = spectcks, labels = spectcks, tck = -.025, pos = minfreq_choice)

            os <-oscillo(wave,
                         f = samprate_choice,
                         #from = 0,
                         #to = round(length(wav@left)/wav@samp.rate,2),
                         plot = FALSE)

            n = nrow(os)
            fac = (maxt)/n

            os <- data.frame(os, index = seq(1, length(os))) %>% mutate(Amplitude = os/1000, index = index - 1)%>%
                mutate("Time (s)" = index * fac)

            p3 <- ggplotly(ggplot(data=os, aes(x = `Time (s)`, y = Amplitude)) + geom_line(size=0.2) +
                               geom_hline(yintercept=0, linetype="dashed",
                                          color = "white", size=0.2)
                           + xlim(0,maxt) 
                               + theme(axis.text.x = element_text(color = "grey20", size = 6, angle = 0, hjust = .5, vjust = .5, face = "plain"),
                                     axis.text.y = element_text(color = "grey20", size = 6, angle = 0, hjust = 1, vjust = 0, face = "plain"),
                                     axis.title.x = element_text(color = "grey20", size = 8, angle = 0, hjust = .5, vjust = 0, face = "plain"),
                                     axis.title.y = element_text(color = "grey20", size = 8, angle = 90, hjust = .5, vjust = .5, face = "plain"),
                                     panel.border = element_rect(colour = "black", fill=NA, size=0.5)) +
                             geom_vline(xintercept=specmin_choice, linetype="dashed", color = "red", size=.3) +
                             geom_vline(xintercept=specmax_choice, linetype="dashed", color = "red", size=.3), dynamicTicks = TRUE)
            ax <- list(
                showline = TRUE,
                linewidth = 0.5,
                linecolor = toRGB("black")
            )

            f <- list(size = 12)

            subplot(ggplotly(p1, tooltip="none", dynamicTicks = TRUE), p2, p3,
                    nrows = 2,
                    widths = c(0.8, 0.2), margin = 0.01,
                    heights= c(0.8, 0.2),
                    shareY = TRUE, shareX = TRUE) %>%
                layout(xaxis = ax, yaxis = ax,  plot_bgcolor='white', annotations = list(
                list(x = 0.25 , y = 1.04, text = paste("Spectrogram of", filetitle), showarrow = F, xref='paper', yref='paper', font = f),
                list(x = 0.97 , y = 1.04, text = "Spectrum", showarrow = F, xref='paper', yref='paper', font = f))
            )


        })
        
        #### Segmentation ####
        output$segment <- renderPlot({
            
            # inFile <- filevalues$file1$datapath
            # ### Read .wav file in ###
            # if (!is.null(inFile)) {
            #     
            #     wav <- readWave(inFile)
            #     sp <- spectro(wav, plot = F)
            #     
            #     filetitle <- filevalues$file1$name
            #     
            # } else if (filevalues$file2 != 1) {
            #     
            #     ### Assign wav File & Title ###
            #     wav <- switch(filevalues$file2,
            #                   "2" = sine1,
            #                   "3" = sine2,
            #                   "4" = sine3,
            #                   "5" = square,
            #                   "6" = triangle,
            #                   "7" = whale,
            #                   "8" = dolphin,
            #                   "9" = noise)
            #     sp <- spectro(wav, plot = F)
            #     filetitle <- switch(filevalues$file2,
            #                         "2" = "Sine1",
            #                         "3" = "Sine2",
            #                         "4" = "Sine3",
            #                         "5" = "Square",
            #                         "6" = "Triangle",
            #                         "7" = "Whale",
            #                         "8" = "Dolphin",
            #                         "9" = "Noise")
            # }
            # 
            # if (exists("wav")) {
            #     ### Determine Sampling Rate ###
            #     if (!is.null(input$samprate)) {
            #         samprate_choice <- as.numeric(input$samprate)
            #     } else {
            #         samprate_choice <- wav@samp.rate
            #         
                  ### Determine Shortest Segment ###
                if (!is.null(input$shortestSyl)) {
                    shortestSyl_choice <- input$shortestSyl

                } else {
                    shortestSyl_choice <- 40
                }

                ### Determine Threshold Limits ###
                if (!is.null(input$threshold)) {
                    threshold_choice <- input$threshold
                } else {
                    threshold_choice <- 0.9
                }

            ## Plot Segmentation ###
                savewav(wav, filename = "tempFile.wav")
                par(mar = c(4,4,0,0.5))
                segment <- segment("tempFile.wav", 
                                   plot = TRUE, 
                                   main = NA,
                                   shortestSyl = shortestSyl_choice,
                                   sylThres = threshold_choice,
                                   xlab = "Time (ms)",
                                   font.lab = 1,
                                   cex.lab=1.1)
                
                segments <- segment$syllables
                segments <- round(segments %>% 
                                      rowwise() %>% 
                                      mutate("Min Frequency" = freq_range("tempFile.wav", start/1000, end/1000)[1,1],
                                             "Max Frequency" = freq_range("tempFile.wav", start/1000, end/1000)[2,1],
                                             "Peak Frequency" = freq_range("tempFile.wav", start/1000, end/1000)[3,1])%>%
                                      rename(Segment = syllable, "Start (ms)" = start,
                                             "End (ms)" = end, "Segment Duration (ms)" = sylLen) %>%
                                      select(-pauseLen),3)
                
                
                output$segments <- DT::renderDataTable({
                    segments},
                    rownames = FALSE,
                    options = list(pageLength = 5))
                
                output$spectro_seg <- renderPlot({
                    ##Spectrogram
                    # minf <- input$minfreq
                    # maxf <- input$maxfreq
                    # mint <- input$mintime
                    # maxt <- input$maxtime
                    input_cols <- input$segments_rows_selected
                    par(mar=c(0.5,4,4,0.5))
                    
                    minf <- minfreq_choice
                    maxf <- maxfreq_choice
                    maxt <- round(length(wav@left)/wav@samp.rate,2)
                    
                    spectro(wave,
                            tlab = "",
                            xaxt='n',
                            font.lab = 1,
                            f = samprate_choice,
                            #tlim = c(0, maxt),
                            flim = c(minf, maxf),
                            #tlim = c(input$mintime, input$maxtime),
                            #flim = c(minf, maxf),
                            main = paste("Spectrogram and Segmentation of", filetitle),
                            font.main = 1,
                            cex.main= 1.5,
                            cex.lab=1,
                            wn = window_choice,
                            zp = zp_choice,
                            ovlp = ovlp_choice,
                            scale = FALSE)
            
                    
                    plot.title = title(main = "", ylab = "Frequency (kHz)")
                
                    segment_box(segments, input_cols)
                })
                
                # Downloadable csv of selected dataset
                output$downloadSegments <- downloadHandler(
                    filename <- function() {
                        paste(filetitle, "-segments", ".csv", sep="")
                    },
                    content <- function(file) {
                        write.csv(segments, file)
                    }
                )
                
            
        })
    
        
        
    }
})

output$audioplay <- renderUI({
  inFile <- filevalues$file1$datapath
  ### Read .wav file in ###
  if (!is.null(inFile)) {

    wav <- readWave(inFile)
    sp <- spectro(wav, plot = F)

    filetitle <- filevalues$file1$name

  } else if (filevalues$file2 != 1) {

    ### Assign wav File & Title ###
    wav <- switch(filevalues$file2,
                  "2" = sine1,
                  "3" = sine2,
                  "4" = sine3,
                  "5" = square,
                  "6" = triangle,
                  "7" = whale,
                  "8" = dolphin,
                  "9" = noise)
    sp <- spectro(wav, plot = F)
    filetitle <- switch(filevalues$file2,
                        "2" = "Sine1",
                        "3" = "Sine2",
                        "4" = "Sine3",
                        "5" = "Square",
                        "6" = "Triangle",
                        "7" = "Whale",
                        "8" = "Dolphin",
                        "9" = "Noise")
  }

  if (exists("wav")) {
    ### Determine Window Function ###
    if (!is.null(input$windowcheck)) {
      if (input$windowcheck) {
        if (!is.null(input$window)) {
          window_choice <- switch(input$window,
                                  "1" = "rectangle",
                                  "2" = "bartlett",
                                  "3" = "hamming",
                                  "4" = "hanning",
                                  "5" = "blackman",
                                  "6" = "flattop")
        } else {
          window_choice <- "hanning"
        }
      } else {
        window_choice <- "hanning"
      }
    } else {
      window_choice <- "hanning"
    }

    ### Determine Zero Padding ###
    if (!is.null(input$zpcheck)) {
      if (input$zpcheck) {
        if (!is.null(input$zp)) {
          zp_choice <- as.numeric(input$zp)
        } else {
          zp_choice <- 0
        }
      } else {
        zp_choice <- 0
      }
    } else {
      zp_choice <- 0
    }


    ### Determine Overlapping ###
    #if (!is.null(input$ovlpcheck)) {
     # if (input$ovlpcheck) {
        if (!is.null(input$ovlp)) {
          ovlp_choice <- as.numeric(input$ovlp)
        } else {
          ovlp_choice <- 0
        }
     # } else {
      #  ovlp_choice <- 0
     # }
    #} else {
    #  ovlp_choice <- 0
  #  }

    ### Determine Spectrum Duration ###
   # if (!is.null(input$spectrumcheck)) {
    #  if (input$spectrumcheck) {
        if (!is.null(input$specmin)) {
          specmin_choice <- input$specmin
          specmax_choice <- input$specmax
        } else {
          specmin_choice <- 0
          specmax_choice <- length(wav@left)/wav@samp.rate
        }
     # } else {
     #   specmin_choice <- 0
      #  specmax_choice <- length(wav@left)/wav@samp.rate
    #  }
   # } else {
   #   specmin_choice <- 0
   #   specmax_choice <- length(wav@left)/wav@samp.rate
  #  }

    ### Determine Sampling Rate ###
  #  if (!is.null(input$sampcheck)) {
  #    if (input$sampcheck) {
        if (!is.null(input$samprate)) {
          samprate_choice <- as.numeric(input$samprate)
        } else {
          samprate_choice <- wav@samp.rate
        }
   #   } else {
   #     samprate_choice <- wav@samp.rate
  #    }
  #  } else {
  #    samprate_choice <- wav@samp.rate
  #  }

    ### Determine Time Limits ###

    # if (!is.null(input$mintime) & !is.null(input$maxtime)) {
    #   mintime_choice <- input$mintime
    #   maxtime_choice <- input$maxtime
    # } else {
    #   mintime_choice <- 0
    #   maxtime_choice <- round(length(wav@left)/wav@samp.rate,3)
    # }

    ### Determine Freq Limits ###
    # if (!is.null(input$minfreq) & !is.null(input$maxfreq)) {
    #   minfreq_choice <- input$minfreq
    #   maxfreq_choice <- input$maxfreq
    # } else {
    #   minfreq_choice <- 0
    #   maxfreq_pos <- which(diff(rev(apply(sp$amp, 1, max) <= -29)) < 0)[1]
    #   if (length(maxfreq_pos) == 0) {
    #     maxfreq_choice <- rev(sp$freq)[1]
    # 
    #   } else {
    #     maxfreq_choice <- round(min(rev(sp$freq)[maxfreq_pos]+.5, max(sp$freq),na.rm = TRUE),3)
    # 
    #   }
    # 
    # }

    savewav(wav, filename = "www/tempFile.wav")
    cut_wav <- readWave("www/tempFile.wav") #  , from = mintime_choice, to = maxtime_choice, units = "seconds")
    file.remove("www/tempFile.wav")
    savewav(cut_wav, filename = "www/tempFile.wav")
    #h3("Audio Playback",
       #br(),
       #tags$small(paste("Start:", 0)),
       #tags$small(paste("End:", round(length(wav@left)/wav@samp.rate,3))),
    tags$audio(src = "tempFile.wav",
                  type = "audio/wav",
                  controls = "controls",
              style="width: 80%;")


  
  
  ################
    # inFile <- filevalues$file1
    # if (!is.null(inFile$datapath) & input$file2 == 1) {
    #     filetitle <- filevalues$file1$name
    #     wav <- readWave(inFile$datapath)
    # 
    #     ### Determine Time Limits ###
    #     if (!is.null(input$mintime) & !is.null(input$maxtime)) {
    #         mintime_choice <- input$mintime
    #         maxtime_choice <- input$maxtime
    #     } else {
    #         mintime_choice <- 0
    #         maxtime_choice <- length(wav@left)/wav@samp.rate
    #     }
    #     savewav(wav, filename = "www/tempFile.wav")
    #     cut_wav <- readWave("www/tempFile.wav", from = mintime_choice, to = maxtime_choice, units = "seconds")
    #     file.remove("www/tempFile.wav")
    #     savewav(cut_wav, filename = "www/tempFile.wav")
    #     h3("Audio Playback",
    #        br(),
    #        tags$small(paste("Start:", round(mintime_choice,3))),
    #        tags$small(paste("End:", round(maxtime_choice,3))),
    #        tags$audio(src = "tempFile.wav",
    #                   type = "audio/wav",
    #                   controls = "controls"))
    # 
    # } else if (input$file2 != 1) {
    #     wav <- switch(input$file2,
    #                       "2" = sine1,
    #                       "3" = sine2,
    #                       "4" = sine3,
    #                       "5" = square,
    #                       "6" = triangle,
    #                       "7" = whale,
    #                       "8" = dolphin,
    #                       "9" = noise)
    # 
    #     ### Determine Time Limits ###
    #     if (!is.null(input$mintime) & !is.null(input$maxtime)) {
    #         mintime_choice <- input$mintime
    #         maxtime_choice <- input$maxtime
    #     } else {
    #         mintime_choice <- 0
    #         maxtime_choice <- length(wav@left)/wav@samp.rate
    #     }
    #     savewav(wav, filename = "www/tempFile.wav")
    #     cut_wav <- readWave("www/tempFile.wav", from = mintime_choice, to = maxtime_choice, units = "seconds")
    #     file.remove("www/tempFile.wav")
    #     savewav(cut_wav, filename = "www/tempFile.wav")
    #     h3("Audio Playback",
    #        br(),
    #        tags$small(paste("Start:", round(mintime_choice,3))),
    #        tags$small(paste("End:", round(maxtime_choice,3))),
    #        div(style="display:inline-block",tags$audio(src = "tempFile.wav",
    #                                                    type = "audio/wav",
    #                                                    controls = "controls"), style="float:right")
    #        )
    }
    })
    

output$shortestSyl <- renderUI({
    
    inFile <- filevalues$file1$datapath
    ### Read .wav file in ###
    if (!is.null(inFile)) {
        wav <- readWave(inFile)
        
        numericInput("shortestSyl", label = "Min Length of Segments (ms):",
                     value = 40, min = 0, max = length(wav@left)/wav@samp.rate*1000)
    } else {
        wav <- switch(filevalues$file2,
                      "2" = sine1,
                      "3" = sine2,
                      "4" = sine3,
                      "5" = square,
                      "6" = triangle,
                      "7" = whale,
                      "8" = dolphin,
                      "9" = noise)
        
        numericInput("shortestSyl", label = "Min Length of Segments (ms):",
                     value = 40, min = 0, max = length(wav@left)/wav@samp.rate*1000)
    }
})


output$threshold <- renderUI({
        inFile <- filevalues$file1$datapath
        ### Read .wav file in ###
        if (!is.null(inFile)) {
            wav <- readWave(inFile)
            
            numericInput("threshold", label = "Amplitude Threshold (proportion):",
                         value = 0.9, max = 1, step = 0.1)
            
        } else {
            wav <- switch(filevalues$file2,
                          "2" = sine1,
                          "3" = sine2,
                          "4" = sine3,
                          "5" = square,
                          "6" = triangle,
                          "7" = whale,
                          "8" = dolphin,
                          "9" = noise)
            
            numericInput("threshold", label = "Amplitude Threshold: (proportion)",
                         value = 0.9,max = 1, step = 0.1)
        }
})


output$segmentsHelpInfo <- renderText({
    times <- input$segmentsHelp
    if (times %% 2 == 1) {
        return("<p>Segments are defined as continuous 
               fragments with amplitude above a given threshold, 
               seperated by silence (or background noise).</p>")
        
    }
})


output$thresHelpInfo <- renderText({
    times <- input$thresHelp
    if (times %% 2 == 1) {
        return("<p>Amplitude threshold for segment detection 
               as a proportion of global mean amplitude of smoothed envelope.</p>")
        
    }
})

output$minSegInfo <- renderText({
  times <- input$segHelp
  if (times %% 2 == 1) {
    return("<p>Minimum acceptable length of syllables, which is a continuous segments with amplitude above threshold,
           measured in ms.</p>")
    
  }
})

#### Segmentation Help ####
# output$SegHelpInfo <- renderText({
#     times <- input$seghelp
#     if (times %% 2 == 1) {
#         return("<p><b>Segmentation Help</b></p>
#                 <p>The following tab divides a sound file into separate segments 
#                 - continuous acoustic fragments separated by what we consider to be “silence”. 
#                 This “silence” can often contain background noise. The function finds bursts of acoustic energy - local maxima in amplitude envelope 
#                 that are high enough both in absolute terms (relative to the global maximum) and 
#                 with respect to the surrounding region (relative to local mimima)
#                 which are denoted by the red stars. 
#                 The function segment() used here, looks for both segments and bursts. Segments are found first,
#                 and then the median length of a segment becomes the expected peak interval, 
#                 guiding burst detection. The method operates with amplitude 
#                 envelopes - smoothed contours of sound intensity. 
#                 For more information run vignette('acoustic_analysis', package = 'soundgen') in 
#                 your R-Studio console. </p>")
#     
#     }
# })
#### UI & Input Elements ####
output$SegHelpInfo <- renderUI({
  
    wavtext <- paste("The following tab divides a sound file into separate segments - continuous acoustic fragments separated by what we consider to be “silence”.",
                     "This “silence” can often contain background noise. The function finds bursts of acoustic energy - local maxima in amplitude envelope",
                     "that are high enough both in absolute terms (relative to the global maximum) and",
                     "with respect to the surrounding region (relative to local mimima)",
                     "which are denoted by the red stars.",
                     "The function segment() used here, looks for both segments and bursts. Segments are found first,",
                     "and then the median length of a segment becomes the expected peak interval,",
                     "guiding burst detection. The method operates with amplitude",
                     "envelopes - smoothed contours of sound intensity.",
                     "For more information run vignette(“acoustic_analysis”, package = “soundgen”) in your R-Studio console.")
    

    
    tags$span(
      popify(bsButton("seghelp", "Hover Here for Segmentation Help"),
             "",
              wavtext))
})

#### About Page Info ####
output$aboutInfo <- renderText({
    return("<p><h2>Welcome to the Bioacoustics Shiny App!</h2></p>
           <p>If you wish to run this app from your own machine you 
            may clone this repository: <a href='https://github.com/hglanz/AcousticsApp-Summer2019'>AcousticApp-Summer2019</a></p>
           <p>To get started, turn to the <b>Exploration Tab</b> where you can upload a .wav file
            or pick one of the preloaded files. Note that the more seconds the recording is, 
            the longer the app will take to load. Recordings under 5 seconds are optimal. 
            At any time, you may reset your file choice and start over using the “Reset Inputs” 
            button. After choosing a file to use, the hover button will provide some 
            information about the .wav file to get you started. The information 
            presented can be changed throughout the menu options in the left panel.</p>
           <p>The audio playback will provide an audio visualization of the file you 
            have uploaded. This feature may not work on browsers other than Google Chrome. 
            If the minimum or maximum time changes, the recording in the audio playback will 
            change accordingly. 
            Similarly, everything that is under General Information can be changed 
            and will effect the graphs, audio, and data table in the Exploration and the 
            Segmentation tabs. Some basic information that can be changed either by typing a 
            number or by clicking the up and down arrows are minimum and maximum time 
            as well as minimum and maximum frequency. More advanced options are presented 
            using check boxes such as spectrum duration, sampling rate, windows functions, 
            overlapping, and zero padding. To change one of those options click the check 
            box and a menu will appear. To get more details about each advanced option, 
            click on the question mark button.</p>
           <p>The main panel to the right includes a Spectrogram, a Spectrum, and an 
            Oscillogram of the provided .wav file. Spectrograms show the acoustic spectrum 
            of each sound so that information about the tone quality, pitch, timbre and 
            general structure may be deduced. The x-axis represent time, and the y-axis represent 
            frequency. A third dimension indicating the amplitude of a particular frequency 
            at a particular time is represented by the intensity or color of each point in 
            the image. The legend describing the different levels of amplitude intensity 
            is provided above the Spectrogram. 
            Below the Spectrogram, you will find the Oscillogram which measures time
            on the x-axis and amplitude on the y-axis. To the right of both graphs you 
            will find the Spectrum. The Spectrum represents a sound in terms of the 
            amount of vibration at each individual frequency. It is presented as a 
            graph of amplitude as a function of frequency.
            A new feature of this app includes the interactive aspect of all the graphs. 
            Feel free to zoom in to any of the three main graphs and see how it changes the other two. 
           You can always reset your grpah to what it was in the start using the top menu of the graphs.</p>
           <p>The <b>Segmentation Tab</b> divides a sound file into 
            separate segments - continuous acoustic fragments separated 
            by silence or background noise. The first graph to the right 
            is the same Spectrogram as on the Exploration page but it includes red boxes 
            denoting segments. The segmentation is based on the graph below, which is a graph
            of time (in milliseconds) as a function of amplitude threshold. 
            The threshold as well as the minimum length of a segment can be adjusted 
            in the left panel. More information is provided about the threshold using 
            the question mark button. The Segmentation Help button will explain more about 
            how the segmentation is calculated (this will show up at the bottom of the page).
            Below the two graphs is a data frame that gives descriptive statistics about each 
            segment. You may download that data table using the download button below. 
            Clicking on a row of the data frame will highlight the segment that the row
            refers to in the spectrogram. </p>
           <p>This app was created by Michal Golovanevsky, 
           advised by Dr. Hunter Glanz and Maddie Schroth-Glanz from the Statistics deparment at Cal Poly. </p>" )
        
    
})

# output$my_ws <- renderWavesurfer({
#     inFile <- filevalues$file1$datapath
#     ### Read .wav file in ###
#     if (!is.null(inFile)) {
#         
#         wav <- readWave(inFile)
#         sp <- spectro(wav, plot = F)
#         
#         filetitle <- filevalues$file1$name
#         
#     } else if (filevalues$file2 != 1) {
#         
#         ### Assign wav File & Title ###
#         wav <- switch(filevalues$file2,
#                       "2" = sine1,
#                       "3" = sine2,
#                       "4" = sine3,
#                       "5" = square,
#                       "6" = triangle,
#                       "7" = whale,
#                       "8" = dolphin,
#                       "9" = noise)
#         sp <- spectro(wav, plot = F)
#         filetitle <- switch(filevalues$file2,
#                             "2" = "Sine1",
#                             "3" = "Sine2",
#                             "4" = "Sine3",
#                             "5" = "Square",
#                             "6" = "Triangle",
#                             "7" = "Whale",
#                             "8" = "Dolphin",
#                             "9" = "Noise")
#     }
#     
#     if (exists("wav")) {
#         
#         savewav(wav, filename = "tempFile.wav")
#         shiny::addResourcePath("wav", getwd())
#     
#         wavesurfer(paste0("wav/","tempFile.wav")) %>%
#             ws_set_wave_color("royalblue") %>%
#             ws_timeline()
#     }
# }) 
# 
# # toggle plugins
# observe({if(input$minimap) ws_minimap("my_ws") else ws_destroy_minimap("my_ws")})
# observe({if(input$spectrogram) ws_spectrogram("my_ws") else ws_destroy_spectrogram("my_ws")})
# observe({if(input$timeline) ws_timeline("my_ws") else ws_destroy_timeline("my_ws")})
# observe({if(input$cursor) ws_cursor("my_ws") else ws_destroy_cursor("my_ws")})
# observeEvent(input$regions, {ws_annotator("my_ws")}) # not toggleable yet
# 
# # controllers
# observeEvent(input$play, {ws_play("my_ws")})
# observeEvent(input$pause, {ws_pause("my_ws")})
# observeEvent(input$mute, {ws_toggle_mute("my_ws")})
# observeEvent(input$skip_forward, {ws_skip_forward("my_ws", 3)})
# observeEvent(input$skip_backward, {ws_skip_backward("my_ws", 3)})
# observeEvent(input$stop, {ws_stop("my_ws")})
# observe({ws_set_volume("my_ws", input$volume/50 )})
    

# delete temporary files created
session$onSessionEnded(function() {
    if (file.exists("www/tempFile.wav")) 
        file.remove("www/tempFile.wav")
    if (file.exists("tempFile.wav")) 
        file.remove("tempFile.wav")
    })
})