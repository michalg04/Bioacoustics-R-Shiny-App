# Bioacoustics Shiny App

## Summer Research, 2019

#### _Students:_ Michal Golovanevsky

#### _Faculty advisors:_ Hunter Glanz & Maddie Schroth-Glanz

### Objective

The objective of this summer research is to develop, expand, and polish an R Shiny app for doing exploratory bioacoustics.

### Deliverables

**A GitHub repository which contains the following:**

1.  An R project which contains all files relevant to the shiny app: [current version](https://mschroth.shinyapps.io/testapp/).

2.  A log of hours spent on the summer research by the student, which includes date, hours, and activity summary.

3.  A presentation to the Statistics Department.

4.  A presentation at the CSM annual research conference.

5.  A manuscript to submit to a journal to be determined.

### Specific Aims

**1.  Utilize GitHub to collaborate on project materials and updates.**

  * Karl Broman's [github tutorial](http://kbroman.org/github_tutorial/)

  * Jenny Bryan's [Happy git with R](http://happygitwithr.com/).
  
  * Also check out [using version control with RStudio](https://support.rstudio.com/hc/en-us/articles/200532077-Version-Control-with-Git-and-SVN) and [this video on Git and RStudio](https://www.rstudio.com/resources/webinars/rstudio-essentials-webinar-series-managing-part-2/).


**2.  Adhere to good programming practices.**
  
  * Write all R code according to [Hadley Wickam's Style Guide](http://adv-r.had.co.nz/Style.html).
  
  * Use the [tidyverse style guide](http://style.tidyverse.org/) for an additional reference.
  
  * Use Hadley Wickham's [R for Data Science](http://r4ds.had.co.nz/) book as a reference (Ch19 also discusses functions).
  
  
  **3.  Create an R Shiny app that performs exploratory bioacoustics.**  

  *  RStudio's introduction and guide to [Shiny](https://shiny.rstudio.com/).
  
  *  Download [RavenLite software](http://ravensoundsoftware.com/software/raven-lite/) to familiarize yourself with working with sound data.
  
  *  Check out [RavenLite Manual](http://www.birds.cornell.edu/brp/raven/Raven14UsersManual.pdf) for information about sound data (Appendix B).
  
  *  Check out Bioacoustics App To-Do items below for initial objectives.
  
  *  Add "tab" to existing app to perform segmentation on the recording, and produce statistics for the segments.
  
  
   **4.  Provide documentation for the R Shiny app.**
  
  *  Write a [vignette](http://r-pkgs.had.co.nz/vignettes.html) to accompany the package.
  
  *  Consider using [pkgdown](http://pkgdown.r-lib.org/index.html) to create a website. 
  
  
  **5. Understand/Learn about bioacoustics.**
  
  *  What is a .wav file?
  
  *  What is a spectrogram?
  
  *  What is an oscillogram?
  
  *  What is a spectrum?
  
  *  What does segmentation mean with respect to a recording (.wav file)?
  
  *  Upload/Import .wav file into RavenLite and play around with it via Raven tutorial videos (through google)
  
  **6. Bioacoustics App To-Do Items and Possible Bugs?**
  
  * Additions to the app (most important to least important):
  
    i. Make the 3 graphs (spectrogram, oscillogram, spectrum) interactive via playback button (done) and zoom feature 
  
    ii. For the spectrum
    
        a. 2 red lines should be thicker/bolder (done)
        
        b. Could be interactive via dragging of red lines by user
        
        c. max dB should be 0 always (done)
        
        d. Horizontal black lines on spectrogram should be on spectrum too
        
        e. Option to specify start time and duration instead of start time and end time?
        
    iii. Button to describe spectrogram with a visualization (TO-DO for Maddie)
    
    iv. Reorganize/condense inputs on left. Could some min/max pairs be on the same line? (done)
    
    v. Sections in input panel: General Information, Extra Options (done)
    
    vi. Loading image while graphs are being created? Not sure if this is possible.
    
  * Things to fix on the app (most important to least important):
  
    i. Spectrum:
    
        a. Why doesn't it show amplitude for all frequences in dolphin file? (done)
        
        b. Choosing a different window function should change both the spectrum and the spectrogram, 
           but it currently doesn't change the spectrum (done)
        
    ii. Help Options:
    
        a. All pictures fit on screen without horizontal scroll bar (done)
        
        b. Make pictures smaller and consistent across help options 
           (use size of right graph in windowed signal as template) (done)
        
        c. f_s in sampling rate should be an actual subscript (done)
        
        d. Some sampling rates don't work for marine related files. Why not?...and fix.
        
        e. Get rid of title "7-series graph" in window function help (use windows.R) (done)
        
        f. Move the 7th graph to right to get rid of overlap with 6 graphs in window 
           function help (use windows.R) (done)
        
        g. Spectrum should have x-axis only go up to 0.4999 (use windows.R) 
           in window function help (done)
        
        h. Need legend in 7th graph (right) in window function help (use windows.R) (done)
        
        i. Make formula in text stand out more in overlapping help (done)
        
        j. Change "loosing" to "losing" in zero padding help (done)
        
    iii. General Fixes:
    
        a. Better error messages
        
        b. Make sure hover button works for everything
        
        c. Noise file needs fixing; currently doesn't work (done)
        
        d. Fonts need to be the same (done)
        
        e. Clicking "Reset Inputs" should reset the whole page/app
  
