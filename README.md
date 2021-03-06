# Bioacoustics R-Shiny App

## Summer 2019 - Fall 2020 Research

#### _Students:_ Michal Golovanevsky

#### _Faculty advisors:_ Hunter Glanz & Maddie Schroth-Glanz


Thank you for checking out this Bioacoustics app!

The link to the app can be found through this link:  https://rstudio.csm.calpoly.edu/connect/#/apps/147/access. 

The username and password can be provided upon request.

If you wish to run this app from your own machine you may clone this repository: https://github.com/michalg04/Bioacoustics-R-Shiny-App/

This is where the app gets updates, so make sure to periodically check there.

Once this app is on your local machine, you may download the most recent version of R-studio and open the server.R file and hit run.
If you are missing some of the packages used for the app, be sure to check all the libraries imported at the top of the server.R page.

Once you open the app, turn to the Exploration Tab where you can upload a .wav file or pick one of the preloaded files.
Note that the more seconds the recording is, the longer the app will take to load. Recordings under 5 seconds are optimal.
At any time, you may reset your file choice and start over using the Reset Inputs button.
After choosing a file to use, the hover button will provide some information about the .wav file to get you started.
The information presented can be changed throughout the menu options in the left panel.
The audio playback will provide an audio visualization of the file you have uploaded.
This feature may not work on browsers other than Google Chrome.

If you wish to change the time range, the frequency range, or the amplitude range, feel free to zoom in and out on the three main graphs. More advanced options are presented on the left hand side such as spectrum duration, sampling rate, windows functions, overlapping, and zero padding.
To change one of those options click the check box and a menu will appear.
To get more details about each advanced option, click on the question mark button.
The main panel to the right includes a Spectrogram, a Spectrum, and an Oscillogram of the provided .wav file.
Spectrograms show the acoustic spectrum of each sound so that information about the tone quality, pitch, timbre and general structure may be deduced.
The x-axis represent time, and the y-axis represent frequency.
A third dimension indicating the amplitude of a particular frequency at a particular time is represented by the intensity or color of each point in the image. The legend describing the different levels of amplitude intensity is provided above the Spectrogram.
Below the Spectrogram, you will find the Oscillogram which measures time on the x-axis and amplitude on the y-axis.
To the right of both graphs you will find the Spectrum. The Spectrum represents a sound in terms of the amount of vibration at each individual frequency.
It is presented as a graph of amplitude as a function of frequency. A new feature of this app includes the interactive aspect of all the graphs. Feel free to zoom in to any of the three main graphs and see how it changes the other two.
You can always reset your graph to what it was in the start using the top menu of the graphs.

The Segmentation Tab divides a sound file into separate segments - continuous acoustic fragments separated by silence or background noise.
The first graph to the right is the same Spectrogram as on the Exploration page but it includes red boxes denoting segments.
The segmentation is based on the graph below, which is a graph of time (in milliseconds) as a function of amplitude threshold.
The threshold as well as the minimum length of a segment can be adjusted in the left panel.
More information is provided about the threshold using the question mark button.
The Segmentation Help button will explain more about how the segmentation is calculated (this will show up at the bottom of the page).
Below the two graphs is a data frame that gives descriptive statistics about each segment.
You may download that data table using the download button below. Clicking on a row of the data frame will highlight the segment that the row refers to in the spectrogram.
