---
output:
  pdf_document: default
  html_document: default
---
# FitGrowth
####Shiny app for modelling bacterial growth kinetics.
This app fits growth data to the continuous logistic equation (https://en.wikipedia.org/wiki/Generalised_logistic_function). 
    The best parameters (<code>n0</code>, <code>k</code> and <code>r</code>) are found using the nonlinear least-squares method (<code>nls</code> in <code>R</code>). 
    The app handles one or many samples (tested with 96), as well as
     NA values. You can get an example file <a href=https://www.dropbox.com/sh/zzf7y3ijwkat55e/AABUvp7BAARIdYBqZWgk1E37a?dl=0>here</a>.</p> Instructions: 
    Upload the data as a text file, the first column <u>must</u> be named <b>time</b>, all other columns are treated as
    samples. Adjust the file input settings until the data is read into the app. After that, take a look at the other tabs. 
    Note that the parameters of the logistic model are re-calculated when the time interval is changed with the slider
