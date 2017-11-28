**FitGrowth**

Shiny app for modelling bacterial growth kinetics.
This version of the app fits growth data using the `drc` package. The app handles one or many samples (tested with 96), as well as NA values. You can get an example file [here](https://www.dropbox.com/sh/zzf7y3ijwkat55e/AABUvp7BAARIdYBqZWgk1E37a?dl=0).

**Instructions:** 
The easiest way to run FitGrowth is if you have [RStudio](http://rstudio.org), you just have to paste this in your console:
```r
shiny::runGitHub("FitGrowth", "angelovangel")
```
You will need these packages, so install them if you haven't done so: `shiny`, `shinydashboard`, `modelr`, `tidyverse`, `broom` and `DT`.
Upload the data as a text file, the first column **must** be named `time`, all other columns are treated as samples. Adjust the file input settings until the data is read into the app. After that, take a look at the other tabs. Note that the parameters of the logistic model are re-calculated when the time interval is changed with the slider.
