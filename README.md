**FitGrowth**

Shiny app for modelling bacterial growth kinetics.
This app fits growth data to the continuous [logistic equation](https://en.wikipedia.org/wiki/Generalised_logistic_function). 
The best parameters are found using the nonlinear least-squares method `nls` in `R`. The app handles one or many samples (tested with 96), as well as NA values. You can get an example file [here](https://www.dropbox.com/sh/zzf7y3ijwkat55e/AABUvp7BAARIdYBqZWgk1E37a?dl=0).
**Instructions:** 
Upload the data as a text file, the first column **must** be named `time`, all other columns are treated as samples. Adjust the file input settings until the data is read into the app. After that, take a look at the other tabs. Note that the parameters of the logistic model are re-calculated when the time interval is changed with the slider.
