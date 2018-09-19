# these two functions perform the following:
# do_drm() --> performs drm (from the drc package) on a long dataframe, using the L.4 logistic model for describing dose-response relationships
# do_drm_plot() --> plots the output of do_drm()


##=====================
# do_drm() usage
# do_drm(df, d, r, x, y, ...)

# the fourth (and subsequent) arguments can be used for grouping by sample, treatment etc. before modelling
# if the fourth argument is not supplied, it fits a drm to all the dose-response values

# df is the long dataframe
# d is the name of the dose column
# r is the name of the response column
# x, y, etc are grouping variables (columns containing grouping information)

# for example
# do_drm(S.alba, Dose, DryMatter, Herbicide)

# if you want a table with the coefficients, pipe the output of this function to
#  unnest(coefs) %>% spread(names, x)

# if you want a nice plot, pipe it to
# do_drm_plot()
##=====================
do_drm <- function(df, d, r, ...) {
# using eclipsis (...) for group_by(...), so that arbitrary number of grouping variables can be used
# like this dplyr works in the function, no need for quosure and !!
  
require(tidyverse)
require(drc)
#require(modelr)
#require(broom)
  
# rename columns to dose and response in order for the drm to work (formula interface problems)
d <- deparse(substitute(d))
r <- deparse(substitute(r))
  
df$dose <- df[[d]]
df$response <- df[[r]]
  
  drm.func <- function(x) {
    drm(response ~ dose, 
      fct = L.4(names = c("Slope", "Lower", "Upper", "ED50")), #(names = c("Slope", "Lower", "Upper", "ED50")),
      data = x)
}

# this new predict.fun gives confidence intervals also
predict.fun <- function(x) {
  ## BUG ###
  #avoid too big dataframes for predictions when working with big numbers, seq has by=1 by default
  # use reasonable number of points to predict, they are used for plotting only anyway, ~ 100
  newdata <- data.frame(dose = seq(min(df$dose) - (0.1 * min(df$dose)),max(df$dose) + (0.1 * max(df$dose)), length.out = 100)
                        )
  #add_predictions(newdata, x) # add 10% below and above the data to the predictions
  pm <- predict(object = x, newdata = newdata, interval = "confidence")
  newdata$pred <- pm[ ,1]
  newdata$predmin <- pm[ ,2]
  newdata$predmax <- pm[ ,3]
  return(newdata)
}


#coefs.fun <- function(x) {coef(x) %>% tidy}

coefs.fun <- function(x) {
  summ <- summary(x)$coefficients 
  
  return(as.data.frame(summ) %>% mutate(param = rownames(summ)))
}

confint_slope.fun <- function(x) {
  slope <- coef(x)
  cis <- confint(x, parm = "Slope")
  data.table(slope = slope[1], lowerCI = cis[1], upperCI = cis[2])
}

df %>% group_by(...) %>% nest() %>% 
  mutate(drmod = map(data, drm.func), 
         pred = map(drmod, predict.fun),
         coefs = map(drmod, coefs.fun),
         confints = map(drmod, confint_slope.fun))

}

# do_drm_plot
# this function plots the results from the
# do_drm() function, which is a df with list-columns
#=========================
# do_drm_plot() usage
# do_drm_plot(df, ed50 = FALSE , aes_arguments)

# for example
# do_drm_plot(df, ed50 = TRUE, color = ~Herbicide) + facet_wrap(...) and so on
# note the ~Herbicide notation here (check the manual for aes_) 
#=========================

do_drm_plot <- function(df, ed50 = FALSE, ...) {
  
  require(tidyverse)
  
  p <- df %>% unnest(data) %>% 
    ggplot() + 
    geom_point(aes_(~dose,  ~response, ...), alpha = 0.6) +  
    geom_line(aes_(~dose, ~pred, ...), data = df %>% unnest(pred)) +
    theme_bw()
  ifelse(ed50 == FALSE,
         p,
         p <- p + geom_vline(aes_(xintercept = ~`ED50:(Intercept)`, ...),
                             linetype = 5,
                             data = df %>% unnest(coefs) %>% spread(names, x))
  )
  return(p)
  
}

