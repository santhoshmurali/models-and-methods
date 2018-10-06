#https://www.analyticsvidhya.com/blog/2017/09/comparative-stock-analysis/
#install.packages('Quandl') #Quandl for Data Download
library(Quandl)

#install.packages('sweep') #Return an array obtained from an input array by sweeping out a summary statistic.
library(sweep)
library(timetk) #part of Sweep

#install.packages('tidyverse')
library(tidyverse) #https://cran.r-project.org/web/packages/tidyverse/tidyverse.pdf https://www.tidyverse.org/
library(forcats) #part of tidyverse

#install.packages('tidyquant') #tidyquant integrates the best resources for collecting and analyzing financial data, xts and zoo, quantmod, TTR, and PerformanceAnalytics with the tidy data infrastructure of the tidyverse allowing for seamless interaction between each.
library(tidyquant)

#install.packages('ggplot')
library(ggplot2)
library(stringr)

library(devtools)

devtools::install_github('dgrtwo/gganimate')
