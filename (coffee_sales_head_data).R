library(readr)
library(ggplot2)
library(tidyr)
library(dplyr)
library(knitr)


coffee <- read_csv("Coffee_sales.csv")

head(coffee)


options(tibble.width = Inf)
head(coffee, 20) 