if(!require(tidyverse)) install.packages("tidyverse")
library(tidyverse)
if(!require(tibble)) install.packages("tibble")
library(tibble)

setwd("/home/e/R/titanic.cart")
x <- read_csv("data/train.csv")

glimpse(x)
summary(x)

