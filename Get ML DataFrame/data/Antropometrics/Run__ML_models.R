rm(list = ls())


# load packages -----------------------------------------------------------

library(dplyr)
library(readxl)
library(ggplot2)
library(ggpubr)
library(lubridate)
library(tidyr)
library(DescTools)
library(tidyverse)
library(openxlsx)
library(caret)


# load compliance ---------------------------------------------------------

compliance<-read_excel("~/Bewegingswetenschappen/Master/Thesis/Code/compliance_complete.xlsx", sheet = 'Tot' )

selected <- compliance%>%filter(`Train+daily`==1)
selected$GROUP<-as.factor(selected$GROUP)

set.seed(1234)

trainIndex <- createDataPartition(selected$GROUP, p = .6, list = FALSE, times = 1) # results in a 13/19 training split which corresponds to roughly 68/32 split

