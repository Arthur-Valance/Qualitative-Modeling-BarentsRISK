
# Set the working directory -----------------------------------------------

rm(list=ls())
getwd()
#setwd(dir="")


# Loading data ------------------------------------------------------------

##FCM


library(tidyverse)
library(gridExtra)
library(readxl)
source("./InferenceFCM.R")
interaction_matrix <- read_xlsx("./METADATA.xlsx",sheet="No_autoregulation",range = "C2:R18",col_names = TRUE)

##Qualitative modeling



source("./dia.r")
source("./community.r")
source("./tk.r")
source("./InferenceQM.R")
#lapply(list.files(QPress_dir), function(f) source(file.path(QPress_dir, f)) )
load("./ws.Rdata")
load("./As.Rdata")
load("./edges.Rdata")


# Scenarios ---------------------------------------------------------------

scenarios <- as.matrix(read_xlsx("./METADATA.xlsx",sheet="Scenarios",range = "C2:R18"))
colnames(scenarios) <- NULL
Scenarios <- list(scenarios[,1],
                  scenarios[,2],
                  scenarios[,3],
                  scenarios[,4],
                  scenarios[,5],
                  scenarios[,6],
                  scenarios[,7],
                  scenarios[,8],
                  scenarios[,9],
                  scenarios[,10],
                  scenarios[,11],
                  scenarios[,12],
                  scenarios[,13],
                  scenarios[,14],
                  scenarios[,15],
                  scenarios[,16])


# Inferences --------------------------------------------------------------


# Probability of a positive impact of a perturbation on the other vertices


QM <- purrr::map(Scenarios,function(x) {
  inferenceQM(x,As)[,3]+inferenceQM(x,As)[,2]/2
}) %>% unlist


# Value of the vertices after perturbation

hidden_pattern <- inferenceFCM(c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0),lambda=2,h=0,interaction_matrix)

FCM <- purrr::map(Scenarios,function(x) {
  inferenceFCM(x,lambda=2,h=0,interaction_matrix)-hidden_pattern}
) %>% unlist


# Results -----------------------------------------------------------------


results <- data.frame('FCM'= FCM, 'QM' = QM,'Scenario'=c(rep(1,16),rep(2,16),rep(3,16),rep(4,16),rep(5,16),rep(6,16),rep(7,16),
                                                         rep(8,16),rep(9,16),rep(10,16),rep(11,16),rep(12,16),rep(13,16),rep(14,16),rep(15,16),rep(16,16)), 
                      'ImpactedVertice' = rep(colnames(interaction_matrix),16))



# Plot --------------------------------------------------------------------

library(ggplot2)
source("./plot_QM_VS_FCM.R")
plot_QM_VS_FCM(results)






