#### Comparison QM VS FCM

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

scenarios_FCM_VS_QM <- as.matrix(read_xlsx("./METADATA.xlsx",sheet="Scenarios",range = "C2:R18"))
colnames(scenarios_FCM_VS_QM) <- NULL
Scenarios_FCM_VS_QM <- split(scenarios_FCM_VS_QM, rep(1:ncol(scenarios_FCM_VS_QM), each = nrow(scenarios_FCM_VS_QM)))

# Inferences --------------------------------------------------------------


# Probability of a positive impact of a perturbation on the other vertices


QM <- purrr::map(Scenarios_FCM_VS_QM,function(x) {
  inferenceQM(x,As)[,3]+inferenceQM(x,As)[,2]/2
}) %>% unlist


# Value of the vertices after perturbation

hidden_pattern <- inferenceFCM(rep(0,length(interaction_matrix)),lambda=2,h=0,interaction_matrix)

FCM <- purrr::map(Scenarios_FCM_VS_QM,function(x) {
  inferenceFCM(x,lambda=2,h=0,interaction_matrix)-hidden_pattern}
) %>% unlist


# Results -----------------------------------------------------------------


results <- data.frame('FCM'= FCM, 'QM' = QM,'Scenario'=c(rep(1:16,each=length(interaction_matrix))), 
                      'ImpactedVertice' = rep(colnames(interaction_matrix),16))



# Plot --------------------------------------------------------------------

library(ggplot2)
source("./plot_QM_VS_FCM.R")
plot_QM_VS_FCM(results)

#### Cumulative impacts

# Scenarios ---------------------------------------------------------------

scenarios_increasing_pressures <- as.matrix(read_xlsx("./METADATA.xlsx",sheet="Scenarios_Increasing_Pressures",range = "B2:L18"))
colnames(scenarios_increasing_pressures) <- NULL
Scenarios_increasing_pressures <- split(scenarios_increasing_pressures, rep(1:ncol(scenarios_increasing_pressures), each = nrow(scenarios_increasing_pressures)))



# Plot --------------------------------------------------------------------

source("./plot_increasing_pressures.R")
FCM_inc_press <- purrr::map(Scenarios_increasing_pressures,function(x) {
  inferenceFCM(x,lambda=2,h=0,interaction_matrix)-hidden_pattern}
) %>% unlist
plot_increasing_pressures(FCM_inc_press)
