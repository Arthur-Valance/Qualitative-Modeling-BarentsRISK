#### Comparison QM VS FCM

# Set the working directory -----------------------------------------------

#rm(list=ls())
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


## Increasing Fishing

# Scenarios ---------------------------------------------------------------



scenarios_increasing_fishing <- as.matrix(read_xlsx("./METADATA.xlsx",sheet="Scenarios_Increasing_Fishing",range = "B2:L18"))
colnames(scenarios_increasing_fishing) <- NULL
Scenarios_increasing_fishing <- split(scenarios_increasing_fishing, rep(1:ncol(scenarios_increasing_fishing), each = nrow(scenarios_increasing_fishing)))



# Plot --------------------------------------------------------------------


inverse_sigmoid <- function(x,lambda=2,h=0){
  return(((-1)/lambda)*log((1-x)/x)-h)
}

inv_sig_hidden_pattern <- inverse_sigmoid(inferenceFCM(rep(0,length(interaction_matrix)),lambda=2,h=0,interaction_matrix))

source("./plot_increasing_pressures.R")
FCM_increasing_fishing <- purrr::map(Scenarios_increasing_fishing,function(x) {
  inverse_sigmoid(inferenceFCM(x,lambda=2,h=0,interaction_matrix))-inv_sig_hidden_pattern}
) %>% unlist
pressures <- c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1)
plot_increasing_pressures(FCM_increasing_fishing,Scenarios_increasing_fishing)

# split_FCM_increasing_fishing  <- split(FCM_increasing_fishing ,rep(1:length(interaction_matrix), length(Scenarios_increasing_fishing)))
# for (i in 1:length(interaction_matrix)){
#   split_FCM_increasing_fishing [[i]] <- unname(split_FCM_increasing_fishing [[i]])
# }
# split_FCM_increasing_fishing 
  


## Increasing Petroleum Activity


# Scenarios ---------------------------------------------------------------

scenarios_increasing_petrolact <- as.matrix(read_xlsx("./METADATA.xlsx",sheet="Scenarios_Increasing_PetrolAct",range = "B2:L18"))
colnames(scenarios_increasing_petrolact) <- NULL
Scenarios_increasing_petrolact <- split(scenarios_increasing_petrolact, rep(1:ncol(scenarios_increasing_petrolact), each = nrow(scenarios_increasing_petrolact)))



# Plot --------------------------------------------------------------------

source("./plot_increasing_pressures.R")
FCM_increasing_petrolact <- purrr::map(Scenarios_increasing_petrolact,function(x) {
  inverse_sigmoid(inferenceFCM(x,lambda=2,h=0,interaction_matrix))-inv_sig_hidden_pattern}
) %>% unlist
plot_increasing_pressures(FCM_increasing_petrolact,Scenarios_increasing_petrolact)

# split_FCM_increasing_petrolact <- split(FCM_increasing_petrolact,rep(1:length(interaction_matrix), length(Scenarios_increasing_fishing)))
# for (i in 1:length(interaction_matrix)){
#   split_FCM_increasing_petrolact[[i]] <- unname(split_FCM_increasing_petrolact[[i]])
# }
# split_FCM_increasing_petrolact


# Crossed scenarios -------------------------------------------------------

crossed_scenarios <- as.matrix(read_xlsx("./METADATA.xlsx",sheet="Scenarios_Fishing_X_PetrolAct",range = "B2:M18"))
colnames(crossed_scenarios) <- NULL
crossed_Scenarios <- split(crossed_scenarios, rep(1:ncol(crossed_scenarios), each = nrow(crossed_scenarios)))



FCM_crossed_scenarios <- purrr::map(crossed_Scenarios,function(x) {
  inverse_sigmoid(inferenceFCM(x,lambda=2,h=0,interaction_matrix))-inv_sig_hidden_pattern}
) %>% unlist

# split_FCM_crossed_scenarios <- split(FCM_crossed_scenarios,rep(1:length(interaction_matrix), length(crossed_Scenarios)))
# for (i in 1:length(interaction_matrix)){
#   split_FCM_crossed_scenarios[[i]] <- unname(split_FCM_crossed_scenarios[[i]])
# }
# split_FCM_crossed_scenarios

source("./plot_crossed_pressures.R")
library("gridExtra")
plot_crossed_pressures(FCM_crossed_scenarios,crossed_Scenarios)

