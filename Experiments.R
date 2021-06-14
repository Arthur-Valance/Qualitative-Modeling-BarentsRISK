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
interaction_matrix_S <- read_xlsx("./METADATA.xlsx",sheet="Autoregulation_components",range = "C2:R18",col_names = TRUE)

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


inverse_sigmoid <- function(x,lambda=2,h=0){
  return(((-1)/lambda)*log((1-x)/x)-h)
}


# Probability of a positive impact of a perturbation on the other vertices


QM <- purrr::map(Scenarios_FCM_VS_QM,function(x) {
  inferenceQM(x,As)[,3]+inferenceQM(x,As)[,2]/2
}) %>% unlist


# Value of the vertices after perturbation

inv_sig_hidden_pattern_S <- inverse_sigmoid(inferenceFCM(rep(0,length(interaction_matrix_S)),lambda=2,h=0,interaction_matrix_S))

FCM_S <- purrr::map(Scenarios_FCM_VS_QM,function(x) {
  inverse_sigmoid(inferenceFCM(x,lambda=2,h=0,interaction_matrix_S))-inv_sig_hidden_pattern_S}
) %>% unlist


# Results -----------------------------------------------------------------


results <- data.frame('FCM'= FCM_S, 'QM' = QM,'Scenario'=c(rep(1:16,each=length(interaction_matrix_S))), 
                      'ImpactedVertice' = rep(colnames(interaction_matrix_S),16))



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



inv_sig_hidden_pattern_S <- inverse_sigmoid(inferenceFCM(rep(0,length(interaction_matrix_S)),lambda=2,h=0,interaction_matrix_S))

source("./plot_increasing_pressures.R")
FCM_increasing_fishing <- purrr::map(Scenarios_increasing_fishing,function(x) {
  inverse_sigmoid(inferenceFCM(x,lambda=2,h=0,interaction_matrix_S))-inv_sig_hidden_pattern_S}
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
  inverse_sigmoid(inferenceFCM(x,lambda=2,h=0,interaction_matrix_S))-inv_sig_hidden_pattern_S}
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
  inverse_sigmoid(inferenceFCM(x,lambda=2,h=0,interaction_matrix_S))-inv_sig_hidden_pattern_S}
) %>% unlist

# split_FCM_crossed_scenarios <- split(FCM_crossed_scenarios,rep(1:length(interaction_matrix_S), length(crossed_Scenarios)))
# for (i in 1:length(interaction_matrix)){
#   split_FCM_crossed_scenarios[[i]] <- unname(split_FCM_crossed_scenarios[[i]])
# }
# split_FCM_crossed_scenarios

source("./plot_crossed_pressures.R")
library("cowplot")
library("viridis")
plot_crossed_pressures(FCM_crossed_scenarios,crossed_Scenarios)


# Models comparison -------------------------------------------------------

# Building Models with uncertain relations (sign and existence)

# Uncertain existence

# Model_1 : Cruise Activity -> + Marine Transport
# Model_2 : Provisioning Service -> + Marine Transport
# Model_3 : Cultural Service ->  + Cruise Activity
# Model_4 : Cultural Service ->  + Regulations
# Model_5 : Cultural Service ->  + Non indigenous Species
# Model_6 : Non indigenous Species -> - Ice Species
# Model_7 : Petroleum Activity -> + Cultural Service
# Model_8 : Cruise Activity -> + Cultural Service

model_1 <- c(4,1,1)
model_2 <- c(16,1,1)
model_3 <- c(15,4,1)
model_4 <- c(15,6,1)
model_5 <- c(15,8,1)
model_6 <- c(8,12,-1)
model_7 <- c(3,15,1)
model_8 <- c(4,15,1)

# Uncertain sign

# Model_9 : Regulations -> + Marine Transport
# Model_10 : Regulations -> - Marine Transport

# Model_11 : Regulations -> + Fishing
# Model_12 : Regulations -> - Fishing

# Model_13 : Regulations -> + Petroleum Activity
# Model_14 : Regulations -> - Petroleum Activity

# Model_15 : Regulations -> + Cruise Activity
# Model_16 : Regulations -> - Cruise Activity

# Model_17 : Non indigenous Species -> + Fish
# Model_18 : Non indigenous Species -> - Fish

# Model_19 : Non indigenous Species -> + Seabirds
# Model_20 : Non indigenous Species -> - Seabirds

# Model_21 : Cultural Service -> + Seabirds
# Model_22 : Cultural Service -> - Seabirds

# Model_23 : Non indigenous Species -> + Marine Mammals
# Model_24 : Non indigenous Species -> - Marine Mammals

# Model_25 : Non indigenous Species -> + Shellfish and Other Bottom Living Animals
# Model_26 : Non indigenous Species -> - Shellfish and Other Bottom Living Animals

# Model_27 : Non indigenous Species -> + Provisioning Service
# Model_28 : Non indigenous Species -> - Provisioning Service

model_9 <- c(6,1,1)
model_10 <- c(6,1,-1)

model_11 <- c(6,2,1)
model_12 <- c(6,2,-1)

model_13 <- c(6,3,1)
model_14 <- c(6,3,-1)

model_15 <- c(6,4,1)
model_16 <- c(6,4,-1)

model_17 <- c(8,9,1)
model_18 <- c(8,9,-1)

model_19 <- c(8,10,1)
model_20 <- c(8,10,-1)

model_21 <- c(15,10,1)
model_22 <- c(15,10,-1)

model_23 <- c(8,11,1)
model_24 <- c(8,11,-1)

model_25 <- c(8,13,1)
model_26 <- c(8,13,-1)

model_27 <- c(8,16,1)
model_28 <- c(8,16,-1)
models <- list(model_1,model_2,model_3,model_4,model_5,model_6,model_7,model_8,model_9,model_10,model_11,model_12,model_13,model_14,
               model_15,model_16,model_17,model_18,model_19,model_20,model_21,model_22,model_23,model_24,model_25,model_26,model_27,
               model_28)
# One alternative model is built from the Model_S and adding 1 uncertain relation
Models <- replicate(28, interaction_matrix_S, simplify=FALSE)


for (k in 1:28){
  Models[[k]][models[[k]][1],models[[k]][2]] <- models[[k]][3]  
}

# FCM inference on all the alternative models 

inv_hidden_pattern_Models <-c()
FCM_Models <- c()
tour <- 0
 for(k in 1:28){
   if (k != 25){
    inv_hidden_pattern_models <- inverse_sigmoid(inferenceFCM(rep(0,length(Models[[k]][1,])),lambda=2,h=0,Models[[k]]))
    inv_hidden_pattern_Models <-c(inv_hidden_pattern_Models,inv_hidden_pattern_models)
   
    for (i in 1:16){
    
    FCM_Models <- c(FCM_Models,inverse_sigmoid(inferenceFCM(Scenarios_FCM_VS_QM[[i]],lambda=2,h=0,Models[[k]]))-inv_hidden_pattern_models)
    tour <- tour+1
    }
  }
   else{
     inv_hidden_pattern_models <- inverse_sigmoid(inferenceFCM(rep(0,length(Models[[k]][1,])),lambda=2,h=0,Models[[k]]))
     inv_hidden_pattern_Models <-c(inv_hidden_pattern_Models,inv_hidden_pattern_models)
     for (i in 1:10){
       
       FCM_Models <- c(FCM_Models,inverse_sigmoid(inferenceFCM(Scenarios_FCM_VS_QM[[i]],lambda=2,h=0,Models[[k]]))-inv_hidden_pattern_models)
       tour <- tour+1
     }
     for (i in 12:16){
       
       FCM_Models <- c(FCM_Models,inverse_sigmoid(inferenceFCM(Scenarios_FCM_VS_QM[[i]],lambda=2,h=0,Models[[k]]))-inv_hidden_pattern_models)
       tour <- tour+1
     }
   }
    
  }

FCM_Model_25 <- FCM_Models[6145:6384] #incomplete data

FCM_Models <- FCM_Models[-c(6145:6384)] #all models expect 25

split_FCM_Models <- split(FCM_Models,rep(1:(length(Models)-1), each = length(interaction_matrix_S*length(Scenarios_FCM_VS_QM))))

source("./plot_models_comparison.R")
Results <- list(rep(0,length(split_FCM_Models)))

for (i in 1:27){
  Results[[i]] <- data.frame('FCM_S'= FCM_S, "FCM_Model" = split_FCM_Models[[i]], 'Scenario'=c(rep(1:16,each=length(interaction_matrix_S))), 
                             'ImpactedVertice' = rep(colnames(interaction_matrix_S),16))

}

plot_models_comparison(Results[[1]])
plot_models_comparison(Results[[2]])
plot_models_comparison(Results[[3]])
plot_models_comparison(Results[[4]])
plot_models_comparison(Results[[5]])
plot_models_comparison(Results[[6]])
plot_models_comparison(Results[[7]])
plot_models_comparison(Results[[8]])
plot_models_comparison(Results[[9]])
plot_models_comparison(Results[[10]])
plot_models_comparison(Results[[11]])
plot_models_comparison(Results[[12]])
plot_models_comparison(Results[[13]])
plot_models_comparison(Results[[14]])
plot_models_comparison(Results[[15]])
plot_models_comparison(Results[[16]])
plot_models_comparison(Results[[17]])
plot_models_comparison(Results[[18]])
plot_models_comparison(Results[[19]])
plot_models_comparison(Results[[20]])
plot_models_comparison(Results[[21]])
plot_models_comparison(Results[[22]])
plot_models_comparison(Results[[23]])
plot_models_comparison(Results[[24]])
plot_models_comparison(Results[[25]])
plot_models_comparison(Results[[26]])
plot_models_comparison(Results[[27]])

library("viridis") 
Models_comparability <- data.frame("Model"=c(1:28)[-25],"Comparabilty with the basic Model"=c(plot_models_comparison(Results[[1]]),
                                                                  plot_models_comparison(Results[[2]]),
                                                                  plot_models_comparison(Results[[3]]),
                                                                  plot_models_comparison(Results[[4]]),
                                                                  plot_models_comparison(Results[[5]]),
                                                                  plot_models_comparison(Results[[6]]),
                                                                  plot_models_comparison(Results[[7]]),
                                                                  plot_models_comparison(Results[[8]]),
                                                                  plot_models_comparison(Results[[9]]),
                                                                  plot_models_comparison(Results[[10]]),
                                                                  plot_models_comparison(Results[[11]]),
                                                                  plot_models_comparison(Results[[12]]),
                                                                  plot_models_comparison(Results[[13]]),
                                                                  plot_models_comparison(Results[[14]]),
                                                                  plot_models_comparison(Results[[15]]),
                                                                  plot_models_comparison(Results[[16]]),
                                                                  plot_models_comparison(Results[[17]]),
                                                                  plot_models_comparison(Results[[18]]),
                                                                  plot_models_comparison(Results[[19]]),
                                                                  plot_models_comparison(Results[[20]]),
                                                                  plot_models_comparison(Results[[21]]),
                                                                  plot_models_comparison(Results[[22]]),
                                                                  plot_models_comparison(Results[[23]]),
                                                                  plot_models_comparison(Results[[24]]),
                                                                  plot_models_comparison(Results[[25]]),
                                                                  plot_models_comparison(Results[[26]]),
                                                                  plot_models_comparison(Results[[27]])))

plot_model_comparability <- ggplot(Models_comparability,aes(x=Model,y=Comparabilty.with.the.basic.Model))+
                            geom_bar(stat="identity",color=Models_comparability$Comparabilty.with.the.basic.Model)
plot_model_comparability <- plot_model_comparability  +
                            scale_color_viridis(option = "D") +
                            geom_text(aes(label=Comparabilty.with.the.basic.Model), vjust=1.6, color="white", size=3.5)+
                            theme_minimal()
plot_model_comparability



