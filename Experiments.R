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
library("viridis")
source("./InferenceFCM.R")
interaction_matrix_S <- read_xlsx("./METADATA.xlsx",sheet="Autoregulation_components",range = "C2:R18",col_names = TRUE)

##Qualitative modeling



source("./dia.r")
source("./community.r")
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


QM_VS_FCM <- data.frame('FCM'= FCM_S, 'QM' = QM,'Scenario'=c(rep(1:16,each=length(interaction_matrix_S))), 
                      'ImpactedVertice' = rep(colnames(interaction_matrix_S),16))



# Plot --------------------------------------------------------------------

library(ggplot2)

source("./plot_QM_VS_FCM.R")
plot_QM_VS_FCM(QM_VS_FCM)



# Statistical analysis of the comparability -------------------------------

#Density plot

samples_QM_VS_FCM <- list(rep(0,1000))
samples_QM_VS_FCM[[1]] <- QM_VS_FCM

for (i in 2:1000){
  samples_QM_VS_FCM[[i]] <-  data.frame('FCM'= sample(FCM_S), 'QM' = QM,'Scenario'=c(rep(1:16,each=length(interaction_matrix_S))), 
                                        'ImpactedVertice' = rep(colnames(interaction_matrix_S),16))
}

samples_comparabilities <- map(1:1000,~plot_QM_VS_FCM(samples_QM_VS_FCM[[.]]))%>% unlist
samples_comparabilities <- split(samples_comparabilities,rep(c(1,2,3),length(samples_QM_VS_FCM)))
Samples_comparabilities <- data.frame("Comparison" = rep(c("Comparability","Null","Difference"), each = length(samples_QM_VS_FCM)),
                                      "Proportion" = c(samples_comparabilities[[1]],samples_comparabilities[[2]],samples_comparabilities[[3]]))

library (dplyr)
d2 <- Samples_comparabilities %>%
  group_by(Comparison) %>%
  summarize(lower = quantile(Proportion, probs = .025),
            upper = quantile(Proportion, probs = .975))

ggplot(Samples_comparabilities, aes(x = Proportion, color = Comparison, fill = Comparison)) +
  geom_histogram(aes(y=..density..), alpha=0.5, binwidth = 1, color ="white",
                 position="identity")+
  geom_density(alpha=.4)+
  scale_colour_manual(values=c("#440154FF","#21908CFF","#FDE725FF"))+
  scale_fill_manual(values=c("#440154FF","#21908CFF","#FDE725FF"))+
  geom_vline(data = d2, aes(xintercept = lower[1]),colour = "#440154FF", linetype = "dashed") +
  geom_vline(data = d2, aes(xintercept = upper[1]),colour = "#440154FF", linetype = "dashed") +
  geom_vline(data = d2, aes(xintercept = lower[2]),colour="#21908CFF", linetype = "dashed") +
  geom_vline(data = d2, aes(xintercept = upper[2]),colour="#21908CFF", linetype = "dashed") +
  geom_vline(data = d2, aes(xintercept = lower[3]),colour="#FDE725FF", linetype = "dashed") +
  geom_vline(data = d2, aes(xintercept = upper[3]),colour="#FDE725FF", linetype = "dashed") +
  geom_vline(xintercept = samples_comparabilities[[1]][1],colour="#440154FF", size=1) +
  geom_vline(xintercept = samples_comparabilities[[3]][1],colour="#21908CFF", size=1) +
  geom_vline(xintercept = samples_comparabilities[[2]][1],colour="#FDE725FF", size=1) +
  geom_hline(yintercept = 0,colour="black", size=0.5) +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(family = "serif", size=18,face="bold"))

#Box plot

ggplot(Samples_comparabilities, aes(x = Comparison, y= Proportion, color = Comparison, fill = Comparison)) +
  geom_boxplot(alpha=0.3, show.legend = F)+
  scale_colour_manual(values=c("#440154FF","#21908CFF","#FDE725FF"))+
  scale_fill_manual(values=c("#440154FF","#21908CFF","#FDE725FF"))+
  geom_point(aes(x=1, y=samples_comparabilities[[1]][1]), colour="#440154FF", size = 4, show.legend = F)+
  geom_point(aes(x=2, y=samples_comparabilities[[3]][1]), colour="#21908CFF", size = 4, show.legend = F)+
  geom_point(aes(x=3, y=samples_comparabilities[[2]][1]), colour="#FDE725FF", size = 4, show.legend = F)+
  theme(axis.text=element_text(size=12),
        axis.title=element_text(family = "serif", size=18,face="bold"))+
  theme(axis.title.x = element_blank())




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

plot_crossed_pressures(FCM_crossed_scenarios,crossed_Scenarios)


# Models comparison -------------------------------------------------------


# 1 interaction added -----------------------------------------------------



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

list_inv_hidden_pattern_Models <-c()
FCM_Models <- c()
tour <- 0
 for(k in 1:28){
   if (k != 25){
    inv_hidden_pattern_models <- inverse_sigmoid(inferenceFCM(rep(0,length(Models[[k]][1,])),lambda=2,h=0,Models[[k]]))
    list_inv_hidden_pattern_Models <-c(list_inv_hidden_pattern_Models,inv_hidden_pattern_models)
   
    for (i in 1:16){
    
    FCM_Models <- c(FCM_Models,inverse_sigmoid(inferenceFCM(Scenarios_FCM_VS_QM[[i]],lambda=2,h=0,Models[[k]]))-inv_hidden_pattern_models)
    tour <- tour+1
    }
  }
   else{
     inv_hidden_pattern_models <- inverse_sigmoid(inferenceFCM(rep(0,length(Models[[k]][1,])),lambda=2,h=0,Models[[k]]))
     list_inv_hidden_pattern_Models <-c(list_inv_hidden_pattern_Models,inv_hidden_pattern_models)
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

#Model by Model

split_by_model_FCM_Models <- split(FCM_Models,rep(1:(length(Models)-1), each = length(interaction_matrix_S)*length(Scenarios_FCM_VS_QM)))

source("./plot_models_comparison.R")
Results <- list(rep(0,length(split_by_model_FCM_Models)))

for (i in 1:27){
  Results[[i]] <- data.frame('FCM_S'= FCM_S, "FCM_Model" = split_by_model_FCM_Models[[i]], 'Scenario'=c(rep(1:16,each=length(interaction_matrix_S))), 
                             'ImpactedVertice' = rep(colnames(interaction_matrix_S),16))

}


plot_models_comparison(Results[[1]])+
plot_models_comparison(Results[[2]])+
plot_models_comparison(Results[[3]])+
plot_models_comparison(Results[[4]])+
plot_models_comparison(Results[[5]])+
plot_models_comparison(Results[[6]])+
plot_models_comparison(Results[[7]])+
plot_models_comparison(Results[[8]])+
plot_models_comparison(Results[[9]])+
plot_models_comparison(Results[[10]])+
plot_models_comparison(Results[[11]])+
plot_models_comparison(Results[[12]])+
plot_models_comparison(Results[[13]])+
plot_models_comparison(Results[[14]])+
plot_models_comparison(Results[[15]])+
plot_models_comparison(Results[[16]])+
plot_models_comparison(Results[[17]])+
plot_models_comparison(Results[[18]])+
plot_models_comparison(Results[[19]])+
plot_models_comparison(Results[[20]])+
plot_models_comparison(Results[[21]])+
plot_models_comparison(Results[[22]])+
plot_models_comparison(Results[[23]])+
plot_models_comparison(Results[[24]])+
plot_models_comparison(Results[[25]])+
plot_models_comparison(Results[[26]])+
plot_models_comparison(Results[[27]])

library("viridis") 
Models_comparability <- data.frame("Model"=as.factor(c(1:28)[-25]),"Comparabilty with the basic Model"=map_dbl(1:27,~plot_models_comparison(Results[[.]])))

plot_model_comparability <- ggplot(Models_comparability,aes(x=Model,y=Comparabilty.with.the.basic.Model,fill=Model))+
                            geom_bar(stat="identity")

plot_model_comparability <- plot_model_comparability  +
                            scale_fill_manual(values=map(1:28,function(x) viridis(28)[x]) %>% unlist)+
                            geom_text(aes(label=Comparabilty.with.the.basic.Model), vjust=-0.5, color="black", size=3.5)+
                            guides( fill = FALSE)+
                            geom_hline(yintercept=50,col="firebrick4",size=2) +
                            theme_minimal()
plot_model_comparability

#Compartment by compartment 

split_by_compartment_FCM_Models <- split(FCM_Models,rep(1:length(interaction_matrix_S), (length(Models)-1)*length(Scenarios_FCM_VS_QM)))

perturbations <- map(1:16,~split(split_by_compartment_FCM_Models[[.]],rep(1:16,27)))%>% unlist

Models_comparability_by_compartment <- tibble("Compartments"= rep(colnames(interaction_matrix_S), each = (length(Models)-1)*length(Scenarios_FCM_VS_QM)),
                                              "Model"= rep(as.character(c(1:28)[-25]),(length(interaction_matrix_S))*length(Scenarios_FCM_VS_QM)),
                                              "Scenario"= rep(rep(1:16, each=length(Models)-1),length(Scenarios_FCM_VS_QM)),
                                              "Perturbation"= perturbations)

Models_comparability_by_compartment$Scenario <- as.factor(Models_comparability_by_compartment$Scenario)
Models_comparability_by_compartment$Model <- as.factor(Models_comparability_by_compartment$Model)
Models_comparability_by_compartment$Compartments <- as.factor(Models_comparability_by_compartment$Compartments)

plot_violin_Models <- ggplot(Models_comparability_by_compartment,aes(x=Compartments,y=Perturbation))+
  geom_violin(scale="width",aes(fill=Compartments))+
  facet_wrap(ncol=4,vars(Scenario))+
  theme(axis.text.x=element_text(angle=45,hjust = 1)) +
  geom_hline(yintercept=c(-0.05,0.05),col="firebrick4",linetype="dashed") +
  theme(legend.position = "none")

plot_violin_Models     


# Several interactions added ----------------------------------------------

basic_model <- interaction_matrix_S
models_bis <- models <- list(model_1,model_2,model_3,model_4,model_5,model_6,model_7,model_8,model_9,model_11,
                              model_13,model_15,model_17,model_19,model_21,model_23,model_25,model_27)
Models_bis <- replicate(10000, interaction_matrix_S, simplify=FALSE)
nb_added_interactions <- c()
for (k in 1:10000){
  for (i in 1:18){
    if (i != 17){
      if (i <= 8){
        models_bis[[i]][3] <- sample(0:models[[i]][3],1)
        Models_bis[[k]][models_bis[[i]][1],models_bis[[i]][2]] <- models_bis[[i]][3]
      }else{
        models_bis[[i]][3] <- sample(c(0,-1,1),1)
        Models_bis[[k]][models_bis[[i]][1],models_bis[[i]][2]] <- models_bis[[i]][3]
      }
    }else{
      models_bis[[i]][3] <- sample(c(0,-1),1)
      Models_bis[[k]][models_bis[[i]][1],models_bis[[i]][2]] <- models_bis[[i]][3]
    }
  }
  nb_added_interactions <- c(nb_added_interactions,sum(Models_bis[[k]] != interaction_matrix_S))
}
# for (k in 1:10000){
#   nb_added_interactions <- c(nb_added_interactions,length(Scenarios_FCM_VS_QM)*length(interaction_matrix_S) - sum(Models_bis[[k]] == interaction_matrix_S))
# }
# 100 models -> 0.4 seconds
# 1000 models -> 3,4  seconds
# 10000 models -> 36  seconds



# FCM inference on all the alternative models 

# list_inv_hidden_pattern_Models_bis <-c()
# FCM_Models_bis <- c()
# tour <- 0
# 
# for(k in 1:10000){
#   
#     inv_hidden_pattern_models <- inverse_sigmoid(inferenceFCM(rep(0,length(Models_bis[[k]][1,])),lambda=2,h=0,Models_bis[[k]]))
#     list_inv_hidden_pattern_Models_bis <-c(list_inv_hidden_pattern_Models_bis,inv_hidden_pattern_models)
#     
#     for (i in 1:16){
#       
#       FCM_Models_bis <- c(FCM_Models_bis,inverse_sigmoid(inferenceFCM(Scenarios_FCM_VS_QM[[i]],lambda=2,h=0,Models_bis[[k]]))-inv_hidden_pattern_models)
#       tour <- tour+1
#     }
# } 

# 10 models -> 2.7 seconds
# 100 models -> 7.1 seconds
# 1000 models -> 79 seconds
# 10000 models -> 35 minutes
load("./FCM_Model_bis.Rdata")
split_by_model_FCM_Models_bis <- split(FCM_Models_bis,rep(1:length(Models_bis), each = length(interaction_matrix_S)*length(Scenarios_FCM_VS_QM)))

source("./plot_models_comparison.R")
Results_bis <- list(rep(0,length(split_by_model_FCM_Models_bis)))

for (i in 1:10000){
  Results_bis[[i]] <- data.frame('FCM_S'= FCM_S, "FCM_Model" = split_by_model_FCM_Models_bis[[i]], 'Scenario'=c(rep(1:16,each=length(interaction_matrix_S))),
                                 'ImpactedVertice' = rep(colnames(interaction_matrix_S),16))

}

comparabilities <- map(1:10000,~plot_models_comparison(Results_bis[[.]]))%>% unlist
mean(comparabilities)

# Relation between the number of interactions added and the comparability of the results with the Model_S

comparabilities_nb_added_interactions<- data.frame("Number of interactions added"= nb_added_interactions , "Comparability with the Model_S" = comparabilities)
comparabilities_nb_added_interactions$Number.of.interactions.added <- as.factor(comparabilities_nb_added_interactions$Number.of.interactions.added)
plot_violin_Models <- ggplot(comparabilities_nb_added_interactions,aes(x=Number.of.interactions.added,y=Comparability.with.the.Model_S))+
  geom_violin(scale="width") +
  geom_boxplot(width=0.1)
plot_violin_Models
plot(comparabilities_nb_added_interactions)

# Relation between the comparability and the interaction/s added

interaction1 <- c()
interaction2 <- c()
interaction3 <- c()
interaction4 <- c()
interaction5 <- c()
interaction6 <- c()
interaction7 <- c()
interaction8 <- c()
interaction9 <- c()
interaction10 <- c()
interaction11 <- c()
interaction12 <- c()
interaction13 <- c()
interaction14 <- c()
interaction15 <- c()
interaction16 <- c()
interaction17 <- c()
interaction18 <- c()
interaction19 <- c()
interaction20 <- c()
interaction21 <- c()
interaction22 <- c()
interaction23 <- c()
interaction24 <- c()
interaction26 <- c()
interaction27 <- c()
interaction28 <- c()

for (i in 1:1000){
  if (Models_bis[[i]][[4,1]] == 1){
    interaction1 <- c(interaction1, comparabilities[i])
  }
  if (Models_bis[[i]][[16,1]] == 1){
    interaction2 <- c(interaction2, comparabilities[i])
  }
  if (Models_bis[[i]][[15,4]] == 1){
    interaction3 <- c(interaction3, comparabilities[i])
  }
  if (Models_bis[[i]][[15,6]] == 1){
    interaction4 <- c(interaction4, comparabilities[i])
  }
  if (Models_bis[[i]][[15,8]] == 1){
    interaction5 <- c(interaction5, comparabilities[i])
  }
  if (Models_bis[[i]][[8,12]] == -1){
    interaction6 <- c(interaction6, comparabilities[i])
  }
  if (Models_bis[[i]][[3,15]] == 1){
    interaction7 <- c(interaction7, comparabilities[i])
  }
  if (Models_bis[[i]][[4,15]] == 1){
    interaction8 <- c(interaction8, comparabilities[i])
  }
  if (Models_bis[[i]][[6,1]] == 1){
    interaction9 <- c(interaction9, comparabilities[i])
  }
  if (Models_bis[[i]][[6,1]] == -1){
    interaction10 <- c(interaction10, comparabilities[i])
  }
  if (Models_bis[[i]][[6,2]] == 1){
    interaction11 <- c(interaction11, comparabilities[i])
  }
  if (Models_bis[[i]][[6,2]] == -1){
    interaction12 <- c(interaction12, comparabilities[i])
  }
  if (Models_bis[[i]][[6,3]] == 1){
    interaction13 <- c(interaction13, comparabilities[i])
  }
  if (Models_bis[[i]][[6,3]] == -1){
    interaction14 <- c(interaction14, comparabilities[i])
  }
  if (Models_bis[[i]][[6,4]] == 1){
    interaction15 <- c(interaction15, comparabilities[i])
  }
  if (Models_bis[[i]][[6,4]] == -1){
    interaction16 <- c(interaction16, comparabilities[i])
  }
  if (Models_bis[[i]][[8,9]] == 1){
    interaction17 <- c(interaction17, comparabilities[i])
  }
  if (Models_bis[[i]][[8,9]] == -1){
    interaction18 <- c(interaction18, comparabilities[i])
  }
  if (Models_bis[[i]][[8,10]] == 1){
    interaction19 <- c(interaction19, comparabilities[i])
  }
  if (Models_bis[[i]][[8,10]] == -1){
    interaction20 <- c(interaction20, comparabilities[i])
  }
  if (Models_bis[[i]][[15,10]] == 1){
    interaction21 <- c(interaction21, comparabilities[i])
  }
  if (Models_bis[[i]][[15,10]] == -1){
    interaction22 <- c(interaction22, comparabilities[i])
  }
  if (Models_bis[[i]][[8,11]] == 1){
    interaction23 <- c(interaction23, comparabilities[i])
  }
  if (Models_bis[[i]][[8,11]] == -1){
    interaction24 <- c(interaction24, comparabilities[i])
  }
  if (Models_bis[[i]][[8,13]] == -1){
    interaction26 <- c(interaction26, comparabilities[i])
  }
  if (Models_bis[[i]][[8,16]] == 1){
    interaction27 <- c(interaction27, comparabilities[i])
  }
  if (Models_bis[[i]][[8,16]] == -1){
    interaction28 <- c(interaction28, comparabilities[i])
  }
}
interactions <- c(interaction1,interaction2,interaction3,interaction4,interaction5,interaction6,interaction7,
                  interaction8,interaction9,interaction10,interaction11,interaction12,interaction13,
                  interaction14,interaction15,interaction16,interaction17,interaction18,interaction19,
                  interaction20,interaction21,interaction22,interaction23,interaction24,interaction26,
                  interaction27,interaction28)
interaction_added <- c(rep(1,length(interaction1)),
                  rep(2,length(interaction2)),
                  rep(3,length(interaction3)),
                  rep(4,length(interaction4)),
                  rep(5,length(interaction5)),
                  rep(6,length(interaction6)),
                  rep(7,length(interaction7)),
                  rep(8,length(interaction8)),
                  rep(9,length(interaction9)),
                  rep(10,length(interaction10)),
                  rep(11,length(interaction11)),
                  rep(12,length(interaction12)),
                  rep(13,length(interaction13)),
                  rep(14,length(interaction14)),
                  rep(15,length(interaction15)),
                  rep(16,length(interaction16)),
                  rep(17,length(interaction17)),
                  rep(18,length(interaction18)),
                  rep(19,length(interaction19)),
                  rep(20,length(interaction20)),
                  rep(21,length(interaction21)),
                  rep(22,length(interaction22)),
                  rep(23,length(interaction23)),
                  rep(24,length(interaction24)),
                  rep(26,length(interaction26)),
                  rep(27,length(interaction27)),
                  rep(28,length(interaction28)))

interactions_added <- data.frame("Relation added" = interaction_added, "Comparability" = interactions)
interactions_added$Relation.added <- as.factor(interactions_added$Relation.added)
ggplot(interactions_added, aes(x=Relation.added, y=Comparability)) + 
  geom_boxplot(aes(y=Comparability)) + 
  theme(axis.text=element_text(size=10),
        axis.title=element_text(family = "serif", size=18,face="bold"))
#Compartment by compartment 

split_by_compartment_FCM_Models_bis <- split(FCM_Models_bis,rep(1:length(interaction_matrix_S), length(Models_bis)*length(Scenarios_FCM_VS_QM)))

perturbations <- map(1:16,~split(split_by_compartment_FCM_Models_bis[[.]],rep(1:16,10000)))%>% unlist

Models_comparability_by_compartment <- tibble("Compartments"= rep(colnames(interaction_matrix_S), each = (length(Models_bis))*length(Scenarios_FCM_VS_QM)),
                                              "Model"= rep(as.character(c(1:10000)),(length(interaction_matrix_S))*length(Scenarios_FCM_VS_QM)),
                                              "Scenario"= rep(rep(1:16, each=length(Models_bis)),length(Scenarios_FCM_VS_QM)),
                                              "Perturbation"= perturbations)

Models_comparability_by_compartment$Scenario <- as.factor(Models_comparability_by_compartment$Scenario)
Models_comparability_by_compartment$Model <- as.factor(Models_comparability_by_compartment$Model)
Models_comparability_by_compartment$Compartments <- as.factor(Models_comparability_by_compartment$Compartments)

plot_violin_Models <- ggplot(Models_comparability_by_compartment,aes(x=Compartments,y=Perturbation))+
  geom_violin(scale="width",aes(fill=Compartments))+
  facet_wrap(ncol=4,vars(Scenario))+
  theme(axis.text.x=element_text(angle=45,hjust = 1)) +
  geom_hline(yintercept=c(-0.05,0.05),col="firebrick4",linetype="dashed") +
  theme(legend.position = "none")

plot_violin_Models     




# Group Visions -----------------------------------------------------------



interaction_matrix_A <- read_xlsx("./METADATA.xlsx",sheet="groupA",range = "C2:T20",col_names = TRUE)
interaction_matrix_B <- read_xlsx("./METADATA.xlsx",sheet="groupB",range = "C2:T20",col_names = TRUE)
interaction_matrix_C <- read_xlsx("./METADATA.xlsx",sheet="groupC",range = "C2:U21",col_names = TRUE)
interaction_matrix_D <- read_xlsx("./METADATA.xlsx",sheet="groupD",range = "C2:V22",col_names = TRUE)
interaction_matrix_E <- read_xlsx("./METADATA.xlsx",sheet="groupE",range = "C2:U21",col_names = TRUE)
interaction_matrix_F <- read_xlsx("./METADATA.xlsx",sheet="groupF",range = "C2:T20",col_names = TRUE)

# Scenarios ---------------------------------------------------------------

scenarios_groupA <- as.matrix(read_xlsx("./METADATA.xlsx",sheet="groupA",range = "C24:R42"))
scenarios_groupB <- as.matrix(read_xlsx("./METADATA.xlsx",sheet="groupB",range = "C24:R42"))
scenarios_groupC <- as.matrix(read_xlsx("./METADATA.xlsx",sheet="groupC",range = "C24:R43"))
scenarios_groupD <- as.matrix(read_xlsx("./METADATA.xlsx",sheet="groupD",range = "C24:R44"))
scenarios_groupE <- as.matrix(read_xlsx("./METADATA.xlsx",sheet="groupE",range = "C24:R43"))
scenarios_groupF <- as.matrix(read_xlsx("./METADATA.xlsx",sheet="groupF",range = "C24:R42"))
colnames(scenarios_groupA) <- NULL
colnames(scenarios_groupB) <- NULL
colnames(scenarios_groupC) <- NULL
colnames(scenarios_groupD) <- NULL
colnames(scenarios_groupE) <- NULL
colnames(scenarios_groupF) <- NULL
Scenarios_groupA <- split(scenarios_groupA, rep(1:ncol(scenarios_groupA), each = nrow(scenarios_groupA)))
Scenarios_groupB <- split(scenarios_groupB, rep(1:ncol(scenarios_groupB), each = nrow(scenarios_groupB)))
Scenarios_groupC <- split(scenarios_groupC, rep(1:ncol(scenarios_groupC), each = nrow(scenarios_groupC)))
Scenarios_groupD <- split(scenarios_groupD, rep(1:ncol(scenarios_groupD), each = nrow(scenarios_groupD)))
Scenarios_groupE <- split(scenarios_groupE, rep(1:ncol(scenarios_groupE), each = nrow(scenarios_groupE)))
Scenarios_groupF <- split(scenarios_groupF, rep(1:ncol(scenarios_groupF), each = nrow(scenarios_groupF)))

# Inferences --------------------------------------------------------------

inv_sig_hidden_pattern_groupA <- inverse_sigmoid(inferenceFCM(rep(0,length(interaction_matrix_A)),lambda=2,h=0,interaction_matrix_A))

FCM_groupA <- purrr::map(Scenarios_groupA,function(x) {
  inverse_sigmoid(inferenceFCM(x,lambda=2,h=0,interaction_matrix_A))-inv_sig_hidden_pattern_groupA}
) %>% unlist

inv_sig_hidden_pattern_groupB <- inverse_sigmoid(inferenceFCM(rep(0,length(interaction_matrix_B)),lambda=2,h=0,interaction_matrix_B))

FCM_groupB <- purrr::map(Scenarios_groupB,function(x) {
  inverse_sigmoid(inferenceFCM(x,lambda=2,h=0,interaction_matrix_B))-inv_sig_hidden_pattern_groupB}
) %>% unlist

inv_sig_hidden_pattern_groupC <- inverse_sigmoid(inferenceFCM(rep(0,length(interaction_matrix_C)),lambda=2,h=0,interaction_matrix_C))

FCM_groupC <- purrr::map(Scenarios_groupC,function(x) {
  inverse_sigmoid(inferenceFCM(x,lambda=2,h=0,interaction_matrix_C))-inv_sig_hidden_pattern_groupC}
) %>% unlist

inv_sig_hidden_pattern_groupD <- inverse_sigmoid(inferenceFCM(rep(0,length(interaction_matrix_D)),lambda=2,h=0,interaction_matrix_D))

FCM_groupD <- purrr::map(Scenarios_groupD,function(x) {
  inverse_sigmoid(inferenceFCM(x,lambda=2,h=0,interaction_matrix_D))-inv_sig_hidden_pattern_groupD}
) %>% unlist

inv_sig_hidden_pattern_groupE <- inverse_sigmoid(inferenceFCM(rep(0,length(interaction_matrix_E)),lambda=2,h=0,interaction_matrix_E))

FCM_groupE <- purrr::map(Scenarios_groupE,function(x) {
  inverse_sigmoid(inferenceFCM(x,lambda=2,h=0,interaction_matrix_E))-inv_sig_hidden_pattern_groupE}
) %>% unlist

inv_sig_hidden_pattern_groupF <- inverse_sigmoid(inferenceFCM(rep(0,length(interaction_matrix_F)),lambda=2,h=0,interaction_matrix_F))

FCM_groupF <- purrr::map(Scenarios_groupF,function(x) {
  inverse_sigmoid(inferenceFCM(x,lambda=2,h=0,interaction_matrix_F))-inv_sig_hidden_pattern_groupF}
) %>% unlist

group_visions <- data.frame("")
