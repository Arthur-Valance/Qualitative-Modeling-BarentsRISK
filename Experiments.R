
#--------------------------------------
####### Set the working directory ##########
#--------------------------------------

#rm(list=ls())
directory <- "C:/Users/avala/OneDrive/Documents/Cours ACO/M2/Stage M2/IMR Tromsö/Qualitative models/Data/R/Comparison QM_FCM"
setwd(dir = directory)


#--------------------------------------
####### Loading data ##########
#--------------------------------------
                                                         
##FCM
#--------------------------------------

library(tidyverse)
library(gridExtra)
source(paste(directory,"/InferenceFCM.R",sep=''))
#load("C:/Users/avala/OneDrive/Documents/Cours ACO/M2/Stage M2/IMR Tromsö/Qualitative models/Data/R/Comparison QM_FCM/w_mat.RData") # load the interaction matrix

# C1  = c( 0, 0, 0, 0, 1, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0)
# C2  = c( 0, 0, 0, 0, 1, 0, 1, 0,-1, 0,-1, 0,-1,-1, 0, 1)
# C3  = c( 0, 0, 0, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 1)
# C4  = c( 0, 0, 0, 0, 1, 0, 0, 0, 0, 0,-1, 0, 0, 0, 0, 0)
# C5  = c( 0, 0, 0, 0, 0, 0, 0, 0,-1,-1,-1,-1,-1, 0, 0, 0)
# C6  = c( 0, 0, 0, 0, 0, 0, 0,-1, 1, 0, 0, 1, 0, 0, 0, 0)
# C7  = c( 0,-1,-1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
# C8  = c( 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,-1, 0, 0, 0, 0, 0)
# C9  = c( 0, 1, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0,-1, 1, 1, 0)
# C10 = c( 0, 0, 0, 0, 0, 0, 0, 0,-1, 0, 0, 0,-1, 1, 1, 0)
# C11 = c( 0,-1, 0, 0, 0, 0, 0, 0,-1, 0, 0, 0,-1, 1, 1, 0)
# C12 = c( 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0)
# C13 = c( 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 0, 0, 1, 1, 0)
# C14 = c( 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 0, 1, 0)
# C15 = c( 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
# C16 = c( 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)

C1  = c( 0, 0, 0, 0, 1, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0)
C2  = c( 0, 0, 0, 0, 1, 0, 1, 0,-1, 0,-1, 0,-1,-1, 0, 1)
C3  = c( 0, 0, 0, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 1)
C4  = c( 0, 0, 0, 0, 1, 0, 0, 0, 0, 0,-1, 0, 0, 0, 0, 0)
C5  = c( 0, 0, 0, 0, 0, 0, 0, 0,-1,-1,-1,-1,-1, 0, 0, 0)
C6  = c( 0, 0, 0, 0, 0, 0, 0,-1, 1, 0, 0, 1, 0, 0, 0, 0)
C7  = c( 0,-1,-1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
C8  = c( 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,-1, 0, 0, 0, 0, 0)
C9  = c( 0, 1, 0, 0, 0, 0, 0, 0,-1, 1, 1, 0,-1, 1, 1, 0)
C10 = c( 0, 0, 0, 0, 0, 0, 0, 0,-1,-1, 0, 0,-1, 1, 1, 0)
C11 = c( 0,-1, 0, 0, 0, 0, 0, 0,-1, 0,-1, 0,-1, 1, 1, 0)
C12 = c( 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,-1, 0, 1, 1, 0)
C13 = c( 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 0,-1, 1, 1, 0)
C14 = c( 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 0, 1, 0)
C15 = c( 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
C16 = c( 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)

w.mat <- matrix(c(C1, C2, C3, C4, C5, C6, C7, C8, C9, C10, C11, C12, C13, C14, C15, C16), nrow =16, ncol=16, byrow=TRUE)
w.mat <- as.data.frame(w.mat)
colnames(w.mat) <- c("Marine Transport","Fishing", "Petroleum Activity", "Cruise Activity", "Pollution", "Regulations","Area Conflict","Non Indigenous Species","Fish",
                     "Seabirds","Marine Mammals","Ice Species","Shellfish and Other Bottom Living Animals","Supporting Service","Cultural Service",
                     "Provisioning Service")
##Qualitative modeling
#--------------------------------------

QPress_dir <- "C:/Users/avala/OneDrive/Documents/Cours ACO/M2/Stage M2/IMR Tromsö/Qualitative models/Data/R/Marzloff/Rlib/lib"
source(paste(QPress_dir,"/dia.r", sep=''))
source(paste(QPress_dir,"/community.r", sep=''))
source(paste(QPress_dir,"/tk.r", sep=''))
source(paste(directory,"/InferenceQM.R",sep=''))
lapply(list.files(QPress_dir), function(f) source(file.path(QPress_dir, f)) )
#load("C:/Users/avala/OneDrive/Documents/Cours ACO/M2/Stage M2/IMR Tromsö/Qualitative models/Data/R/Comparison QM_FCM/ws.Rdata")
#load("C:/Users/avala/OneDrive/Documents/Cours ACO/M2/Stage M2/IMR Tromsö/Qualitative models/Data/R/Comparison QM_FCM/As.Rdata")
#load("C:/Users/avala/OneDrive/Documents/Cours ACO/M2/Stage M2/IMR Tromsö/Qualitative models/Data/R/Comparison QM_FCM/edges.Rdata")


# labels <- c("Marine Transport",
#             "Fishing",
#             "Petroleum Activity",
#             "Cruise Activity",
#             "Pollution",
#             "Regulations",
#             "Area Conflict",
#             "Non Indigenous Species",
#             "Fish",
#             "Seabirds",
#             "Marine Mammals",
#             "Ice Species",
#             "Shellfish and Other Bottom Living Animals",
#             "Supporting Service",
#             "Cultural Service",
#             "Provisioning Service")
# setwd(dir = "C:/Users/avala/OneDrive/Documents/Cours ACO/M2/Stage M2/IMR Tromsö/Qualitative models/Data/R/Model Stability/S.Model")
# edges <- model.dia("./S.Model.dia",labels=labels)
# edges <- enforce.limitation(edges)


#A <- adjacency.matrix(edges,required.groups=c(0)) # Examine unweighted adjacency matrix

# s <- community.sampler(edges) # Functions to generate the community matrix
# n.sims <- 5000
# As <- vector("list",n.sims)
# ws <- matrix(0,n.sims,nrow(edges))
# i <- 0
# while(i < n.sims) {
#   
#   ## Sample community matrix
#   z <- s$select(runif(1))
#   W <- s$community()
#   
#   ## Check press condition and stability
#   #if(!(press(W) && stable.community(W))) next
#   if(!stable.community(W)) next
#   
#   i <- i+1
#   As[[i]] <- -solve(W)
#   ws[i,] <- s$weights(W)
# }

#nodes <- node.labels(edges)


#--------------------------------------
####### Scenarios preparation ##########
#--------------------------------------

scenario0 <- c( 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0) # Hidden pattern (no perturbation)

##Fishing X Petroleum Activity
#--------------------------------------

scenario1  <- c( 0, 1,-1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0) # Fishing+ / PetroleumActivity-
scenario2  <- c( 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0) # Fishing+ / PetroleumActivity0
scenario3  <- c( 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0) # Fishing+ / PetroleumActivity+
scenario4  <- c( 0, 0,-1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0) # Fishing0 / PetroleumActivity-
scenario5  <- c( 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0) # Fishing0 / PetroleumActivity+
scenario6  <- c( 0,-1,-1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0) # Fishing- / PetroleumActivity-
scenario7  <- c( 0,-1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0) # Fishing- / PetroleumActivity0
scenario8  <- c( 0,-1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0) # Fishing- / PetroleumActivity+

##Fish X Marine Mammals
#--------------------------------------

scenario9  <- c( 0, 0, 0, 0, 0, 0, 0, 0, 1, 0,-1, 0, 0, 0, 0, 0) # Fish+ / MarineMammals-
scenario10 <- c( 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0) # Fish+ / MarineMammals0 
scenario11 <- c( 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0, 0, 0) # Fish+ / MarineMammals+
scenario12 <- c( 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,-1, 0, 0, 0, 0, 0) # Fish0 / MarineMammals-
scenario13 <- c( 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0) # Fish0 / MarineMammals+
scenario14 <- c( 0, 0, 0, 0, 0, 0, 0, 0,-1, 0,-1, 0, 0, 0, 0, 0) # Fish- / MarineMammals-
scenario15 <- c( 0, 0, 0, 0, 0, 0, 0, 0,-1, 0, 0, 0, 0, 0, 0, 0) # Fish- / MarineMammals0
scenario16 <- c( 0, 0, 0, 0, 0, 0, 0, 0,-1, 0, 1, 0, 0, 0, 0, 0) # Fish- / MarineMammals+

scenarios <- list(scenario1,
                  scenario2,
                  scenario3,
                  scenario4,
                  scenario5,
                  scenario6,
                  scenario7,
                  scenario8,
                  scenario9,
                  scenario10,
                  scenario11,
                  scenario12,
                  scenario13,
                  scenario14,
                  scenario15,
                  scenario16)

#--------------------------------------
####### Inferences ##########
#--------------------------------------

# Probability of a positive impact of a perturbation on the other vertices

QM <- purrr::map(scenarios,function(x) {
  inferenceQM(x)[,3]+inferenceQM(x)[,2]/2
}) %>% unlist

# Value of the vertices after perturbation

hidden_pattern <- inferenceFCM(scenario0)

FCM <- purrr::map(scenarios,function(x) {
  inferenceFCM(x)-hidden_pattern}
) %>% unlist

#--------------------------------------
####### Plot results ##########
#--------------------------------------

library(ggplot2)
nodes <- c("Marine Transport",
                        "Fishing",
                       "Petroleum Activity",
                       "Cruise Activity",
                       "Pollution",
                       "Regulations",
                       "Area Conflict",
                       "Non Indigenous Species",
                       "Fish",
                       "Seabirds",
                       "Marine Mammals",
                       "Ice Species",
                       "Shellfish and Other Bottom Living Animals",
                       "Supporting Service",
                       "Cultural Service",
                       "Provisioning Service")

##Round
#--------------------------------------
 
#QM  <- round(QM , digits=3)
#FCM <- round(FCM, digits=3)


##Build data.frame
#--------------------------------------

results <- data.frame('FCM'= FCM, 'QM' = QM,'Scenario'=c(rep(1,16),rep(2,16),rep(3,16),rep(4,16),rep(5,16),rep(6,16),rep(7,16),
                      rep(8,16),rep(9,16),rep(10,16),rep(11,16),rep(12,16),rep(13,16),rep(14,16),rep(15,16),rep(16,16)), 
                      'ImpactedVertice' = rep(nodes,16))


##Comparability
#--------------------------------------

#FCM_QM

pos_pos <- subset(results, FCM > 0.025 & QM > 0.55)
share_pos_pos <- length(pos_pos$FCM)*100/256

pos_neg <- subset(results, FCM > 0.025 & QM < 0.45)
share_pos_neg <- length(pos_neg$FCM)*100/256

neg_pos <- subset(results, FCM < -0.025 & QM > 0.55)
share_neg_pos <- length(neg_pos$FCM)*100/256

neg_neg <- subset(results, FCM < -0.025 & QM < 0.45)
share_neg_neg <- length(neg_neg$FCM)*100/256

zeros_pos <- subset(results, FCM >= -0.025 & FCM <= 0.025 & QM > 0.55)
share_zeros_pos <- length(zeros_pos$FCM)*100/256

zeros_neg <- subset(results, FCM >= -0.025 & FCM <= 0.025 & QM < 0.45)
share_zeros_neg <- length(zeros_neg$FCM)*100/256

pos_zeros <- subset(results, FCM > 0.025 & QM >= 0.45 & QM <= 0.55)
share_pos_zeros <- length(pos_zeros $FCM)*100/256

neg_zeros <- subset(results, FCM < -0.025 & QM >= 0.45 & QM <= 0.55)
share_neg_zeros  <- length(neg_zeros$FCM)*100/256

zeros_zeros   <- subset(results, FCM >= -0.025 & FCM <= 0.025 & QM >= 0.45 & QM <= 0.55)
share_zeros_zeros <- length(zeros_zeros$FCM)*100/256

comparability <- ((length(pos_pos$FCM) + length(neg_neg$FCM) + length(zeros_zeros$FCM))*100)/256
difference <- ((length(pos_neg$FCM) + length(neg_pos$FCM))*100)/256

##Plot
#--------------------------------------

plot_comparison_methods <- ggplot(results,aes(x=FCM)) +
                           geom_point(aes(y=QM),color="#56B4E9",size=1) +
                           xlim(-0.6,0.6) +
                           ylim(0,1) +
                           geom_hline(yintercept=c(0.45,0.55),col="firebrick4") +
                           geom_vline(xintercept=c(-0.025,0.025),col="firebrick4") 

plot_comparison_methods <- plot_comparison_methods + annotate("text",x=c(-0.36,0.36,0,-0.36,0,0.36,-0.36,0,0.36),y=c(0.82,0.82,0.82,0.5,0.5,0.5,0.18,0.18,0.18),
                                   label=c(round(share_neg_pos,digits=1),round(share_pos_pos,digits=1),
                                           round(share_zeros_pos,digits=1),round(share_neg_zeros,digits=1),
                                           round(share_zeros_zeros,digits=1),round(share_pos_zeros,digits=1),
                                           round(share_neg_neg,digits=1),round(share_zeros_neg,digits=1),
                                           round(share_pos_neg,digits=1)),color="firebrick4",size=5)

plot_comparison_methods + annotate("text",x=c(0.55,0.55,0.55),y=c(1,0.5,0),
                                   label=c(round(share_neg_neg + share_zeros_zeros + share_pos_pos, digits=1),
                                           round(share_neg_zeros + share_zeros_pos + share_pos_zeros + share_zeros_neg,digits=1),
                                           round(share_neg_pos + share_pos_neg)), color = "darkblue", size=6.5)
                                          






