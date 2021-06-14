plot_crossed_pressures <- function(FCM_cros_press,crossed_scenars){
  

  # Fishing x Petroleum Activity
  
  pollution <- subset(data.frame("value" = FCM_cros_press,"vertice"=rep(colnames(interaction_matrix_S),length(crossed_scenars))),vertice == "Pollution")
  area_conflict <- subset(data.frame("value" = FCM_cros_press,"vertice"=rep(colnames(interaction_matrix_S),length(crossed_scenars))),vertice == "Area Conflict")
  seabirds <- subset(data.frame("value" = FCM_cros_press,"vertice"=rep(colnames(interaction_matrix_S),length(crossed_scenars))),vertice == "Seabirds")
  icespecies <- subset(data.frame("value" = FCM_cros_press,"vertice"=rep(colnames(interaction_matrix_S),length(crossed_scenars))),vertice == "Ice Species")
  provisioning_service <- subset(data.frame("value" = FCM_cros_press,"vertice"=rep(colnames(interaction_matrix_S),length(crossed_scenars))),vertice == "Provisioning Service")
  
  sum_up <- c(pollution$value[1],
              pollution$value[2],
              pollution$value[3],
              pollution$value[4],
              pollution$value[5],
              pollution$value[6],
              area_conflict$value[1],
              area_conflict$value[2],
              area_conflict$value[3],
              area_conflict$value[4],
              area_conflict$value[5],
              area_conflict$value[6],
              seabirds$value[1],
              seabirds$value[2],
              seabirds$value[3],
              seabirds$value[4],
              seabirds$value[5],
              seabirds$value[6],
              icespecies$value[1],
              icespecies$value[2],
              icespecies$value[3],
              icespecies$value[4],
              icespecies$value[5],
              icespecies$value[6],
              provisioning_service$value[1],
              provisioning_service$value[2],
              provisioning_service$value[3],
              provisioning_service$value[4],
              provisioning_service$value[5],
              provisioning_service$value[6])
  
  crossed_pressures_Fishing_X_PetrolAct <- data.frame("Impacted vertice"= c(rep(c("Pollution","Pollution","Pollution cumulative"),2),
                                                                                rep(c("Area Conflict","Area Conflict","Area Conflict cumulative"),2),
                                                                                rep(c("Seabirds","Seabirds","Seabirds cumulative"),2),
                                                                                rep(c("Ice Species","Ice Species","Ice Species cumulative"),2),
                                                                                rep(c("Provisioning Service","Provisioning Service","Provisioning Service cumulative"),2)),
                                                      "Perturbation"=sum_up,
                                                      "Scenario"=rep(c("Fg1/PA0","Fg0/PA1","Fg1/PA1","Fg-1/PA0","Fg0/PA-1","Fg-1/PA-1"),5))
  plot_results <- ggplot(crossed_pressures_Fishing_X_PetrolAct,aes(x=Impacted.vertice,y=Perturbation)) +
                  geom_col(aes(fill=Scenario),width = 0.6, position = position_nudge(x=c(0.2,0.2,-0.2,0.2,0.2,-0.2,0.2,0.2,-0.2,0.2,0.2,-0.2,0.2,0.2,-0.2,0.2,0.2,-0.2,0.2,0.2,-0.2,0.2,0.2,-0.2,0.2,0.2,-0.2,0.2,0.2,-0.2),
                                                                                     y=c(0,0.354724914,0,-0.136967372,0,0,0,0.354724914,0,-0.136967372,0,0,0,-0.321560510,0,0.004759919,0,0,0,-0.207731522,0,0.007622376,0,0,0,0.354724914,0,-0.136967372,0,0))) +
                  scale_fill_manual(values=c("#440154FF","#433E85FF","#2D708EFF","#1E9B8AFF","#51C56AFF","#FDE725FF"))+
                  theme(axis.text.x=element_text(angle=45))
  plot_results
  
  # same for fish X Marine Mammals
  
  MarineMammals <- subset(data.frame("value" = FCM_cros_press,"vertice"=rep(colnames(interaction_matrix_S),length(crossed_scenars))),vertice == "Marine Mammals")
  Icespecies <- subset(data.frame("value" = FCM_cros_press,"vertice"=rep(colnames(interaction_matrix_S),length(crossed_scenars))),vertice == "Ice Species")
  Shellfish <- subset(data.frame("value" = FCM_cros_press,"vertice"=rep(colnames(interaction_matrix_S),length(crossed_scenars))),vertice == "Shellfish and Other Bottom Living Animals")
  CulturalService <- subset(data.frame("value" = FCM_cros_press,"vertice"=rep(colnames(interaction_matrix_S),length(crossed_scenars))),vertice == "Cultural Service")
  
  
  sum_up_2 <- c(MarineMammals$value[7],
              MarineMammals$value[8],
              MarineMammals$value[9],
              MarineMammals$value[10],
              MarineMammals$value[11],
              MarineMammals$value[12],
              Icespecies$value[7],
              Icespecies$value[8],
              Icespecies$value[9],
              Icespecies$value[10],
              Icespecies$value[11],
              Icespecies$value[12],
              Shellfish$value[7],
              Shellfish$value[8],
              Shellfish$value[9],
              Shellfish$value[10],
              Shellfish$value[11],
              Shellfish$value[12],
              CulturalService$value[7],
              CulturalService$value[8],
              CulturalService$value[9],
              CulturalService$value[10],
              CulturalService$value[11],
              CulturalService$value[12])
  
  crossed_pressures_Fish_X_MarineMammals <- data.frame("Impacted vertice"= c(rep(c("Marine Mammals","Marine Mammals","Marine Mammals cumulative"),2),
                                                                            rep(c("Ice Species","Ice Species","Ice Species cumulative"),2),
                                                                            rep(c("Shellfish and Other Bottom Living Animals","Shellfish and Other Bottom Living Animals","Shellfish and Other Bottom Living Animals cumulative"),2),
                                                                            rep(c("Cultural Service","Cultural Service","Cultural Service cumulative"),2)),
                                                      "Perturbation"=sum_up_2,
                                                      "Scenario"=rep(c("F1/MM0","F0/MM1","F1/MM1","F-1/MM0","F0/MM-1","F-1/MM-1"),4))
  plot_results_2 <- ggplot(crossed_pressures_Fish_X_MarineMammals,aes(x=Impacted.vertice,y=Perturbation)) +
    geom_col(aes(fill=Scenario),width = 0.6, position = position_nudge(x=c(0.2,0.2,-0.2,0.2,0.2,-0.2,0.2,0.2,-0.2,0.2,0.2,-0.2,0.2,0.2,-0.2,0.2,0.2,-0.2,0.2,0.2,-0.2,0.2,0.2,-0.2),
                                                                       y = c(0,0.12941512,0,-0.95631994,0,0,0,0.01840738,0,-0.01778739,0,0,0,-0.36886784,0,0.02039941,0,0,0,0.35991200,0,-0.11452667,0,0))) + 
    scale_fill_manual(values=c("#440154FF","#433E85FF","#2D708EFF","#1E9B8AFF","#51C56AFF","#FDE725FF"))+
    theme(axis.text.x=element_text(angle=45))
  plot_results_2
  plot_grid(plot_results, plot_results_2, labels=c("A", "B"), ncol = 1, nrow = 2)
}