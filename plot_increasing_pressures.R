plot_increasing_pressures <- function(FCM_inc_press,scenars) {
  increasing_pressures <- data.frame("Pressure applied"= rep(pressures,each = length(interaction_matrix)),
                                     "Impacted compartment" = rep(colnames(interaction_matrix),length(scenars)),
                                     "Value of the impact" = FCM_inc_press)
  plot_inc_press <- ggplot(increasing_pressures,aes(x=Pressure.applied)) +
    geom_line(aes(y = Value.of.the.impact, color = Impacted.compartment)) 
    scale_color_discrete(name = "Impacted Vertice", labels = c("Marine Transport", "Regulations","Petroleum Activity","Cruise Activity","Pollution","Fishing",
                                                               "Shellfish and Other Bottom Living Animals","Non Indigenous Species","Fish","Seabirds","Marine Mammals","Ice Species",
                                                               "Area Conflict","Supporting Service","Cultural Service","Provisioning Service"))
  plot_inc_press
  
}

