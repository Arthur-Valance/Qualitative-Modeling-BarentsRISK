plot_increasing_pressures <- function(FCM_inc_press) {
  c1  <- c()
  c2  <- c()
  c3  <- c()
  c4  <- c()
  c5  <- c()
  c6  <- c()
  c7  <- c()
  c8  <- c()
  c9  <- c()
  c10 <- c()
  c11 <- c()
  c12 <- c()
  c13 <- c()
  c14 <- c()
  c15 <- c()
  c16 <- c()
  pressures <- c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1)
  for (k in 0:10){
      c1  <- c(c1,FCM_inc_press[16*k+1])
      c2  <- c(c2,FCM_inc_press[16*k+2])
      c3  <- c(c3,FCM_inc_press[16*k+3])
      c4  <- c(c4,FCM_inc_press[16*k+4])
      c5  <- c(c5,FCM_inc_press[16*k+5])
      c6  <- c(c6,FCM_inc_press[16*k+6])
      c7  <- c(c7,FCM_inc_press[16*k+7])
      c8  <- c(c8,FCM_inc_press[16*k+8])
      c9  <- c(c9,FCM_inc_press[16*k+9])
      c10 <- c(c10,FCM_inc_press[16*k+10])
      c11 <- c(c11,FCM_inc_press[16*k+11])
      c12 <- c(c12,FCM_inc_press[16*k+12])
      c13 <- c(c13,FCM_inc_press[16*k+13])
      c14 <- c(c14,FCM_inc_press[16*k+14])
      c15 <- c(c15,FCM_inc_press[16*k+15])
      c16 <- c(c16,FCM_inc_press[16*k+16])
  }    
  increasing_pressures <- data.frame("Pressure applied on the Fishing compartment"= pressures,
                                     "Impact on Marine Transport" = c1,
                                     "Impact on Fishing" = c2,
                                     "Impact on Petroleum Activity" = c3,
                                     "Impact on Cruise Activity" = c4,
                                     "Impact on Pollution" = c5,
                                     "Impact on Regulations" = c6,
                                     "Impact on Area Conflict" = c7,
                                     "Impact on Non Indigenous Species" = c8,
                                     "Impact on Fish" = c9,
                                     "Impact on Seabirds" = c10,
                                     "Impact on Marine Mammals" = c11,
                                     "Impact on Ice Species" = c12,
                                     "Impact on Shellfish and Other Bottom Living Animals" = c13,
                                     "Impact on Supporting Service" = c14,
                                     "Impact on Cultural Service" = c15,
                                     "Impact on Provisioning Service" = c16)
  plot_inc_press <- ggplot(increasing_pressures,aes(x=pressures)) +
    geom_line(aes(y = c1, color = "#4C00FF")) +
    geom_line(aes(y = c2, color = "#0F00FF")) +
    geom_line(aes(y = c3, color = "#002EFF")) +
    geom_line(aes(y = c4, color = "#006BFF")) +
    geom_line(aes(y = c5, color = "#00A8FF")) +
    geom_line(aes(y = c6, color = "00E5FF")) +
    geom_line(aes(y = c7, color = "#00FF4D")) +
    geom_line(aes(y = c8, color = "#00FF00")) +
    geom_line(aes(y = c9, color = "#4DFF00")) +
    geom_line(aes(y = c10, color = "#99FF00")) +
    geom_line(aes(y = c11, color = "#E6FF00")) +
    geom_line(aes(y = c12, color = "#FFFF00")) +
    geom_line(aes(y = c13, color = "#FFEA2D")) +
    geom_line(aes(y = c14, color = "#FFDE59")) +
    geom_line(aes(y = c15, color = "#FFDB86")) +
    geom_line(aes(y = c16, color = "#FFE0B3"))
  plot_inc_press
  
}