plot_QM_VS_FCM <- function(results){
  #Comparability of the methods
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
  #plot comparison
  plot_comparison_methods <- ggplot(results,aes(x=FCM)) +
    geom_point(aes(y=QM),color=results$Scenario,size=2) +
     xlim(-0.6,0.6) +
     ylim(0,1) +
     geom_hline(yintercept=c(0.45,0.55),col="firebrick4") +
     geom_vline(xintercept=c(-0.025,0.025),col="firebrick4") 
  
  plot_comparison_methods
  
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
  
}
