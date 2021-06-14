plot_models_comparison <- function(results){
  pos_pos <- subset(results, FCM_S > 0.05 & FCM_Model > 0.05) 
  share_pos_pos <- length(pos_pos$FCM_S)*100/length(results$FCM_S)
  
  pos_neg <- subset(results, FCM_S > 0.05 & FCM_Model < -0.05)
  share_pos_neg <- length(pos_neg$FCM_S)*100/length(results$FCM_S)
  
  neg_pos <- subset(results, FCM_S < -0.05 & FCM_Model > 0.05)
  share_neg_pos <- length(neg_pos$FCM_S)*100/length(results$FCM_S)
  
  neg_neg <- subset(results, FCM_S < -0.05 & FCM_Model < -0.05)
  share_neg_neg <- length(neg_neg$FCM_S)*100/length(results$FCM_S)
  
  zeros_pos <- subset(results, FCM_S >= -0.05 & FCM_S <= 0.05 & FCM_Model > 0.05)
  share_zeros_pos <- length(zeros_pos$FCM_S)*100/length(results$FCM_S)
  
  zeros_neg <- subset(results, FCM_S >= -0.05 & FCM_S <= 0.05 & FCM_Model < -0.05)
  share_zeros_neg <- length(zeros_neg$FCM_S)*100/length(results$FCM_S)
  
  pos_zeros <- subset(results, FCM_S > 0.05 & FCM_Model >= -0.05 & FCM_Model <= 0.05)
  share_pos_zeros <- length(pos_zeros $FCM_S)*100/length(results$FCM_S)
  
  neg_zeros <- subset(results, FCM_S < -0.05 & FCM_Model >= -0.05 & FCM_Model <= 0.05)
  share_neg_zeros  <- length(neg_zeros$FCM_S)*100/length(results$FCM_S)
  
  zeros_zeros   <- subset(results, FCM_S >= -0.05 & FCM_S <= 0.05 & FCM_Model >= -0.05 & FCM_Model <= 0.05)
  share_zeros_zeros <- length(zeros_zeros$FCM_S)*100/length(results$FCM_S)
  models_comparison <- ggplot(results,aes(x=FCM_S))+
    geom_point(aes(y = FCM_Model),color=results$Scenario)+
    geom_hline(yintercept=c(-0.05,0.05),col="firebrick4") +
    geom_vline(xintercept=c(-0.05,0.05),col="firebrick4") 
  models_comparison
  models_comparison <- models_comparison + annotate("text",x=c(-0.36,0.36,0,-0.36,0,0.36,-0.36,0,0.36),y=c(0.36,0.36,0.36,0,0,0,-0.36,-0.36,-0.36),
                                                                label=c(round(share_neg_pos,digits=1),round(share_pos_pos,digits=1),
                                                                        round(share_zeros_pos,digits=1),round(share_neg_zeros,digits=1),
                                                                        round(share_zeros_zeros,digits=1),round(share_pos_zeros,digits=1),
                                                                        round(share_neg_neg,digits=1),round(share_zeros_neg,digits=1),
                                                                        round(share_pos_neg,digits=1)),color="firebrick4",size=5)
  models_comparison <- models_comparison + annotate("text",x=c(0.75,0.75,0.75),y=c(1,0,-1),
                                     label=c(round(share_neg_neg + share_zeros_zeros + share_pos_pos, digits=1),
                                             round(share_neg_zeros + share_zeros_pos + share_pos_zeros + share_zeros_neg,digits=1),
                                             round(share_neg_pos + share_pos_neg)), color = "darkblue", size=6.5)
  models_comparison
  return(round(share_neg_neg + share_zeros_zeros + share_pos_pos, digits=1))
}
