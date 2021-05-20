inferenceFCM <- function(act.vec,lambda=2,h=0){
  scenario <- act.vec
  eps <- 1
  while (eps >= 1.0E-4){
    act.vec.t <- act.vec
    x <- t(w.mat)%*%act.vec + scenario
    act.vec <- 1/(1+exp(-lambda*(x+h)))
    eps <- sum(abs(act.vec-act.vec.t))  # display the difference between two time steps - should converge towards zero
  }
  rownames(act.vec) <- NULL
  return(act.vec[,1])
}
