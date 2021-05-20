inferenceQM <- function(scenario) {
  results <- 0
  for(i in 1:length(As)) {
    impact <- signum(drop(As[[i]]%*%scenario))
    results <- results + outer(impact,-1:1,'==')
  }
  if(length(results)>0) {
    prop <- results/length(As)
  }
  return(prop)
}
