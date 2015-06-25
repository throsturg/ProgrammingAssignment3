best <- function(state, outcome) {
  ## Read outcome data
  data <- read.csv("outcome-of-care-measures.csv", na.string="Not Available", stringsAsFactors=FALSE)
  outcome.lookup <- c("heart attack"=11,"heart failure"=17, "pneumonia"=23)
  outcome.index <- outcome.lookup[outcome]
  ## data[,outcome.index] <- as.numeric(data[,outcome.index])
  
  ## Check that state and outcome are valid
  validState <- state %in% data[,7]
  if (!validState) {
    stop("invalid state")
  } 
  
  validOutcome <- outcome %in% names(outcome.lookup)
  if (!validOutcome) {
    stop("invalid outcome")
  }
  
  ## Return hospital name in that state with lowest 30-day death rate
  data.state <- data[data$State==state,c(2,outcome.index)]
  data.state[which.min(data.state[[2]]),][[1]]
  
}