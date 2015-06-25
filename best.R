best <- function(state, outcome) {
  ## Read outcome data
  data <- read.csv("outcome-of-care-measures.csv", colClass="character")
  outcome.lookup <- c("heart attack"=11,"heart failure"=17, "pneumonia"=23)
  
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
  
  return(validState)
}