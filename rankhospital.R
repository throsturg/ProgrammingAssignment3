rankhospital <- function(state, outcome, num = "best") {
  ## Read outcome data
  data <- read.csv("outcome-of-care-measures.csv", na.string="Not Available", stringsAsFactors=FALSE)
  outcome.lookup <- c("heart attack"=11,"heart failure"=17, "pneumonia"=23)
  outcome.index <- outcome.lookup[outcome]
  if(is.numeric(num)) {
    rank <- num
  }
  else if (num == "best") {
    rank <- 1
  }
  else {
    rank <- length((data.state))
  }
  
  ## Check that state and outcome are valid
  ## Check that state and outcome are valid
  validState <- state %in% data[,7]
  if (!validState) {
    stop("invalid state")
  } 
  
  validOutcome <- outcome %in% names(outcome.lookup)
  if (!validOutcome) {
    stop("invalid outcome")
  }
  
  ## Return hospital name in that state with the given rank
  ## 30-day death rate
  data.state <- data[data$State==state,c(2,outcome.index)]
  data.state <- data.state[complete.cases(data.state),]
  data.state.length <- data.state[,1]
  
  if(is.numeric(num) && num <= data.state.length) {
    rank <- num
  }
  else if (num == "best") {
    rank <- 1
  }
  else if ( num == "worst" ){
    rank <- length(data.state.length)
    message(rank)
  }
  else {
    return(NA)
  }
  
  
  data.state[order(data.state[[2]],data.state[[1]]),][rank,1]
}