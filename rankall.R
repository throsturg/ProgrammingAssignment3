rankall <- function(outcome, num = "best") {
  ## Read outcome data
  data <- read.csv("outcome-of-care-measures.csv", na.string="Not Available", stringsAsFactors=FALSE)
  taoutcome.lookup <- c("heart attack"=11,"heart failure"=17, "pneumonia"=23)
  outcome.index <- outcome.lookup[outcome]
  
  ## Check that state and outcome are valid
  validOutcome <- outcome %in% names(outcome.lookup)
  if (!validOutcome) {
    stop("invalid outcome")
  }
  
  ## For each state, find the hospital of the given rank
  
  ## Return a data frame with the hospital names and the
  ## (abbreviated) state name
}
