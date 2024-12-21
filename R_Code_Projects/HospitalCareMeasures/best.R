best <- function(state, outcome) {
	## Read outcome data
	careMeasures <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
	#Convert columns to numeric for ranking
	careMeasures[, 11] <- as.numeric(careMeasures[, 11])
	careMeasures[, 17] <- as.numeric(careMeasures[, 17])
	careMeasures[, 23] <- as.numeric(careMeasures[, 23])
	
	## Check that state and outcome are valid
	# States
	states <- unique(careMeasures[7])	
	# Validation of the argument - state 
	if (!(state %in% states[,1])) {
		stop("invalid state")
	}
	# Accepted Outcomes
	outcomes <- c("heart attack", "heart failure", "pneumonia")
	# Validation of the argument - outcome 
	if (!(outcome %in% outcomes)) {
		stop("invalid outcome")
	}
	
	## Return hospital name in that state with lowest 30-day death rate
	# Corresponding Columns - 30 day Death Rate Heart Attack:11,Heart Failure:17,Pneumonia:23
	outcomecolumns <- c(11, 17 , 23)
	outcomeColumnMap <- setNames(as.list(outcomecolumns), outcomes)
	columnOutcome <- outcomeColumnMap[outcome][[1]]
	
	#Get outcomes for the state
	stateCareMeasures <- subset(careMeasures, State==state)
	#Order tows in state
	# Hospital Name: Column 2
	stateOrder <- stateCareMeasures[order(stateCareMeasures[[columnOutcome]], stateCareMeasures[[2]], na.last=NA),]
	return(stateOrder[1, 2])
}