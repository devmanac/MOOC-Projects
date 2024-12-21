rankall <- function(outcome, num = "best") {
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
	
	## For each state, find the hospital of the given rank
	## Return a data frame with the hospital names and the
	## (abbreviated) state name
	
	# Corresponding Columns - 30 day Death Rate Heart Attack:11,Heart Failure:17,Pneumonia:23
	outcomecolumns <- c(11, 17 , 23)
	outcomeColumnMap <- setNames(as.list(outcomecolumns), outcomes)
	columnOutcome <- outcomeColumnMap[outcome][[1]]
	
	orderedStates <- states[order(states[[1]]),]
	finalDataframe <- data.frame(hospital = NA, state = orderedStates)
	for (s in orderedStates) {
		#Get outcomes for the state
		stateCareMeasures <- subset(careMeasures, State==s)		
		#Rank hospital in state first with outcome and then with hospital name
		# Hospital Name: Column 2
		stateOrder <- stateCareMeasures[order(stateCareMeasures[[columnOutcome]], stateCareMeasures[[2]], na.last=NA),]		
		if (nrow(stateOrder) == 0) {			
			break
		}
		# num can be number, "best" or "worst"
		if(is.numeric(num)) {
			# when arg num is a number
			if (nrow(stateOrder) > num) {				
				finalDataframe[finalDataframe["state"] == s, "hospital"] <- stateOrder[num, 2]				
			} 
		} else if (num == "best") {
			finalDataframe[finalDataframe["state"] == s, "hospital"] <- stateOrder[1, 2]						
		} else if (num == "worst") {
			finalDataframe[finalDataframe["state"] == s, "hospital"] <- tail(stateOrder, n=1)[,2]			
		}		
	}
	return(finalDataframe)
}
