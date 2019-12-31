##1 Plot the 30-day mortality rates for heart attack
##Read the outcome data into R via the read.csv function and look at the first few rows.
##> outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
##> head(outcome)
##There are many columns in this dataset. You can see how many by typing ncol(outcome) (you can see the number of rows with the nrow function). In addition, you can see the names of each column by typing names(outcome) (the names are also in the PDF document.
## To make a simple histogram of the 30-day death rates from heart attack (column 11 in the outcome dataset), run
# # > outcome[, 11] <- as.numeric(outcome[, 11])
## >You may get a warning about NAs being introduced; that is okay > hist(outcome[, 11])                                                                                                                                                                                                                   1
##Because we originally read the data in as character (by specifying colClasses = "character" we need to coerce the column to be numeric. You may get a warning about NAs being introduced but that is okay.
best <- function(state, outcome) {
  ## Read the outcome data
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  read_outcome_data   <- as.data.frame(cbind(
    data[, 2],   # hospital!
    data[, 7],   # state
    data[, 11],  # heart attack
    data[, 17],  # heart failure
    data[, 23]), # pneumonia
    stringsAsFactors = FALSE)
  colnames(read_outcome_data) <- c("hospital", "state", "heart attack", "heart failure", "pneumonia")
  
  ## Check that state and outcome are valid
  if(!state %in% read_outcome_data[, "state"]){
    stop('invalid state')
  } else if(!outcome %in% c("heart attack", "heart failure", "pneumonia")){
    stop('invalid outcome')
  } else {
    data_state <- which(read_outcome_data[, "state"] == state)
    extracting_data <- read_outcome_data[data_state, ]    # extracting data for the called state
    evaluated_outcome <- as.numeric(extracting_data[, eval(outcome)])
    min_outcome <- min(evaluated_outcome, na.rm = TRUE)
    result  <- extracting_data[, "hospital"][which(evaluated_outcome == min_outcome)]
    function_output  <- result[order(result)]
  }
  return(function_output)
}

rankhospital <- function(state, outcome, rank = "best"){
  ## Read outcome data
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  read_outcome_data  <- as.data.frame(cbind(data[, 2],  # hospital
                                            data[, 7],  # state
                                            data[, 11],  # heart attack
                                            data[, 17],  # heart failure
                                            data[, 23]), # pneumonia
                                      stringsAsFactors = FALSE)
  colnames(read_outcome_data) <- c("hospital", "state", "heart attack", "heart failure", "pneumonia")
  
  ## Check that state and outcome are valid
  if (!state %in%  read_outcome_data[, "state"]) {
    stop('invalid state')
  } else if (!outcome %in% c("heart attack", "heart failure", "pneumonia")){
    stop('invalid outcome')
  } else if (is.numeric(rank)) {
    data_state <- which( read_outcome_data[, "state"] == state)
    extracting_data <-  read_outcome_data[data_state, ]                     # extracting dataframe for the called state
    extracting_data[, eval(outcome)] <- as.numeric(extracting_data[, eval(outcome)])
    extracting_data <- extracting_data[order(extracting_data[, eval(outcome)], extracting_data[, "hospital"]), ]
    function_output <- extracting_data[, "hospital"][rank]
  } else if (!is.numeric(rank)){
    if (rank == "best") {
      function_output <- best(state, outcome)
    } else if (rank == "worst") {
      data_state <- which( read_outcome_data[, "state"] == state)
      extracting_data <-  read_outcome_data[data_state, ]    
      extracting_data[, eval(outcome)] <- as.numeric(extracting_data[, eval(outcome)])
      extracting_data <- extracting_data[order(extracting_data[, eval(outcome)], extracting_data[, "hospital"], decreasing = TRUE), ]
      function_output <- extracting_data[, "hospital"][1]
    } else {
      stop('invalid rank')
    }
  }
  return(function_output)
}


rankall <- function(outcome, num = "best"){
  ## Read outcome data
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  read_outcome_data   <- as.data.frame(cbind(data[, 2],  # hospital
                                             data[, 7],  # state
                                             data[, 11],  # heart attack
                                             data[, 17],  # heart failure
                                             data[, 23]), # pneumonia
                                       stringsAsFactors = FALSE)
  colnames(read_outcome_data) <- c("hospital", "state", "heart attack", "heart failure", "pneumonia")
  read_outcome_data[, eval(outcome)] <- as.numeric(read_outcome_data[, eval(outcome)])
  
  ## Check that state and outcome are valid
  
  if (!outcome %in% c("heart attack", "heart failure", "pneumonia")){
    stop('invalid outcome')
  } else if (is.numeric(num)) {
    bystate <- with(read_outcome_data, split(read_outcome_data, state))
    inorder  <- list()
    for (i in seq_along( bystate )){
      bystate [[i]] <-  bystate [[i]][order( bystate [[i]][, eval(outcome)], 
                                             bystate [[i]][, "hospital"]), ]
      inorder [[i]]  <- c( bystate [[i]][num, "hospital"],  bystate [[i]][, "state"][1])
    }
    result <- do.call(rbind, inorder )
    function_output<- as.data.frame(result, row.names = result[, 2], stringsAsFactors = FALSE)
    names(function_output) <- c("hospital", "state")
  } else if (!is.numeric(num)) {
    if (num == "best") {
      bystate  <- with(read_outcome_data, split(read_outcome_data, state))
      inorder   <- list()
      for (i in seq_along( bystate )){
        bystate [[i]] <-  bystate [[i]][order( bystate [[i]][, eval(outcome)], 
                                               bystate [[i]][, "hospital"]), ]
        inorder [[i]]  <- c( bystate [[i]][1, c("hospital", "state")])
      }
      result <- do.call(rbind, inorder )
      function_output <- as.data.frame(result, stringsAsFactors = FALSE)
      rownames(function_output) <- function_output[, 2]
    } else if (num == "worst") {
      bystate  <- with(read_outcome_data, split(read_outcome_data, state))
      inorder   <- list()
      for (i in seq_along( bystate )){
        bystate [[i]] <-  bystate [[i]][order( bystate [[i]][, eval(outcome)], 
                                               bystate [[i]][, "hospital"], 
                                               decreasing = TRUE), ]
        inorder [[i]]  <- c( bystate [[i]][1, c("hospital", "state")])
      }
      result <- do.call(rbind, inorder)
      function_output <- as.data.frame(result, stringsAsFactors = FALSE)
      rownames(function_output) <- function_output[, 2]
    } else {
      stop('invalid num')
    }
  }
  return(function_output)
}
