################################################################################
#                                                                              #
# R Programming Assignment : Coursera                                          #
#                                                                              #
# Problem:  Evaluation of hospitals using national data                        #
#                                                                              #
# Author:   Anubhav Singh                                                      #
#                                                                              #
# References:                                                                  #
#   1. https://www.coursera.org/learn/r-programming/supplement/w1c7p/          #
#       programming-assignment-3-instructions-hospital-quality                 #                              #
#                                                                              #
################################################################################
###########
# DATASET #
###########
## Dataset obtained from https://hospitalcompare.hhs.gov/
## The purpose of the web site is to provide data and information about the
## quality of care at over 4,000 Medicare-certified hospitals in the U.S.

## The functions in this code are used for following analysis
## 1. Plot the 30-day mortality rates for heart attack
## 2. Finding the best hospital in a state
## 3. Ranking hospitals by outcome in a state
## 4. Ranking hospitals in all states

## The followiung function can be used to plot histogram using a numeric column
## in the csv file passed as argument
histPlotMortalityRate   <-  function(file, columnnum) {
    #####
    # Args:
    #       file(charater vector)       -   file path
    #       colClasses(numeric value)   -   column number
    # Returns:
    #       generates histogram of the column values
    #####

    data                <-  read.csv(file, colClasses = "character")
    data[,columnnum]    <-  as.numeric(data[,columnnum])
    hist(data[,columnnum])
}

## Determinines the best hospital on the basis of minimum mortality rate
best    <-  function(file, state, outcome) {
    #####
    # Args:
    #       file(character vector)      -   file path
    #       state(charater vector)      -   a state within USA(abbreviated)
    #       outcome(charater vector)    -   "heart attack"/"heart failure"/
    #                                       "pneumonia"
    # Returns:
    #       Hospital(character vector)  - hospital with lowest mortality rate
    #####

    ## read the file
    data        <-  read.csv(file, colClasses = "character")
    ailments    <-   c("heart attack", "heart failure", "pneumonia")

    ## Check that state and outcome are valid
    if(!(state %in% unique(data[,"State"]))) {
        stop("invalid state")
    }
    if(!(outcome) %in% ailments) {
        stop("invalid outcome")
    }

    ## Return hospital name in that state with lowest 30-day death rate
    {
        if(outcome == "heart attack"){
            valcolumn   <- paste("Hospital.30.Day.Death..Mortality",
                            "..Rates.from.Heart.Attack", sep="")
            coldata     <-  split(data[, c(valcolumn
                                    ,"Hospital.Name")],data$State)
        }
        else if(outcome == "heart failure"){
            valcolumn   <-  paste("Hospital.30.Day.Death..Mortality",
                            "..Rates.from.Heart.Failure", sep="")
            coldata     <-  split(data[, c(valcolumn
                                    ,"Hospital.Name")],data$State)
        }
        else if(outcome == "pneumonia"){
            valcolumn   <-  paste("Hospital.30.Day.Death..Mortality",
                            "..Rates.from.Pneumonia", sep="")
            coldata     <-  split(data[, c(valcolumn
                                    ,"Hospital.Name")],data$State)
        }
        statedata               <-  coldata[[state]]
        statedata[,valcolumn]   <-  as.numeric(statedata[,valcolumn])
        ## find minimum value in the column
        minval                  <-  min(statedata[,valcolumn], na.rm = TRUE)
        ## tie breaking
        hnames  <-  statedata[,"Hospital.Name"][which(statedata[,
                                                valcolumn] == minval)]
        sort(hnames)[[1]]
    }
}

## Ranks the hospitals on the basis of minimum mortality rate, then returns
## the hostipal name corresponding to the rank argument passed to the function
rankhospital    <-  function(file, state, outcome, num = "best") {
    #####
    # Args:
    #       file(character vector)      -   file path
    #       state(charater vector)      -   a state within USA(abbreviated)
    #       outcome(charater vector)    -   "heart attack"/"heart failure"/
    #                                       "pneumonia"
    # Returns:
    #       Hospital(character vector)  - hospital with lowest mortality rate
    #####

    ## read the file
    data        <-  read.csv(file, colClasses = "character")
    ailments    <-   c("heart attack", "heart failure", "pneumonia")

    ## Check that state and outcome are valid
    if(!(state %in% unique(data[,"State"]))) {
        stop("invalid state")
    }
    if(!(outcome) %in% ailments) {
        stop("invalid outcome")
    }

    ## Return hospital name in that state with the given rank 30-day death rate
    {
        if(outcome == "heart attack"){
            valcolumn   <- paste("Hospital.30.Day.Death..Mortality",
                            "..Rates.from.Heart.Attack", sep="")
            coldata     <-  split(data[, c(valcolumn
                                    ,"Hospital.Name")],data$State)
        }
        else if(outcome == "heart failure"){
            valcolumn   <-  paste("Hospital.30.Day.Death..Mortality",
                            "..Rates.from.Heart.Failure", sep="")
            coldata     <-  split(data[, c(valcolumn
                                    ,"Hospital.Name")],data$State)
        }
        else if(outcome == "pneumonia"){
            valcolumn   <-  paste("Hospital.30.Day.Death..Mortality",
                            "..Rates.from.Pneumonia", sep="")
            coldata     <-  split(data[, c(valcolumn
                                    ,"Hospital.Name")],data$State)
        }
        statedata               <-  coldata[[state]]
        statedata[,valcolumn]   <-  as.numeric(statedata[,valcolumn])
        statedata               <-  na.omit(statedata)

        ## order the records on the valcolmn and hospital name (in that order)
        statedata               <-  statedata[order(statedata[,valcolumn],
                                        statedata[,"Hospital.Name"]),]
        if(num == "best") {
            head(statedata[,"Hospital.Name"],n=1)
        }
        else if(num == "worst") {
            tail(statedata[,"Hospital.Name"],n=1)
        }
        else if(class(num) == "numeric" & length(num) == 1){
            if(num > nrow(statedata)){
                return("NA")
            }
            statedata[,"Hospital.Name"][[num]]
        }
        else{
            stop("invalid rank")
        }
    }
}

##
rankall <-  function(file, outcome, num = "best") {
    #####
    # Args:
    #       file(character vector)      -   file path
    #       state(charater vector)      -   a state within USA(abbreviated)
    #       outcome(charater vector)    -   "heart attack"/"heart failure"/
    #                                       "pneumonia"
    # Returns:
    #       Hospital(character vector)  - hospital with lowest mortality rate
    #####

    ## read the file
    data        <-  read.csv(file, colClasses = "character")
    ailments    <-   c("heart attack", "heart failure", "pneumonia")

    ## Check that state and outcome are valid
    if(!(outcome) %in% ailments) {
        stop("invalid outcome")
    }

    ## For each state, find the hospital of the given rank
    ## Return hospital name in that state with the given rank 30-day death rate
    {
        if(outcome == "heart attack"){
            valcolumn   <- paste("Hospital.30.Day.Death..Mortality",
                            "..Rates.from.Heart.Attack", sep="")
        }
        else if(outcome == "heart failure"){
            valcolumn   <-  paste("Hospital.30.Day.Death..Mortality",
                            "..Rates.from.Heart.Failure", sep="")
        }
        else if(outcome == "pneumonia"){
            valcolumn   <-  paste("Hospital.30.Day.Death..Mortality",
                            "..Rates.from.Pneumonia", sep="")
        }

        data[,valcolumn]    <-  as.numeric(data[,valcolumn])
        data                <-  na.omit(data)

        ## order the records on the valcolmn and hospital name (in that order)
        if(num == "best") {
            statedata       <-  by(data, data[,"State"],
                                function(x) head(x[order(x[,valcolumn],
                                            x[,"Hospital.Name"]),],
                                            n=1)[c("Hospital.Name","State")])
        }
        else if(num == "worst") {
            statedata       <-  by(data, data[,"State"],
                                function(x) tail(x[order(x[,valcolumn],
                                            x[,"Hospital.Name"]),],
                                            n=1)[c("Hospital.Name","State")])
        }
        else if(class(num) == "numeric" & length(num) == 1){
            statedata       <-  by(data, data[,"State"],
                                    function(x) {
                                        if(num > nrow(x)){
                                            return(c("<NA>",
                                            unique(x[,"State"])))
                                        }
                                        x[order(x[,valcolumn],
                                        x[,"Hospital.Name"]),][num,
                                        ][c("Hospital.Name","State")]
                                    }
                                )
        }
        else{
            stop("invalid rank")
        }
    }

    ## Return a data frame with the hospital names and the
    ## (abbreviated) state name
    df        <-  do.call(rbind, statedata)
    # assign names to the columns
    names(df) <-  c("hospital", "state")
    df
}
