#################################################################################
#                                                                              #
# R Programming Assignment : Coursera                                          #
#                                                                              #
# Problem:  Data Analysis                                                      #
#                                                                              #
# Author:   Anubhav Singh                                                      #
#                                                                              #
# References:                                                                  #
#   1. https://www.coursera.org/learn/r-programming/supplement/                #
#       amLgW/programming-assignment-1-instructions-air-pollution              #                              #
#                                                                              #
################################################################################

###########
# DATASET #
###########
# The dataset contains 332 csv files with Date, Sulfate Level and Nitrate Level

# Calculates mean of pollutant (sulfate or nitrate) across specfied list
# of csv files (from monitors/ sensors)
pollutantmean <- function(directory, pollutant, id = 1:332){
    # Args:
    #       directory(charater vector) - location of csv files
    #       pollutant(charater vector) - sulfate/ nitrate
    #       id       (integer vector)  - range of monitor ids to be used
    # Returns:
    #       Mean of the pollutant across all monitors
    #       ignoring any missing values coded as NA

    filenames <- list.files(directory, pattern="*.csv", full.names=TRUE)
    ldf <- lapply(filenames, read.csv) #dataframes for each file

    sum <- 0
    count <- 0
    for(i in id){
        temp    <- ldf[[i]][pollutant][!is.na(ldf[[i]][pollutant])]
        count   <- count + length(temp)
        sum     <- sum + sum(temp)
    }
    sum/count #mean
}
ldf[[i]][!(is.na(ldf[[1]]["sulfate"]) | is.na(ldf[[1]]["nitrate"]))]
complete <- function(directory, id = 1:332){
    # Args:
    #       directory(charater vector) - location of csv files
    #       id       (integer vector)  - range of monitor ids to be used
    # Returns:
    #       (dataframes) Count of complete records(without missing data)
    #                    , for each file

    filenames <- list.files(directory, pattern="*.csv", full.names=TRUE)
    ldf <- lapply(filenames, read.csv) #dataframes for each file

    sum <- 0
    count <- 0
    for(i in id){
        temp    <- ldf[[i]][pollutant][(!is.na(ldf[[i]][pollutant]))]
        count   <- count + length(temp)
        sum     <- sum + sum(temp)
    }
    sum/count #mean

}

corr <- function(directory, threshold = 0){

}
