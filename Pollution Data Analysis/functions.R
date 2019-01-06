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

    for(i in id){
        
    }

}

complete <- function(directory, id = 1:332){

}

corr <- function(directory, threshold = 0){

}
