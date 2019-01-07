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
## The dataset contains 332 csv files with Date, Sulfate Level and Nitrate Level

## Calculates mean of pollutant (sulfate or nitrate) across specfied list
## of csv files (from monitors/ sensors)

pollutantmean   <-  function(directory, pollutant, id = 1:332){
    #####
    # Args:
    #       directory(charater vector) - location of csv files
    #       pollutant(charater vector) - sulfate/ nitrate
    #       id       (integer vector)  - range of monitor ids to be used
    # Returns:
    #       Mean of the pollutant across all monitors
    #       ignoring any missing values coded as NA
    #####
    filenames   <-  list.files(directory, pattern="*.csv", full.names=TRUE)
    ldf         <-  lapply(filenames, read.csv) #dataframes for each file
    sum         <-  0
    count       <-  0
    for (i in id) {
        # remove NA values in pollutant column
        temp    <-  ldf[[i]][pollutant][!is.na(ldf[[i]][pollutant])]
        count   <-  count + length(temp)
        sum     <-  sum + sum(temp)
    }
    sum/count #mean
}

## Calculates the count of complete records in each of the files

complete    <-  function(directory, id = 1:332){
    # Args:
    #       directory(charater vector) - location of csv files
    #       id       (integer vector)  - range of monitor ids to be used
    # Returns:
    #       (dataframes) Count of complete records(without missing data)
    #                    , for each file

    filenames   <-  list.files(directory, pattern="*.csv", full.names=TRUE)
    ldf         <-  lapply(filenames, read.csv) #dataframes for each file
    idvec       <-  NULL
    nobsvec     <-  NULL

    for (i in id) {
        # remove records where any record has NA values
        temp    <-  na.omit(ldf[[i]])
        count   <-  nrow(temp)
        idvec   <-  c(idvec, i)
        nobsvec <-  c(nobsvec, count)
    }
    data.frame(id = idvec, nobs = nobsvec) #create dataframe
}

## Calculates correlation between sulfate and nitrate values for
## monitors where the count of complete record is above the threshold

corr    <-  function(directory, threshold = 0){
    # Args:
    #       directory(charater vector) - location of csv files
    #       threshold(integer value)   - minimum count of complete observations
    #                                   from a monitor/ sensor in a file
    # Returns:
    #       (numeric vector) - correlations

    filenames   <-  list.files(directory, pattern="*.csv", full.names=TRUE)
    ldf         <-  lapply(filenames, read.csv) #dataframes for each file
    sulfatevec  <-  vector("numeric")
    nitratevec  <-  vector("numeric")
    corrvec     <-  NULL

    for (i in 1:length(ldf)) {
        # remove records where any record has NA values
        temp        <-  na.omit(ldf[[i]])
        count       <-  nrow(temp)

        if (count <= threshold){
            next
        }
        else{
            corrvec   <-  c(corrvec, cor(temp[["sulfate"]], temp[["nitrate"]]))
        }
    }

    corrvec #correlations vector
}
