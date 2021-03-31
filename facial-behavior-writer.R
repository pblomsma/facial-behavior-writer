####################################################################################################
# Author: Peter Blomsma
# Date: January 2019
# Description: Utilizes AU-encoded datasets to produce sequences of facial behavior. 
#
# Model is used for mutliple publications, including:
# Vaitonyte, Julija, et al. "Generating Facial Expression Data: Computational and Experimental Evidence." 
# Proceedings of the 19th ACM International Conference on Intelligent Virtual Agents. 2019.
# 
# This research has been funded by a grant No.: PROJ-007246 fromthe European Union, OP Zuid, the Ministry of
# Economic affairs,the Province of Noord-Brabant, and the municipality of Tilburg awarded to Max M. Louwerse.
####################################################################################################
# Contents:
# 0. Libraries and constants.
# 
# 2. Functions to initiate Markov model
# 3. Functions to generate sequences based on Markov model
# 
# 5. Main code to generate test set.
####################################################################################################

####################################################################################################
# 0. Libraries and constants.
####################################################################################################
library(reshape)
library(reshape2)
library(stringr)
library(dplyr)
library(ggplot2)
library(sjPlot)
library(data.table)
library(prodlim)

####################################################################################################
# 2. Functions to initiate Markov model
####################################################################################################
DeriveStateIndex <-  function(dataset)
{
  # Args:
  #  dataset: Facial Expression Dataset that contains a Face_state_id column.
  #
  # Returns: 
  #    A Factor with all unique facial expressions.
  return(sort(unique(dataset$Face_state_id)))
}

CreateTransitionCountMatrix <-  function(dataset, state.index)
{
  # Counts the transitions from each facial configuration to all other facial configurations within a Sequence.
  #
  # Args:
  #  dataset: Facial Expression Dataset that contains a Face_state_id column and Sequence_id column.
  #  state.index: List of all distinct facial expressions that the dataset contains.
  # Returns: 
  #    A square dataframe with as many dimensions as the state.index list length.
  
  transitionMatrix.count <- data.frame(matrix(0, ncol = length(state.index), nrow = length(state.index)))
  
  for (row in 1:(nrow(dataset) - 1)) 
  {
    if (dataset[row,]$Subject_id == dataset[row + 1,]$Subject_id)
    {
      m <- which(state.index == dataset[row, ]$Face_state_id)
      n <- which(state.index == dataset[row + 1, ]$Face_state_id)
      transitionMatrix.count[m , n] <- transitionMatrix.count[m , n] + 1 
    }
  }
  return(transitionMatrix.count)
}

CreateProbabilityTransitionMatrix <-  function(transitionMatrix.count)
{
  # Converts counts to probabilities.
  #
  # Args:
  #  transitionMatrix.count: A square dataframe with as many dimensions.
  # Returns: 
  #    A square dataframe with as many dimensions as the state.index list length.
  
  transitionMatrix.prob <- transitionMatrix.count
  for (m in 1:nrow(transitionMatrix.count)) 
  {
    rowsum <- sum(transitionMatrix.count[m,])
    
    for (n in 1:ncol(transitionMatrix.count)) 
    {
      transitionMatrix.prob[m, n] <- transitionMatrix.count[m,n] / rowsum
      
      if (is.nan(transitionMatrix.prob[m, n]))
      { 
        transitionMatrix.prob[m, n] <- .Machine$double.eps #Smallest positive number.
      }
    }
  }
  return(transitionMatrix.prob)
}

####################################################################################################
# 3. Functions to generate sequences based on Markov model
####################################################################################################
GenerateSamplePath <-  function(transitionMatrix.prob, state.index, state.start = 1, path.length = 100)
{
  # Randomly generates a sequence of facial expressions based on the probability matrix.
  #
  # Args:
  #  transitionMatrix.prob: Transitional probabilities for the facial expressions.
  #  state.index: List of all distinct facial expressions that the dataset contains.
  #  state.start: the state from which the sequence starts. (not part of the resulting sequence)
  #  length: Length of sequence that is going to be generated. 
  # Returns: 
  #    A vector with values from state.index based on the probabilities supplied in transitionMatrix.prob.

  
  
  state.index <-  as.character(state.index)
  path <- vector(mode = "character", length = path.length)
  state.current = state.start
  
  for (pointer in 1:length(path))
  {
    state.value <- sample(state.index, 1, prob = transitionMatrix.prob[state.current, ], replace = TRUE)
    state.current <- grep(state.value, state.index)
    path[pointer] <- state.index[state.current]
  }
  return(path)
}

SubsequenceOf <- function(path, dataset)
{
  # Methods finds the path as a subsequence of the dataset. 
  #
  # Returns:
  # Subsequence of dataset that describes path if available. If not, NA is returned
  
  dataset$row_num <- seq.int(nrow(dataset))
  starting_positions <- dataset[which(dataset$Face_state_id == path[1]),]
  
  for (a in 1:nrow(starting_positions))
  {
    starting_positions.current <- starting_positions[a,]
    
    for (i in 2:length(path))
    {
      if ((starting_positions.current$row_num + i - 1) > nrow(dataset))
      {
        return (NA) #end of dataset, no results.    
      }
      else if(path[i] != dataset[starting_positions.current$row_num + i - 1, ]$Face_state_id)
      {
        break;
      }
      else if(i == length(path))
      {
        return(dataset[starting_positions.current$row_num:(starting_positions.current$row_num + i - 1),])
      }
    }
  }
  return (NA)
}

####################################################################################################
# 4. Functions to convert sequences to movies.
####################################################################################################

ConvertPathToFrames <-  function(path, dataset)
{
  # Converts a list of facial expressions to actual frames from the dataset. 
  #
  # Args:
  #  path: a vector with facial expressions encoded in the same format as the dataset's Face_state_id column
  #  dataset: dataframe in which each row describes the facial expression of one movie frame.
  #
  # Returns: 
  #    Data frame that describes the sequence as supplied by path with corresponing pictures.  
  
  
  #Little trick to get the correct column names for the return value.
  path.frames <- sample_n(dataset, 1)[-1,]
  
  for (i in 1:length(path))
  {
    current <- sample_n((dataset %>% filter(Face_state_id == path[i])), 1)
    path.frames <- rbind(path.frames, current)
  }
  return(path.frames)
}

SelectRandomSubsequence <-  function(dataset, length)
{
  # Selects a random subsequence from the dataset of specific length. 
  #
  # Args:
  #  dataset: dataframe in which each row describes the facial expression of one movie frame.
  #  length: length of subsequence. 
  
  # Returns: 
  #    Random subsequence of dataset. If length is smaller than dataset length, it return the whole dataset.
  if(length > nrow(dataset))
  {
    return(dataset)
  }
  
  start <- sample(1:(nrow(dataset) - length), 1)
  return(dataset[start:(start + length - 1), ])
}


####################################################################################################
# 5. Main code to generate test set.
####################################################################################################
# Requirements:
# Have a AU dataset variable called subset, which contains a column named Face_state_id, that 
# represents the specific AU configuration for that state.
####################################################################################################

matrix.index <-  DeriveStateIndex(subset)
matrix.count <-  CreateTransitionCountMatrix(subset, matrix.index)
matrix.prob <- CreateProbabilityTransitionMatrix(matrix.count)

sample.path <- GenerateSamplePath(matrix.prob, matrix.index, state.start = 1)
sample.frames <- ConvertPathToFrames(sample.path, subset)