library(tidyverse)
library(shiny)
library(shinythemes)

runExample("06_tabsets")



#read dataset 
read_data <- function(filename){
  obesity_df = read.csv(filename)
  return(obesity_df)
}

main <- function(){
  obesity_df = read.csv("ObesityDataSet_raw_and_data_sinthetic.csv")
}

