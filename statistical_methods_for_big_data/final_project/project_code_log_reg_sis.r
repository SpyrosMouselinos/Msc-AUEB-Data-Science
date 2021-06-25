##################################    1. Libraries    ###########################################
#                                                                                               #
#                                                                                               #
#                                                                                               #

rm(list = ls())
#Sys.setlocale("LC_TIME", "C")
options(scipen=10000)

if ("ggplot2" %in% rownames(installed.packages()) == F){
  install.packages("ggplot2")}
require(ggplot2)
if ("dplyr" %in% rownames(installed.packages()) == F){
  install.packages("dplyr")}
require(dplyr)
if ("readxl" %in% rownames(installed.packages()) == F){
  install.packages("readxl")}
require(readxl)


##################################    2. Load Data    ###########################################
#                                                                                               #
#                                                                                               #
#                                                                                               #

# Set working directory
setwd("C:/Users/sofia.baltzi/OneDrive - Accenture/Desktop/Statistics for BD/Group Project - SIS")

# Read data
data = read_excel("alldata together.xls")

##################################    3. Data Manipulation    ###################################
#                                                                                               #
#                                                                                               #
#                                                                                               #

# Rename 2 first columns and remove first line because it is empty
data = data %>% 
  dplyr::rename(`Systematic name` = 1,
                `Gene name` = 2) %>% 
  dplyr::filter(row_number()!=1) 

#data = data[2:nrow(data),3:ncol(data)]

# Replace character "Nan" with NA
data[data=="NaN"] = NA

# Count NA values per column (observation)
na_count = colSums(is.na(data)) %>% data.frame()

# Extract life expectancy (and y), age and erp from data labels
life_expectancy = str_extract(colnames(data), ">5|<5")
y = gsub(">5", 1, life_expectancy)
y = gsub("<5", 0, y)
y = as.numeric(y)

age = as.numeric(sub(".*?age .*?(\\d+).*", "\\1", colnames(data)))

erp_str = str_extract(str_extract(colnames(data), "erp no|erp yes"),"no|yes")
erp = gsub("yes", 1, erp_str)
erp = gsub("no", 0, erp)
erp = as.numeric(erp)


# Combine life_expectancy, age, erp and the transposed data
data = t(data) %>% data.frame()
data = cbind(y, age, erp, data)

# note: drop rows Systematic name and Gene name?
# if yes, delete rows 44-47 and uncomment row 49