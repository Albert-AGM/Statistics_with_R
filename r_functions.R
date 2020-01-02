# ================================================================================
# Function checks all features in the data set and calculates the number of 
# NA's 
#
# Params: df         - Data set to evaluate NA's   
#         show       - show results
# Return: df         - changed data set  


CheckNA <- function( df , show = 1) {
  
  na_count <- sapply(df, function(x) sum(is.na(x)))
  data.na_count <- data.frame(na_count)
  data.merged <- data.frame(cbind(c(row.names(data.na_count)), data.na_count[, 1]))
  colnames(data.merged) <- c('feature', 'No_NA')
  
  data.na <- data.merged[data.merged$No_NA != 0,]
  
  if (show == 0) { 
    
  }
  
  if(show == 1){
    data.na
  }
  
  if(show == 2){
    cScheme <- dim(data.na)[1]
    ggplot(data = data.na, aes(x = data.na$feature, y = data.na$No_NA, fill = feature)) +
      geom_bar(stat = "identity") +
      xlab("feature") + ylab("Number of NA") +
      ggtitle("Number of NA's in dataset") +
      theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
      geom_text(aes(label = No_NA), vjust = 1.6, color = "black", position = position_dodge(0.9), size = 3) + 
      guides(FALSE) +
      scale_fill_manual(values = colorRampPalette(c("gray","steelblue" ))(cScheme))
  }
}


# ================================================================================
# function that maps a categorical values into its corresponding numeric value and 
# returns that column to the data frame
#
# Params: cols       - columns which need attention
#         map.param  - param to replace
#         df         - Data set to be changed   
#  
# Return: df         - changed data set   
map.fcn = function(cols, map.param, df){
  
  for (col in cols) {
    df[col] = as.character(df[,col])
    df[col] = as.numeric(map.param[df[,col]])
  }
  return(df)
}


# ================================================================================
# Function to clean empty fields by placing a deined value NA 
#
# Params: df         - Data set to clean    
#
# Return: df         - cleaned data set   

cleanEmptyFields <- function(df, term){
  
  feature.list <- colnames(df)
  for (feature in feature.list) {
    df[feature][df[feature] == ''] <- term
  }
  return(df)
}

# ================================================================================
# Function to create formula based on given data set 
# 
# Params: data       - Data set to evaluate formula   
#         label      - dependent label          
# Return: formula    - formula 
getFormula <- function(data = data, label = label) {
  
  # label Variable
  labelVar <- label
  
  # separate measure Variable y column from rest 
  groupVariables <- setdiff(colnames(data), list(label))
  
  # create formula for model 
  formula <- as.formula(paste(labelVar, paste(groupVariables, collapse = ' + '), sep = ' ~ '))
  
}


# ================================================================================
# Min/max normalization of numeric columns
#
# Params: x         - features to normalize   
# Return:           - normalized values
normalize <- function(x) {
  
  return((x - min(x)) / (max(x) - min(x)))
}


# ================================================================================
# getFormula  - will retrieve the formula based on a data set
# 
# Params: data     - dataset
#         label    - label 
# Return:          - formula
getFormula <- function(data = data, label = label) {
  
  # label Variable
  labelVar <- label
  
  # separate measure Variable y column from rest 
  groupVariables <- setdiff(colnames(data), list(label))
  
  # create formula for model 
  formula <- as.formula(paste(labelVar, paste(groupVariables, collapse = ' + '), sep = ' ~ '))
  
}


# ================================================================================
# prepNormalized_Df  - Extact and remove column of dependent variable from data set. 
#                      Column will be reattached after normalization of data set
# 
# Params: data     - dataset
#         feature  - dependent variable
# Return: data     - normalized restructured dataset
NormalizeDF <- function(data, feature){
  
  # Extact and remove column of dependent variable from data set. Column will be reattached after 
  # normalization of data set
  depVariable <- data.frame(data[feature])
  data[feature] <- NULL
  
  # Normalize the data set and attach price column again
  df.normalized <- normalize(data)
  df.normalized  <- cbind(df.normalized, depVariable)
  
  return(df.normalized)
}


# ================================================================================
# RMSE  - Calcuates the Root Mean Squared Error
# 
# Params: actual     - actual values
#         predicted  - predicted values 
# Return: rmse       - Root Mean Squared Error

rmse <- function(actual, predicted) {
  
  return(sqrt( mean((actual - predicted)^2)))
}

# ================================================================================
# CheckForOutliners  - Checks for outliners in the data set
#                      via resid 
# 
# Params: df         - data set
#         model      - proposed model 
# Return: df         - cleaned dataset
CheckForOutliners <- function(data, model){
  
  # evaluate the house with the max residuals 
  idx_outliner <- which(abs(resid(model)) == max(abs(resid(model))))
  idx_outliner <- unname(idx_outliner)
  
  data <- data[-c(idx_outliner), ]
  
  return(data)
}


# ================================================================================
# removeOutliners  - Removes outliners in the data set
# 
# Params: df         - data set
#         outliners  - outliners to remove 
# Return: df         - cleaned dataset
removeOutliners <- function(df, outliners){
  
  # based on our analysis we'll remove the most vicious 
  # outliners
  df <- df[-outliners, ]
  
  return(df)
}


# ================================================================================
# gg_pairs color function for lower regression parts
# 
# Return: gglplot       - return regression plot
ggpairs_lower <- function(data, mapping, ...) { 
  ggplot(data = data, mapping = mapping) + 
    geom_point(col = 'steelblue') + 
    geom_smooth(method = loess, fill = "red", color = "red", ...) +
    geom_smooth(method = lm, fill = "blue", color = "blue", ...)
  
}

# ================================================================================
# gg_pairs color function for diag density parts
# 
# Return: gglplot       - return density plot
ggpairs_diag <- function(data, mapping, ...){
  ggplot(data = data, mapping = mapping) + 
    geom_density(size = 1, colour = 'darkgreen')
  
}

