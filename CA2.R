# Tim Browning - Data Mining CA2
# Due Date 25/04/2020
# Data Imputation


# Imports
if (!require("dplyr")) install.packages("dplyr")
library(dplyr)

if (!require("mice")) install.packages("mice")
library(mice)


# Remove working environment variables
rm(list=ls())

# Read CSV file
Dataset <- read.csv("Data2.csv", header=TRUE, sep =  ",")

# List all data & chec relevancy
str(Dataset, list.len=ncol(Dataset))

# Firsty colukmn name changed to Income
names(Dataset)[1] <- "Income"

DatasetSmall <- Dataset %>% select(c(Income, 
                          samptype,
                          borough,
                          sex,
                          r_age,
                          educ,
                          phones,
                          race,
                          single,
                          college,
                          assets,
                          ownhome,
                          mkthome,
                          owncar,
                          mktcar,
                          workmos,
                          workhrs,
                          poor
                          
))

# List relevant data
str(DatasetSmall, list.len=ncol(Dataset))

Data1 <- DatasetSmall %>% select(-c(Income))

#impute missing data to all the remaining variables  
imputed = complete(mice(Data1,m=5,maxit=50,meth='pmm',seed=500))
View(imputed)

Data2 <- Dataset

DatasetFinal <- imputed
DatasetFinal$Income <- Data2$Income
View(DatasetFinal)

#############################################################################################################################

# **Part 1**: The first step is to determine the missing values of “Income” that are actually equal to 0. To do this,
# you are required to define a new variable, say “Binary_Income” with 0, 1 and missing values, for the
# cases which have 0, non-zero and missing values in the “Income” variable, respectively.

#############################################################################################################################

Function1 <- function(x)
{
  num <- dim(length(x))
  num[which(x==0)] <- 0
  num[which(!x==0)] <- 1
  return(num)
}

Function2 <- function(x)
{
  num <- dim(length(x))
  num[which(is.na(x))] <- 0
  num[which(!is.na(x))] <- 1
  return(num)
}

# If observation has income enter 1 if the observation does not add 0
DatasetFinal$Binary_Income <- Function1(DatasetFinal$Income)

# Function which Adds a 0 where there is no value for income, Used for GLM(Part 2)
DatasetFinal$Binary_Income2<- Function2(DatasetFinal$Income)

DatasetFinal$Binary_Income <- as.factor(DatasetFinal$Binary_Income)
DatasetFinal$Binary_Income2 <- as.factor(DatasetFinal$Binary_Income2)

#str(DatasetFinal, list.len=ncol(Dataset))

#############################################################################################################################

# **Part 2**: Then use the cases with “Binary_Income” equal to either 0 or 1, to develop a logistic regression
#model.  

#############################################################################################################################

# Select all rows with Binary Income 0 or 1 and Remove columns "Income" + "Binary_Income2"
Data1 <- DatasetFinal[!is.na(DatasetFinal$Binary_Income), ] %>% select(-c(Income, Binary_Income2))
View(Data1)

#Logistic Regression
glm <- glm(Binary_Income~., data = Data1, family = 'binomial')
summary(glm) 

# Find Predictions
pred = predict(glm, type="response")

# Confusion matrix
table_matrix = table(Data1$Binary_Income, pred >= 0.5)
table_matrix

# accuracy of the model
accuracy <- sum(diag(table_matrix)) / sum(table_matrix)
accuracy

#############################################################################################################################

# **Part 3**: The outcome of this model would allow you to predict the missing values in the “Binary_Income” 
# variable, as 0 or 1.

#############################################################################################################################

prediction <- predict(glm, newdata=DatasetFinal, type = "response")

for(i in 1:nrow(DatasetFinal))
{
  if ( is.na(DatasetFinal$Binary_Income[i]) )
    
  { DatasetFinal$Binary_Income[i] = round(prediction[i], 0)
  }
}

count(DatasetFinal[is.na(DatasetFinal$Income),]) # Count Rows - 124
View(DatasetFinal[is.na(DatasetFinal$Income),])  # View all rows

#############################################################################################################################

# **Part 4**: For any case which “Binary_Income” is predicted as 0, put the actual “Income” equal to 0 as well. 

#############################################################################################################################


for(i in 1:nrow(DatasetFinal))
{
  if ( DatasetFinal$Binary_Income[i] == 0 )
  { 
    DatasetFinal$Income[i] = 0
  }
}
count(DatasetFinal[is.na(DatasetFinal$Income), ]) #Count Rows - 118
View(DatasetFinal[is.na(DatasetFinal$Income), ])  # View all rows

#############################################################################################################################

# **Part 5**: Those cases who their “Binary_Income” is predicted as 1, are used along with other cases with  
#nonzero “Income” to develop a linear regression model.

#############################################################################################################################

#Create a dataset with income values == 1, remove Binary_income columns
Data2 <- DatasetFinal[ DatasetFinal$Binary_Income == 1, ] %>% select(-c(Binary_Income, Binary_Income2))
View(Data2)


linearRegression = lm(Income~., data=Data2)  # linear regression

sumry <- summary(linearRegression) 
sumry

#############################################################################################################################

# **Part 6**: The outcome of such model would allow you to predict the missing values for the “Income” variable, 
#for the cases which their “Binary_Income” variable is predicted as 1 and therefore are expected to have 
#non-zero “Income”.  

#############################################################################################################################

alpha = 0.05
beta = 0

# For loop through dataset, if the coefficients is greater then alpah(0.05) the column us statistically significant
for (i in 1:length(Data2))
{
  if (sumry$coefficients[i,4] > alpha )
  { beta[i] = 0  }
  else
  { 
    if ( i == 1)
    { beta[i] = 1
    } else {
      beta[i] = sumry$coefficients[i,1]
    }
  } 
}

sumry

for(i in 1:nrow(DatasetFinal))
{
  if ( is.na(DatasetFinal$Income[i]) )
  { 
    DatasetFinal$Income[i] = beta[1]                     +
      beta[2]  * DatasetFinal$samptype[i] +
      beta[3]  * DatasetFinal$borough[i]  +
      beta[4]  * DatasetFinal$sex[i]      +
      beta[5]  * DatasetFinal$r_age[i]    +
      beta[6]  * DatasetFinal$educ[i]     +
      beta[7]  * DatasetFinal$phones[i]   +
      beta[8]  * DatasetFinal$race[i]     +
      beta[9]  * DatasetFinal$single[i]   +
      beta[10]  * DatasetFinal$college[i]  +
      beta[11]  * DatasetFinal$assets[i]   +
      beta[12]  * DatasetFinal$ownhome[i]   +
      beta[13]  * DatasetFinal$mkthome [i]   +
      beta[14]  * DatasetFinal$owncar [i]   +
      beta[15]  * DatasetFinal$mktcar [i]   +
      beta[16] * DatasetFinal$workmos[i]  +
      beta[17] * DatasetFinal$workhrs[i] +
      beta[18] * DatasetFinal$poor[i]
    
    # Set negative values to zero
    if (DatasetFinal$Income[i] < 0)
    { DatasetFinal$Income[i] = 0 }
  }
}


View(DatasetFinal)


#############################################################################################################################
#**Report 1**: In Doc - GLM


#############################################################################################################################


#############################################################################################################################

# **Report 2**: The number of cases with predicted “Binary_Income” as either 0 or 1.  

#############################################################################################################################

predicted_Binary_zero= DatasetFinal[ DatasetFinal$Binary_Income == 0 & DatasetFinal$Binary_Income2 == 0, ]
nrow(predicted_Binary_zero)

predicted_Binary_one  = DatasetFinal[ DatasetFinal$Binary_Income == 1 & DatasetFinal$Binary_Income2 == 0, ]
nrow(predicted_Binary_one)

#############################################################################################################################

# **Report 3**: The Mean and SD for the non-zero predicted “Income”. 

#############################################################################################################################

# Mean & standard deviation on entire dataset
mean = mean(DatasetFinal$Income[DatasetFinal$Income > 0])
mean
sd   = sd(DatasetFinal$Income[DatasetFinal$Income > 0])
sd

# Mean & standard deviation on entire predicted rows
mean = mean(DatasetFinal$Income[DatasetFinal$Income > 0 & DatasetFinal$Binary_Income2 == 0])
mean
sd   = sd(DatasetFinal$Income[DatasetFinal$Income > 0 & DatasetFinal$Binary_Income2 == 0])
sd
