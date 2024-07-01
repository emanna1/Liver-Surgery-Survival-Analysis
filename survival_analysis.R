# Install and load required packages
install.packages(c("tidyverse", "ggplot2", "corrplot", "PerformanceAnalytics", "car", "formatR"))
library(tidyverse)
library(dplyr)
library(ggplot2)
library(formatR)
library(corrplot)
library(car)
library("PerformanceAnalytics")

knitr::opts_chunk$set(echo = TRUE)
knitr::opts_chunk$set(tidy.opts = list(width.cutoff = 60), tidy = TRUE)
options(tibble.width = Inf) # displays all columns.
options(tibble.print_max = Inf) # to show all the rows.


# Read the data from the working directory
Survival <- read_csv("surgical.csv")

# Relabel the variables; transform the response variable; convert gender to nominal
Mod_Survival <- Survival %>%   rename(Blood_Clotting_Score = "x1",
                                      Prognostic_Index = "x2",
                                      Enzyme_Score = "x3",
                                      Liver_Score = "x4",
                                      Age = "x5",
                                      Gender = "x6",
                                      Survival_Days = "y") %>%
  mutate(ln_Survival_Days = log(Survival_Days)) %>%
  mutate(Gender = as.factor(Gender))

## Distribution Analysis for Survival_Days and log(Survival_Days)

# Distribution plot of Survival Days
ggplot(Mod_Survival, aes(x=Survival_Days)) + 
  geom_histogram(aes(y=..density..), colour="black", fill="white")+
  geom_density(alpha=.2, fill="#FF6666")+
  ggtitle("Distribution of Survival Days")

# Distribution plot of log(Survival Days)
ggplot(Mod_Survival, aes(x=ln_Survival_Days)) + 
  geom_histogram(aes(y=..density..), colour="black", fill="white")+
  geom_density(alpha=.2, fill="#FF6666")+
  ggtitle("Distribution of log(Survival Days)")


## Get the Correlation Matrix

attach(Mod_Survival)
res <- cor(cbind(Blood_Clotting_Score, Prognostic_Index, Enzyme_Score, Liver_Score, Age, Gender, ln_Survival_Days))
round(res, 2)
corrplot(res, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)
col<- colorRampPalette(c("blue", "white", "red"))(20)
heatmap(x = res, col = col, symm = TRUE)
my_data <- Mod_Survival[, c(1,2,3,4,5, 8)]
chart.Correlation(my_data, histogram=TRUE, pch=19)
```

## Comparing Survival Times between Males and Females

ggplot(Mod_Survival, aes(x=Gender, y=ln_Survival_Days, fill=Gender)) +
  geom_boxplot() +
  ggtitle('Survival times by Gender') 


## The Model Build
Survival_OLS <- lm(ln_Survival_Days ~ ., data = Mod_Survival[,-7])
summary(Survival_OLS)

## Boxplot of the Residuals
ggplot(Survival_OLS, aes(x=1, y=rstudent(Survival_OLS))) + 
  geom_boxplot() + xlab(NULL) + ggtitle("Boxplot of the Residuals") + theme(axis.text.y = element_blank()) +
  coord_flip()


## Residual Analysis

# Normal Probability Plots and Check for Homoscedasticity
par(mfrow=c(2,2)) # Change the panel layout to 2 x 2
plot(Survival_OLS)
par(mfrow=c(1,1)) # Change back to 1 x 1

# Histogram of the Residuals
ggplot(Survival_OLS, aes(x=rstudent(Survival_OLS))) + geom_histogram() + ggtitle("Histogram of the Residuals") + xlab("Standardized Residuals")

## Residuals versus Predictor Plots

getPlots <- function(X, title) {
  return(ggplot(Mod_Survival, aes(x = X, y = rstudent(Survival_OLS))) + geom_point() + ggtitle("Residual versus ", title)) + xlab(title)
}
getPlots(Mod_Survival$Blood_Clotting_Score, 'Blood Clotting Score')
getPlots(Mod_Survival$Blood_Clotting_Score, 'Blood Clotting Score')
getPlots(Mod_Survival$Prognostic_Index, 'Prognostic Index')
getPlots(Mod_Survival$Enzyme_Score, 'Enzyme Score')
getPlots(Mod_Survival$Liver_Score, 'Liver Score')
getPlots(Mod_Survival$Age, 'Age')
getPlots(Mod_Survival$Gender, 'Gender')


## Get the Variance Inflation Factors

vif(Survival_OLS)


## Calculating the Predicted Value


predict(Survival_OLS, data.frame(Blood_Clotting_Score = 3,  Prognostic_Index= 40, Enzyme_Score = 50, Liver_Score = 2.6, Age = 30, Gender = "1"))


## Calculating the Confidence Interval for the Mean Response and Prediction

newdata = data.frame(Blood_Clotting_Score = 3,  Prognostic_Index= 40, Enzyme_Score = 50, Liver_Score = 2.6, Age = 30, Gender = "1")
predict(Survival_OLS, newdata, interval="predict") 
predict(Survival_OLS, newdata, interval="confidence")

