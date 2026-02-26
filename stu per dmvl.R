# Install if not already installed
install.packages(c("tidyverse", "caret", "corrplot", "ggplot2"))

# Load libraries
library(tidyverse)
library(caret)
library(corrplot)
library(ggplot2)

data <- read.csv("C:/Users/priya/Downloads/dataset/StudentsPerformance.csv")

# View structure
str(data)

# First few rows
head(data)
summary(data)
ggplot(data, aes(x = math.score)) +
  geom_histogram(fill = "skyblue", bins = 20) +
  theme_minimal()
ggplot(data, aes(x = reading.score)) +
  geom_histogram(fill = "lightgreen", bins = 20) +
  theme_minimal()
ggplot(data, aes(x = writing.score)) +
  geom_histogram(fill = "pink", bins = 20) +
  theme_minimal()
boxplot(data$math.score, main="Math Score Boxplot")
boxplot(data$reading.score, main="Reading Score Boxplot")
boxplot(data$writing.score, main="Writing Score Boxplot")
boxplot(data)
colSums(is.na(data))
Q1 <- quantile(data$math.score, 0.25)
Q3 <- quantile(data$math.score, 0.75)
IQR_value <- Q3 - Q1

lower <- Q1 - 1.5 * IQR_value
upper <- Q3 + 1.5 * IQR_value

outliers <- data$math.score[data$math.score < lower | data$math.score > upper]
outliers
data <- data[data$math.score > lower & data$math.score < upper, ]
outliers
data$gender <- as.factor(data$gender)
data$race.ethnicity <- as.factor(data$race.ethnicity)
data$parental.level.of.education <- as.factor(data$parental.level.of.education)
data$lunch <- as.factor(data$lunch)
data$test.preparation.course <- as.factor(data$test.preparation.course)
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

data$math_norm <- normalize(data$math.score)
data$reading_norm <- normalize(data$reading.score)
data$writing_norm <- normalize(data$writing.score)
data$math_std <- scale(data$math.score)
data$reading_std <- scale(data$reading.score)
data$writing_std <- scale(data$writing.score)
numeric_data <- data[, c("math.score", "reading.score", "writing.score")]
cor_matrix <- cor(numeric_data)

corrplot(cor_matrix, method="circle")
set.seed(123)

trainIndex <- createDataPartition(data$math.score, p = 0.8, list = FALSE)

trainData <- data[trainIndex, ]
testData <- data[-trainIndex, ]
