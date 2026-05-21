#Step 1: Load and inspect dataset

# Load Titanic dataset
titanic <- read.csv("C:/Users/nancy/Downloads/Titanic-Dataset.csv")


# Inspect dataset
str(titanic)
summary(titanic)
View(titanic)

# Check number of rows and columns
nrow(titanic)
ncol(titanic)

# Check column names
colnames(titanic)

# Check missing values
colSums(is.na(titanic))

# Check duplicated rows
sum(duplicated(titanic))


#Step 2: Select variables for analysis

# Select variables needed for research questions and modelling
titanic_data <- subset(titanic,
                       select = c(Survived,
                                  Pclass,
                                  Sex,
                                  Age,
                                  SibSp,
                                  Parch,
                                  Fare,
                                  Embarked))

str(titanic_data)
summary(titanic_data)
View(titanic_data)

# Check missing values in selected variables
colSums(is.na(titanic_data))


#Step 3: Treat missing values

# Replace missing Age values with median Age
titanic_data$Age[is.na(titanic_data$Age)] <- median(titanic_data$Age,
                                                    na.rm = TRUE)

# Remove blank Embarked rows
titanic_data <- subset(titanic_data, Embarked != "")

# Check missing values after cleaning
colSums(is.na(titanic_data))
summary(titanic_data)


#Step 4: Convert variables for analysis and modelling

# Convert categorical variables into factor type
titanic_data$Survived <- as.factor(titanic_data$Survived)
titanic_data$Pclass <- as.factor(titanic_data$Pclass)
titanic_data$Sex <- as.factor(titanic_data$Sex)
titanic_data$Embarked <- as.factor(titanic_data$Embarked)

str(titanic_data)
summary(titanic_data)


#Step 5: Research Question 1

# RQ1: Is survival associated with passenger sex?

# Create table for Sex and Survived
table(titanic_data$Sex)

table(titanic_data$Survived)

sex_survival_table <- table(titanic_data$Sex,
                            titanic_data$Survived)

sex_survival_table

# H0: Survival and Sex are independent
# H1: Survival and Sex are associated

chisq.test(sex_survival_table)

windows(20, 10)

par(mfrow = c(1, 3))

# Plot survival by sex
barplot(sex_survival_table,
        beside = TRUE,
        legend = TRUE,
        main = "Survival by Passenger Sex",
        xlab = "Survived",
        ylab = "Number of Passengers")


#Step 6: Research Question 2

# RQ2: Is survival associated with passenger class?

# Create table for Pclass and Survived
table(titanic_data$Pclass)

table(titanic_data$Survived)

class_survival_table <- table(titanic_data$Pclass,
                              titanic_data$Survived)

class_survival_table

# H0: Survival and Passenger Class are independent
# H1: Survival and Passenger Class are associated

chisq.test(class_survival_table)



# Plot survival by passenger class
barplot(class_survival_table,
        beside = TRUE,
        legend = TRUE,
        main = "Survival by Passenger Class",
        xlab = "Survived",
        ylab = "Number of Passengers")


#Step 7: Research Question 3

# RQ3: Is survival associated with embarkation port?

# Create table for Embarked and Survived
table(titanic_data$Embarked)

table(titanic_data$Survived)

embarked_survival_table <- table(titanic_data$Embarked,
                                 titanic_data$Survived)

embarked_survival_table

# H0: Survival and Embarked are independent
# H1: Survival and Embarked are associated

chisq.test(embarked_survival_table)


# Plot survival by embarkation port
barplot(embarked_survival_table,
        beside = TRUE,
        legend = TRUE,
        main = "Survival by Embarkation Port",
        xlab = "Survived",
        ylab = "Number of Passengers")


#Step 8: Research Question 4

# RQ4: Is Age different between survived and non-survived passengers?

# Boxplot for Age by Survived
windows(16, 8)

par(mfrow = c(1, 2))

boxplot(Age ~ Survived,
        data = titanic_data,
        main = "Age by Survival",
        xlab = "Survived",
        ylab = "Age")

# Check normality of Age for non-survived passengers
shapiro.test(titanic_data$Age[titanic_data$Survived == "0"])

# Check normality of Age for survived passengers
shapiro.test(titanic_data$Age[titanic_data$Survived == "1"])

# H0: Mean Age is equal between groups
# H1: Mean Age is different between groups

t.test(Age ~ Survived,
       data = titanic_data)

# Non-parametric test for Age difference
wilcox.test(Age ~ Survived,
            data = titanic_data)


#Step 9: Research Question 5

# RQ5: Does Fare differ across passenger classes?

# Boxplot for Fare by Pclass
boxplot(Fare ~ Pclass,
        data = titanic_data,
        main = "Fare by Passenger Class",
        xlab = "Passenger Class",
        ylab = "Fare")

# Check normality of Fare
shapiro.test(titanic_data$Fare)

# H0: Mean Fare is equal across passenger classes
# H1: Mean Fare is different across passenger classes

fare_class_model <- aov(Fare ~ Pclass,
                        data = titanic_data)

summary(fare_class_model)

# Non-parametric test for Fare difference
kruskal.test(Fare ~ Pclass,
             data = titanic_data)


#Step 10: Create numeric dataset for relationship checking

# Select numeric variables for relationship checking
titanic_relationships <- subset(titanic_data,
                                select = c(Survived,
                                           Pclass,
                                           Age,
                                           SibSp,
                                           Parch,
                                           Fare))

# Convert factor variables into numeric
titanic_relationships$Survived <- as.numeric(as.character(titanic_relationships$Survived))
titanic_relationships$Pclass <- as.numeric(as.character(titanic_relationships$Pclass))

# Remove missing values
titanic_relationships <- na.omit(titanic_relationships)

str(titanic_relationships)
summary(titanic_relationships)
colSums(is.na(titanic_relationships))


#Step 11: Examine initial relationships

# Load psych package
library(psych)

windows(20, 10)

# Create pairs panel plot
pairs.panels(titanic_relationships,
             smooth = FALSE,
             scale = FALSE,
             density = TRUE,
             ellipses = FALSE,
             method = "spearman",
             pch = 21,
             lm = FALSE,
             cor = TRUE,
             jiggle = FALSE,
             factor = 2,
             hist.col = 4,
             stars = TRUE,
             ci = TRUE)


#Step 12: Detailed scatter plots

windows(20, 12)
par(mfrow = c(3, 2))

# Survival and Passenger Class
scatter.smooth(x = titanic_relationships$Pclass,
               y = titanic_relationships$Survived,
               xlab = "Passenger Class",
               ylab = "Survived",
               main = "Correlation of Survival ~ Passenger Class")

# Survival and Age
scatter.smooth(x = titanic_relationships$Age,
               y = titanic_relationships$Survived,
               xlab = "Age",
               ylab = "Survived",
               main = "Correlation of Survival ~ Age")

# Survival and SibSp
scatter.smooth(x = titanic_relationships$SibSp,
               y = titanic_relationships$Survived,
               xlab = "SibSp",
               ylab = "Survived",
               main = "Correlation of Survival ~ SibSp")

# Survival and Parch
plot(titanic_relationships$Parch,
     titanic_relationships$Survived,
     xlab = "Parch",
     ylab = "Survived",
     main = "Correlation of Survival ~ Parch")

# Survival and Fare
scatter.smooth(x = titanic_relationships$Fare,
               y = titanic_relationships$Survived,
               xlab = "Fare",
               ylab = "Survived",
               main = "Correlation of Survival ~ Fare")


#Step 13: Examine categorical variables

# Frequency tables
table(titanic_data$Sex)

table(titanic_data$Embarked)

# Cross tables with survival
table(titanic_data$Sex,
      titanic_data$Survived)

table(titanic_data$Embarked,
      titanic_data$Survived)


#Step 14: Correlation analysis

# Correlation matrix
cor(titanic_relationships)

attach(titanic_relationships)

# Correlation for Survival and Passenger Class
paste("Correlation for Survival and Passenger Class: ",
      round(cor(titanic_relationships$Survived,
                titanic_relationships$Pclass), 2))

# Correlation for Survival and Age
paste("Correlation for Survival and Age: ",
      round(cor(titanic_relationships$Survived,
                titanic_relationships$Age), 2))

# Correlation for Survival and SibSp
paste("Correlation for Survival and SibSp: ",
      round(cor(titanic_relationships$Survived,
                titanic_relationships$SibSp), 2))

# Correlation for Survival and Parch
paste("Correlation for Survival and Parch: ",
      round(cor(titanic_relationships$Survived,
                titanic_relationships$Parch), 2))

# Correlation for Survival and Fare
paste("Correlation for Survival and Fare: ",
      round(cor(titanic_relationships$Survived,
                titanic_relationships$Fare), 2))


#Step 15: Check for outliers

windows(20, 10)
par(mfrow = c(3, 2))

# Boxplots for outlier checking
boxplot(titanic_relationships$Survived,
        main = "Survived")

boxplot(titanic_relationships$Pclass,
        main = "Passenger Class")

boxplot(titanic_relationships$Age,
        main = "Age")

boxplot(titanic_relationships$SibSp,
        main = "SibSp")

boxplot(titanic_relationships$Parch,
        main = "Parch")

boxplot(titanic_relationships$Fare,
        main = "Fare")


#Step 16: Generate exact outlier values

# Age outliers
outlier_values <- boxplot.stats(titanic_relationships$Age)$out
paste("Age outliers: ", paste(outlier_values, sep = ", "))

# SibSp outliers
outlier_values <- boxplot.stats(titanic_relationships$SibSp)$out
paste("SibSp outliers: ", paste(outlier_values, sep = ", "))

# Parch outliers
outlier_values <- boxplot.stats(titanic_relationships$Parch)$out
paste("Parch outliers: ", paste(outlier_values, sep = ", "))

# Fare outliers
outlier_values <- boxplot.stats(titanic_relationships$Fare)$out
paste("Fare outliers: ", paste(outlier_values, sep = ", "))


#Step 17: Check normality and skewness

# Load e1071 package
library(e1071)

windows(30, 20)
par(mfrow = c(2, 2))

# Density plot for Age
plot(density(titanic_relationships$Age),
     main = "Density Plot: Age",
     ylab = "Frequency",
     xlab = "Age",
     sub = paste("Skewness:",
                 round(e1071::skewness(titanic_relationships$Age), 2)))
polygon(density(titanic_relationships$Age), col = "red")

# Density plot for SibSp
plot(density(titanic_relationships$SibSp),
     main = "Density Plot: SibSp",
     ylab = "Frequency",
     xlab = "SibSp",
     sub = paste("Skewness:",
                 round(e1071::skewness(titanic_relationships$SibSp), 2)))
polygon(density(titanic_relationships$SibSp), col = "red")

# Density plot for Parch
plot(density(titanic_relationships$Parch),
     main = "Density Plot: Parch",
     ylab = "Frequency",
     xlab = "Parch",
     sub = paste("Skewness:",
                 round(e1071::skewness(titanic_relationships$Parch), 2)))
polygon(density(titanic_relationships$Parch), col = "red")

# Density plot for Fare
plot(density(titanic_relationships$Fare),
     main = "Density Plot: Fare",
     ylab = "Frequency",
     xlab = "Fare",
     sub = paste("Skewness:",
                 round(e1071::skewness(titanic_relationships$Fare), 2)))
polygon(density(titanic_relationships$Fare), col = "red")


#Step 18: Q-Q plots and Shapiro normality tests

#Q-Q plots

windows(16, 8)

par(mfrow = c(1, 2))

# Q-Q plot for Age
qqnorm(titanic_relationships$Age,
       main = "Q-Q Plot: Age")

qqline(titanic_relationships$Age,
       col = "red")

# Q-Q plot for Fare
qqnorm(titanic_relationships$Fare,
       main = "Q-Q Plot: Fare")

qqline(titanic_relationships$Fare,
       col = "red")

# Normality test for Age
shapiro.test(titanic_relationships$Age)

# Normality test for SibSp
shapiro.test(titanic_relationships$SibSp)

# Normality test for Parch
shapiro.test(titanic_relationships$Parch)

# Normality test for Fare
shapiro.test(titanic_relationships$Fare)


#Step 19: Transform skewed variable

# Transform Fare because it is skewed
titanic_data$Fare_new <- log(titanic_data$Fare + 1)

windows(16, 10)

# Histogram of transformed Fare
hist(titanic_data$Fare_new,
     main = "Histogram of Transformed Fare",
     xlab = "Fare_new")

# Normality test for transformed Fare
shapiro.test(titanic_data$Fare_new)


#Step 20: Build initial full predictive model

# Build logistic regression model
model_1 <- glm(Survived ~
                 Pclass +
                 Sex +
                 Age +
                 SibSp +
                 Parch +
                 Fare_new +
                 Embarked,
               data = titanic_data,
               family = binomial)

summary(model_1)

# Model fit statistics
AIC(model_1)

BIC(model_1)


#Step 21: Check multicollinearity

# Load faraway package
library(faraway)

# Check VIF values
v1 <- vif(model_1)
v1


#Step 22: Build revised model based on results

# Remove weaker variables and build revised model
model_2 <- glm(Survived ~
                 Pclass +
                 Sex +
                 Age +
                 Fare_new +
                 Embarked,
               data = titanic_data,
               family = binomial)

summary(model_2)

# Compare AIC values
AIC(model_1)
AIC(model_2)

# Compare BIC values
BIC(model_1)
BIC(model_2)

# Check VIF values for revised model
v2 <- vif(model_2)
v2


#Step 23: Select final model

# Select revised model as final model
final_model_full_data <- model_2

summary(final_model_full_data)

AIC(final_model_full_data)

BIC(final_model_full_data)

vif(final_model_full_data)


#Step 24: Training and testing data split

# Set seed to get same sample each time
set.seed(123)

nrow(titanic_data)

# Select 80% of records for training data
no_of_records <- sample(1:nrow(titanic_data),
                        0.8 * nrow(titanic_data))

str(no_of_records)

# Create training data
training_data <- titanic_data[no_of_records, ]
training_data
nrow(training_data)

# Create testing data
testing_data <- titanic_data[-no_of_records, ]
testing_data
nrow(testing_data)


#Step 25: Build final model on training data

# Build final model using training data
final_model <- glm(Survived ~
                     Pclass +
                     Sex +
                     Age +
                     Fare_new +
                     Embarked,
                   data = training_data,
                   family = binomial)

final_model

summary(final_model)

AIC(final_model)

BIC(final_model)


#Step 26: Predict survival using testing data

# Predict survival probability
survival_probability <- predict(final_model,
                                testing_data,
                                type = "response")

survival_probability

# Convert probabilities into predicted survival values
survival_predicted <- ifelse(survival_probability > 0.5,
                             1,
                             0)

survival_predicted


#Step 27: Actuals and predicted dataframe

# Create actuals and predicted dataframe
actuals_preds <- data.frame(cbind
                            (actuals = as.numeric(as.character(testing_data$Survived)),
                              predicted = survival_predicted,
                              probability = survival_probability))

actuals_preds

head(actuals_preds)


#Step 28: Model validation

# Confusion matrix
table(actuals_preds$actuals,
      actuals_preds$predicted)

# Accuracy
accuracy <- mean(actuals_preds$actuals ==
                   actuals_preds$predicted)

accuracy

# Correlation accuracy
attach(actuals_preds)

correlation_accuracy <- cor(actuals,
                            predicted)

correlation_accuracy


#Step 29: Residual checks

# Calculate residuals
model_residuals <- residuals(final_model)

# Residuals normally distributed
# H0: Residuals are normally distributed
shapiro.test(model_residuals)

# Residuals different from zero
# H0: Mean of residuals equal to zero
t.test(model_residuals,
       mu = 0)

# Randomness of residuals
library(lmtest)

dwtest(final_model)

# Check multicollinearity
library(faraway)

v_final <- vif(final_model)

v_final


#Step 30: Model forecasting

# Create new passenger details for forecasting
forecast_data <- data.frame(
  
  Pclass = factor(c(1, 3, 1, 3, 2),
                  levels = levels(titanic_data$Pclass)),
  
  Sex = factor(c("female", "male", "male", "female", "male"),
               levels = levels(titanic_data$Sex)),
  
  Age = c(25, 40, 5, 30, 60),
  
  Fare_new = log(c(100, 8, 80, 15, 30) + 1),
  
  Embarked = factor(c("C", "S", "S", "Q", "S"),
                    levels = levels(titanic_data$Embarked))
)

# Predict survival probability for new passengers
forecast_probability <- predict(final_model,
                                forecast_data,
                                type = "response")

forecast_probability

# Convert probability into survival prediction
forecast_prediction <- ifelse(forecast_probability > 0.5,
                              1,
                              0)

forecast_prediction

# Create forecast results dataframe
forecast_results <- data.frame(cbind
                               (forecast_data,
                                 survival_probability = forecast_probability,
                                 predicted_survival = forecast_prediction))

forecast_results


#Step 31: Save and load final model

# Save final model
saveRDS(final_model,
        "./titanic_survival_model.rds")

# Load final model
loaded_model <- readRDS("./titanic_survival_model.rds")

summary(loaded_model)

