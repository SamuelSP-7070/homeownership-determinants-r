---

# ============================================================
# Project: Determinants of Homeownership (ACS 2022 / IPUMS)
# Author: Samuel Pulapakuri
# Purpose:
#   Clean ACS microdata and model homeownership using logistic
#   regression. Outputs saved to /outputs.
#
# Reproducibility:
#   Raw data not included due to file size limits.
#   See: data/raw/DATA_INSTRUCTIONS.md
# ============================================================

# -----------------------------
# 1) Libraries
# -----------------------------
library(tidyverse)
library(caret)
library(pROC)

set.seed(123)

# -----------------------------
# 2) Paths
# -----------------------------
raw_path  <- "data/raw/acs_homeownership_2022.csv"
proc_path <- "data/processed/homeownership_clean.csv"

plot_dir  <- "outputs/plots"
table_dir <- "outputs/tables"

dir.create("data/processed", recursive = TRUE, showWarnings = FALSE)
dir.create(plot_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(table_dir, recursive = TRUE, showWarnings = FALSE)

# -----------------------------
# 3) Load data (local only)
# -----------------------------
if (!file.exists(raw_path)) {
  stop(
    "Raw data not found.\n",
    "Please follow data/raw/DATA_INSTRUCTIONS.md\n",
    "Expected file: data/raw/acs_homeownership_2022.csv"
  )
}

df <- read.csv(raw_path, stringsAsFactors = FALSE)

title: "BDA 610 - Advanced Statistics Project"
author: "Group 4: Sharmeen Rahimani, Myat Aye, Rock Chen, Samuel Subhakar"
date: "2025-02-21"
output: html_document
---

### Data Cleaning

```{r}
# Loading the Data
file_path <- "~/Downloads/usa_00004.csv"
orig_income_data <- read.csv(file_path)
```

```{r}
#Dropping Irrelevant Variables 
orig_income_data <- orig_income_data[, !(names(orig_income_data) %in% c("YEAR", "SAMPLE", "SERIAL", "PERNUM", "OWNERSHPD", "RACED", "EDUCD", "CLASSWKRD", "IND", "MIGRATE1", "MIGRATE1D", "MORTGAGE"))]

```

```{r}
#Assigning 9999999 to NA
orig_income_data$FTOTINC[orig_income_data$FTOTINC == 9999999] <- NA
orig_income_data$INCTOT[orig_income_data$INCTOT == 9999999] <- NA
```

The IQR method was utilized to remove the outliers.

```{r}
# Function to filter outliers based on IQR method
remove_outliers<-function(data, column) {
  Q1 <- quantile(data[[column]], 0.25, na.rm = TRUE)
  Q3 <- quantile(data[[column]], 0.75, na.rm = TRUE)
  IQR_value <- IQR(data[[column]], na.rm = TRUE)
  
# Filter out values outside of Q1 - 1.5*IQR and Q3 + 1.5*IQR
  data %>%
    filter((data[[column]] >= (Q1 - 1.5 * IQR_value)) &
           (data[[column]] <= (Q3 + 1.5 * IQR_value)))
}

# Remove the outliers 
suppressPackageStartupMessages(library(dplyr))
income_IQR<-orig_income_data %>% remove_outliers("INCTOT") %>% remove_outliers("FTOTINC") %>% remove_outliers("AGE") %>% remove_outliers("FAMSIZE") %>% remove_outliers("NCHILD")
```

```{r}
income_data <- subset(income_IQR, INCTOT >= 0 & FTOTINC >= 0 & AGE >=22 & AGE <= 67)
summary(income_data)
```

### Data Dimension Creation & Reduction

```{r}
#Creating a binary homeowner variable with 1 being Owned, and 0 Rented.
income_data$Homeowner <- with(income_data, 
                              ifelse(OWNERSHP == 1, 1, 
                              ifelse(OWNERSHP == 2, 0, NA)))
```

```{r}
#Checking how many NAs for homeowner
table(income_data$Homeowner, useNA = "ifany")
```

```{r}
#Gender with 1 being male, 0 being female, and other values NA
income_data$Gender <- with(income_data, 
                               ifelse(SEX == 1, 1, 
                                      ifelse(SEX == 2, 0, NA)))
```

```{r}
#Checking how many NAs for Gender
table(income_data$Gender, useNA = "ifany")
```

```{r}
#Creating a NADULT variable (FAMSIZE - NCHILD)
income_data$NADULT <- income_data$FAMSIZE - income_data$NCHILD
```

```{r}
#Checking how many NAs for NADULT
table(income_data$NADULT)
```

```{r}
#Grouping Marital Status to Married = 1 , Not Married = 0
income_data$Married_binary <- with(income_data,
                                   ifelse(MARST %in% c(1, 2), 1, 0))
```

```{r}
#Checking how many NAs for Married_binary
table(income_data$Married_binary, useNA = "ifany")
```

```{r}
#Grouping major Races
income_data$race_group <- with(income_data, 
  ifelse(RACE == 1, 1,                      #White
    ifelse(RACE == 2, 2,                    #Black/African American
      ifelse(RACE %in% c(4, 5, 6), 3, 4)    #Asian (Chinese, Japanese, Other Asian or Pacific Islander)
    )                                       # Other (American Indian/Alaska Native, Other race nec, Two major races, Three or more major races)
  )
)

```

```{r}
#Checking NAs
table(income_data$race_group, useNA = "ifany")
```

```{r}
#Grouping education levels
income_data$Educ_group <- with(income_data,
  ifelse(EDUC %in% c(0, 1, 2, 3, 4, 5), 1,           # Less than high school
    ifelse(EDUC == 6, 2,                             # High school graduate
      ifelse(EDUC %in% c(7, 8, 9), 3,                # Some college
        ifelse(EDUC %in% c(10, 11), 4, NA)           # College graduate or higher
      )
    )
  )
)
```

```{r}
#Checking NAs
table(income_data$Educ_group, useNA = "ifany")
```

```{r}
#WFH Variable created from TRANWORK
income_data$Work_from_home <- with(income_data,
  ifelse(TRANWORK == 80, 1,           # Worked at home → 1
         ifelse(TRANWORK == 0, NA, 0)) # Code 00 (N/A) becomes NA; all others → 0
)
```

```{r}
#Checking NAs , what do we do with the NAs?
table(income_data$Work_from_home, useNA = "ifany")
```

```{r}
#Grouping the CLASSWKR where 1 is Self-employed, 0 indicates Works for wages and NA indicates N/A
income_data$Classwkr_binary <- with(income_data,
  ifelse(CLASSWKR == 1, 1,
         ifelse(CLASSWKR == 2, 0, NA))
)
```

```{r}
#Checking NAs
table(income_data$Classwkr_binary, useNA = "ifany")
```

```{r}
# #Converting categorical variables to factor
income_data$Homeowner <- as.factor(income_data$Homeowner)
income_data$Gender <- as.factor(income_data$Gender)
income_data$Married_binary <- as.factor(income_data$Married_binary)
income_data$race_group <- as.factor(income_data$race_group)
income_data$Educ_group <- as.factor(income_data$Educ_group)
income_data$Classwkr_binary <- as.factor(income_data$Classwkr_binary)
income_data$Work_from_home <- as.factor(income_data$Work_from_home)
```

```{r}
#Checking the factor change
str(income_data)
```

```{r}
#Subsetting with the relevant variables for the model
subset_income_data <- income_data[, c("FAMSIZE", "NCHILD", "AGE", "INCTOT", "FTOTINC", "Homeowner", "Gender", "NADULT", "Married_binary", "race_group", "Educ_group", "Work_from_home", "Classwkr_binary")]
```

```{r}
str(subset_income_data)
```

```{r}
6779187 - 3413287
3365900/6779187
```

```{r}
summary(subset_income_data)
```

### Exploratory Data Analysis

```{r}
#Distribution of Total Income
library(ggplot2)
scale_x_continuous(labels = scales::comma)
ggplot(subset_income_data, aes(x = INCTOT)) +
  geom_histogram(bins = 50, fill = "skyblue", color = "black") +
  labs(title = "Distribution of Total Personal Income", x = "Total Personal Income", y = "Number of Observations") +
   scale_x_continuous(labels = scales::dollar_format(prefix = "$", big.mark = ",")) + 
  theme_minimal()
```

```{r}
#Distribution of Family Total Income
scale_x_continuous(labels = scales::comma)
ggplot(subset_income_data, aes(x = FTOTINC)) +
  geom_histogram(bins = 50, fill = "skyblue", color = "black") +
  labs(title = "Distribution of Total Family Income", 
       x = "Total Family Income", 
       y = "Number of Observations") +
  scale_x_continuous(labels = scales::dollar_format(prefix = "$", big.mark = ",")) +
  theme_minimal()
```

```{r}
# Home ownership by Age Group 
ggplot(subset_income_data, 
       aes(x = AGE, 
           fill = factor(Homeowner, levels = c(0, 1), labels = c("Rent", "Own")))) +
  geom_bar(position = "fill") +
  labs(x = "Age Group", 
       y = "Proportion", 
       title = "Homeownership by Age Group",
       fill = "Homeowner") +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal()
```

```{r}
# home ownership by Race Group 
ggplot(subset_income_data, 
       aes(x = factor(race_group, levels = c(1, 2, 3, 4),
                      labels = c("White", "Black/African", "Asian", "Others")),
           fill = factor(Homeowner, levels = c(0, 1), labels = c("Rent", "Own")))) +
  geom_bar(position = "fill") +
  labs(x = "Race Group", 
       y = "Proportion", 
       title = "Homeownership by Race Group",
       fill = "Homeowner") +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal()

```

```{r}
ggplot(subset_income_data, 
       aes(x = factor(Homeowner, levels = c(0, 1), labels = c("Rent", "Own")), 
           y = FTOTINC)) +
  geom_boxplot(fill = "lightyellow") +
  labs(title = "Total Family Income by Homeownership Status", 
       x = "Homeowner", 
       y = "Total Family Income") +
  scale_y_continuous(labels = scales::dollar_format(prefix = "$", big.mark = ",")) +
  theme_minimal()

```

```{r}
ggplot(subset_income_data,   aes(x = factor(Homeowner, levels = c(0, 1), labels = c("Rent", "Own")), y = INCTOT)) +
  geom_boxplot(fill = "lightblue") +
  labs(title = "Total Personal Income by Homeownership Status", x = "Homeowner", y = "Total Personal Income") +
  theme_minimal()
```


```{r}
cor(subset_income_data$FAMSIZE, subset_income_data$NCHILD)
cor(subset_income_data$INCTOT, subset_income_data$FTOTINC)
```

```{r}
# log FTOTINC + 1 to remove -Inf error
subset_income_data$lFTOTINC <- log(subset_income_data$FTOTINC+1)
subset_income_data$lINCTOT <- log(subset_income_data$INCTOT+1)
```

```{r}
# Removing the remaining NAs 
complete_data <- na.omit(subset_income_data)
```

### Model Selection

```{r}
logit_model1 <- glm(Homeowner ~ FAMSIZE + NCHILD + AGE + Married_binary + race_group + Educ_group + Classwkr_binary + lINCTOT, 
              data=complete_data, family=binomial(link="logit"))
summary(logit_model1)

# logistic goodness of fit McFadden’s Pseudo R-Square
(RSQ1 <- (1 - logit_model1$deviance/logit_model1$null.deviance))

```

```{r}
logit_model2 <- glm(Homeowner ~ FAMSIZE + AGE + Married_binary + race_group + Educ_group + Classwkr_binary + lINCTOT, 
              data=complete_data, family=binomial(link="logit"))
summary(logit_model2)
(RSQ2 <- (1 - logit_model2$deviance/logit_model2$null.deviance))
```

```{r}
logit_model3 <- glm(Homeowner ~ NADULT + NCHILD + AGE + Married_binary + race_group + Educ_group + Classwkr_binary + lINCTOT, 
              data=complete_data, family=binomial(link="logit"))
summary(logit_model3)
(RSQ3 <- (1 - logit_model3$deviance/logit_model3$null.deviance))
```

```{r}
logit_model4 <- glm(Homeowner ~ FAMSIZE + NCHILD + AGE + Married_binary + race_group + Educ_group + Classwkr_binary + lFTOTINC, 
              data=complete_data, family=binomial(link="logit"))
summary(logit_model4)

# logistic goodness of fit McFadden’s Pseudo R-Square
(RSQ4 <- (1 - logit_model4$deviance/logit_model4$null.deviance))
```

```{r}
logit_model5 <- glm(Homeowner ~ FAMSIZE + AGE + Married_binary + race_group + Educ_group + Classwkr_binary + lFTOTINC, 
              data=complete_data, family=binomial(link="logit"))
summary(logit_model5)
(RSQ5 <- (1 - logit_model5$deviance/logit_model5$null.deviance))
```

```{r}
logit_model6 <- glm(Homeowner ~ NCHILD + NADULT + AGE + Married_binary + race_group + Educ_group + Classwkr_binary + lFTOTINC, 
              data=complete_data, family=binomial(link="logit"))
summary(logit_model6)
(RSQ6 <- (1 - logit_model6$deviance/logit_model6$null.deviance))
```

```{r}
logit_model7 <- glm(Homeowner ~ NCHILD + NADULT + AGE + I(AGE^2) + Married_binary + race_group + Educ_group + Classwkr_binary + lFTOTINC, 
              data=complete_data, family=binomial(link="logit"))
summary(logit_model7)
(RSQ7 <- (1 - logit_model7$deviance/logit_model7$null.deviance))
```

```{r}
logit_model8 <- glm(Homeowner ~ NCHILD + NADULT + AGE + I(AGE^2) + Married_binary + race_group + Educ_group + lFTOTINC, 
              data=complete_data, family=binomial(link="logit"))
summary(logit_model8)
(RSQ8 <- (1 - logit_model8$deviance/logit_model8$null.deviance))
```

```{r}
logit_model9 <- glm(Homeowner ~ NCHILD + NADULT + AGE + I(AGE^2) + Married_binary + race_group + Educ_group + Classwkr_binary + lFTOTINC + Married_binary*Educ_group, 
              data=complete_data, family=binomial(link="logit"))
summary(logit_model9)
(RSQ9 <- (1 - logit_model9$deviance/logit_model9$null.deviance))
```

```{r}
stargazer::stargazer(logit_model1,logit_model6, logit_model7, logit_model9, type = "text", digits = 4,intercept.bottom = FALSE)
```

```{r}
stargazer::stargazer(RSQ1,RSQ6, RSQ7, RSQ9, type = "text", digits = 6, intercept.bottom = FALSE)
```

```{r}
suppressPackageStartupMessages(library(lmtest))
(lrtest1 <- lrtest(logit_model1))
(lrtest6 <- lrtest(logit_model6))
(lrtest7 <- lrtest(logit_model7))
(lrtest9 <- lrtest(logit_model9))
```

```{r}
summary(logit_model9)
```

### Confusion Matrix

```{r}
suppressPackageStartupMessages(library(kableExtra))
tble <- data.frame(table(true=complete_data$Homeowner, 
              predicted=round(fitted(logit_model9))))
kable(tble, align='c', caption="Logit Confusion Matrix")

# Calculate sensitivity and specificity
# Create confusion matrix

confusion_matrix <- xtabs(Freq ~ true + predicted, tble)

# Rename rows and columns for clarity
rownames(confusion_matrix) <- c("Actual 0", "Actual 1")
colnames(confusion_matrix) <- c("Predicted 0", "Predicted 1")

# View the confusion matrix
kable(confusion_matrix, align='c', caption="Logit Confusion Matrix")

sensitivity <- confusion_matrix[2, 2] / (confusion_matrix[2, 2] + confusion_matrix[2, 1]) 

specificity <- confusion_matrix[1, 1] / (confusion_matrix[1, 1] + confusion_matrix[1, 2])

accuracy <- (confusion_matrix[2, 2] + confusion_matrix[1, 1])  / (confusion_matrix[2, 2] + confusion_matrix[1, 1] + confusion_matrix[2, 1] + confusion_matrix[1, 2])

results_table <- data.frame(Metric = c("Sensitivity", "Specificity", "Accuracy"), Value = c(sensitivity, specificity, accuracy))

kable(results_table, align='c', caption="Logit Confusion Matrix")
```

### Coefficient Plot
```{r}
library(broom)
library(ggplot2)

# Tidy regression results
logit_model9 <- broom::tidy(logit_model9)

# Coefficient plot
ggplot(logit_model9, aes(x = reorder(term, estimate), y = estimate)) +
  geom_point(color = "blue", size = 3) +                             # Plot coefficients
  geom_errorbar(aes(ymin = estimate - std.error, ymax = estimate + std.error), width = 0.2) +
  labs(title = "Multiple Regression Coefficients", x = "Predictors", y = "Coefficient Estimate") +
  coord_flip() +                                                     # Flip for better readability
  theme_minimal()

```


```{r}
summary(logit_model9)
```

```{r}
#Inflection Point is not making sense because the anti-log coefficient is minimal 
b <- 0.05796531
c <- -0.0001390636 
inflection_point <- (-b / (2 * c))
inflection_point
```

```{r}
logit_model9 <- glm(Homeowner ~ NCHILD + NADULT + AGE + I(AGE^2) + Married_binary + race_group + Educ_group + Classwkr_binary + lFTOTINC + Married_binary*Educ_group, 
              data=complete_data, family=binomial(link="logit"))
summary(logit_model9)
```

Commenting the code bellow as we are not able to knit it but it perfectly runs in R.
```{r}
# Exponentiation of coefficients to obtain odds ratios
# odds_ratios <- exp(logit_model9$coefficients)
# print(odds_ratios)
```

```{r}
# Create a sequence of age values within the observed range
age_vals <- seq(min(complete_data$AGE), max(complete_data$AGE), length.out = 100)

# Create a new data frame with other predictors set to typical values (or their means/modes)
new_data <- data.frame(
  AGE = age_vals,
  NCHILD = mean(complete_data$NCHILD, na.rm = TRUE),
  NADULT = mean(complete_data$NADULT, na.rm = TRUE),
  Married_binary = "0",  
  race_group = "1",     
  Educ_group = "1",      
  Classwkr_binary = "0", 
  lFTOTINC = mean(complete_data$lFTOTINC, na.rm = TRUE)
)

# Get predicted probabilities
new_data$predicted_prob <- predict(logit_model9, newdata = new_data, type = "response")

# Plot the predictions
library(ggplot2)
ggplot(new_data, aes(x = AGE, y = predicted_prob)) +
  geom_line(color = "blue", size = 1) +
  labs(title = "Predicted Probability vs. Age",
       x = "Age",
       y = "Predicted Probability") +
  theme_minimal()
```



