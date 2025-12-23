# 🏠 Determinants of Homeownership (R)

## 📌 Business / Policy Problem
Homeownership is a critical indicator of financial stability and long-term wealth accumulation.  
Understanding which demographic and economic factors most strongly influence homeownership can help **financial institutions**, **housing policymakers**, and **lending programs** design more inclusive and effective strategies.

This project analyzes the probability of homeownership using large-scale U.S. census microdata and interpretable statistical modeling.

---

## 🎯 Objectives
- Predict the likelihood of homeownership (Owner vs. Renter)
- Identify and quantify key demographic and financial drivers
- Interpret results using odds ratios for real-world decision-making
- Translate statistical findings into policy and lending insights

---

## 📊 Data
- **Source:** American Community Survey (ACS) 2022 microdata via IPUMS USA  
- **Initial size:** ~6.7 million observations  
- **Final analytic sample:** ~3.0 million observations after cleaning  
- **Target variable:** Homeownership status (binary)

### Key Variables
- Income: Personal income, family income (log-transformed)
- Demographics: Age, race, gender, marital status
- Household structure: Number of children, number of adults
- Education level
- Employment type: Wage worker vs. self-employed

### Data Limitations
- No credit score, mortgage rate, or housing price information
- No geographic (urban/rural or regional) controls
- Large sample size makes many coefficients statistically significant

---

## 🧹 Data Preparation & Feature Engineering
- Removed irrelevant and administrative variables
- Replaced placeholder values (e.g., `9999999`) with missing values
- Outlier handling using the IQR method for income, age, and household variables
- Filtered to working-age population (ages 22–67)
- Created engineered features:
  - Binary homeownership indicator
  - Number of adults in household
  - Binary marital status
  - Grouped race and education categories
  - Binary employment class
  - Log-transformed family income

---

## 🧠 Methodology
- **Primary model:** Logistic Regression
- Tested multiple model specifications with incremental covariates
- Included interaction effects (Married × Education)
- Model evaluation using:
  - Confusion Matrix
  - ROC Curve
  - Area Under the Curve (AUC)
  - Pseudo R²

---

## 📈 Key Results
- **Model performance:**  
  - AUC ≈ **0.78**, indicating good discriminatory power  
  - Accuracy ≈ **76%**
  - High sensitivity (~92%), lower specificity (~38%)

- **Strongest drivers of homeownership (odds interpretation):**
  - +85% odds per unit increase in log family income
  - +46% odds per additional adult in the household
  - +12% odds per additional child
  - +40% odds for married individuals
  - +60–82% higher odds for individuals with college education
  - Self-employed individuals show ~19% higher odds of homeownership

- **Equity insight:**  
  Even after controlling for income and education, race remains a significant predictor, with Black, Asian, and other racial groups exhibiting lower odds of homeownership relative to White households.

---

## 💡 Business & Policy Implications
- **Lenders:**  
  Can use interpretable models to refine mortgage screening and design products for underserved groups.
- **Policymakers:**  
  Results highlight the need for targeted housing affordability and education-based interventions.
- **Housing programs:**  
  Income support alone may be insufficient without addressing structural and demographic disparities.

---

## 🛠 Tools & Technologies
R, tidyverse, caret, pROC, statistical modeling, data visualization
