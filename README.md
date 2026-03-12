# Air Pollution and Cardiovascular Hospital Admissions

This research project was completed as part of the **Mathematical Statistics** course during my exchange semester at Bocconi University, as part of my BSc in Mathematics and Computer Science at École Polytechnique.

This project investigates whether **short-term delayed exposure to air pollution** can help explain or predict hospital admissions in a cardiology unit.

The full academic report describing the methodology and results is available here:

📄 [Full Research Report](Research_Project_Report_Yanis_Zaamoun.pdf)

---

# Dataset

The dataset combines two sources:

• **Hospital admissions data** (daily cardiology admissions)  
• **Environmental data** including air pollution indicators and meteorological variables

After aggregation and merging:

- 725 daily observations  
- 28 environmental features  
- Study period: **April 2017 – March 2019**

---

# Methodology

The analysis follows four main steps.

### 1. Exploratory Data Analysis
- Summary statistics and visualization  
- Correlation analysis  
- Time series plots  

### 2. Lagged Feature Engineering
To capture delayed pollution effects, **lagged variables (0–30 days)** were created for each pollutant.  
The most relevant lag for each variable was selected based on correlation with hospital admissions.

### 3. Statistical Modeling

Two approaches were used:

**Linear Regression**
- Stepwise feature selection (AIC)  
- Evaluation of statistical significance  
- Residual diagnostics  

**Logistic Regression**
- Prediction of high-admission days  
- Greedy feature selection  
- Train/test split and cross-validation  

### 4. Seasonal Air Quality Analysis

Seasonal differences in air quality were investigated using **Welch's ANOVA** on the Air Quality Index (AQI), allowing for unequal variances across seasons.
