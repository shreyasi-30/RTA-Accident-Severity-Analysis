# RTA-Accident-Severity-Analysis
# 🚦 Road Traffic Accident Severity Prediction

## 📌 Project Overview

Road traffic accidents can result in outcomes ranging from slight injuries to serious injuries and fatalities. This project uses **Machine Learning in R** to predict the severity of road traffic accidents based on accident-related factors such as driver characteristics, road conditions, weather, collision type, vehicle movement, and causes of accidents.

The project implements and compares three classification algorithms:

- Decision Tree
- Random Forest
- Multinomial Logistic Regression

An optimized Random Forest model is developed by tuning its major hyperparameters and is selected as the final model based on overall test accuracy.

---

## 🎯 Objectives

The main objectives of this project are:

1. Explore and clean the road traffic accident dataset.
2. Analyze the distribution of accident severity.
3. Train and compare Decision Tree, Random Forest, and Multinomial Logistic Regression models.
4. Tune the Random Forest hyperparameters.
5. Evaluate models using accuracy, precision, recall, F1-score, and confusion matrix.
6. Identify important variables contributing to Random Forest predictions.
7. Analyze the limitations of the model, especially for minority severity classes.

---

## 📊 Dataset

The project uses the `RTA_Dataset.csv` dataset.

### Dataset Statistics

| Property | Value |
|---|---:|
| Total Records | 12,316 |
| Total Columns | 32 |
| Training Records | 9,852 |
| Testing Records | 2,464 |
| Target Variable | `Accident_severity` |
| Number of Classes | 3 |

### Target Variable

The target variable `Accident_severity` contains three classes:

| Severity Class | Records | Percentage |
|---|---:|---:|
| Fatal injury | 158 | 1.28% |
| Serious Injury | 1,743 | 14.15% |
| Slight Injury | 10,415 | 84.56% |

The dataset is highly imbalanced, with **Slight Injury** being the dominant class.

> **Dataset Source:** The original dataset source, publisher, geographical area, and collection period were not documented in the available project files.

---

## 🔍 Features Used

Thirteen variables were used as predictors:

- `Day_of_week`
- `Age_band_of_driver`
- `Sex_of_driver`
- `Driving_experience`
- `Types_of_Junction`
- `Road_surface_conditions`
- `Light_conditions`
- `Weather_conditions`
- `Type_of_collision`
- `Number_of_vehicles_involved`
- `Number_of_casualties`
- `Vehicle_movement`
- `Cause_of_accident`

### Target

```text
Accident_severity
🧹 Data Preprocessing
The following preprocessing steps were performed:
- Checked for ? placeholders.
- Checked for missing values.
- Checked for duplicate records.
- Converted character columns into R factors.
- Converted the target variable into a factor.
- Kept vehicle and casualty counts as integer variables.
- Used set.seed(123) for reproducibility.
- Split the dataset into 80% training and 20% testing data.
Train-Test Split
Training Data: 9,852 records
Testing Data:  2,464 records

No class weighting, oversampling, undersampling, feature scaling, or synthetic data generation was used in the reported methodology.
📈 Exploratory Data Analysis
Exploratory Data Analysis was performed to understand the distribution and characteristics of the accident dataset.
Visualizations
The project includes visualizations for:
- Accident severity distribution
- Weather conditions
- Light conditions
- Road surface conditions
- Junction type
- Driver age
- Driving experience
- Collision type
- Cause of accident
- Driver sex
- Severity vs Weather
- Severity vs Light Conditions
- Severity vs Road Surface Conditions
- Severity vs Collision Type
Key Findings
- Slight Injury is the dominant accident severity class.
- 8,798 accidents occurred in daylight.
- Friday recorded the highest number of accidents.
- The median number of vehicles involved was 2.
- The median number of casualties was 1.
- "No distancing" was the most frequently recorded cause of accidents.
The major finding from EDA is the strong class imbalance in accident severity.
🤖 Machine Learning Methodology
Three classification algorithms were implemented and compared.
1. Decision Tree
A Decision Tree was implemented using the rpart package.
Parameters
cp = 0.001
minsplit = 20
minbucket = 10
maxdepth = 10

Decision Trees recursively split the data based on predictor variables to classify accident severity.
2. Random Forest
A Random Forest was implemented using the randomForest package.
Baseline Random Forest
ntree = 300
mtry = package default
importance = TRUE

Random Forest combines multiple decision trees and uses majority voting to determine the predicted class.
3. Multinomial Logistic Regression
Multinomial Logistic Regression was implemented using:
nnet::multinom

Default model settings were used.
This model provides a linear classification baseline for the three accident severity classes.
⚙️ Random Forest Optimization
The Random Forest model was further optimized using a grid search.
Parameters Tested
Number of Trees
300
500
700

Variables Considered at Each Split
2
3
4
5

Minimum Node Size
1
3
5

A total of 36 parameter combinations were evaluated.
The configuration with the lowest Out-of-Bag (OOB) error was selected.
Best Parameters
ntree = 700
mtry = 5
nodesize = 5

Best OOB Error
0.1481

📊 Model Performance
All models were evaluated using the same test dataset.
Model	Accuracy	Macro Precision	Macro Recall	Macro F1
Decision Tree	84.66%	0.4545	0.3561	0.3520
Random Forest	84.98%	0.5024	0.3501	0.3399
Multinomial Logistic Regression	84.66%	0.6155	0.3343	0.3076
Optimized Random Forest	85.06%	0.4912	0.3561	0.3510


The Optimized Random Forest achieved the highest overall test accuracy of 85.06%.
However, the improvement over the baseline Random Forest was only 0.08 percentage points.
🏆 Final Model
The final selected model is:
Optimized Random Forest
ntree = 700
mtry = 5
nodesize = 5

Test Accuracy
85.06%

Correct Predictions
2,096 out of 2,464 test records

📋 Confusion Matrix
The final Optimized Random Forest produced the following confusion matrix:
Actual / Predicted	Fatal Injury	Serious Injury	Slight Injury
Fatal Injury	0	1	33
Serious Injury	0	26	319
Slight Injury	0	15	2,070


📌 Classification Performance
Class	Test Records	Correctly Classified	Precision	Recall	F1 Score
Fatal Injury	34	0	0.00%	0.00%	0.00%
Serious Injury	345	26	61.90%	7.54%	13.44%
Slight Injury	2,085	2,070	85.47%	99.28%	91.86%


Overall Metrics
Accuracy       = 85.06%
Macro Precision = 49.12%
Macro Recall    = 35.61%
Macro F1        = 35.10%

⭐ Feature Importance
The Random Forest feature importance analysis identified the following variables as the most important contributors to the model:
1. Cause_of_accident
2. Day_of_week
3. Driving_experience
4. Vehicle_movement
5. Types_of_Junction
Other variables included:
- Number_of_casualties
- Number_of_vehicles_involved
- Light_conditions
- Road_surface_conditions
- Sex_of_driver
Feature importance indicates which variables contributed strongly to the fitted Random Forest's splitting decisions. It does not establish that these variables causally produce more severe accidents.
⚠️ Important Limitation
Although the final model achieved 85.06% accuracy, accuracy alone does not fully represent the model's practical performance.
The dataset is highly imbalanced, with Slight Injury representing approximately 84.56% of all records.
The final model:
- Detected 0 of 34 Fatal Injury cases.
- Detected only 26 of 345 Serious Injury cases.
- Achieved 7.54% recall for Serious Injury.
- Achieved 0% recall for Fatal Injury.
- Achieved 99.28% recall for Slight Injury.
Therefore, the current model should not be considered reliable for identifying severe accidents.
This project demonstrates why accuracy alone can be misleading when working with highly imbalanced classification datasets.
🔮 Future Scope
The following improvements could be explored in future work:
- Class weighting
- Cost-sensitive learning
- Oversampling minority classes
- Undersampling the majority class
- Threshold adjustment
- Gradient Boosting
- Other ensemble learning methods
- Feature engineering
- Better handling of blank and "Unknown" values
- Stratified cross-validation
- Repeated cross-validation
- Hyperparameter optimization using macro F1-score
- Optimization based on Fatal Injury recall
- Validation using data from another region
- Validation using data from another time period
These approaches could potentially improve the model's ability to detect Fatal and Serious Injury cases.
🛠️ Technologies Used
Programming Language
- R
Machine Learning
- Decision Tree
- Random Forest
- Multinomial Logistic Regression
R Packages
rpart
randomForest
nnet

Techniques
- Data Cleaning
- Exploratory Data Analysis
- Classification
- Hyperparameter Tuning
- Confusion Matrix
- Feature Importance
- Model Evaluation
