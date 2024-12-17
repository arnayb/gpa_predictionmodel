GPA Prediction Model


Goal: 
To build a predictive model examining factors influencing GPA. The final model includes six predictors: Drinks, CognitionZScore, StressScore, AnxietyScore, ClassesMissed, AverageSleep


Process

1.) Exploratory Data Analysis: Visualized scatterplots to understand relationships between predictors and GPA.

2.) Simple Linear Regression: Identified significant predictors (p < 0.05): CognitionZScore, AnxietyScore, StressScore, and Drinks.

3.) Model Building: Removed DASScore due to multicollinearity. Conducted forward/backward selection and used Mallow's Cp.

4.) Model Selection: The model with the lowest Mallow’s Cp aligns with forward/backward selection results.


Model Verification

1.) Linearity: Residuals satisfy the linearity assumption.

2.) Constant Variance: Residuals show consistent spread.

3.) Normality: QQ-plot indicates normality of residuals.
