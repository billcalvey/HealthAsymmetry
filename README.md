# HealthAsymmetry
An R script which allows for the reproduction of findings regarding health asymmetry and its influence on depressive symptoms in older adults.

Health asymmetry involves the comparison of self-rated health (SRH) scores with frailty scores as measured by the Frailty Index (FI). 

Both SRH and FI scores are standardized, converted into Z scores, and a 1 standard deviation cut-off point is used to determine which health asymmetry group
each older adult is assigned to: health pessimistic, health realistic, or health optimistc. 

Once health asymmetry status has been obtained, a set of covariates are combined into one singular dataset, which requires imputation. The missing values
within the dataset are imputed.

Then, statistical analyses are conducted. Kruskal-wallis rank sum tests determine whether there are cross-sectional differences between health asymmetry
status in terms of depressive symptoms. A growth curve model determines whether health asymmetry status along with interactions with time, are 
significant predictors of change in depressive symptoms. 
