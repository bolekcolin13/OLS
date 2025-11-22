# OLS

An ongoing project of manually coding linear models and comparing them to results from statistics packages in R and Python.  

## Manual OLS in R

A project from a course in econometrics, ```Manual_OLS.R``` uses a data set of 200 Chicago families to determine if there exists a relationship between the miles traveled by those families in a year for vacation and a number of relevant variables; data available as ```vacation.xlsx``` in this repository. The model used is a standard linear specification and includes both homoskedastic and heteroskedastic robust (HC1, specifically) standard errors. 

## Manual OLS [lm()] in R

The file ```Manual_OLS_lm().py``` replicates ```Manual_OLS.R``` in Python while introducing the functionality of the user being able to choose which model (homoskedastic or HC1 robust) manual model they would like to produce.

## Manual OLS in General, R

Using data uploaded into R by the user, this replicates the functionality of the lm() call in R while allowing for specification of HC0 or HC1 standard error correction.

