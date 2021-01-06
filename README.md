# Medicare_Fraud
Medical provider fraud is one of the biggest problems facing the US public healthcare system.
Medicare, one of the largest public insurance programs primarily for people aged 65 or older,
accounts for a majority of public healthcare spending and has been the target of many fraud
schemes.

This project is to investigate this fraud. When a provider is flagged as committing potential fraud, all claims involving this provider are audited. This is an extremely costly, time consuming, and potentially adversarial process. Thus, accurate identification of providers who should be targeted with an audit is of utmost importance to Medicare.


# Data
A dataset is provided contains a sample of anonymized data for beneficiaries, as well as their inpatient and outpatient insurance
claims between Dec 2008 and Dec 2009. A separate table of health care provider flags show those who appear to have engaged in fraud. A data dictionary describing the fields in each table is also provided. The information is saved under the Data folder.

# Code
Code using XGBoost to analyze this problem is saved under the Code folder.

# Output
The Output folder includes:
1) A ranking of most important variables in detection of fraud.
2) Area Under Curve (AUC) which measures the model performance.
3) Cluster in the tree
