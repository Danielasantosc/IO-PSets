## Problem Set 1
## ABM
## October 5th, 2022

# Importing the necessary libraries
import pandas as pd
import statsmodels.api as sm
from linearmodels import PooledOLS
from linearmodels.panel import RandomEffects
from linearmodels import PanelOLS
from linearmodels.panel import BetweenOLS
from linearmodels.panel import compare

# Importing the data 
df = pd.read_stata("../data/PS1_Data.dta")

## Question 1

# Full Sample 
df[['Y', 'L', 'I', 'K', 'A']].describe()

# Creating a balanced sub-panel 
df['firm'].value_counts() # Can already see that some firms do not show up every period
df_balanced = df.groupby('firm').filter(lambda x : len(x) == 10)
df_balanced[['Y', 'L', 'I', 'K', 'A']].describe()

# Exiters panel 
df_exiters = df.groupby('firm').filter(lambda x : len(x) < 10)
df_exiters[['Y', 'L', 'I', 'K', 'A']].describe()

## Question 2

# Adjust dataset
year = pd.Categorical(df_balanced.year)
df_balanced = df_balanced.set_index(['firm', 'year'])
df_balanced["year"] = year

# Pooled OLS
exog_vars = ["A", "K", "L", "year"] 
exog = sm.add_constant(df_balanced[exog_vars]) # this adds a constant 
mod = PooledOLS(df_balanced.Y, exog) # this automatically excludes dummy year = 1
pooled_res = mod.fit()

# Random Effects 
exog_vars = ["A", "K", "L", "year"] 
exog = sm.add_constant(df_balanced[exog_vars]) # this adds a constant 
mod = RandomEffects(df_balanced.Y, exog) # this automatically excludes dummy year = 1
re_res = mod.fit()

# Within estimator 
exog_vars = ["A", "K", "L", "year"]
exog = sm.add_constant(df_balanced[exog_vars])
mod = PanelOLS(df_balanced.Y, exog, entity_effects = True, drop_absorbed=True)
fe_res = mod.fit()

# Between estimator 
exog_vars = ["A", "K", "L", "year"] 
exog = sm.add_constant(df_balanced[exog_vars])
mod = BetweenOLS(df_balanced.Y, exog)
be_res = mod.fit()

# Comparing the 4 estimators 
print(compare({"Between": be_res, "RE": re_res, "Pooled": pooled_res, "Within": fe_res}))

## Question 3

# Re-adjust dataset 
df_balanced = df_balanced.drop(columns = ["year"])
df_balanced = df_balanced.reset_index()

# Create differenced dataframes 
# I create a dictionary where I store the three new dataframes (first difference, second difference, third difference)
diffs = {}
for i in range(1, 4):
    df = df_balanced[["Y", "A", "K", "L", "year"]].diff(i)
    df["firm"] = df_balanced["firm"]
    year = pd.Categorical(df.year)
    df = df.set_index(['firm', 'year'])
    df["year"] = year
    diffs[i] = df

#  Difference estimators
diffs_estimator = {}
for i in range(1, 4):
    exog_vars = ["A", "K", "L", "year"]
    exog = sm.add_constant(diffs[i][exog_vars])
    mod = PanelOLS(diffs[i].Y, exog)
    diffs_estimator[i] = mod.fit()

# COmpare the difference estimators
print(compare({"1st difference": diffs_estimator[1], "2nd difference": diffs_estimator[2], "3rd difference": diffs_estimator[3]}))

