import pandas as pd
import csv
import os

raw_Path = r"C:\Users\ilebe\Documents\!Masters!\RETN Community\0_Data\Raw\Wildtrax Patch Retention"
# Set your working directory
data = pd.read_csv(os.path.join(raw_Path,"BU_Harvest_Patch_Retention_-_Community_2023_main_report.csv"))

#data['point_count'] = data['location'] + " " +  data['recording_date'] + " " +data['recording_time']

# dummies from the species code column, drops column
data['dummies'] = data['species_code']

abundance = pd.get_dummies(data=data, columns= ['dummies'], prefix='', prefix_sep='')

# add the species code column back
# df = pd.concat([data['species_code'], abundance], axis=1) 

with open(os.path.join(raw_Path,'columnsList.csv'), newline='') as f:
    reader = csv.reader(f)
    columnsList = list(reader)[0]
print(columnsList)

exclude_these = ['AMGO', 'AMPI', 'BCFR', 'BLPW', 'BRSP', 'CICA', 'COWW', 'COYT', 'DOGG', 'EAPH', 'GRSP', 'HENO', 'HEWI', 'LIBA', 'LINO', 'LIRA', 'LITF', 'LIWI', 'MOBA', 'MONO', 'MORA', 'MOTF', 'MOWI', 'NESP', 'NLFR', 'PHVI', 'SAVS', 'SEWR', 'TRES', 'VEER', 'WEME', 'WOFR', 'WOLF', 'WTDE', 'YHBL']
columnsListFixed = [i for i in columnsList if i not in exclude_these ]
# make a dict of the aggregate function for each column. For the species code (length 4), use sum. Use first for the rest. Exclude 'location at indox 0.
columns_dictB =  { i : 'sum' for i in columnsListFixed[1:] if len(i) == 4}
columns_dictA = { i : 'first' for i in columnsListFixed[1:] if len(i) != 4}
columns_dictA.update(columns_dictB)
# Agg function for species_code is the list of all species counted at that location.
columns_dictA['species_code'] = lambda x: list(x)

# Use selcted columns only
df1 = abundance[columnsListFixed]

# Group the report by unique location 
grouped = df1.groupby('location').agg(columns_dictA)

grouped.to_csv('community_abundance_by_location.csv')
