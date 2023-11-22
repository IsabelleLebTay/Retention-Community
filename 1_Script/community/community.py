import pandas as pd
import csv
import os

raw_Path = r"C:\Users\ilebe\Documents\!Masters!\RETN Community\0_Data\Raw\Wildtrax Patch Retention"
processed_Path = r"C:\Users\ilebe\Documents\!Masters!\RETN Community\0_Data\Processed"
# Set your working directory
data = pd.read_csv(os.path.join(raw_Path,"BU_Harvest_Patch_Retention_-_Community_2023_main_report.csv"))

#data['point_count'] = data['location'] + " " +  data['recording_date'] + " " +data['recording_time']

# dummies from the species code column, drops column

data = data[data['aru_task_status'] == 'Transcribed']
data['dummies'] = data['species_code']

abundance = pd.get_dummies(data=data, columns= ['dummies'], prefix='', prefix_sep='')

# add the species code column back
# df = pd.concat([data['species_code'], abundance], axis=1) 

# with open(os.path.join(raw_Path,'columnsList.csv'), newline='') as f:
#     reader = csv.reader(f)
#     columnsList = list(reader)[0]

species_list = list(data['species_code'].unique())

columnsList = ['location', 'recording_date_time', 'latitude', 'longitude', 'species_code'] + species_list
# print(columnsList)

exclude_these = ['NONE', 'RESQ', 'UNWO', 'UNKN', 'UNPA', 'UPCH', 'UNBI', 'UNSP', 'UNTR', 'UNTH', 'UNMA', 'UNWA', 'UNFL', 'UNVI',  'UNCV', 'UNTE']
columnsListFixed = [i for i in columnsList if i not in exclude_these]
# print(columnsListFixed)
# make a dict of the aggregate function for each column. For the species code (length 4), use sum. Use first for the rest. Exclude 'location at indox 0.
columns_dictB =  { i : 'sum' for i in columnsListFixed[1:] if len(i) == 4}
columns_dictA = { i : 'first' for i in columnsListFixed[1:] if len(i) != 4}
columns_dictA.update(columns_dictB)
# Agg function for species_code is the list of all species counted at that location.
columns_dictA['species_code'] = lambda x: list(x)

print(abundance.head())
# Use selcted columns only
df1 = abundance[columnsListFixed]
# print(df1.columns)
print(df1.head())
# Group the report by unique location 
grouped = df1.groupby('location').agg(columns_dictA)
print(grouped.columns)
print(grouped.head())
grouped.to_csv(os.path.join(processed_Path,'community_abundance_by_location.csv'))
