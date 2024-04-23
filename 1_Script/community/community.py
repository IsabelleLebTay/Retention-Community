import pandas as pd
import csv
import os

raw_Path = r"C:\Users\ilebe\Documents\!Masters!\RETN Community\0_Data\Raw\Wildtrax Limited Percept"
processed_Path = r"C:\Users\ilebe\Documents\!Masters!\RETN Community\0_Data\Processed"
# Set your working directory
data = pd.read_csv(os.path.join(raw_Path,"BU_Harvest_Patch_Retention_-_Community_&_Limited_perceptibility_main_report.csv"))

#data['point_count'] = data['location'] + " " +  data['recording_date'] + " " +data['recording_time']

# dummies from the species code column, drops column

data = data[data['aru_task_status'] == 'Transcribed']
data = data[data['vocalization'] == 'Song']
data['dummies'] = data['species_code']
# data['dummies'] = data['species_common_name']
 #species_common_name
# full_species_list = list(data['species_common_name'].unique())

abundance = pd.get_dummies(data=data, columns= ['dummies'], prefix='', prefix_sep='')

# add the species code column back
df = pd.concat([data['species_code'], abundance], axis=1) 

# with open(os.path.join(raw_Path,'columnsList.csv'), newline='') as f:
#     reader = csv.reader(f)
#     columnsList = list(reader)[0]

full_species_list = list(data['species_code'].unique())
# full_species_list = list(data['species_common_name'].unique())

# exclude_these = ['NONE', 'Red Squirrel', "Red-tailed Hawk", 'Unidentified Woodpecker', 'Unidentified signal', 'Unidentified Passerine', 'Unidentified Poecile (Chickadee)', 'Unidentified bird', 'Unidentified Sparrow', 'Unidentified Trill', 'Unidentified Thrush', 'Unidentified Mammal', 'Unidentified Warbler', 'Unidentified Flycatcher', 'Unidentified Vireo', 'Unidentified Corvid', 'Unidentified Tern']
exclude_these = ['NONE', 'RESQ', "RTHA", 'UNWO', 'UNKN', 'UNPA', 'UPCH', 'UNBI', 'UNSP', 'UNTR', 'UNTH', 'UNMA', 'UNWA', 'UNFL', 'UNVI',  'UNCV', 'UNTE']

select_species = [i for i in full_species_list if i not in exclude_these]

columnsList = ['location', 'recording_date_time', 'latitude', 'longitude', 'task_comments'] + select_species
# print(columnsList)

# make a dict of the aggregate function for each column. For the species code (length 4), use sum. Use first for the rest. Exclude 'location at index 0.

# For species_code
# columns_dictB =  { i : 'sum' for i in columnsList[1:] if len(i) == 4}
# columns_dictA = { i : 'first' for i in columnsList[1:] if len(i) != 4}

# For common names

columns_dictB =  { i : 'sum' for i in columnsList[1:] if i in select_species}
columns_dictA = { i : 'first' for i in columnsList[1:] if i not in select_species}
columns_dictA.update(columns_dictB)
# Agg function for species_code is the list of all species counted at that location.
# columns_dictA['species_code'] = lambda x: list(x)

# print(abundance.head())
# Use selcted columns only
df1 = abundance[columnsList]
# print(df1.columns)
print(df1.head())
# Group the report by unique location 
grouped = df1.groupby('location').agg(columns_dictA)
print(grouped.columns)
print(grouped.head())
grouped.to_csv(os.path.join(processed_Path, "Limited perceptibility", "Pre truncation", 'Multi_spp_occupancy.csv'))
