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
# data['dummies'] = data['species_code']
data['dummies'] = data['species_common_name']
 #species_common_name
full_species_list = list(data['species_common_name'].unique())
print(full_species_list)
