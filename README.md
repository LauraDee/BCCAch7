# BCCAch7
BCCA Chapter 7 Team GitHub for systematic review 

Code Folder:
'001_reshape_flow2columns.R' takes into compiled output from form Jan 9, 2025 with 407 entries
from the data folder. 
> adds four columns to have a binary variable (0 or 1) for each flow group: Biotic, Physical, Sociocultural, Human 
> writes out: reshaped_1_flowtypes.csv

'002_reshape_byFlowEntry.R' takes in reshaped_1_flowtypes.csv, then:
- rename column names 
- filter out test paper entries 
- convert wide to long data 
- reshape by entries in the form -- because there can be multiple rows per paper (vs one per paper)
- checking characteristics of the data: unique DOIs (175) but 221 DOI by flow entry - meaning some have more than 1 entry
outputs: reshaped_2_byFlowEntry.csv

'003_reshape_byFlow.R' -takes in reshaped_2_byFlowEntry.csv, multiple subflows by flow entry and paper. 

