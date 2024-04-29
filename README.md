# ssi3-data-analysis
This repository stores replication files for SOSC 13300 Section-X Final Project. 

Create a virtual environment. 

```
python3 -m venv ssi3
source ssi3/bin/activate
```

Then install requirements. 
```
pip install -r requirements.txt
python -m ipykernel install --name=ssi3
```

0. Raw data should be saved *locally* in the `Datasets/` folder. Do not push raw data to the repository. 
1. Clean the data. Run `Code/SOSC Data Cleaner.do` This will generate `/data/ssi-data-cleaned.csv`
2. Run analysis. 
  - Question 1 can be found in pilot_analysis, question 2/3 can be found in dataAnalysis. 
