# ssi3-data-analysis
This repository stores replication files for SOSC 13300 Section 1 Final Project. 

To run the python code, create a virtual environment. 

```
python3 -m venv ssi3
source ssi3/bin/activate
```

Then install requirements. 
```
pip install -r requirements.txt
python -m ipykernel install --name=ssi3
```

1. Raw data should be saved *locally* in the `Datasets/` folder. Do not push raw data to the repository. 
2. Clean the data. Run `Code/SOSC Data Cleaner.do` in Stata. This will generate `/data/ssi-data-cleaned.csv`
3. Run analysis. Run the ipython notebook `Code/dataAnalysis.ipynb`.
