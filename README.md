# ssi3-data-analysis

This repository stores replication files for "Which frame fits? Policy learning with framing for climate change policy attitudes"

**Pre-registration:** https://osf.io/2ztxe

## Setup

Create a virtual environment and install dependencies:

```bash
python3 -m venv ssi3
source ssi3/bin/activate
pip install -r requirements.txt
python -m ipykernel install --user --name ssi3 --display-name "Python (ssi3)"
```

## Replication

### Starting from raw data

1. Place raw data files in the `data/` directory (not included in repo)
2. Run `code/SOSC Data Cleaner.do` in Stata to generate `data/ssi-data-cleaned.csv`

### Starting from cleaned data

3. Run `code/dataAnalysis.ipynb` in Jupyter (select the "Python (ssi3)" kernel)
4. Run R scripts for tables and figures:
   ```R
   source("code/dataAnalysis.R")
   source("code/figure-code.R")
   ```

## Requirements

- **Python 3.x**: See `requirements.txt`
- **R 4.0+**: tidyverse, estimatr, modelsummary, kableExtra, grf
- **Stata 14+** (only if cleaning raw data)

## Repository Structure

- `code/` - Analysis scripts (Stata, Python, R)
- `data/` - Data files (cleaned data only)
- `preregistration/` - Pre-analysis plan
- `tables/` - Generated LaTeX tables
- `figures/` - Generated figures

## License

MIT License - see LICENSE file for details
