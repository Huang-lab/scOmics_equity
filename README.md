# Equity & RepresentationBias in Major Single-Cell/Spatial Omics Datasets
This repository contains comprehensive equity bias analyses across three major biomedical research domains: neuropsychiatric disorders (PsychAD), cancer research (HTAN), and cell atlas studies (HCA).

## Installation

### Prerequisites
- R (version 4.0 or higher)
- RStudio (recommended)

### Install Required R Packages

Run the following commands in R or RStudio to install all dependencies:

```r
# Install required packages
required_packages <- c(
  "ggplot2",
  "dplyr", 
  "tidyr",
  "stringr",
  "scales",
  "readxl",
  "car"
)

# Check which packages need to be installed
missing_packages <- required_packages[!(required_packages %in% installed.packages()[,"Package"])]

# Install missing packages
if(length(missing_packages) > 0) {
  install.packages(missing_packages)
}

# Load all packages to verify installation
lapply(required_packages, library, character.only = TRUE)
```

### Verify Installation

```r
# Quick test to ensure all packages load correctly
library(ggplot2)
library(dplyr)
library(tidyr)
library(stringr)
library(scales)
library(readxl)
library(car)

cat("All packages installed successfully!\n")
```

## Data Setup

Ensure your data files are placed in the `data/` directory:
- `data/psych-AD_media-1.csv` (for PsychAD analysis)
- `data/HTAN scrnaseq data final.xlsx` (for HTAN analysis)  
- `data/HCA scrnaseq data final.xlsx` (for HCA analysis)

## Analysis Scripts

- `equity_bias_analysis_PsychAD.R` - Analysis of psychAD media dataset
- `equity_bias_analysis_HTAN.R` - Analysis of HTAN cancer dataset  
- `equity_bias_analysis_HCA.R` - Analysis of Human Cell Atlas dataset

## Key Data Post-Processing

### Ancestry Standardization

All three analyses standardize ancestry/race variables to consistent categories:

**PsychAD Dataset:**
```r
# Maps original ancestry codes to standardized categories
AFR → African
AMR → Latino  
EAS, SAS → Asian (combined East Asian and South Asian)
EUR → European
EAS_SAS → Asian (mixed category)
Unknown/NA → Unknown
Other values → Other
```

**HTAN Dataset:**
```r
# Uses race and ethnicity columns with Latino precedence
Hispanic/Latino ethnicity → Latino (regardless of race)
White (non-Latino) → European
Black/African American → African
Asian → Asian
Other → Other
Not reported/Unknown/NA → Unknown
```

**HCA Dataset:**
```r
# Uses ethnicity ontology aggregated field
european → European
african → African
asian → Asian
hispanic or latino → Latino
mixed → Other
unknown/not provided/NA → Unknown
```

### Sex/Gender Standardization

**PsychAD Dataset:**
```r
# Uses 'sex' column (already clean)
female → female
male → male
# No missing values in this dataset
```

**HTAN Dataset:**
```r
# Uses 'Gender' column
Female → female
Male → male
Unknown/Not Reported/NA → NA
```

**HCA Dataset:**
```r
# Uses 'donor_organism.sex' column
female → female
male → male
unknown/yes/homo sapiens/NA → NA
```

## Disease/Condition Processing

**PsychAD Dataset:**
- Combines cross-disorder and single diagnosis variables
- Creates unified disease categories (AD, SCZ, DLBD, etc.)
- Adds control category for samples without any listed diseases

**HTAN Dataset:**
- Extracts cancer types from Primary Diagnosis field
- Maps to standardized cancer categories (Breast, Lung, Colorectal, etc.)

**HCA Dataset:**
- Uses tissue sheet names as primary tissue type classification
- Maintains original tissue categorization from HCA

## Usage

```bash
# Run individual analyses
Rscript equity_bias_analysis_PsychAD.R
Rscript equity_bias_analysis_HTAN.R  
Rscript equity_bias_analysis_HCA.R
```

## Output

All analyses generate outputs in the `out/` directory:
- PDF visualizations
- Statistical analysis logs
- R workspace files for further analysis
