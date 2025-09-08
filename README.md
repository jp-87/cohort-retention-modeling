# Student Persistence and Revenue Risk Analysis

A comprehensive analytical framework for Berklee College of Music to identify and prioritize student persistence and revenue risk by modeling cohort-based enrollment patterns, net tuition revenue, and retention factors across BCM (Berklee College of Music) and BCB (Berklee College of Business) programs.

## 🎯 Project Overview

This repository contains advanced R scripts for cohort segmentation analysis that integrates historical student data, financial aid information, and enrollment patterns to support strategic enrollment management and financial forecasting. The analysis helps route insights to targeted aid strategies and optimize revenue while supporting student success.

## 📊 Key Analytics Components

### Cohort Segmentation Analysis (`cohort_segmentation_jschofield.r`)

The primary analytical engine that processes multi-year student data to generate actionable insights:

#### **Core Functionality:**
- **Temporal Data Processing**: Custom term calculation functions that convert academic terms (e.g., 2024FA, 2025SP) into comparable numeric values for longitudinal analysis
- **Cross-Campus Integration**: Unified processing of BCM and BCB student data with school-specific logic
- **Revenue Modeling**: Net Tuition Revenue (NTR) calculations combining tuition charges, institutional aid, and comprehensive fees
- **Cohort Tracking**: Student progression analysis across multiple academic terms with matriculation source classification

#### **Data Sources Integration:**
- **BCM Data Pipeline**: Tableau Prep workflows from Office of Student Accounts (OSA) with historical census snapshots
- **BCB Historical Data**: Multi-year spring and fall semester data (2018-2025) with financial components
- **Reference Mappings**: Cross-reference tables for program, degree, and enrollment type standardization
- **Archive Integration**: Historical enrollment data with matriculation source tracking

#### **Advanced Processing Logic:**
- **Dual Matching System**: Primary and alternate matching algorithms for student record classification
- **Data Quality Assurance**: Comprehensive filtering for non-degree and miscellaneous student types
- **Missing Data Handling**: Robust NA value processing and cohort assignment for incomplete records
- **Standardization**: Consistent field naming and data type conversions across disparate data sources

### Pipeline Architecture (`cohort_pipeline`)

The updated pipeline script that serves as the execution framework for the analytical workflow.

## 📈 Key Analytical Insights

### **Revenue Patterns:**
- **Multi-Component NTR**: Analysis reveals complex revenue structures combining tuition, institutional grants, and comprehensive fees
- **Cohort-Based Trends**: Distinct revenue patterns by entry cohort, program type, and attendance mode
- **Regional Variations**: Significant revenue differences across geographic regions and program classifications

### **Retention Risk Factors:**
- **Enrollment Status Tracking**: Focus on actively enrolled students (status 'E') for retention modeling
- **Term-to-Term Progression**: Identification of critical transition points where students are at risk
- **Program-Specific Patterns**: Different retention behaviors between undergraduate, graduate, and non-degree programs

### **Data Quality Findings:**
- **Missing Data Patterns**: Systematic identification of incomplete cohort assignments and demographic data
- **Cross-Reference Gaps**: Unmatched student records requiring alternate classification logic
- **Temporal Consistency**: Validation of term sequences and academic year alignment

### **Strategic Implications:**
- **Targeted Intervention**: Data-driven identification of at-risk student populations
- **Financial Planning**: Revenue forecasting capabilities for budget and aid allocation
- **Enrollment Strategy**: Insights for optimizing recruitment and retention efforts

## 🔧 Technical Architecture

### **Dependencies:**
```r
library(readr)        # Data import/export
library(dplyr)        # Data manipulation
library(stringr)      # String processing
library(tidyr)        # Data tidying
library(janitor)      # Data cleaning
library(data.table)   # High-performance data operations
library(lubridate)    # Date/time handling
library(readxl)       # Excel file processing
library(googlesheets4) # Google Sheets integration
library(ggplot2)      # Data visualization
library(gt)           # Table formatting
```

### **Output Artifacts:**
- **Primary Datasets**: `cl1_RetentionFact.csv` - Integrated retention analysis dataset
- **Summary Reports**: `bcm_term__sum_202509.csv`, `bcb_term__sum.csv` - Aggregated revenue summaries
- **Quality Metrics**: `map_it_*.csv` files - Data profiling and validation reports
- **Exception Reports**: `cl1_RetentionFact_na.csv` - Records with missing critical data

### **Data Processing Workflow:**
1. **Extract**: Multi-source data ingestion from Tableau Prep flows and historical archives
2. **Transform**: Complex data cleaning, standardization, and enrichment processes
3. **Integrate**: Cross-campus data unification with consistent schema
4. **Analyze**: Cohort-based aggregations and retention calculations
5. **Export**: Multiple output formats for downstream analysis and reporting

## 🚀 Usage Instructions

### **Prerequisites:**
- R version 4.0+ with required packages installed
- Access to Berklee's data infrastructure and file systems
- Sufficient computational resources for large dataset processing

### **Execution:**
```bash
# Run the primary analysis
Rscript cohort_segmentation_jschofield.r

# Or execute the pipeline
Rscript cohort_pipeline
```

### **Configuration:**
- Update file paths in the script header for your environment
- Modify date ranges and cohort parameters as needed
- Adjust output file naming conventions for your workflow

## 📋 Data Governance

### **Privacy & Security:**
- Student data is processed in accordance with FERPA regulations
- All exports contain de-identified or aggregated information
- Access controls align with institutional data governance policies

### **Quality Assurance:**
- Automated data validation checks throughout the pipeline
- Exception reporting for manual review and correction
- Version control for reproducible analytical results

## 🔄 Version Control & Updates

### Getting Last Commit Information

To find out when the most recent changes were made to the codebase:

#### Quick Method
```bash
./get_last_commit.sh
```

#### Manual Method
```bash
# Get last commit date
git log -1 --pretty=format:'%ad' --date=iso

# Get full last commit info
git log -1 --pretty=format:'%H %ad %s' --date=iso
```

## 📞 Support & Collaboration

For questions about the analytical methodology, data sources, or technical implementation, please refer to the script documentation or contact the development team. This analysis framework supports Berklee's strategic enrollment initiatives and financial planning processes.
