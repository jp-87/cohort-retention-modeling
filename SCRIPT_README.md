# Cohort Segmentation Analysis 

### Prerequisites
- R 4.0+ with required packages (see script header)
- Student enrollment and financial data (see DATA_STRUCTURE.md)

### Installation
```r
# Install required packages
install.packages(c("readr", "dplyr", "stringr", "tidyr", "janitor", 
                   "data.table", "lubridate", "readxl", "googlesheets4", 
                   "ggplot2", "gt", "here"))
```

### Basic Usage
1. Place your data files in the `data/` directory (see DATA_STRUCTURE.md)
2. Run the script:
```r
source("cohort_pipeline_publication_ready.r")
```

### Configuration
The script uses flexible configuration variables at the top:
- Modify `DATA_DIR` and `OUTPUT_DIR` paths as needed
- Update file naming patterns to match your data
- Customize column mappings for different data sources

## Features
### Analytics Capabilities
- **Multi-Campus Integration**: Combines data from multiple campuses
- **Cohort Tracking**: Student progression analysis across terms
- **Financial Modeling**: Net tuition revenue calculations
- **Retention Analysis**: Comprehensive retention metrics
- **Data Quality**: Automated quality assessment and reporting

### Technical Features
- **Error Handling**: Graceful handling of missing files and columns
- **Flexible Matching**: Primary and alternate cross-reference matching
- **Data Cleaning**: Automated standardization and validation
- **Progress Reporting**: Informative console output during processing
- **Quality Reports**: Detailed data profiling and missing data analysis

## Output Files

The script generates several analytical outputs:

| File | Description |
|------|-------------|
| `retention_fact_final.csv` | Integrated retention analysis dataset |
| `campus_a_summary_YYYYMM.csv` | Campus A financial summaries |
| `campus_b_ntr_data_YYYYMM.csv` | Campus B combined data |
| `data_quality_*.csv` | Data quality assessment reports |
| `retention_fact_missing_data.csv` | Records with missing critical information |

## Data Requirements

See [DATA_STRUCTURE.md](DATA_STRUCTURE.md) for complete data specifications including:
- Required file formats and column structures
- Expected data types and naming conventions
- Directory organization
- Quality requirements

## Key Functions

### Term Calculation Functions
- `term_code_to_num()`: Convert academic terms to comparable numbers
- `get_next_term()`: Calculate sequential academic terms
- `get_terms_ahead()`: Project future terms
- `get_num_terms_between()`: Calculate term distances

### Data Processing Functions
- Campus A and Campus B data integration
- Cross-reference matching with fallback logic
- Financial aggregation and Net Tuition Revenue calculation
- Retention cohort preparation

### Quality Assurance
- `map_it()`: Data profiling and quality assessment
- Missing data identification and reporting
- Data type validation and standardization

## Customization

The script is designed for easy customization:

1. **File Paths**: Update configuration variables for your environment
2. **Column Mapping**: Modify rename operations for different schemas
3. **Business Logic**: Adjust filtering and categorization rules
4. **Output Format**: Customize aggregation levels and export formats

## Example Usage

```r
# Load the script
source("cohort_pipeline_publication_ready.r")

# The script will:
# 1. Check for required data files
# 2. Process Campus A data with cross-reference matching
# 3. Load and combine Campus B multi-year data
# 4. Create integrated retention dataset
# 5. Generate quality reports and final outputs
# 6. Provide progress updates and summary statistics
```

## Error Handling

The script includes comprehensive error handling:
- Missing file detection with helpful error messages
- Graceful handling of missing columns
- Data type validation and conversion
- Progress reporting and quality statistics

## Contributing

- **Generalizable**: Works with different institutional data structures (with some tweaks)
- **Maintainable**: Clear documentation and modular design
- **Extensible**: Easy to add new features or data sources

## License

This script is provided for educational and research purposes. Please ensure compliance with your institution's data governance policies when using with student data.
