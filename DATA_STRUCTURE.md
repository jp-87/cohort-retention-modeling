# FROM Data Configuration Template


# This file shows the expected data structure and file naming conventions
# for the Cohort Segmentation and Net Tuition Revenue Analysis script

## Directory Structure
```
your_project/
├── data/
│   ├── campus_a_merged_nr.csv          # Primary campus net revenue data
│   ├── campus_a_xreference.csv         # Primary campus cross-reference mappings
│   ├── campus_a_cohorts_current.csv    # Primary campus cohort assignments
│   ├── enrollment_archive.csv          # Historical enrollment archive
│   ├── campus_b_xreference.csv         # Business campus cross-reference
│   └── campus_b_raw/                   # Business campus raw data by term
│       ├── campus_b_2024_spring.csv
│       ├── campus_b_2024_fall.csv
│       ├── campus_b_2023_spring.csv
│       └── ... (additional terms)
└── output/                             # Generated output files
```

## Required Data Files

### 1. Campus A Merged Net Revenue Data (`campus_a_merged_nr.csv`)
Expected columns (flexible naming supported):
- student_id / id_stu / people_id: Student identifier
- term: Academic term (e.g., "2024FA", "2025SP")
- term_key: Unique term-student key
- student_type: Student classification
- student_program: Academic program
- program_active: Active program designation
- enroll_source: Enrollment source code
- tuition_charge: Tuition amount charged
- fa_institutional: Institutional financial aid
- comp_fee: Comprehensive fees
- cohort: Student cohort term

### 2. Campus A Cross-Reference (`campus_a_xreference.csv`)
Expected columns:
- pgm_dgr_enroll_tmpl: Program/degree/enrollment template key
- region: Geographic or program region
- enrollment_type: Type of enrollment
- attendance_type: Full-time/part-time status
- student_type: Student classification
- program: Program designation

### 3. Campus A Cohorts (`campus_a_cohorts_current.csv`)
Expected columns:
- student_id: Student identifier (matches primary data)
- cohort: Assigned cohort term
- Additional cohort-related fields

### 4. Enrollment Archive (`enrollment_archive.csv`)
Expected columns:
- term_key: Unique term-student key (matches primary data)
- matriculation_source: Source of matriculation
- Additional archive fields

### 5. Campus B Data Files (`campus_b_raw/*.csv`)
Expected columns:
- people_id / student_id: Student identifier
- academic_year: Academic year
- academic_term: Academic term (Spring/Fall/Summer)
- program: Academic program
- degree: Degree type
- enrollment_source: Enrollment source
- tuition_charge: Tuition amount
- scholarships: Scholarship/aid amount
- comp_charge: Comprehensive fee charges
- enrollment_status: Enrollment status code
- Additional campus-specific fields

### 6. Campus B Cross-Reference (`campus_b_xreference.csv`)
Expected columns:
- pgm_dgr_enroll_tmpl: Program/degree/enrollment template key
- region_xref: Geographic or program region
- enroll_type_xref: Type of enrollment
- attend_type_xref: Attendance type
- student_type_xref: Student classification
- program_xref: Program designation

## Data Quality Requirements

### Required Fields
- All files must have student identifier columns
- Date/term fields are required for temporal analysis
- Financial fields (tuition, aid, fees) should be numeric
- Cross-reference files must have matching template keys

### Data Formats
- Dates: Academic terms in YYYYTT format (e.g., "2024FA", "2025SP")
- Currency: Numeric values (no currency symbols)
- Text: Consistent categorical coding
- Missing values: Use NA or leave blank (avoid "NULL", "N/A" text)

### File Formats
- All files should be CSV format with headers
- UTF-8 encoding recommended
- Consistent column naming across related files

## Customization Notes

The script is designed to be flexible with column naming. It will attempt to:
1. Find columns using pattern matching (case-insensitive)
2. Handle missing optional columns gracefully
3. Provide informative error messages for missing required files
4. Generate data quality reports for validation

To customize for your environment:
1. Update file paths in the configuration section
2. Ensure your data files match the expected structure
3. Run the script and check the data quality reports
4. Adjust column mappings if needed using the rename() functions

## Output Files

The script generates several output files in the `output/` directory:
- `retention_fact_final.csv`: Main analytical dataset
- `campus_a_summary_YYYYMM.csv`: Campus A aggregated data
- `campus_a_export_YYYYMM.csv`: Campus A detailed export
- `campus_b_ntr_data_YYYYMM.csv`: Campus B combined raw data
- `data_quality_*.csv`: Data quality assessment reports
- `retention_fact_missing_data.csv`: Records with missing critical data

## Support

For questions about data structure or script customization:
1. Check the roxygen documentation in the script
2. Review the data quality reports after running
3. Examine the sample data structure above
4. Verify file paths and permissions
