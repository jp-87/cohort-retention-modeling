# Student Persistence & Revenue Risk Analysis  

A data analysis framework for Berklee College of Music to track student persistence, identify retention risks, and model tuition revenue patterns across BCM and BCB programs.  

## Overview  

Think of it as a map. Each new class of students is a cohort, and we follow them term by term. We track who stays, who leaves, and how financial aid and tuition shape the bigger picture. The goal is to show both where students face challenges and how those challenges ripple into the school’s finances.  

## How It Works  

The core scripts are written in R and process several years of student and financial data. One script groups students into cohorts and converts academic terms like “2024FA” into a clean timeline. Another script runs the pipeline that ties everything together, from data extraction to final reports. The system standardizes records from multiple sources, fills in gaps where possible, and calculates net tuition revenue by combining tuition, aid, and fees.  

## What It Shows  

The analysis uncovers patterns in revenue, showing that tuition is only part of the story. Aid, grants, and fees change the balance, and the mix varies by program, entry year, and region.  

It also highlights when students are most likely to drop off between terms. The risk points look different for undergraduates, graduate students, and non-degree programs. By spotting these points early, the school can better support the students who need it most.  

Along the way, the scripts flag data quality issues. Missing records, inconsistent classifications, or timeline errors are caught and reported for review.  

## Why It Matters  

For students, this helps identify where extra support can make the difference between staying and leaving. For the institution, it makes financial forecasts more accurate and guides recruitment and retention strategies. It shows that student success and financial planning are not separate problems but two sides of the same one.  

## Technical Notes  

The project uses R with common packages for data wrangling, visualization, and reporting. Outputs include cleaned datasets, revenue summaries, and exception reports. The pipeline follows a simple pattern: extract, clean, integrate, analyze, and export.  

## Governance and Versioning  

All student data is handled according to FERPA rules. Exports are either aggregated or de-identified. Automated checks keep the data consistent, and version control ensures results can be reproduced.  

To see the latest update, run:  
```bash
./get_last_commit.sh