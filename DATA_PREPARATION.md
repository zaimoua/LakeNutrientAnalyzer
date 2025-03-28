# Data Preparation Guide for Lake Nutrient Analyzer

This document provides detailed instructions for preparing the database required by the Lake Nutrient Analyzer application.

## Overview

The Lake Nutrient Analyzer requires the `IWR66_nutrients_lakes.sqlite` database file, which contains water quality data from Florida's Impaired Waters Rule (IWR) program. This document explains three methods to obtain or create this database.

## Option 1: Download Pre-Processed Database (Recommended)

For users who want to get started quickly, we provide a pre-processed SQLite database ready for use.

1. Download the database file from [Google Drive](https://drive.google.com/file/d/1XU1Ggoyxxnph3UGgDpesha0VknGR4rRQ/view?usp=sharing)
2. Save the file as `IWR66_nutrients_lakes.sqlite` in the `data/` directory of the application
3. No further processing is required

This is the recommended approach for most users, especially those without database experience.

## Option 2: Automated Conversion

If you prefer to process the raw data yourself but want to automate the conversion, we provide a helper script.

1. Download the IWR Access database file from [Florida DEP](https://publicfiles.dep.state.fl.us/dear/IWR/)
2. Place the Access file in a location accessible to R
3. Run the automated conversion script:
   ```r
   source("install_database.R")
   ```
4. Follow the on-screen instructions to select the Access file and specify the output location

The script will handle the export and conversion process and create the SQLite database with the correct structure.

## Option 3: Manual Conversion

For advanced users who want complete control over the database creation process, follow these steps:

### Prerequisites
- Microsoft Access (to open the IWR database)
- SQLite command-line tool (`sqlite3`)

### Step 1: Obtain Raw Data
1. Download the IWR Access database from [Florida DEP](https://publicfiles.dep.state.fl.us/dear/IWR/)
2. Open the database in Microsoft Access

### Step 2: Export Data as CSV
1. Open each of the following tables in Access:
   - `rawDataDB1`
   - `rawDataDB2`
   - `rawDataDB3`
   - `rawDataDB4`
2. Export each table as a CSV file:
   - Click "External Data" tab
   - Select "Text File" under Export
   - Choose CSV format
   - Save as `rawData1.csv`, `rawData2.csv`, etc.

### Step 3: Create SQLite Database
1. Open a command prompt/terminal
2. Navigate to your working directory
3. Create a new SQLite database:
   ```bash
   sqlite3 data/IWR66_nutrients_lakes.sqlite
   ```
4. Create the RawData table:
   ```sql
   CREATE TABLE RawData(
     wbid text,
     STA text,
     year integer,
     month integer,
     day integer,
     time integer,
     depth float,
     param integer,
     mastercode text,
     result float,
     rcode text,
     xcode text,
     lab integer,
     mdl text,
     pql text,
     dunits text,
     ds text,
     season integer,
     hurricaneflag integer,
     week2 integer,
     planperiod text,
     verperiod text,
     epa text
   );
   ```

5. Import the CSV files:
   ```sql
   .separator ,
   .import /path/to/rawData1.csv RawData
   .import /path/to/rawData2.csv RawData
   .import /path/to/rawData3.csv RawData
   .import /path/to/rawData4.csv RawData
   ```

6. Create an index for performance:
   ```sql
   CREATE INDEX search_speed ON RawData (wbid, sta, year, month, day, mastercode, result);
   ```

