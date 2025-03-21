import pandas as pd
import numpy as np
import os
from pathlib import Path
import sys

def process_with_cpi_adjustment():
    """
    Process agricultural data using Consumer Price Index (CPI) to adjust monetary values.
    Uses 2020 as the base year for inflation adjustment.
    Applies the adjustment formula: Original Value * (1 + (1 - adj_value))
    where adj_value is the ratio of base year CPI to the given year's CPI.
    """
    try:
        # Get script directory and project root
        script_dir = os.path.dirname(os.path.abspath(__file__))
        project_root = os.path.dirname(script_dir)  # Go up one level from scripts folder
        
        print(f"Script directory: {script_dir}")
        print(f"Project root: {project_root}")
        
        # Set up file paths relative to project root
        input_csv = os.path.join(project_root, "data", "with_revenue", "mapped_data_with_revenue.csv")
        output_dir = os.path.join(project_root, "data", "cpi_adjusted_data")
        
        # Ensure input path exists
        if not os.path.exists(input_csv):
            print(f"Could not find input file at: {input_csv}")
            input_csv = input("Please enter the full path to mapped_data_with_revenue.csv: ")
        
        # Create output directory
        os.makedirs(output_dir, exist_ok=True)
        
        # Set output file paths
        output_file = os.path.join(output_dir, "final_mapped_data_with_cpi.csv")
        
        print(f"Reading input file: {input_csv}")
        mapped_data = pd.read_csv(input_csv)
        
        # Define CPI values with 2024 as base year (2024 = 100)
        # Source: U.S. Bureau of Labor Statistics, Consumer Price Index (CPI-U, All items, Not Seasonally Adjusted)
        # Annual averages calculated from monthly BLS data
        cpi_by_year = {
            1980: 26.23,
            1981: 28.93,
            1982: 30.72,
            1983: 31.69,
            1984: 33.07,
            1985: 34.25,
            1986: 34.88,
            1987: 36.16,
            1988: 37.65,
            1989: 39.47,
            1990: 41.60,
            1991: 43.37,
            1992: 44.65,
            1993: 46.00,
            1994: 47.17,
            1995: 48.50,
            1996: 49.94,
            1997: 51.09,
            1998: 51.88,
            1999: 53.02,
            2000: 54.81,
            2001: 56.34,
            2002: 57.24,
            2003: 58.54,
            2004: 60.11,
            2005: 62.16,
            2006: 64.16,
            2007: 65.99,
            2008: 68.53,
            2009: 68.29,
            2010: 69.40,
            2011: 71.59,
            2012: 73.07,
            2013: 74.14,
            2014: 75.34,
            2015: 75.44,
            2016: 76.39,
            2017: 78.02,
            2018: 79.92,
            2019: 81.37,
            2020: 82.37,
            2021: 86.25,
            2022: 93.14,
            2023: 97.21,
            2024: 100.00,
            2025: 101.33
        }
        
        # Calculate CPI adjustment factors using the R script's formula approach
        # Instead of 100/cpi_value, we'll use (1 + (1 - adj_value)) where adj_value = year_cpi/base_cpi
        base_year_cpi = cpi_by_year[2024]  # Using 2024 as base year
        
        # Calculate adj_value for each year (ratio of year's CPI to base year CPI)
        adj_values = {year: cpi_value / base_year_cpi for year, cpi_value in cpi_by_year.items()}
        
        # Calculate the adjustment factor using the R script's formula: (1 + (1 - adj_value))
        cpi_adjustment_factors = {year: (1 + (1 - adj_val)) for year, adj_val in adj_values.items()}
        
        print("Using CPI values with 2020 as base year (matching R script approach)")
        print(f"CPI adjustment factors: {cpi_adjustment_factors}")
        
        print("Cleaning and processing data...")
        # Clean column names (remove leading/trailing whitespace)
        mapped_data.columns = [col.strip() for col in mapped_data.columns]
        
        # Print column names for debugging
        print("Input file columns:", mapped_data.columns.tolist())
        
        # Filter out rows with invalid year
        year_col = 'Year'
        if year_col not in mapped_data.columns:
            # Try to find the year column with different capitalization or spaces
            for col in mapped_data.columns:
                if col.lower().strip() == 'year':
                    year_col = col
                    break
            
        print(f"Using '{year_col}' as the year column")
        valid_data = mapped_data[pd.to_numeric(mapped_data[year_col], errors='coerce').notna()]
        valid_data[year_col] = valid_data[year_col].astype(int)
        
        print(f"Filtered {len(mapped_data) - len(valid_data)} rows with invalid years")
        
        # Find price and value column names
        price_col = None
        value_col = None
        gross_revenue_col = None
        
        for col in mapped_data.columns:
            if 'price' in col.lower() and 'p/u' in col.lower():
                price_col = col
            elif col.lower().strip() == 'value':
                value_col = col
            elif 'gross' in col.lower() and 'revenue' in col.lower():
                gross_revenue_col = col
        
        if not price_col:
            print("Warning: Could not find a price column. Using default 'Price P/U'")
            price_col = 'Price P/U'
        if not value_col:
            print("Warning: Could not find a value column. Using default 'Value'")
            value_col = 'Value'
        if not gross_revenue_col:
            print("Warning: Could not find a gross revenue column. Using default 'Gross Revenue'")
            gross_revenue_col = 'Gross Revenue'
            
        print(f"Using columns: Year='{year_col}', Price='{price_col}', Value='{value_col}', Gross Revenue='{gross_revenue_col}'")
        
        # Apply CPI adjustment to each row using the R script's formula
        def apply_cpi_adjustment(row):
            year = row[year_col]
            adjustment_factor = cpi_adjustment_factors.get(year, 1.0)
            
            # Clean and convert values
            try:
                price = float(row[price_col]) if pd.notna(row[price_col]) else 0
            except:
                price = 0
                
            try:
                value = float(str(row[value_col]).replace(',', '')) if pd.notna(row[value_col]) else 0
            except:
                value = 0
                
            try:
                gross_revenue = float(row[gross_revenue_col]) if pd.notna(row[gross_revenue_col]) else 0
            except:
                gross_revenue = 0
            
            # Calculate adjusted values using the R script's formula: original_value * (1 + (1 - adj_value))
            adjusted_price = price * adjustment_factor
            adjusted_value = value * adjustment_factor
            adjusted_total_production_value = value * adjustment_factor / 1000000  # Convert to millions
            adjusted_gross_revenue = gross_revenue * adjustment_factor
            
            # Add new columns
            row['adj_value'] = f"{adjusted_total_production_value:.5f}"
            row['Adjusted Price'] = f"{adjusted_price:.2f}"
            row['Adjusted Total Production Value'] = f"{adjusted_total_production_value:.5f}"
            row['Adjusted Gross Revenue'] = f"{adjusted_gross_revenue:.2f}"
            row['CPI Used'] = cpi_by_year.get(year, "N/A")
            row['CPI Adjustment Factor'] = adjustment_factor
            row['CPI Percent of Base Year'] = adj_values.get(year, "N/A")
            
            return row
        
        print("Applying CPI adjustment to each row...")
        processed_data = valid_data.apply(apply_cpi_adjustment, axis=1)
        
        # Write to CSV
        print(f"Writing output to: {output_file}")
        processed_data.to_csv(output_file, index=False)
        print(f"Successfully created {output_file} with {len(processed_data)} rows")
        
        # Also save a copy with a simpler name
        alt_output_file = os.path.join(output_dir, "cpi_adjusted_data.csv")
        processed_data.to_csv(alt_output_file, index=False)
        print(f"Also saved a copy to {alt_output_file}")
        
        return {"success": True, "row_count": len(processed_data), "output_file": output_file}
    
    except Exception as e:
        import traceback
        print(f"Error processing data: {str(e)}")
        print(traceback.format_exc())
        return {"success": False, "error": str(e)}

if __name__ == "__main__":
    print("Starting CPI inflation adjustment process...")
    result = process_with_cpi_adjustment()
    if result["success"]:
        print(f"Processing completed successfully with {result['row_count']} rows.")
        print(f"Output saved to {result['output_file']}")
        print("Data adjusted using Consumer Price Index (CPI) with 2020 as base year")
        print("Using adjustment formula from R script: original_value * (1 + (1 - adj_value))")
    else:
        print(f"Processing failed: {result['error']}")