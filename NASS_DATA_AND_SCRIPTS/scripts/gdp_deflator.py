import pandas as pd
import numpy as np
import os

# GDP deflators by year (based on the examples in your document with 2017=100)
gdp_deflators = {
    1980: 42.28, 1981: 46.64, 1982: 49.46, 1983: 51.44, 1984: 53.45, 1985: 55.24, 
    1986: 56.32, 1987: 57.93, 1988: 60.07, 1989: 62.24, 1990: 64.39, 1991: 66.79, 
    1992: 68.11, 1993: 69.88, 1994: 71.37, 1995: 72.93, 1996: 74.46, 1997: 75.83, 
    1998: 76.61, 1999: 77.78, 2000: 79.87, 2001: 81.82, 2002: 83.33, 2003: 85.27, 
    2004: 88.01, 2005: 91.25, 2006: 94.19, 2007: 96.51, 2008: 98.63, 2009: 99.80, 
    2010: 101.22, 2011: 103.31, 2012: 105.28, 2013: 106.91, 2014: 108.81, 2015: 109.83, 
    2016: 110.99, 2017: 100.00, 2018: 103.08, 2019: 105.99, 2020: 108.26, 2021: 114.39, 
    2022: 121.83, 2023: 126.98, 2024: 131.05
}

def apply_gdp_deflator(input_file, output_file, target_year=2024):
    """
    Apply GDP deflator adjustment to prices in agricultural data CSV
    
    Parameters:
    -----------
    input_file : str
        Path to the input CSV file
    output_file : str
        Path to save the adjusted CSV file
    target_year : int
        Year to adjust prices to (default: 2024)
    """
    # Read the CSV file
    print(f"Reading data from {input_file}...")
    df = pd.read_csv(input_file, skipinitialspace=True)
    
    # Clean up column names by stripping whitespace
    df.columns = df.columns.str.strip()
    
    # Create a copy of the original DataFrame
    adjusted_df = df.copy()
    
    # Clean and convert Year to integer, handling potential errors
    adjusted_df['Year'] = pd.to_numeric(adjusted_df['Year'], errors='coerce')
    
    # Clean and convert Price P/U to numeric, handling potential errors
    adjusted_df['Price P/U'] = pd.to_numeric(adjusted_df['Price P/U'], errors='coerce')
    
    # Add GDP deflator columns
    print("Calculating GDP deflator adjustments...")
    
    # Function to get GDP deflator for a given year
    def get_deflator(year):
        if pd.isna(year) or year not in gdp_deflators:
            return np.nan
        return gdp_deflators[year]
    
    # Add GDP deflator for the original year
    adjusted_df['GDP_Deflator_Original_Year'] = adjusted_df['Year'].apply(get_deflator)
    
    # Add GDP deflator for the target year
    target_deflator = gdp_deflators.get(target_year, np.nan)
    adjusted_df['GDP_Deflator_Target_Year'] = target_deflator
    
    # Calculate the adjustment factor
    adjusted_df['Adjustment_Factor'] = adjusted_df['GDP_Deflator_Target_Year'] / adjusted_df['GDP_Deflator_Original_Year']
    
    # Calculate the adjusted price
    adjusted_df['Price_Adjusted_to_Target_Year'] = adjusted_df['Price P/U'] * adjusted_df['Adjustment_Factor']
    
    # Save the adjusted DataFrame to a new CSV
    print(f"Saving adjusted data to {output_file}...")
    adjusted_df.to_csv(output_file, index=False)
    
    print(f"Processed {len(adjusted_df)} rows. Adjustment complete!")
    return adjusted_df

if __name__ == "__main__":
    # Since the script is in the scripts folder, adjust the relative paths
    input_file = "../data/final_mapped_data/mapped_combineddata_2024_4.csv"
    
    # Create output directory if it doesn't exist
    output_dir = "../data/gdp_adjusted_data"
    os.makedirs(output_dir, exist_ok=True)
    
    # Path for your output file
    output_file = os.path.join(output_dir, "gdp_adjusted_data.csv")
    
    # Run the adjustment (change target_year if needed)
    apply_gdp_deflator(input_file, output_file, target_year=2024)
