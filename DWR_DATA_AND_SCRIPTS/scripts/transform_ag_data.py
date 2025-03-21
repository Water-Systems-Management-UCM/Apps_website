import pandas as pd
import os
import re
import sys

def extract_county_name(county_str):
    """Extract just the county name from the format like '25_Modoc'"""
    if isinstance(county_str, str) and '_' in county_str:
        return county_str.split('_')[1]
    return county_str

def process_agricultural_data(aw_file, total_land_file, tw_file, output_file):
    """
    Process agricultural data from three Excel files and create a consolidated CSV file.
    
    Parameters:
    -----------
    aw_file : str
        Path to Excel file containing AW (Average Water) data
    total_land_file : str
        Path to Excel file containing Total Land data
    tw_file : str
        Path to Excel file containing TW (Total Water) data
    output_file : str
        Path to output CSV file
    """
    print(f"Processing {aw_file}...")
    aw_data = pd.read_excel(aw_file)
    
    print(f"Processing {total_land_file}...")
    total_land_data = pd.read_excel(total_land_file)
    
    print(f"Processing {tw_file}...")
    tw_data = pd.read_excel(tw_file)
    
    # Extract county names (remove prefixes like "25_")
    aw_data['NAME'] = aw_data['County'].apply(extract_county_name)
    total_land_data['NAME'] = total_land_data['County'].apply(extract_county_name)
    tw_data['NAME'] = tw_data['County'].apply(extract_county_name)
    
    # Get all crop columns (excluding metadata columns)
    metadata_cols = ['Year', 'RO', 'HR', 'County', 'NAME', 'Total ICA', 'Total AW', 'Avg. AW']
    
    # Process each dataset to create a long format dataframe
    results = []
    
    # Process AW data (Average Water)
    for _, row in aw_data.iterrows():
        county = row['NAME']
        year = row['Year']
        
        for col in aw_data.columns:
            if col not in metadata_cols and col is not None and pd.notna(col):
                crop = col.strip()
                value = row[col]
                
                # Only include rows with actual values
                if pd.notna(value) and value != 0:
                    results.append({
                        'NAME': county,
                        'Crop': crop,
                        'Year': year,
                        'AW': value,
                        'Total_Land': 0,  # Default value, will update later
                        'TW': 0           # Default value, will update later
                    })
    
    # Process Total Land data
    for _, row in total_land_data.iterrows():
        county = row['NAME']
        year = row['Year']
        
        for col in total_land_data.columns:
            if col not in metadata_cols and col is not None and pd.notna(col):
                crop = col.strip()
                value = row[col]
                
                # Only include rows with actual values
                if pd.notna(value) and value != 0:
                    # Check if an entry already exists
                    existing = next((item for item in results if 
                                     item['NAME'] == county and 
                                     item['Crop'] == crop and 
                                     item['Year'] == year), None)
                    
                    if existing:
                        existing['Total_Land'] = value
                    else:
                        results.append({
                            'NAME': county,
                            'Crop': crop,
                            'Year': year,
                            'AW': 0,       # Default value
                            'Total_Land': value,
                            'TW': 0        # Default value
                        })
    
    # Process TW data (Total Water)
    for _, row in tw_data.iterrows():
        county = row['NAME']
        year = row['Year']
        
        for col in tw_data.columns:
            if col not in metadata_cols and col is not None and pd.notna(col):
                crop = col.strip()
                value = row[col]
                
                # Only include rows with actual values
                if pd.notna(value) and value != 0:
                    # Check if an entry already exists
                    existing = next((item for item in results if 
                                     item['NAME'] == county and 
                                     item['Crop'] == crop and 
                                     item['Year'] == year), None)
                    
                    if existing:
                        existing['TW'] = value
                    else:
                        results.append({
                            'NAME': county,
                            'Crop': crop,
                            'Year': year,
                            'AW': 0,        # Default value
                            'Total_Land': 0, # Default value
                            'TW': value
                        })
    
    # Convert to DataFrame
    result_df = pd.DataFrame(results)
    
    # Sort by NAME, Crop, and Year
    result_df = result_df.sort_values(['NAME', 'Crop', 'Year'])
    
    # Write to CSV
    result_df.to_csv(output_file, index=False)
    print(f"CSV file created successfully: {output_file}")
    print(f"Total rows in the CSV: {len(result_df)}")
    
    # Display sample of the data
    print("\nSample data (first 5 rows):")
    print(result_df.head(5))

if __name__ == "__main__":
    # Define file paths based on the directory structure
    script_dir = os.path.dirname(os.path.abspath(__file__))
    base_dir = os.path.dirname(script_dir)  # Go up one level from scripts
    
    # Cleaned data files
    cleaned_data_dir = os.path.join(base_dir, "data", "cleaned_data")
    aw_file = os.path.join(cleaned_data_dir, "AW_DATA.xlsx")
    total_land_file = os.path.join(cleaned_data_dir, "TOTAL_LAND_DATA.xlsx")
    tw_file = os.path.join(cleaned_data_dir, "TW_DATA.xlsx")
    
    # Output directory
    transformed_dir = os.path.join(base_dir, "data", "transformed_ag_data")
    # Create output directory if it doesn't exist
    os.makedirs(transformed_dir, exist_ok=True)
    
    output_file = os.path.join(transformed_dir, "consolidated_agricultural_data.csv")
    
    # Process the data
    try:
        process_agricultural_data(aw_file, total_land_file, tw_file, output_file)
        print(f"Script executed successfully. Output saved to {output_file}")
    except Exception as e:
        print(f"Error occurred: {str(e)}")
        sys.exit(1)