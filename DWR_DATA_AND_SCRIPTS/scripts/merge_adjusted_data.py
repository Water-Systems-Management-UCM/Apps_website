import pandas as pd
import os
import re

def merge_agricultural_and_cpi_data():
    # Define file paths when running from scripts directory
    cpi_data_path = "../data/cpi_adjusted_data_from_NASS/final_mapped_data_with_cpi.csv"
    ag_data_path = "../data/transformed_ag_data/consolidated_agricultural_data.csv"
    output_path = "../data/transformed_ag_data/consolidated_agricultural_data_with_cpi.csv"
    
    # Load the datasets
    print("Loading CPI-adjusted data...")
    cpi_df = pd.read_csv(cpi_data_path)
    
    print("Loading agricultural data...")
    ag_df = pd.read_csv(ag_data_path)
    
    # Print the shapes of both datasets
    print(f"\nCPI data shape: {cpi_df.shape}")
    print(f"Agricultural data shape: {ag_df.shape}")
    
    print("\nCPI data columns:")
    print(cpi_df.columns.tolist())
    
    print("\nAgricultural data columns:")
    print(ag_df.columns.tolist())
    
    # Function to standardize county names
    def standardize_county(county):
        if pd.isna(county):
            return county
        
        # Convert to string if not already
        county = str(county).strip()
        
        # Remove "County" suffix if present
        county = re.sub(r'\s+County$', '', county, flags=re.IGNORECASE)
        
        # Handle common variations
        county_mapping = {
            'SF': 'San Francisco',
            'SLO': 'San Luis Obispo',
            'San Luis Obisp': 'San Luis Obispo',
            # Add more mappings as needed
        }
        
        return county_mapping.get(county, county)
    
    # Function to standardize crop names
    def standardize_crop(crop):
        if pd.isna(crop):
            return crop
        
        # Convert to string if not already
        crop = str(crop).strip()
        
        # Handle common variations and groupings
        crop_mapping = {
            'ALFALFA': 'Alfalfa',
            'ALFALFA HAY': 'Alfalfa',
            'ALMONDS': 'Almonds & Pistachios',
            'PISTACHIOS': 'Almonds & Pistachios',
            # Add more mappings as needed
        }
        
        return crop_mapping.get(crop, crop)
    
    # Standardize column names in both datasets
    # For CPI data:
    cpi_columns = {
        'Year': 'year',
        'County': 'NAME',
        'Crop Name': 'crop',
        'Crop': 'crop'
    }
    
    # Apply the mappings that exist in the dataframe
    for old_col, new_col in cpi_columns.items():
        if old_col in cpi_df.columns:
            cpi_df.rename(columns={old_col: new_col}, inplace=True)
    
    # For agricultural data:
    ag_columns = {
        'Year': 'year',
        'NAME': 'NAME',
        'Name': 'NAME',
        'Crop': 'crop'
    }
    
    # Apply the mappings that exist in the dataframe
    for old_col, new_col in ag_columns.items():
        if old_col in ag_df.columns:
            ag_df.rename(columns={old_col: new_col}, inplace=True)
    
    print("\nRenamed CPI data columns:")
    print(cpi_df.columns.tolist())
    
    print("\nRenamed Agricultural data columns:")
    print(ag_df.columns.tolist())
    
    # Clean and standardize data for merging
    # 1. Standardize county and crop names
    if 'NAME' in cpi_df.columns:
        cpi_df['NAME'] = cpi_df['NAME'].apply(standardize_county)
    
    if 'NAME' in ag_df.columns:
        ag_df['NAME'] = ag_df['NAME'].apply(standardize_county)
    
    if 'crop' in cpi_df.columns:
        cpi_df['crop'] = cpi_df['crop'].apply(standardize_crop)
    
    if 'crop' in ag_df.columns:
        ag_df['crop'] = ag_df['crop'].apply(standardize_crop)
    
    # 2. Convert years to same format (integers)
    if 'year' in cpi_df.columns:
        cpi_df['year'] = pd.to_numeric(cpi_df['year'], errors='coerce').fillna(0).astype(int)
    
    if 'year' in ag_df.columns:
        ag_df['year'] = pd.to_numeric(ag_df['year'], errors='coerce').fillna(0).astype(int)
    
    # Create standardized columns for case-insensitive matching
    cpi_df['name_std'] = cpi_df['NAME'].str.lower() if 'NAME' in cpi_df.columns else ''
    cpi_df['crop_std'] = cpi_df['crop'].str.lower() if 'crop' in cpi_df.columns else ''
    
    ag_df['name_std'] = ag_df['NAME'].str.lower() if 'NAME' in ag_df.columns else ''
    ag_df['crop_std'] = ag_df['crop'].str.lower() if 'crop' in ag_df.columns else ''
    
    # Print samples to verify data cleaning
    print("\nCPI data sample after standardization (first 3 rows):")
    print(cpi_df[['year', 'NAME', 'crop', 'name_std', 'crop_std']].head(3))
    
    print("\nAgricultural data sample after standardization (first 3 rows):")
    print(ag_df[['year', 'NAME', 'crop', 'name_std', 'crop_std']].head(3))
    
    # Verify year ranges for overlap
    cpi_years = cpi_df['year'].unique()
    ag_years = ag_df['year'].unique()
    
    print(f"\nCPI data year range: {min(cpi_years)} to {max(cpi_years)}")
    print(f"Agricultural data year range: {min(ag_years)} to {max(ag_years)}")
    
    overlapping_years = set(cpi_years).intersection(set(ag_years))
    print(f"Number of overlapping years: {len(overlapping_years)}")
    if overlapping_years:
        print(f"Overlapping years: {sorted(overlapping_years)[:10]}...")
    
    # Only add the specific CPI columns requested
    cpi_columns_to_add = []
    for col in cpi_df.columns:
        if any(pattern in col.lower() for pattern in ['adjusted price', 'adjusted total production value', 'adjusted gross revenue']):
            cpi_columns_to_add.append(col)
    
    print(f"\nDetected CPI columns to add: {cpi_columns_to_add}")
    
    # Merge datasets using the standardized fields for matching
    print("\nMerging datasets...")
    merged_df = pd.merge(
        ag_df,
        cpi_df[['name_std', 'crop_std', 'year'] + cpi_columns_to_add],
        left_on=['name_std', 'crop_std', 'year'],
        right_on=['name_std', 'crop_std', 'year'],
        how='left'
    )
    
    # Check for merge success
    non_null_count = merged_df[cpi_columns_to_add].notnull().any(axis=1).sum() if cpi_columns_to_add else 0
    print(f"\nRows with matched CPI data: {non_null_count} out of {len(merged_df)} ({non_null_count/len(merged_df)*100:.2f}%)")
    
    if non_null_count < len(merged_df) * 0.5:  # Less than 50% match
        print("\nWARNING: Low match rate! Trying fuzzy matching for unmatched rows...")
        
        # Identify unmatched rows
        unmatched_mask = merged_df[cpi_columns_to_add[0]].isnull() if cpi_columns_to_add else pd.Series(False, index=merged_df.index)
        unmatched_df = merged_df[unmatched_mask].copy()
        
        # Only process if we have unmatched rows and both crop and name columns
        if len(unmatched_df) > 0 and 'crop' in unmatched_df.columns and 'NAME' in unmatched_df.columns:
            print(f"Attempting fuzzy match for {len(unmatched_df)} unmatched rows...")
            
            # Create a dictionary of CPI data for lookup
            cpi_lookup = {}
            for _, row in cpi_df.iterrows():
                key = (row['year'], row['name_std'])
                if key not in cpi_lookup:
                    cpi_lookup[key] = []
                cpi_lookup[key].append({
                    'crop': row['crop_std'],
                    'values': {col: row[col] for col in cpi_columns_to_add}
                })
            
            # Function for fuzzy matching crops
            def find_closest_crop(row):
                year = row['year']
                county = row['name_std']
                crop = row['crop_std']
                
                # First try exact year and county match
                key = (year, county)
                if key in cpi_lookup:
                    # Find most similar crop name
                    best_match = None
                    best_similarity = 0
                    
                    for entry in cpi_lookup[key]:
                        cpi_crop = entry['crop']
                        
                        # Simple similarity: common substring
                        similarity = 0
                        if crop in cpi_crop or cpi_crop in crop:
                            # Length of common substring relative to longer string
                            similarity = min(len(crop), len(cpi_crop)) / max(len(crop), len(cpi_crop))
                        
                        if similarity > best_similarity:
                            best_similarity = similarity
                            best_match = entry
                    
                    # If we found a reasonable match
                    if best_match and best_similarity > 0.6:
                        return best_match['values']
                
                return None
            
            # Apply fuzzy matching to unmatched rows
            fuzzy_matched = 0
            for idx, row in unmatched_df.iterrows():
                match_values = find_closest_crop(row)
                if match_values:
                    for col, value in match_values.items():
                        merged_df.at[idx, col] = value
                    fuzzy_matched += 1
            
            print(f"Fuzzy matching added data to {fuzzy_matched} previously unmatched rows.")
    
    # Clean up temporary columns
    for col in ['name_std', 'crop_std']:
        if col in merged_df.columns:
            merged_df.drop(col, axis=1, inplace=True)
    
    # Final check of results
    non_null_count = merged_df[cpi_columns_to_add].notnull().any(axis=1).sum() if cpi_columns_to_add else 0
    print(f"\nFinal rows with matched CPI data: {non_null_count} out of {len(merged_df)} ({non_null_count/len(merged_df)*100:.2f}%)")
    
    # Save the merged dataset
    print(f"\nSaving merged dataset to {output_path}...")
    merged_df.to_csv(output_path, index=False)
    print("Merge completed successfully!")
    
    # Return sample rows to display results
    sample_rows = min(10, len(merged_df))
    return merged_df.head(sample_rows)

if __name__ == "__main__":
    try:
        result = merge_agricultural_and_cpi_data()
        print("\nSample of merged data:")
        print(result)
    except Exception as e:
        print(f"Error occurred: {str(e)}")