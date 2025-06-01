import pandas as pd
import os

# Get absolute paths
current_dir = os.path.dirname(os.path.abspath(__file__))
project_root = os.path.dirname(current_dir)

# Define the input and output paths
input_path = os.path.join(
    project_root, 
    "data", 
    "cpi_adjusted_data", 
    "final_mapped_data_with_cpi.csv"
)
output_dir = os.path.join(project_root, "data", "remove_duplicates")
os.makedirs(output_dir, exist_ok=True)
output_path = os.path.join(output_dir, "final_mapped_data_without_duplicates.csv")

def fix_specific_county_names(input_file, output_file):
    """
    Very specific function to fix exactly two issues:
    1. Convert ALL variations of San Luis Obispo to exactly "San Luis Obispo"
    2. Convert "State Totals" to exactly "State Total"
    """
    # Read the CSV file
    print(f"Reading from: {input_file}")
    df = pd.read_csv(input_file)
    
    # Print original shape
    print(f"Original shape: {df.shape}")
    
    # Create a copy of the dataframe to work with
    df_copy = df.copy()
    
    # SPECIFIC FIX 1: First, check for "State Total" and "State Totals"
    state_counties = [c for c in df_copy['County'].unique() if isinstance(c, str) and 'State' in c]
    print("\nState-related counties BEFORE fixing:")
    for c in sorted(state_counties):
        print(f"  - '{c}'")
    
    # Replace all variations of "State Total/Totals" with "State Total"
    # Make sure to strip whitespace and handle case sensitivity
    df_copy['County'] = df_copy['County'].apply(
        lambda x: 'State Total' if isinstance(x, str) and x.strip().lower() in ['state total', 'state totals'] else x
    )
    
    # SPECIFIC FIX 2: Check for San Luis Obispo variations
    luis_counties = [c for c in df_copy['County'].unique() if isinstance(c, str) and 'Luis' in c]
    print("\nSan Luis counties BEFORE fixing:")
    for c in sorted(luis_counties):
        print(f"  - '{c}'")
    
    # Replace all Luis variations except the correct one
    for idx, row in df_copy.iterrows():
        if isinstance(row['County'], str) and 'Luis' in row['County'] and row['County'] != 'San Luis Obispo':
            df_copy.at[idx, 'County'] = 'San Luis Obispo'
    
    # Verify fixes worked
    state_counties_after = [c for c in df_copy['County'].unique() if isinstance(c, str) and 'State' in c]
    print("\nState-related counties AFTER fixing:")
    for c in sorted(state_counties_after):
        print(f"  - '{c}'")
    
    luis_counties_after = [c for c in df_copy['County'].unique() if isinstance(c, str) and 'Luis' in c]
    print("\nSan Luis counties AFTER fixing:")
    for c in sorted(luis_counties_after):
        print(f"  - '{c}'")
    
    # Print data before aggregation to verify State Total entries
    state_total_data = df_copy[df_copy['County'] == 'State Total']
    print(f"\nNumber of 'State Total' entries before aggregation: {len(state_total_data)}")
    if len(state_total_data) > 0:
        print(f"Sample years in State Total data: {sorted(state_total_data['Year'].unique()[:5])}")
        print(f"Sample commodities in State Total data: {sorted(state_total_data['Commodity Code'].unique()[:5])}")
    
    # Group by key columns to consolidate duplicates
    key_columns = ['Year', 'Commodity Code', 'Crop Name', 'County']
    
    # All other columns will be aggregated
    value_columns = [col for col in df_copy.columns if col not in key_columns]
    
    # Define specific aggregation methods for numeric columns
    agg_dict = {}
    for col in value_columns:
        if col in ['Harvested Acres', 'Value', 'Adjusted Total Production Value', 
                  'Gross Revenue', 'Adjusted Gross Revenue', 'Production']:
            agg_dict[col] = 'sum'  # Sum these columns
        elif col in ['Yield', 'Price P/U', 'Adjusted Price']:
            # For these columns, we want weighted averages, but if not possible, use regular mean
            agg_dict[col] = 'mean'
        elif col == 'Unit Type':
            # For Unit Type, we'll use the most common value
            agg_dict[col] = lambda x: x.mode()[0] if not x.empty else None
        else:
            agg_dict[col] = 'first'  # Default for categorical columns
    
    # Group and aggregate
    df_consolidated = df_copy.groupby(key_columns, as_index=False).agg(agg_dict)
    
    # Print final shape
    print(f"Final shape after consolidation: {df_consolidated.shape}")
    
    # Verify State Total entries after aggregation
    state_total_after = df_consolidated[df_consolidated['County'] == 'State Total']
    print(f"\nNumber of 'State Total' entries after aggregation: {len(state_total_after)}")
    if len(state_total_after) > 0:
        print(f"Sample years in State Total data: {sorted(state_total_after['Year'].unique()[:5])}")
        print(f"Sample commodities in State Total data: {sorted(state_total_after['Commodity Code'].unique()[:5])}")
    
    # Save the result
    df_consolidated.to_csv(output_file, index=False)
    print(f"Processed data saved to: {output_file}")
    
    return df_consolidated

# Run the function
if __name__ == "__main__":
    try:
        fix_specific_county_names(input_path, output_path)
    except Exception as e:
        print(f"Error occurred: {str(e)}")
        import traceback
        traceback.print_exc()