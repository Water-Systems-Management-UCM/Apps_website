import pandas as pd

"""

This script is only for cleaning data from 2021-forward. They have a new format that requires a cleaning process.

"""

def reduce_columns(input_file, output_file, columns_to_keep=11):
    """
    Reads a CSV file and keeps only the specified number of columns.
    
    Parameters:
    input_file (str): Path to input CSV file
    output_file (str): Path to save the output CSV file
    columns_to_keep (int): Number of columns to keep (default: 12)
    """
    try:
        # Read the CSV file
        df = pd.read_csv(input_file)
        
        # Get current number of columns
        original_cols = len(df.columns)
        
        if original_cols <= columns_to_keep:
            print(f"File already has {original_cols} columns, which is <= {columns_to_keep}")
            return df
            
        # Keep only the first n columns
        df_reduced = df.iloc[:, :columns_to_keep]
        
        # Save to new CSV file
        df_reduced.to_csv(output_file, index=False)
        
        print(f"Successfully reduced columns from {original_cols} to {columns_to_keep}")
        print(f"New columns: {', '.join(df_reduced.columns)}")
        
        return df_reduced
        
    except Exception as e:
        print(f"An error occurred: {str(e)}")
        return None

# Example usage
if __name__ == "__main__":
    input_file = "../data/2021_forward_data_for_cleaning/2022_ag_data.csv"
    output_file = "../data/raw_data/reduced_2022_output_file_2.csv"
    df = reduce_columns(input_file, output_file)