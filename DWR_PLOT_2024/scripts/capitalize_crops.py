import pandas as pd

def capitalize_crop_names(file_path):
    """
    Read CSV file and capitalize all crop names in the 'Crop Name' column 
    or third column if 'Crop Name' doesn't exist.
    
    Parameters:
    file_path (str): Path to your CSV file
    
    Returns:
    pandas.DataFrame: DataFrame with capitalized crop names
    """
    # Read the CSV file
    df = pd.read_csv(file_path)
    
    # Try to find the crop name column
    if 'Crop Name' in df.columns:
        column_name = 'Crop Name'
    else:
        # Use the third column (index 2) if 'Crop Name' doesn't exist
        column_name = df.columns[2]
    
    # Capitalize all crop names
    df[column_name] = df[column_name].str.title()
    
    # Save the modified data back to CSV
    output_file = 'capitalized_' + file_path
    df.to_csv(output_file, index=False)
    
    return df

# Example usage
if __name__ == "__main__":
    # Replace 'your_file.csv' with your actual file name
    file_path = 'combined.csv'
    
    try:
        result_df = capitalize_crop_names(file_path)
        print(f"Successfully capitalized crop names and saved to 'capitalized_{file_path}'")
        # Display first few rows to verify
        print("\nFirst few rows of the modified data:")
        print(result_df.head())
    except FileNotFoundError:
        print("Error: File not found. Please check the file path.")
    except Exception as e:
        print(f"An error occurred: {str(e)}")