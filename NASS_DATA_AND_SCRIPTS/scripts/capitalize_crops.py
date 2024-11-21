import pandas as pd
import os

def capitalize_crop_names(file_path, output_dir="../data/capitalized_data"):
    """
    Read CSV file and capitalize all crop names in the 'Crop Name' column 
    or third column if 'Crop Name' doesn't exist. Save output to specified directory.
    
    Parameters:
    file_path (str): Path to your CSV file
    output_dir (str): Directory path where the output file should be saved
    
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
    
    # Create output directory if it doesn't exist
    os.makedirs(output_dir, exist_ok=True)
    
    # Get the original filename without path
    original_filename = os.path.basename(file_path)
    
    # Create the output filepath
    output_file = os.path.join(output_dir, f'capitalized_{original_filename}')
    
    # Save the modified data to CSV
    df.to_csv(output_file, index=False)
    
    return df

# Example usage
if __name__ == "__main__":
    # Replace 'your_file.csv' with your actual file name
    file_path = '../data/aggregated_raw_data/combined.csv'
    output_dir = "../data/capitalized_data"
    
    try:
        result_df = capitalize_crop_names(file_path, output_dir)
        print(f"Successfully capitalized crop names and saved to '{output_dir}/capitalized_{os.path.basename(file_path)}'")
        # Display first few rows to verify
        print("\nFirst few rows of the modified data:")
        print(result_df.head())
    except FileNotFoundError:
        print("Error: File not found. Please check the file path.")
    except Exception as e:
        print(f"An error occurred: {str(e)}")