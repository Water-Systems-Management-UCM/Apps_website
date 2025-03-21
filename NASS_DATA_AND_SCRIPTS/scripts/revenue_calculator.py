import pandas as pd

def calculate_gross_revenue(csv_path, output_path):
    """
    Calculate gross revenue from agricultural data using Yield and Price.
    
    Parameters:
    csv_path (str): Path to input CSV file
    output_path (str): Path to save the processed CSV file
    """
    try:
        # Read the CSV file
        df = pd.read_csv(csv_path)
        
        # Print column names for debugging
        print("Available columns:", df.columns.tolist())
        
        # Clean and convert Yield column
        df[' Yield'] = df[' Yield'].replace(r'[\$,]', '', regex=True)
        df[' Yield'] = pd.to_numeric(df[' Yield'], errors='coerce')
        
        # Clean and convert Price column
        df[' Price P/U'] = df[' Price P/U'].replace(r'[\$,]', '', regex=True)
        df[' Price P/U'] = pd.to_numeric(df[' Price P/U'], errors='coerce')
        
        # Calculate gross revenue (Yield * Price per Unit)
        df['Gross Revenue'] = df[' Yield'] * df[' Price P/U']
        
        # Round to 2 decimal places
        df['Gross Revenue'] = df['Gross Revenue'].round(2)
        
        # Handle any NaN values in the result
        df['Gross Revenue'] = df['Gross Revenue'].fillna(0)
        
        # Save the updated DataFrame to a new CSV file
        df.to_csv(output_path, index=False)
        
        print(f"Successfully processed data and saved to {output_path}")
        print(f"Added gross revenue calculations for {len(df)} rows")
        
        # Print summary of any rows where conversion failed
        nan_rows = df[df['Gross Revenue'] == 0].shape[0]
        if nan_rows > 0:
            print(f"Warning: {nan_rows} rows resulted in zero revenue (possibly due to invalid data)")
            
    except FileNotFoundError:
        print(f"Error: Could not find the file {csv_path}")
    except Exception as e:
        print(f"An error occurred: {str(e)}")
        print("Please check that your CSV file contains valid numeric data in the Yield and Price P/U columns")

# Example usage
if __name__ == "__main__":
    input_file = "../data/final_mapped_data/mapped_combineddata_2024_4.csv"
    output_file = "../data/with_revenue/mapped_data_with_revenue.csv"
    
    calculate_gross_revenue(input_file, output_file)