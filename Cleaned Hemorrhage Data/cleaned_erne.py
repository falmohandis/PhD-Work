# I used gpt to write this code to keep part of the ERNE data that I needed. It keeps the first 3 rows and the last 3001 rows of each sheet, but only if the sheet has more than 2500 rows. This is useful for large datasets where you want to retain specific parts of the data.

import pandas as pd

# Load the Excel file
file_path = "Total ERNE.xlsx"
xls = pd.ExcelFile(file_path)

# Dictionary to hold processed sheets
processed_sheets = {}

for sheet_name in xls.sheet_names:
    df = pd.read_excel(file_path, sheet_name=sheet_name)
    total_rows = len(df)
    
    if total_rows < 2500:
        print(f"Sheet has less than 2500 rows: {sheet_name}")
        continue  # Skip modification for small sheets
    
    # Keep first 3 rows and last 3001 rows
    first_part = df.iloc[:3]
    last_part = df.iloc[-3001:]
    new_df = pd.concat([first_part, last_part])
    
    processed_sheets[sheet_name] = new_df

# Save modified sheets to a new file
output_file = "Total_ERNE_trimmed.xlsx"
with pd.ExcelWriter(output_file, engine='openpyxl') as writer:
    for sheet_name, data in processed_sheets.items():
        data.to_excel(writer, sheet_name=sheet_name, index=False)

print(f"Processed file saved as: {output_file}")
