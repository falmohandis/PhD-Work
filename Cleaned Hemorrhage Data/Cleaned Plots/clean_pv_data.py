import pandas as pd

# Bad data dictionary: subject -> list of bad time points
bad_data = {
    "ERNE7": ["T0-1", "T29-30"], # Fatal: Early Death
    "ERNE10": ["T0-1"],
    "ERNE14": ["T29-30"],
    "ERNE21": ["T29-30"],
    "ERNE26": ["T0-1"],
    "ERNE29": ["T0-1", "T29-30"],  # Fatal: Early Death
    "ERNE38": ["T0-1", "T29-30"],
    "ERNE39": ["T0-1", "T29-30"],
    "ERNE40": ["T29-30"],
    "ERNE41": ["T0-1"],
    "ERNE43": ["T0-1", "T29-30"],  # Fatal: Early Death
    "ERNE44": ["T0-1","T29-30"],
    "ERNE45": ["T29-30"],
    "ERNE48": ["T29-30"],
    "ERNE54": ["T0-1", "T29-30"],
    "ERNE58": ["T0-1"],
    "ERNE67": ["T0-1"],
    "ERNE74": ["T0-1"],
    "ERNE78": ["T0-1"],
    "ERNE80": ["T29-30"],
    "ERNE83": ["T29-30"],
    "ERNE88": ["T29-30"],
    "ERNE91": ["T29-30"],
    "ERNE94": ["T0-1"],
    "ERNE96": ["T0-1"],
    "ERNE98": ["T29-30"],
    "ERNE101": ["T29-30"],
    "ERNE105": ["T29-30"],
    "ERNE106": ["T29-30"],
    "ERNE109": ["T29-30"],
    "ERNE110": ["T29-30"],
    "ERNE111": ["T29-30"],
    "ERNE113": ["T0-1", "T29-30"],
    "ERNE114": ["T29-30"],
    "ERNE115": ["T29-30"],
    "ERNE116": ["T29-30"],
    "ERNE117": ["T29-30"],
    "ERNE118": ["T29-30"],
    "ERNE119": ["T29-30"],
    "ERNE120": ["T0-1","T29-30"],    
    "ERNE122": ["T29-30"],
    "ERNE125": ["T29-30"],
    "ERNE126": ["T29-30"]
}

def remove_bad_data(file_path, output_path):
    # Load Excel sheet
    df = pd.read_excel(file_path)

    # Iterate over bad_data and filter out matching rows
    for subject, bad_times in bad_data.items():
        df = df[~((df["Parent Folder"] == subject) & (df["Time Point"].isin(bad_times)))]

    # Save cleaned dataframe
    df.to_excel(output_path, index=False)

# Apply function to both sheets
remove_bad_data("PV_Fractional_Increase_T0-1.xlsx", "PV_Fractional_Increase_T0-1_cleaned.xlsx")
remove_bad_data("PV Performance.xlsx", "PV_Performance_cleaned.xlsx")
