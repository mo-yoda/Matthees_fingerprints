import pandas as pd

def rename_cols(table):
    # Create a new list for updated column names
    corrected_columns = [table.columns[0]]

    # Iterate over the columns and rename accordingly, skipping the "Iso[µM]" column
    for i, col in enumerate(table.columns[1:], 1):
        if "Unnamed" not in col or i == 1:
            base_name = col
            n = 1
            corrected_columns.append(f"{base_name}_{n}")
        else:
            n += 1
            corrected_columns.append(f"{base_name}_{n}")

    # Update the dataframe with the corrected column names
    table.columns = corrected_columns
    print(table.columns[0:15])

    return table

# path to test data
path_to_file = "C:/Users/monar/Google Drive/Arbeit/homeoffice/230918_EM_PROGRAM/"

# Read the first 7 rows of the CSV file
test_table = pd.read_excel(path_to_file + "gpt/test_b2adr.xlsx", sheet_name=None, nrows=7)
print(test_table)
rename_cols(test_table["b2AR_bArr1"])


# Read the entire Excel workbook
data = pd.read_excel(path_to_file + '/230913_overview_b2,b2V2,V2b2,V2_bArrs-confChange_prepR.xlsx',
                     sheet_name=None, nrows=7)
# imported as dictionary where:
#   Key: Sheet name
#   Value: Data in that sheet as a DataFrame

# Displaying sheet names to confirm successful reading
sheet_names = list(data.keys())
print(sheet_names)

# apply renaming of cols for each table in dictionary
for sheet in data:
    rename_cols(data[sheet])


# print(data[sheet_names[5]])
# data[sheet_names[5]].to_excel(path_to_file + "output_test.xlsx")

###

# Initialize an empty DataFrame for the reformatted data
reformatted_data = pd.DataFrame(columns=[
    "ligand",
    "ligand_conc",
    "condition",
    "GPCR",
    "bArr",
    "cell_background",
    "FlAsH",
    "n",
    "signal",
    "ID"])
ID = 1  # Initialize ID counter

# Iterate over each sheet in the test_table workbook
for sheet_name, df in test_table.items():
    print("################")
    print(sheet_name)
    GPCR, bArr = sheet_name.split('_')  # Splitting sheet name into GPCR and bArr

    # Iterate over each column in the current sheet
    for col in df.columns[1:]:
        parts = col.split('_')
        if len(parts) == 3:  # Making sure the column has 3 parts split by "_"
            cell_background, FlAsH, n = parts

            # Create a DataFrame for the current column
            temp_df = pd.DataFrame({
                'ligand': df.columns[0],
                'ligand_conc': df[df.columns[0]],
                'condition': sheet_name,
                'GPCR': GPCR,
                'bArr': bArr,
                'cell_background': cell_background,
                'FlAsH': FlAsH,
                'n': n,
                'signal': df[col],
                'ID': range(ID, ID + len(df))
            })

            # drop rows with missing Ns
            temp_df_filtered = temp_df.dropna(subset=['signal'])

            ID += len(temp_df_filtered)  # Update ID counter

            # Append to the reformatted_data DataFrame
            # print(temp_df_filtered)

            # append not available in pandas 2.0
            # reformatted_data = reformatted_data.append(temp_df_filtered, ignore_index=True)
            reformatted_data = pd.concat([reformatted_data, temp_df_filtered], ignore_index=True)

print(reformatted_data.head())

reformatted_data.to_excel(path_to_file + "formatted_test.xlsx")