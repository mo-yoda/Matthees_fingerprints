import pandas as pd

# renaming columns according to condition + n
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

    return table

# reformat dictionary to one table
def dic_to_table(dic):
    print("Reformatting...")
    # Initialize a list to store temporary DataFrames
    temp_df_list = []

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

    # Iterate over each sheet in the data workbook
    for sheet_name, df in dic.items():
        print(sheet_name)

        # Splitting sheet name into GPCR and bArr; SN in sheet names is ignored
        splitted_sheetName = sheet_name.split('_')
        GPCR = splitted_sheetName[0]
        bArr = splitted_sheetName[1]

        first_col = df.iloc[:, 0]  # Store the ligand data for reuse

        # Iterate over each column in the current sheet
        for col in df.columns[1:]:
            parts = col.split('_')
            if len(parts) == 3:  # Making sure the column has 3 parts split by "_"
                cell_background, FlAsH, n = parts

                # Create a DataFrame for the current column
                temp_df = pd.DataFrame({
                    'ligand': first_col.name[:-4],
                    'ligand_conc': first_col,
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

                # Add the filtered DataFrame to the list
                temp_df_list.append(temp_df_filtered)

    # concat list of temp dfs instead of appending in loop for better performance
    reformatted_data = pd.concat(temp_df_list, ignore_index=True)

    return reformatted_data

# Path definition
# tower PC path
path_to_file = "C:/Users/monar/Google Drive/Arbeit/homeoffice/231119_EM_PROGRAM_newdata/"
# laptop path
# path_to_file = "C:/Users/marli/Desktop/231119_EM_PROGRAM_newdata/"

# Read the entire Excel workbook
all_data = pd.read_excel(f"{path_to_file}/231029_overview_b2,b2V2,V2b2,V2_bArrs-confChange_prepR.xlsx",
                         sheet_name=None, nrows=7)
# imported as dictionary where:
#   Key: Sheet name
#   Value: Data in that sheet as a DataFrame

# Displaying sheet names to confirm successful reading
sheet_names = list(all_data.keys())
print("Imported workbook with following sheets:")
print(sheet_names)

# apply renaming of cols for each table in dictionary
for sheet in all_data:
    rename_cols(all_data[sheet])

# split dictionary for normalised and non-normalised data
data_SN = {key: value for key, value in all_data.items() if "SN" in key}
data_OG = {key: value for key, value in all_data.items() if "SN" not in key}

# use dic_to_table function to create result table containing all data
results = {'OG': dic_to_table(data_OG), 'SN': dic_to_table(data_SN)}

results['OG'].to_excel(f"{path_to_file}Master_reformat.xlsx")
results['SN'].to_excel(f"{path_to_file}Master_SN_reformat.xlsx")

