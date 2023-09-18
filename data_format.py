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
# data = pd.read_csv(path_to_file + "gpt/test_b2adr.csv", nrows=7)
# print(data)

#rename_cols(data)

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

print(data[sheet_names[5]])

data[sheet_names[5]].to_excel(path_to_file + "output_test.xlsx")

