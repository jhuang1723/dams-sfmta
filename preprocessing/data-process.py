import pandas as pd
import openpyxl
import csv
import numpy as np
import os

csv_file_name = 'all-paths-data.csv'
bart_graph = {'RM': {'EN'}, 'EN': {'RM', 'EP'}, 'EP': {'NB', 'EN'}, 
              'NB': {'BK', 'EP'}, 'BK': {'NB', 'AS'}, 'AS': {'BK', 'MRA'}, 
              'MRA': {'RR', 'AS', 'MA'}, 'MA': {'MRA', '19'}, 
              '19': {'12', 'MA'}, '12': {'LM', 'OW', '19'}, 
              'OW': {'12', 'EM', 'LM'}, 'EM': {'MT', 'OW'}, 
              'MT': {'EM', 'PL'}, 'PL': {'MT', 'CC'}, 'CC': {'16', 'PL'}, 
              '16': {'24', 'CC'}, '24': {'16', 'GP'}, 'GP': {'24', 'BP'}, 
              'BP': {'DC', 'GP'}, 'DC': {'CM', 'BP'}, 'CM': {'SS', 'DC'}, 
              'SS': {'CM', 'SB'}, 'SB': {'SS', 'SFC'}, 'SFC': {'SB', 'MB', 'SO'}, 
              'SO': {'SFC'}, 'OA': {'CL'},
              'MB': {'SFC'}, 'LM': {'12', 'OW', 'FV'}, 'FV': {'LM', 'CL'}, 
              'CL': {'SL', 'FV', 'OA'}, 'SL': {'CL', 'BF'}, 'BF': {'BHC', 'SL'}, 
              'BHC': {'HY', 'CV', 'BF'}, 'HY': {'SH', 'BHC'}, 'SH': {'HY', 'UC'}, 
              'UC': {'SH', 'FM'}, 'FM': {'UC', 'WS'}, 'WS': {'FM', 'ML'}, 
              'ML': {'BE', 'WS'}, 'BE': {'ML'}, 'AN': {'PC'}, 'PC': {'WP', 'AN'}, 
              'WP': {'NC', 'PC'}, 'NC': {'WP', 'CN'}, 'CN': {'NC', 'PH'}, 
              'PH': {'CN', 'WC'}, 'WC': {'LF', 'PH'}, 'LF': {'OR', 'WC'}, 
              'OR': {'RR', 'LF'}, 'RR': {'OR', 'MRA'}, 'ED': {'WD'}, 
              'WD': {'CV', 'ED'}, 'CV': {'WD', 'BHC'}}

# dfs to find intermediate stations along route start:end
def dfs(node, end, route, visited):
    if node == end:
        route.append(end)
        return True
    if node in visited: 
        return False
    visited.add(node)
    route.append(node)
    for neighbor in bart_graph[node]:
        if neighbor not in visited:
            if dfs(neighbor, end, route, visited):
                return True
    route.pop()
    return False

# find all intermediate stations from dataframe using dfs
def create_routes(datatable_fp):
    df = pd.read_excel(datatable_fp, skiprows = 1, usecols=lambda x: x not in [0, 1],  index_col=0, sheet_name="Total Trips OD")
    # creating row names
    all_possible_routes = []
    route_breakdowns = []
    for col_index, row in df.iloc[:-1].iterrows():
        for row_index, value in row[:-1].items():
            if col_index != row_index:
                all_possible_routes.append(f"{row_index}-{col_index}")
                route = []
                if dfs(str(row_index), str(col_index), route, set()):
                    route_string = "~".join([f"{route[i]}-{route[i+1]}" for i in range(len(route)-1)])
                    route_breakdowns.append(route_string)
                else:
                    print("Route not created: something went wrong")
    return all_possible_routes, route_breakdowns


# route names + intermediate breakdowns to csv
def write_route_paths():
    fp = "ridership_data/ridership_2023/Ridership_202301.xlsx"
    all_possible_routes, route_breakdowns = create_routes(fp)
    rows = zip(all_possible_routes, route_breakdowns)
    with open(csv_file_name, 'w', newline='') as csvfile:
        csv_writer = csv.writer(csvfile)
        csv_writer.writerow(['Name', 'Routes'])
        csv_writer.writerows(rows)

# if traversal is the same, will be in the same order
# for other dataframes, would need to make sure columns are in EXACT SAME order
def monthly_ridership(month_dt):
    df = pd.read_excel(month_dt, skiprows = 1, usecols=lambda x: x not in [0, 1],  index_col=0, sheet_name="Total Trips OD")
    ridership = [value for col_index, row in df.iloc[:-1].iterrows() for row_index, value in row[:-1].items() if col_index != row_index]
    return ridership

# add new column to csv file
def append_month_ridership_tocsv(ridership, col_header):
    with open(csv_file_name, 'r') as csv_file:
        reader = csv.reader(csv_file)
        header = next(reader)  
        header.append(col_header)
        rows = [row + [new_data] for row, new_data in zip(reader, ridership)]
    with open(csv_file_name, 'w', newline='') as csv_file:
        writer = csv.writer(csv_file)
        writer.writerow(header)
        writer.writerows(rows)

# writing data starts here
write_route_paths()
big_folder = "ridership_data"
for year in range(18, 24):
    str_year = str(year)
    folder = f"ridership_20{str_year}"
    for month in range(1, 13):
        month_str = str(month).zfill(2)
        fp = f"{big_folder}/{folder}/Ridership_20{str_year}{month_str}.xlsx"
        if not os.path.exists(fp):
            print("Skipped", month_str, "-", str_year)
            continue
        ridership = monthly_ridership(fp)
        append_month_ridership_tocsv(ridership, f"{month_str}/01/{str_year}")
        print(month_str, "-", str_year, "completed!")
    