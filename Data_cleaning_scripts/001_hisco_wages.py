# Here we HISCO code Table 10 from statistics Denmark, 1916
# It contains wage information for different occupations

import pandas as pd
from histocc import OccCANINE



data = pd.read_csv(r"Data\TableX_census1916_stats_from1915_wadditonal.csv", sep = ";", encoding = "latin-1")

# Light data cleaning

# Sort away "Kategori" start with "Kapital"
data = data[~data["Kategori"].str.startswith("Kapital")]

# Sort away "Kategori" containing "Kvinder"
data = data[~data["Kategori"].str.contains("Kvinder")]

mod = OccCANINE()
res = mod(data.Erhverv, lang = "da")

# Drop index for both
data = data.reset_index(drop = True)
res = res.reset_index(drop = True)

res.join(data, how = "left").to_csv(r"Data\HISCO_coded_income.csv", index = False)
