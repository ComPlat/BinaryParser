import parser_xray as px
import pandas as pd
from typeguard import typechecked
from typing import List

search_for = {
    "GONIOMETER_RADIUS": 217.5,
    "FIXED_DIVSLIT": 0.6,
    "SAMPLESLIT": 0.0,
    "DETSLIT": 10.39,
    "ANTISLIT": 6.17,
    "START": 20.0,
    "THETA": 10.0,
    "THETA2": 20.0,
    "TIMESTARTED": 14,
    "TEMP_RATE": -1,
    "TEMP_DELAY": -1,
    "KV": 35,
    "MA": 45,
    "WL1": 1.540600,
    "WL2": 1.544390,
    "WL3": 1.392220,
}

res = px.readDoubles(
    "/home/konrad/Documents/GitHub/RProjects/chromatogramsR/Bruker/PD.raw", 0
)
df = pd.DataFrame(res)
df.columns = ["data"]

for key, value in search_for.items():
    result_df = df[df["data"].eq(value)]
    if not result_df.empty:
        print(f"Match found for {key} at index {result_df.index[0]}")

print()
print()
print()

res = px.readFloates(
    "/home/konrad/Documents/GitHub/RProjects/chromatogramsR/Bruker/PD.raw", 0
)
df = pd.DataFrame(res)
df.columns = ["data"]

for key, value in search_for.items():
    result_df = df[df["data"].eq(value)]
    if not result_df.empty:
        print(f"Match found for {key} at index {result_df.index[0]}")


# data starts at 0x420 --> 1056 --> in float index 264 --> 263 in python
# data is read as float until end of file
print()
print()
print()


path = "/home/konrad/Documents/GitHub/RProjects/chromatogramsR/Bruker/WeitereDaten/XRD/7_80_3_001651_Cu_SSZ13_05_7.raw"
# _WL1=1.540600
# _WL2=1.544390
# _WL3=0.00000
# _WLRATIO=0.500000
# _START=7.000000
# _THETA=3.500000
# _2THETA=7.000000

# xxd
# 000003c8: 0000 5c00 0000 0200  ..\..... --> 968
# 000003d0: 0000 3254 6865 7461  ..2Theta --> 976
# 000003d8: 0000 0000 0000 0000  ........
# 000003e0: 0000 0000 0000 0000  ........
# 000003e8: 0000 0000 0000 0000  ........
# 000003f0: 0000 0000 0000 0000  ........
# 000003f8: 0000 0100 0000 0000  ........
# 00000400: 0000 0000 1c40 0000  .....@..
# 00000408: 0000 0000 0000 0000  ........
# 00000410: 0000 0000 0000 0000  ........
# 00000418: 0000 0000 0000 0000  ........
# 00000420: 0000 3200 0000 5c00  ..2...\.


res = px.readDoubles(path, 0)
df = pd.DataFrame(res)
df.columns = ["data"]
print(df.iloc[100:130])
df = df[(df.round() != 0.0) & (df < 1000.0) & (df > 0.0)].dropna()
df = df.iloc[0:60]


res = px.readFloates(path, 0)

df = pd.DataFrame(res)
print(df.iloc[240:250])
df.columns = ["data"]
df = df[(df.round() != 0.0) & (df < 1000.0) & (df > 0.0)].dropna()
df = df.iloc[0:60]
