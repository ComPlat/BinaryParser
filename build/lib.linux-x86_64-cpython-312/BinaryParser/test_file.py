import BinaryParser as bp
import pandas as pd
import plotly.express as px

file_path = "/home/konrad/Documents/BinaryParser/Chemstation/ChemStationData/LCMS_DatenAgilent_SVS/SVS_1025F1.D/MSD1.MS"
df = bp.read_chemstation_file(file_path)
df.to_csv("output.csv", index=False)
tic_df = df.groupby("retention_time", as_index=False)["intensity"].sum()
fig = px.line(tic_df, x="retention_time", y="intensity",
              title="Total Ion Chromatogram (TIC)",
              labels={"retention_time": "Retention Time (min)", "intensity": "Total Ion Intensity"})
fig.show()
