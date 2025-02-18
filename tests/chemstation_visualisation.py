import matplotlib.pyplot as plt
import BinaryParser as bp
import pandas as pd

file_path = "./tests/Chemstation/SVS_1025F1.D/MSD1.MS"
df = bp.read_chemstation_file(file_path)

tic_df = df.groupby("retention_time", as_index=False)["intensity"].sum()

plt.figure(figsize=(10, 5))
plt.plot(tic_df["retention_time"], tic_df["intensity"], color="black", linewidth=1)

plt.xticks(
    ticks=range(
        int(tic_df["retention_time"].min()), int(tic_df["retention_time"].max()), 1
    )
)
plt.xlabel("Retention Time (min)")
plt.ylabel("Total Ion Current (TIC)")
plt.title("Total Ion Chromatogram (TIC)")

plt.grid(True, linestyle="--", alpha=0.5)

plt.show()
