import binary_parser as bp
import pandas as pd
import numpy as np
import os

base = "./tests/Chemstation/SVS476F1.D"

def compare(expected, got):
    n = min(len(expected), len(got))
    diff = np.abs(expected.values[:n] - got.values[:n])
    return(diff.sum())

def read_export_csv(path):
    df = pd.read_csv(path, sep=",",
                     decimal=".", engine="python", encoding = "utf-16",
                     header = None)
    return df

files = [
  "TRUE_VALUES_A.CSV",
  "TRUE_VALUES_B.CSV",
  "TRUE_VALUES_C.CSV",
  "TRUE_VALUES_D.CSV",
  "TRUE_VALUES_E.CSV"
]

def test_uv_new_version():
    values = bp.read_chromatograms(base)
    wavelength_cols = [c for c in values.columns if c.startswith("Wavelength_")]
    wavelengths = np.array([int(c.split("_")[1]) for c in wavelength_cols])
    order = np.argsort(wavelengths)

    for i in range(len(files)):
        fpath = base + "/" + files[i]
        true_values = read_export_csv(fpath)
        res_time = compare(true_values.iloc[:, 0], values.iloc[:, 5])
        assert(res_time < 1e-8)
        res = compare(true_values.iloc[:, 1], values.iloc[:, order[i]])
        assert(res < 1e-8)

    def compare_relative(expected, got):
        n = min(len(expected), len(got))
        expected = expected.values[:n]
        got = got.values[:n]
        mask = expected != 0
        rel_diff = np.abs(expected[mask] - got[mask]) / np.abs(expected[mask])
        return rel_diff.mean()

def test_ms_new_version():
    path_ms = base + "/MSD1.MS"
    df = bp.read_chemstation_file(path_ms)
    tic_df = df.groupby("retention_time", as_index=False)["intensity"].sum()
    true_tic = read_export_csv(base + "/TRUE_TIC.CSV")
    res_time = compare(true_tic.iloc[:, 0], tic_df.iloc[:, 0])
    assert res_time < 1e-8
    res_tic = compare_relative(true_tic.iloc[:, 1], tic_df.iloc[:, 1])
    assert res_tic < 0.01  # 1%
