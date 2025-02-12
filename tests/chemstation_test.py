import BinaryParser as bp
import pandas as pd
import numpy as np


def ca(a, b):
    return abs(a - b) < 0.01


def compare_series(a, b):
    for i, j in zip(a, b):
        if not ca(i, j):
            print(f"{i} != {j}")
        assert ca(i, j)


def ca_intensity(a, b):
    if a >= b:
        return (1 - a / b) < 0.1
    else:
        return (1 - b / a) < 0.1


def compare_intensities(a, b):
    for i, j in zip(a, b):
        if not ca_intensity(i, j):
            print(f"{i} != {j}")
        assert ca_intensity(i, j)


def nearest_filter(mz_obs, mz_true, tol=0.5):
    mz_obs_filtered = []
    for mz in mz_true:
        diff = np.abs(mz_obs - mz)
        if np.any(diff < tol):
            mz_obs_filtered.append(np.argmin(diff))
    return np.array(mz_obs_filtered)


def compare_spectra_intensities(intensity_obs, intensity_true):
    for i, j in zip(intensity_obs, intensity_true):
        assert i == j


def compare_spectras(df, spectra_true, time):
    spectra_obs = df[df["retention_time"].round(3) == time]
    mz_true = spectra_true.iloc[:, 2]
    indices = nearest_filter(spectra_obs["mz"], mz_true)
    spectra_obs = spectra_obs.iloc[indices]
    spectra_obs = spectra_obs.iloc[:, [0, 1]]
    spectra_true = spectra_true.iloc[:, [2, 4]]
    compare_spectra_intensities(
        spectra_obs["intensity"], spectra_true.iloc[:, 1]
    )


def test_svs1025f1():
    file_path = "./tests/Chemstation/SVS_1025F1.D/MSD1.MS"
    df = bp.read_chemstation_file(file_path)
    # Compute TIC
    tic_df = df.groupby("retention_time", as_index=False)["intensity"].sum()
    tic_true = pd.read_csv(
        "./tests/Chemstation/TIC_SVS1025F1.CSV",
        delimiter=",", encoding="utf-16"
    )
    # Validate TIC
    assert tic_df.shape == (465, 2)
    compare_series(tic_df.retention_time, tic_true.RT)
    compare_intensities(tic_df.intensity, tic_true.Intensity)

    # Validate spectra at different retention times
    for time, filename in [(4.687, "RT4687_SVS1025F1.CSV"), (6.484, "RT6484_SVS1025F1.CSV")]:
        spectra_true = pd.read_csv(
            f"./tests/Chemstation/{filename}",
            delimiter=",", encoding="utf-16", header=None
        )
        compare_spectras(df, spectra_true, time)


test_svs1025f1()


def test_scs776roh():
    file_path = "./tests/Chemstation/SVS-776ROH.D/MSD1.MS"
    df = bp.read_chemstation_file(file_path)
    # Compute TIC
    tic_df = df.groupby("retention_time", as_index=False)["intensity"].sum()
    tic_true = pd.read_csv(
        "./tests/Chemstation/TIC_SVS776ROH.CSV",
        delimiter=",", encoding="utf-16"
    )
    # Validate TIC
    assert tic_df.shape == tic_true.shape
    compare_series(tic_df.retention_time, tic_true.RT)
    compare_intensities(tic_df.intensity, tic_true.Intensity)
    # Validate spectra at different retention times
    for time, filename in [(2.310, "RT2310_SVS776ROH.CSV"), (6.529, "RT6529_SVS776ROH.CSV")]:
        spectra_true = pd.read_csv(
            f"./tests/Chemstation/{filename}",
            delimiter=",", encoding="utf-16", header=None
        )
        compare_spectras(df, spectra_true, time)


test_scs776roh()
