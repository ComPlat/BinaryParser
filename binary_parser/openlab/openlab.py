import os
import re
from typing import List

import netCDF4 as nc
import numpy as np
import pandas as pd




def get_files(path: str) -> List[str]:
    """
    Return list of .cdf files in the given directory, sorted naturally.
    """
    fs = [os.path.join(path, f) for f in os.listdir(path) if f.endswith(".cdf")]
    assert fs, "No files found"

    def natkey(p: str):
        name = os.path.basename(p)
        is_spectra = 1 if "_spectra" in name else 0
        base = name.replace("_spectra", "")
        parts = re.split(r"(\d+)", base)
        parts = [int(x) if x.isdigit() else x.lower() for x in parts]
        return (parts, is_spectra)

    return sorted(fs, key=natkey)

# Attributes

def _get_attr(path: str):
    """Read global NetCDF attributes from a file."""
    with nc.Dataset(path, "r") as dataset:
        attr = {key: dataset.getncattr(key) for key in dataset.ncattrs()}
    return attr



def read_attr(path: str) -> pd.DataFrame:
    """
    Read all NetCDF global attributes across all .cdf files in the directory.
    Returns a normalized DataFrame.
    """
    fs = get_files(path)
    attrs_lc = [pd.DataFrame([_get_attr(fs[x])]) for x in range(len(fs))]
    attrs_lc = pd.concat(attrs_lc, ignore_index=True)
    return attrs_lc


# ---------------------------------------------------------------------------
# LC Data
# ---------------------------------------------------------------------------


def get_lc_data(path: str) -> pd.DataFrame:
    """Read LC detector signals from NetCDF."""
    with nc.Dataset(path, "r") as dataset:
        detector_signals = dataset.variables["ordinate_values"][:]
        global_atts = {key: dataset.getncattr(key) for key in dataset.ncattrs()}
        detector = global_atts.get("detector_name", "")
        run_time_length = dataset.variables["actual_run_time_length"][...]

    data = pd.DataFrame(
        {
            "RetentionTime": np.linspace(0, run_time_length, num=len(detector_signals)),
            "DetectorSignal": detector_signals,
        }
    )
    data.attrs["detector"] = detector
    return data



def process_detector_info(df_list: List[pd.DataFrame]) -> List[pd.DataFrame]:
    """Extract wavelength from LC detector metadata."""
    for df in df_list:
        detector_name = df.attrs.get("detector", "")
        wl_match = (
            re.search(r"\d+", detector_name.split(",")[1])
            if "," in detector_name
            else None
        )
        wl = float(wl_match.group()) if wl_match else None
        df["wavelength"] = wl
    return df_list



def read_lc(path: str) -> pd.DataFrame:
    """Read all LC files containing 'DAD' in filename and concatenate."""
    fs = get_files(path)
    # Filter fs --> Files which contain DAD within their name
    fs = [f for f in fs if "DAD" in os.path.basename(f)]
    df = [get_lc_data(fs[x]) for x in range(len(fs))]
    df = process_detector_info(df)
    df = pd.concat(df, ignore_index=True)
    return df


# ---------------------------------------------------------------------------
# MS Data
# ---------------------------------------------------------------------------


def _get_point_counts(path: str) -> np.ma.MaskedArray:
    with nc.Dataset(path, "r") as dataset:
        res = dataset.variables["point_count"][:]
        return res



def _get_ms_data(path: str) -> pd.DataFrame:
    with nc.Dataset(path, "r") as dataset:
        mz_values = dataset.variables["mass_values"][:]
        intensities = dataset.variables["intensity_values"][:]
    return pd.DataFrame({"mz": mz_values, "intensities": intensities})



def _get_scan_time(path: str) -> np.ma.MaskedArray:
    with nc.Dataset(path, "r") as dataset:
        time = dataset.variables["scan_acquisition_time"][:]
    return time / 60



def _split_data(
    data: pd.DataFrame, point_counts: np.ma.MaskedArray
) -> List[pd.DataFrame]:
    end_indices = np.cumsum(point_counts)
    start_indices = np.insert(end_indices[:-1], 0, 0)
    res = [data.iloc[start:end] for start, end in zip(start_indices, end_indices)]
    return res



def _normalise(data_list: List[pd.DataFrame]) -> List[pd.DataFrame]:
    return [
        df.assign(intensities=df["intensities"] * (100 / df["intensities"].max()))
        for df in data_list
    ]



def read_ms(path: str) -> List[pd.DataFrame]:
    fs = get_files(path)
    fs_ms = [f for f in fs if "spectra" in os.path.basename(f)]
    data_minus = _get_ms_data(fs_ms[0])
    point_counts_minus = _get_point_counts(fs_ms[0])
    time_minus = _get_scan_time(fs_ms[0])
    df_minus = _normalise(_split_data(data_minus, point_counts_minus))

    data_plus = _get_ms_data(fs_ms[1])
    point_counts_plus = _get_point_counts(fs_ms[1])
    time_plus = _get_scan_time(fs_ms[1])
    df_plus = _normalise(_split_data(data_plus, point_counts_plus))

    df_minus = pd.concat([df.assign(time=t) for df, t in zip(df_minus, time_minus)])
    df_plus = pd.concat([df.assign(time=t) for df, t in zip(df_plus, time_plus)])
    return [df_minus, df_plus]
