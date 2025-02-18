import os
import netCDF4 as nc
import pandas as pd
import numpy as np
import re
from typeguard import typechecked
from typing import List


@typechecked
def get_files(path: str) -> List[str]:
    fs = [os.path.join(path, f) for f in os.listdir(path) if f.endswith(".cdf")]
    assert len(fs) > 0, "No files found"
    return fs


# Attributes
@typechecked
def get_attr(path: str):
    with nc.Dataset(path, "r") as dataset:
        attr = {key: dataset.getncattr(key) for key in dataset.ncattrs()}
    return attr


@typechecked
def read_attr(path: str) -> pd.DataFrame:
    fs = get_files(path)
    attrs_lc = [pd.DataFrame([get_attr(fs[x])]) for x in range(len(fs))]
    attrs_lc = pd.concat(attrs_lc, ignore_index=True)
    return attrs_lc


# LC Data
@typechecked
def get_lc_data(path: str) -> pd.DataFrame:
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


@typechecked
def process_detector_info(df_list: List[pd.DataFrame]) -> List[pd.DataFrame]:
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


@typechecked
def read_lc(path: str) -> pd.DataFrame:
    fs = get_files(path)
    # Filter fs --> Files which contain DAD within their name
    fs = [f for f in fs if "DAD" in os.path.basename(f)]
    df = [get_lc_data(fs[x]) for x in range(len(fs))]
    df = process_detector_info(df)
    df = pd.concat(df, ignore_index=True)
    return df


# MS Data
@typechecked
def get_point_counts(path: str) -> List[int]:
    with nc.Dataset(path, "r") as dataset:
        return dataset.variables["point_count"][:]


@typechecked
def get_ms_data(path: str) -> pd.DataFrame:
    with nc.Dataset(path, "r") as dataset:
        mz_values = dataset.variables["mass_values"][:]
        intensities = dataset.variables["intensity_values"][:]
    return pd.DataFrame({"mz": mz_values, "intensities": intensities})


@typechecked
def get_scan_time(path: str) -> List[float]:
    with nc.Dataset(path, "r") as dataset:
        time = dataset.variables["scan_acquisition_time"][:]
    return time / 60


@typechecked
def split_data(data: pd.DataFrame, point_counts: List[int]) -> List[pd.DataFrame]:
    end_indices = np.cumsum(point_counts)
    start_indices = np.insert(end_indices[:-1], 0, 0)
    return [data.iloc[start:end] for start, end in zip(start_indices, end_indices)]


@typechecked
def normalise(data_list: List[pd.DataFrame]) -> List[pd.DataFrame]:
    return [
        df.assign(intensities=df["intensities"] * (100 / df["intensities"].max()))
        for df in data_list
    ]


@typechecked
def read_ms(path: str) -> List[pd.DataFrame]:
    fs = get_files(path)
    fs_ms = [f for f in fs if "spectra" in os.path.basename(f)]
    data_minus = get_ms_data(fs_ms[0])
    point_counts_minus = get_point_counts(fs_ms[0])
    time_minus = get_scan_time(fs_ms[0])
    df_minus = normalise(split_data(data_minus, point_counts_minus))

    data_plus = get_ms_data(fs_ms[1])
    point_counts_plus = get_point_counts(fs_ms[1])
    time_plus = get_scan_time(fs_ms[1])
    df_plus = normalise(split_data(data_plus, point_counts_plus))

    df_minus = pd.concat([df.assign(time=t) for df, t in zip(df_minus, time_minus)])
    df_plus = pd.concat([df.assign(time=t) for df, t in zip(df_plus, time_plus)])
    return [df_minus, df_plus]
