import binary_parser.helper.parser_hplc as ph
import pandas as pd
import plotly.graph_objs as go
import plotly.express as px
import re
from os import listdir
from os.path import isfile, join

from binary_parser.helper.utils import NumList
from typing import List



def check_identical_lists(lst: List[List[float]]) -> bool:
    if not lst:
        return False
    first_sublist: List[float] = lst[0]
    for sublist in lst[1:]:
        if sublist != first_sublist:
            return False
    return True


def read_version(file_path: str) -> int:
    with open(file_path, "rb") as f:
        data = f.read(10)
    digits = bytes([b for b in data if 48 <= b <= 57])
    return int(digits.decode("ascii"))

def read_time(file_path: str, length: int, offsetTime: int) -> NumList:
    time:NumList = ph.readTime(file_path, offsetTime)
    step_size: float = (time[1] - time[0]) / (length - 1)
    res: List[float] = [time[0] + i * step_size for i in range(length)]
    return res



def read_file_info(file_path: str, offsetFileInfo) -> int:
    res = ph.readUint8(file_path, offsetFileInfo)
    res = [x if x != "\x00" else "" for x in res]
    res = "".join(res)
    matches = int(re.findall(r"Sig=(\d+),", res)[0])
    return matches



def scale_data(file_path: str, l: NumList, scaleOffset1, scaleOffset2) -> NumList:
    intercept: float = ph.readDouble(file_path, scaleOffset1)
    slope: float = ph.readDouble(file_path, scaleOffset2)
    res: List[float] = [float(i) * slope + intercept for i in l]
    return res



def read_chromatograms(path: str) -> pd.DataFrame:
    files: List[str] = [
        path + "/" + f
        for f in listdir(path)
        if isfile(join(path, f)) and f.endswith(".ch")
    ]

    versions: List[str] = [read_version(i) for i in files]
    if len(set(versions)) != 1:
        raise ValueError("Files have different versions")

    v = versions[0]
    if v not in [30, 130]:
        raise ValueError("Found unsupported version")

    version_offsets = {
        30: {
            "offsetFileInfo": int("00000258", 16),
            "offsetTime": int("0000011a", 16),
            "offsetData": int("00000400", 16),
            "scaleOffset1": 636,
            "scaleOffset2": 644
        },
        130: {
            "offsetFileInfo": int("00001080", 16),
            "offsetTime": int("0000011a", 16),
            "offsetData": int("00001800", 16),
            "scaleOffset1": 4724,
            "scaleOffset2": 4732
        }
    }
    offsets = version_offsets[v]

    wavelengths: List[str] = ["Wavelength_" + str(read_file_info(i, offsets["offsetFileInfo"])) for i in files]

    result: List[NumList] = [ph.DeltaCompression(i, offsets["offsetData"], 12) for i in files]
    result_scaled: List[NumList] = [
        scale_data(files[i], result[i], offsets["scaleOffset1"], offsets["scaleOffset2"]) for i in range(0, len(result))
    ]
    times: List[List[float]] = [read_time(i, len(result[0]), offsets["offsetTime"]) for i in files]
    if not check_identical_lists(times):
        raise ValueError("File Error")
    time: List[float] = times[0]
    df: pd.DataFrame = pd.DataFrame(result_scaled).transpose()
    df.columns = wavelengths
    df["time"] = time
    return df



def plot_chromatograms(path: str):
    df = read_chromatograms(path)
    time = df["time"]
    data = df.drop(columns=["time"])
    wavelengths = df.columns[:-1]
    df_melted = df.melt(id_vars="time", var_name="Wavelengths", value_name="Data")
    fig = px.line_3d(
        df_melted, x="time", y="Wavelengths", z="Data", color="Wavelengths"
    )
    fig.update_traces(marker=dict(size=5))
    fig.show()



def read_uv(path: str) -> pd.DataFrame:
    uv = ph.UVClass(path)
    time: pd.DataFrame = pd.DataFrame(uv.getTime())
    wavelengths: List[int] = uv.getWavelengths().astype("int").tolist()
    data: pd.DataFrame = pd.DataFrame(uv.getData())
    if (len(wavelengths) == len(data.columns)):
        data.columns = ["Wavelength_" + str(i) for i in wavelengths]
    else:
        data.columns = ["Wavelength_" + str(i) for i in range(len(data.columns))]
    data["time"] = time
    df_melted = data.melt(id_vars="time", var_name="Wavelengths", value_name="Data")
    max_data = df_melted["Data"].max()
    df_melted["Normalized_Data"] = df_melted["Data"] / max_data
    df_unmelted = df_melted.pivot_table(
        index="time", columns="Wavelengths", values="Normalized_Data"
    ).reset_index()
    return df_unmelted



def plot_uv(path: str):
    df = read_uv(path)
    time = df["time"]
    data = df.drop(columns=["time"])
    wavelengths = df.columns[:-1]
    trace = go.Surface(x=wavelengths, y=time, z=data.values)
    fig = go.Figure(data=[trace])
    fig.show()
