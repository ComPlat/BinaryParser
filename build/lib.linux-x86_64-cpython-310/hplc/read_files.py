import parser_hplc as ph
import pandas as pd
import numpy as np
import plotly.graph_objs as go
import plotly.express as px
import re
import sys
from os import listdir
from os.path import isfile, join
from typeguard import typechecked
from typing import List

@typechecked
def check_identical_lists(lst: List[List[float]]) -> bool:
    if not lst:
        return False  
    first_sublist: List[float] = lst[0]
    for sublist in lst[1:]:
        if sublist != first_sublist:
            return False  
    return True

@typechecked
def read_time(file_path: str, length: int) -> List[float]:
	offsetTime = int("0000011a", 16)
	time: List[float] = ph.readTime(file_path, offsetTime)
	step_size: float = (time[1] - time[0]) / (length - 1)
	res: List[float] = [time[0] + i * step_size for i in range(length)]
	return(res)

@typechecked
def read_file_info(file_path: str) -> int:
	offsetFileInfo = int("00001080", 16)
	res = ph.readUint8(file_path, offsetFileInfo)
	res = [x if x != '\x00' else '' for x in res]
	res = ''.join(res)
	matches = int(re.findall(r"Sig=(\d+),", res)[0])
	return(matches)

@typechecked
def scale_data(file_path: str, l: List[int]) -> List[float]:
	intercept: float = ph.readDouble(file_path, 4724)
	slope: float = ph.readDouble(file_path, 4732)
	res: List[float] = [float(i) *slope + intercept for i in l]
	return(res)

@typechecked
def read_chromatograms(path: str) -> pd.DataFrame:
	files: List[str] = [path + '/' + f for f in listdir(path) if isfile(join(path, f)) and f.endswith(".ch")]
	wavelengths: List[str] = ["Wavelength_" + str(read_file_info(i)) for i in files]
	offset: int = int("00001800", 16)
	result: List[List[int]] = [ph.DeltaCompresion(i, offset, 12) for i in files]
	result_scaled: List[List[float]] = [scale_data(files[i], result[i]) for i in range(0, len(result))]
	times: List[List[float]] = [read_time(i, len(result[0])) for i in files] 
	if not check_identical_lists(times):
		sys.exit("Error")
	time: List[float] = times[0]
	df: pd.DataFrame = pd.DataFrame(result_scaled).transpose()
	df.columns = wavelengths
	df['time'] = time
	return(df)

@typechecked
def plot_chromatograms(path: str):
	df = read_chromatograms(path)
	time = df['time']
	data = df.drop(columns = ['time'])
	wavelengths = df.columns[:-1]  
	df_melted = df.melt(id_vars='time', var_name='Wavelengths', value_name = 'Data')
	fig = px.line_3d(df_melted, x='time', y='Wavelengths', z='Data', color='Wavelengths')
	fig.update_traces(marker=dict(size=5))
	fig.show()

@typechecked
def read_uv(path: str) -> pd.DataFrame:
	uv = ph.UVClass(path)
	time: pd.DataFrame = pd.DataFrame(uv.getTime())
	wavelengths: List[int] = uv.getWavelengths().astype('int').tolist()
	data: pd.DataFrame = pd.DataFrame(uv.getData())	
	data.columns = ["Wavelength_" + str(i) for i in wavelengths]
	data['time'] = time
	df_melted = data.melt(id_vars='time', var_name='Wavelengths', value_name = 'Data')
	max_data = df_melted['Data'].max()
	df_melted['Normalized_Data'] = df_melted['Data'] / max_data
	df_unmelted = df_melted.pivot_table(index='time', columns='Wavelengths', values='Normalized_Data').reset_index()
	return(df_unmelted)

@typechecked
def plot_uv(path: str):
	df = read_uv(path)
	time = df['time']  
	data = df.drop(columns = ['time'])
	wavelengths = df.columns[:-1]  
	trace = go.Surface(
	    x = wavelengths,
	    y = time,
	    z = data.values
	)
	fig = go.Figure(data = [trace])
	fig.show()

#path = "/home/konrad/Documents/GitHub/chromatogramsR/X-Vials/X3346.D/dad1.uv"
#plot_uv(path)
