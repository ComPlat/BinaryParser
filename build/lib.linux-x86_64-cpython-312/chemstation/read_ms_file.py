import parser_ms as pm
import pandas as pd
import plotly.graph_objs as go
import plotly.express as px
from typeguard import typechecked
from typing import List


@typechecked
def convert_cycles_to_dfs(cycles: List[dict]) -> List[pd.DataFrame]:
    """Convert Chemstation LC-MS cycles into a list of Pandas DataFrames."""
    cycle_dfs = []
    for i, cycle in enumerate(cycles):
        df = pd.DataFrame({
            "mz": cycle["mz"],
            "intensity": cycle["intensity"],
            # Repeat for each row
            "retention_time": [cycle["retention_time"]] * len(cycle["mz"])
        })
        df["cycle_id"] = i
        cycle_dfs.append(df)
    return cycle_dfs


@typechecked
def merge_cycles_into_df(cycles: List[dict]) -> pd.DataFrame:
    """Convert all cycles into a single Pandas DataFrame with cycle_id."""
    cycle_dfs = convert_cycles_to_dfs(cycles)
    return pd.concat(cycle_dfs, ignore_index=True)


@typechecked
def read_chemstation_file(file_path: str) -> pd.DataFrame:
    cycles = pm.read_cycles(file_path)
    cycle_dfs = convert_cycles_to_dfs(cycles)
    return merge_cycles_into_df(cycles)
