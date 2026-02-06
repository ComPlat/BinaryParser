import binary_parser.helper.parser_ms as pm
import pandas as pd

from typing import List



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



def merge_cycles_into_df(cycles: List[dict]) -> pd.DataFrame:
    """Convert all cycles into a single Pandas DataFrame with cycle_id."""
    cycle_dfs = convert_cycles_to_dfs(cycles)
    return pd.concat(cycle_dfs, ignore_index=True)



def read_chemstation_file(file_path: str) -> pd.DataFrame:
    cycles = pm.read_cycles(file_path)
    return merge_cycles_into_df(cycles)
