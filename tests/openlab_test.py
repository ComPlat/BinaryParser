import binary_parser.openlab as bp
import binary_parser.openlab.openlab as openlab
import pandas as pd
import matplotlib.pyplot as plt

path = "./tests/OpenLab/"


def test_read_attr():
    attr = bp.read_attr(path)
    assert attr.shape == (12, 49)
    assert attr["detector_unit"][1] == "mAU"



def test_read_ls():
    data = bp.read_lc(path)
    assert data.shape == (48000, 3)
    assert data.columns.tolist() == ["RetentionTime", "DetectorSignal", "wavelength"]
    assert all(data["wavelength"].unique() == [210, 230, 254, 280, 366, 450, 550, 580])



def test_read_ms():
    ms = bp.read_ms(path)
    assert type(ms) == list
    assert ms[1].shape == (1358778, 3)
    assert ms[0].shape == (1324471, 3)



def test_plot_ms_combines_minus_plus(monkeypatch):
    def fake_read_ms(_path):
        return [
            pd.DataFrame(
                {"time": [1.0, 1.0, 2.0], "mz": [100, 101, 102], "intensities": [10, 20, 30]}
            ),
            pd.DataFrame(
                {"time": [1.0, 2.0, 2.0], "mz": [200, 201, 202], "intensities": [40, 50, 60]}
            ),
        ]

    monkeypatch.setattr(openlab, "read_ms", fake_read_ms)
    fig = openlab.plot_ms("unused", show=False)

    assert [trace.name for trace in fig.data] == ["minus", "plus"]
    assert list(fig.data[0].x) == [1.0, 2.0]
    assert list(fig.data[0].y) == [30, 30]
    assert list(fig.data[1].x) == [1.0, 2.0]
    assert list(fig.data[1].y) == [40, 110]

path_2 = "./tests/X32932.D/"


def test_read_attr_2():
    attr = bp.read_attr(path_2)
    assert attr.shape == (12, 49)
    assert attr["detector_unit"][1] == "mAU"



def test_read_ls_2():
    data = bp.read_lc(path_2)
    assert data.shape == (48000, 3)
    assert data.columns.tolist() == ["RetentionTime", "DetectorSignal", "wavelength"]
    assert all(data["wavelength"].unique() == [210, 230, 254, 280, 366, 450, 550, 580])



def test_read_ms_2():
    [df_minus, df_plus] = bp.read_ms(path_2)
    bp.plot_ms(path_2)

    assert df_plus.shape == (1329242, 3)
    assert df_minus.shape == (1328958, 3)
