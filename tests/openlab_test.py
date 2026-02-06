import binary_parser.openlab as bp

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
    assert ms[0].shape == (1358778, 3)
    assert ms[1].shape == (1324471, 3)

