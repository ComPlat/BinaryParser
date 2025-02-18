import BinaryParser as bp

path = "./tests/OpenLab/"


def test_read_attr():
    attr = bp.read_attr(path)
    assert attr.shape == (12, 49)
    assert attr["detector_unit"][1] == "mAU"


test_read_attr()


def test_read_ls():
    data = bp.read_lc(path)
    assert data.shape == (48000, 3)
    assert data.columns.tolist() == ["RetentionTime", "DetectorSignal", "wavelength"]
    assert all(data["wavelength"].unique() == [254, 550, 280, 450, 210, 230, 366, 580])


test_read_ls()


def test_read_ms():
    ms = bp.read_ms(path)
    assert type(ms) == list
    assert ms[0].shape == (1358778, 3)
    assert ms[1].shape == (1324471, 3)


test_read_ms()
