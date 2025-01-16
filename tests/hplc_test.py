import BinaryParser as bp


def test_read_chromatograms():
    path = "./tests/X3346.D"
    df = bp.read_chromatograms(path)
    assert df.size == 20706
    assert df.shape == (3451, 6)
