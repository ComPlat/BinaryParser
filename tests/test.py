import BinaryParser as bp

path = "./tests/X3346.D"
df = bp.read_chromatograms(path)

assert df.columns.tolist() == [
    "Wavelength_280",
    "Wavelength_230",
    "Wavelength_300",
    "Wavelength_400",
    "Wavelength_254",
    "time",
]
assert df.size == 20706
assert df.shape == (3451, 6)
print("All tests passed!")
