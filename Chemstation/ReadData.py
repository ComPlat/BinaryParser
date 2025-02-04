import BinaryParser as bp

path = "./ChemStationData/LCMS_DatenAgilent_SVS/SVS_1025F1.D/"
df = bp.read_chromatograms(path)
print(df)
print(df.shape)
bp.plot_chromatograms(path)
