import parser_binary.hplc as ph

path = "/home/konrad/Documents/GitHub/chromatogramsR/X-Vials/X3346.D"
df = ph.read_chromatograms(path)
print(df)

