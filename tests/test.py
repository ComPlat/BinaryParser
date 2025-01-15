import parser_binary.hplc as ph
import hplc as ph

path = "/home/konrad/Documents/GitHub/RProjects/chromatogramsR/X-Vials/X3346.D"
df = ph.read_chromatograms(path)
print(df)
