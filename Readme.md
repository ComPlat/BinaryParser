# BinaryParser

**BinaryParser** is a Python package designed to parse HPLC UVVIS files located in `.D` folders.
It provides an easy-to-use interface for extracting and analyzing chromatographic data.

## Installation

For Ubuntu users, the package is precompiled and can be installed directly from GitHub using pip:

```bash
pip install git+https://github.com/ComPlat/BinaryParser.git
```

### Dependencies

The following dependencies are required:

    pybind11
    pandas
    numpy
    typeguard
    plotly

These dependencies are specified in the install_requires section of the setup.py file.
Usage

After installation, you can use BinaryParser as follows:

import BinaryParser as bp

path = "./tests/X3346.D"
df = bp.read_chromatograms(path)
bp.plot_chromatograms(path)

### License

The project is licensed under the GNU General Public License v3.0.
See the LICENSE file for more information.
