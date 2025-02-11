from pybind11.setup_helpers import Pybind11Extension, build_ext
from setuptools import setup, find_packages

__version__ = "0.0.1"

ext_modules = [
    Pybind11Extension(
        "parser_hplc",
        ["src/parser_hplc.cpp"],
        define_macros=[("VERSION_INFO", __version__)],
        extra_compile_args=["-std=c++17"],
    ),
    Pybind11Extension(
        "parser_ms",
        ["src/parser_ms.cpp"],
        define_macros=[("VERSION_INFO", __version__)],
        extra_compile_args=["-std=c++17"],
    ),
    Pybind11Extension(
        "parser_xray",
        ["src/parser_xray.cpp"],
        define_macros=[("VERSION_INFO", __version__)],
        extra_compile_args=["-std=c++17"],
    ),
]

setup(
    name="BinaryParser",
    version=__version__,
    author="Konrad Krämer",
    author_email="konrad.kraemer@kit.edu",
    description="Parsing binary files",
    long_description="",
    ext_modules=ext_modules,
    extras_require={"test": "pytest"},
    cmdclass={"build_ext": build_ext},
    zip_safe=False,
    python_requires=">=3.7",
    # packages=find_packages(),
    packages=(["BinaryParser"] + find_packages()),
    package_dir={"BinaryParser": "."},
    setup_requires=["pybind11"],
    install_requires=["pybind11", "pandas", "numpy", "typeguard", "plotly"],
)
