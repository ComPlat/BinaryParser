#include <pybind11/pybind11.h>
#include <pybind11/stl.h>
#include <pybind11/stl_bind.h>
#include <pybind11/operators.h>
#include <pybind11/numpy.h>
#include <fstream>
#include <cstdint>
#include <vector>
#include <iostream>

namespace py = pybind11;

#define STRINGIFY(x) #x
#define MACRO_STRINGIFY(x) STRINGIFY(x)

template <typename T>
void endianSwap16(T &x)
{
    x = (((x) >> 8) & 0xFF) | (((x) & 0xFF) << 8);
}

template <typename T>
void endianSwap32(T &x)
{
    x = ((x >> 24) & 0xFF) | ((x << 8) & 0xFF0000) | ((x >> 8) & 0xFF00) | (x << 24);
}

void endianSwapU32(uint32_t &x)
{
    x = ((x << 24) & 0xFF000000) | ((x << 8) & 0x00FF0000) | ((x >> 8) & 0x0000FF00) | ((x >> 24) & 0x000000FF);
}

uint16_t endianSwapU16(uint16_t value) 
{
    return ((value & 0xFF) << 8) | ((value >> 8) & 0xFF);
}

pybind11::list DeltaCompresion(std::string filepath, int offset, int n)
{
    std::ifstream file(filepath, std::ios::binary | std::ios::ate);
    if (!file.is_open()) throw std::runtime_error("Error opening file");
    size_t sizeFile = file.tellg();
    std::vector<int32_t> res(sizeFile / 2);
    size_t currentPos = 0;
    file.seekg(currentPos + offset, std::ios::beg);

    int16_t buffer1 = 0;
    int32_t buffer2 = 0;
    int16_t buffer3 = 0;
    int32_t buffer4 = 0;

    int iter = 0;
    while (currentPos < sizeFile)
    {
        file.read(reinterpret_cast<char *>(&buffer1), sizeof(int16_t));
        endianSwap16(buffer1);
        buffer2 = buffer4;

        if (buffer1 << 12 == 0)
        {
            res.resize(iter);
            break;
        }

        for (int i = 0; i < (buffer1 & 4095); i++)
        {
            file.read(reinterpret_cast<char *>(&buffer3), sizeof(int16_t));
            endianSwap16(buffer3);
            if (buffer3 != -32768)
            {
                buffer2 = buffer2 + (int32_t)buffer3;
                res[iter] = buffer2;
                iter++;
            }
            else
            {
                file.read(reinterpret_cast<char *>(&buffer2), sizeof(int32_t));
                endianSwap32(buffer2);
                res[iter] = buffer2;
                iter++;
            }
        }
        buffer4 = buffer2;
        currentPos = file.tellg();
        file.seekg(currentPos, std::ios::beg);
    }
    return pybind11::cast(res);
}

double readDouble(std::string &filepath, int offset)
{
    std::ifstream file(filepath, std::ios::binary | std::ios::ate);
    if (!file.is_open()) throw std::runtime_error("Error opening file");
    size_t currentPos = 0;
    file.seekg(currentPos + offset, std::ios::beg);
    double buffer = 0;

    for (int i = 0; i < 1; i++)
    {
        auto pos = file.tellg();
        file.read(reinterpret_cast<char *>(&buffer), sizeof(double));
        uint64_t *ptr = reinterpret_cast<uint64_t *>(&buffer);
        *ptr = __builtin_bswap64(*ptr);
    }
    file.close();
    return buffer;
}

double readInt(std::string &filepath, int offset)
{
    std::ifstream file(filepath, std::ios::binary | std::ios::ate);
    if (!file.is_open())
        throw std::runtime_error("Error opening file");
    size_t currentPos = 0;
    file.seekg(currentPos + offset, std::ios::beg);
    int32_t buffer = 0;

    for (int i = 0; i < 1; i++)
    {
        auto pos = file.tellg();
        file.read(reinterpret_cast<char *>(&buffer), sizeof(int32_t));
    }
    file.close();
    return buffer;
}

pybind11::list readUint8(std::string& filepath, int offset) {
  std::ifstream file(filepath, std::ios::binary | std::ios::ate);
  if (!file.is_open())
    throw std::runtime_error("Error opening file");
size_t currentPos = 0;
file.seekg(currentPos + offset, std::ios::beg);
uint8_t buffer = 0;
std::vector<std::string> res;

for(int i = 0; i < 40; i++) { 
    auto pos = file.tellg();
    file.read(reinterpret_cast<char*>(&buffer), sizeof(uint8_t));   
    res.push_back(std::string(1, static_cast<char>(buffer)));
}
file.close();
return pybind11::cast(res);
}

pybind11::list readTime(std::string& filepath, int offset) {
  std::ifstream file(filepath, std::ios::binary | std::ios::ate);
  if (!file.is_open())
    throw std::runtime_error("Error opening file");
size_t currentPos = 0;
file.seekg(currentPos + offset, std::ios::beg);
int32_t buffer = 0;
std::vector<double> res(2);
for(int i = 0; i < 2; i++) {
    file.read(reinterpret_cast<char*>(&buffer), sizeof(int32_t));   
    endianSwap32(buffer);
    res[i] = static_cast<double>(buffer)/60000.0;
}
file.close();
return pybind11::cast(res);
}

size_t updatePos(std::ifstream& file, int offset) {
  size_t currentPos = file.tellg();
  file.seekg(currentPos + offset, std::ios::beg);
  currentPos = file.tellg();
  return currentPos;
}


double readInt32(std::string &filepath, int offset)
{
    std::ifstream file(filepath, std::ios::binary | std::ios::ate);
    if (!file.is_open())
        throw std::runtime_error("Error opening file");
    size_t currentPos = 0;
    file.seekg(currentPos + offset, std::ios::beg);
    int32_t buffer = 0;

    for (int i = 0; i < 1; i++)
    {
        auto pos = file.tellg();
        file.read(reinterpret_cast<char *>(&buffer), sizeof(int32_t));
    }
    file.close();
    endianSwap32(buffer);
    return buffer;
}

struct UVClass {
    UVClass(std::string filepath_) : filepath(filepath_) {
        int offset = 0x1002;
        int nscansOffset = 0x116;
        int nscans = readInt32(filepath, nscansOffset);
        std::ifstream file(filepath, std::ios::binary | std::ios::ate);
        if (!file.is_open()) std::runtime_error("Error opening file");
        size_t sizeFile = file.tellg();
        size_t currentPos = 0;
        uint16_t buffer1 = 0;
        uint32_t buffer2 = 0;
        uint16_t buffer3 = 0;
        uint16_t buffer4 = 0;
        uint16_t buffer5 = 0;
        int16_t buffer6 = 0;
        int32_t buffer7 = 0;
        time.resize(nscans);
        ndata.resize(nscans);
        
        for(int i = 0; i < nscans; i++) {
            file.seekg(currentPos + offset, std::ios::beg);
            file.read(reinterpret_cast<char*>(&buffer1), sizeof(uint16_t)); // 2
            offset += buffer1;
            file.read(reinterpret_cast<char*>(&buffer2), sizeof(uint32_t)); // 4
            time[i] = static_cast<double>(buffer2) / 60000.0;
            file.read(reinterpret_cast<char*>(&buffer3), sizeof(uint16_t)); // 2
            file.read(reinterpret_cast<char*>(&buffer4), sizeof(uint16_t)); // 2
            file.read(reinterpret_cast<char*>(&buffer5), sizeof(uint16_t)); // 2
            for (int wv = buffer3; wv < buffer4; wv += buffer5) {
                double current_w = static_cast<double>(wv) / 20.0;
                auto it = std::find(wavelengths.begin(), wavelengths.end(), current_w);
                if(it == wavelengths.end()) {
                  wavelengths.push_back(current_w);
                }
            }
            auto max_wavelength_it = std::max_element(wavelengths.begin(), wavelengths.end());
            int max_index = std::distance(wavelengths.begin(), max_wavelength_it);
            std::vector<int> wv_index_map;
            for (int val = max_index + 1; val < wavelengths.size(); ++val) {
                wv_index_map.push_back(val);
            }
            for (int val = 0; val <= max_index; ++val) {
                wv_index_map.push_back(val);
            }
            ndata[i].resize(wavelengths.size());
            for (int j = 0; j < wv_index_map.size(); j++) {
                file.read(reinterpret_cast<char*>(&buffer6), sizeof(int16_t)); // 2
                if(buffer6 == -32768) {
                    file.read(reinterpret_cast<char*>(&buffer7), sizeof(int32_t)); // 4
                } else {
                    buffer7 += buffer6;
                }
                ndata[i][j] = buffer7;  // / 2000.0; // correct?
            }
        }
        file.close();
    }

    py::array_t<double> getTime() const { 
        return py::cast(time);
    }

    py::array_t<double> getWavelengths() const {
        return py::cast(wavelengths);
    }

    py::array_t<double> getData() const {
        std::size_t nRows = ndata.size();
        std::size_t nCols = ndata.empty() ? 0 : ndata[0].size();
        pybind11::array_t<double> npArray({nRows, nCols});
        auto ptr = npArray.mutable_data();
        for (std::size_t i = 0; i < nRows; ++i) {
            for (std::size_t j = 0; j < nCols; ++j) {
                ptr[i * nCols + j] = ndata[i][j];
            }
        }
        return npArray;
    }

    std::string filepath; 
    std::vector<double> time;
    std::vector<double> wavelengths;
    std::vector<std::vector<double>> ndata;
};

PYBIND11_MODULE(parser_hplc, m)
{   
   py::class_<UVClass>(m, "UVClass")
   .def(py::init<std::string>())
   .def("getTime", &UVClass::getTime)
   .def("getWavelengths", &UVClass::getWavelengths)
   .def("getData", &UVClass::getData);

   m.doc() = R"pbdoc(
   Pybind11 example plugin
   -----------------------

   .. currentmodule:: parser_hplc

   .. autosummary::
   :toctree: _generate

   readInt
   readDouble 
   DeltaCompresionCpp
   )pbdoc";
   m.def("DeltaCompresion", &DeltaCompresion, R"pbdoc(
    read content of ch file and conduct delta compression on data
    )pbdoc");
   m.def("readDouble", &readInt, R"pbdoc(
    Reads a double at a specific location of a ch file
    )pbdoc");
   m.def("readInt", &readInt, R"pbdoc(
    Reads an int32_t at a specific location of a ch file
    )pbdoc");
   m.def("readUint8", &readUint8, R"pbdoc(
    Reads an uint8_t at a specific location of a ch file
    )pbdoc");
   m.def("readTime", &readTime, R"pbdoc(
    Reads the time of a ch file
    )pbdoc");

#ifdef VERSION_INFO
   m.attr("__version__") = MACRO_STRINGIFY(VERSION_INFO);
#else
   m.attr("__version__") = "dev";
#endif
}