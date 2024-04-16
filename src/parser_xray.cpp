#include <pybind11/pybind11.h>
#include <pybind11/stl.h>
#include <pybind11/stl_bind.h>
#include <pybind11/operators.h>
#include <pybind11/numpy.h>
#include <fstream>
#include <cstdint>
#include <vector>
#include <iostream>
#include  <iomanip>

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

std::vector<int32_t> readInt32(std::string &filepath, int offset)
{
    std::ifstream file(filepath, std::ios::binary | std::ios::ate);
    if (!file.is_open())
        throw std::runtime_error("Error opening file");
    size_t currentPos = 0;
    file.seekg(currentPos + offset, std::ios::beg);
    int32_t buffer = 0;
    int n = 1000;
    std::vector<int32_t> res(n);
    for (int i = 0; i < n; i++)
    {
        auto pos = file.tellg();
        buffer = 0;
        file.read(reinterpret_cast<char *>(&buffer), sizeof(int32_t));
        endianSwap32(buffer);
        res[i] = buffer;
    }
    file.close();
    return res;
}

pybind11::list readDoubles(std::string &filepath, int offset)
{
    std::ifstream file(filepath, std::ios::binary | std::ios::ate);
    if (!file.is_open()) throw std::runtime_error("Error opening file");
    size_t currentPos = 0;
    size_t fileSize = file.tellg();
    size_t size = fileSize / sizeof(double);
    file.seekg(currentPos + offset, std::ios::beg);
    double buffer = 0;
    std::vector<double> res(size);

    for (int i = 0; i < size; i++)
    {
        auto pos = file.tellg();
        file.read(reinterpret_cast<char *>(&buffer), sizeof(double));
        res[i] = buffer;
    }
    file.close();
    return pybind11::cast(res);
}

pybind11::list readFloates(std::string &filepath, int offset)
{
    std::ifstream file(filepath, std::ios::binary | std::ios::ate);
    if (!file.is_open()) throw std::runtime_error("Error opening file");
    size_t currentPos = 0;
    size_t fileSize = file.tellg();
    size_t size = fileSize / sizeof(double);
    file.seekg(currentPos + offset, std::ios::beg);
    float buffer = 0;
    std::vector<float> res(size);

    for (int i = 0; i < size; i++)
    {
        auto pos = file.tellg();
        file.read(reinterpret_cast<char *>(&buffer), sizeof(float));
        res[i] = buffer;
    }
    file.close();
    return pybind11::cast(res);
}


void readChars(std::string &filepath) {
    std::ifstream file(filepath, std::ios::binary);
    file.seekg(0, std::ios::end);
    size_t fileSize = file.tellg();
    file.seekg(0, std::ios::beg);
    std::vector<char> buffer(fileSize);
    file.read(buffer.data(), fileSize);
    file.close();

    int address = 0x00000000;
    size_t n = 8;
    size_t size = buffer.size();
    for(size_t i = 0; i < (size / n); i++) {
        for(size_t j = 0; j < n; j++) {
            std::cout << std::hex << std::setfill('0') << std::setw(3) << address << " ";     
            address++;
        }
        std::cout << std::endl;
        for(size_t j = 0; j < n; j++) {
            std::cout << std::dec << "'" << buffer[i*n + j] << "'" << " "; 
            address++;
        }
        std::cout << std::endl;
    }
}

PYBIND11_MODULE(parser_xray, m)
{   

   m.doc() = R"pbdoc(
   Pybind11 example plugin
   -----------------------

   .. currentmodule:: parser_xray

   .. autosummary::
   :toctree: _generate

   test
   )pbdoc";
   m.def("readChars", &readChars, R"pbdoc(
    Read content of file as chars
    )pbdoc");
   m.def("readDoubles", &readDoubles, R"pbdoc(
    Read content of file as doubles
    )pbdoc");
   m.def("readFloates", &readFloates, R"pbdoc(
    Read content of file as floates
    )pbdoc");

#ifdef VERSION_INFO
   m.attr("__version__") = MACRO_STRINGIFY(VERSION_INFO);
#else
   m.attr("__version__") = "dev";
#endif
}
