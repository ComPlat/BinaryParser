// TODO: find min mz and max mz in meta data
// TODO: read meta data
// NOTE: Reading data of MSD1.MS file from
// Agilent ChemStation
#include <cmath>
#include <cstddef>
#include <cstdint>
#include <fstream>
#include <iostream>
#include <pybind11/pybind11.h>
#include <pybind11/stl.h>
#include <vector>

uint16_t endianSwapU16(uint16_t value) {
  return ((value & 0xFF) << 8) | ((value >> 8) & 0xFF);
}

void endianSwapU32(uint32_t &x) {
  x = ((x << 24) & 0xFF000000) | ((x << 8) & 0x00FF0000) |
      ((x >> 8) & 0x0000FF00) | ((x >> 24) & 0x000000FF);
}

std::vector<char> ReadFile(const std::string &file_path) {
  std::ifstream file(file_path, std::ios::binary | std::ios::ate);
  if (!file.is_open()) {
    throw std::runtime_error("Error opening file");
  }
  std::size_t size = file.tellg();
  if (size == 0) {
    throw std::runtime_error("File is empty");
  }
  std::vector<char> buffer(size);
  file.seekg(0, std::ios::beg);
  file.read(buffer.data(), size);
  if (!file) {
    throw std::runtime_error("Error reading file");
  }
  file.close();
  return buffer;
}

std::uint16_t CastToUint16(std::vector<char> &buffer, std::size_t offset) {
  std::uint16_t res = *reinterpret_cast<const uint16_t *>(&buffer[offset]);
  return endianSwapU16(res);
}

std::uint32_t CastToUint32(std::vector<char> &buffer, std::size_t offset) {
  std::uint32_t res = *reinterpret_cast<const uint32_t *>(&buffer[offset]);
  endianSwapU32(res);
  return res;
}

std::size_t NumberOfCycles(std::vector<char> &buffer) {
  int data_start = 0x116;
  return CastToUint32(buffer, data_start);
}

std::size_t FindDataStart(std::vector<char> &buffer) {
  int data_start = 0x10A;
  int offset_correction = CastToUint16(buffer, data_start);
  int where = offset_correction * 2 - 2;
  return where;
}

std::vector<double> ConvertMZIntensity(std::vector<uint16_t> &data) {
  std::vector<double> mz_intensity;
  mz_intensity.resize(data.size());
  for (std::size_t i = 0; i < data.size(); i++) {
    if (i % 2 != 0) {                     // Intensity
      uint16_t head_bits = data[i] >> 14; // Shift right by 14 bits
      uint16_t tail_bits =
          data[i] & 0x3FFF; // Extract tail: 0x3FFF = 0011111111111111 (14 bits)
      mz_intensity[i] = std::pow(8, head_bits) * tail_bits;
    } else { // MZ
      mz_intensity[i] = static_cast<double>(data[i]) / 20;
    }
  }
  return mz_intensity;
}

struct Cycle {
  std::vector<double> mz;
  std::vector<double> intensity;
  double retention_time;

  // Convert Cycle to a Python dictionary
  std::map<std::string, pybind11::object> to_dict() const {
    return {{"mz", pybind11::cast(mz)},
            {"intensity", pybind11::cast(intensity)},
            {"retention_time", pybind11::cast(retention_time)}};
  }
};

void ReadCycleData(Cycle &cycle, std::vector<char> &buffer,
                   std::size_t data_start, std::size_t cycle_size) {
  std::vector<std::uint16_t> data;
  data.resize(cycle_size);
  for (std::size_t i = 0; i < cycle_size; i++) {
    data[i] = CastToUint16(buffer, data_start);
    data_start += 2;
  }
  std::vector<double> mz_intensity = ConvertMZIntensity(data);
  cycle.mz.resize(mz_intensity.size() / 2);
  cycle.intensity.resize(mz_intensity.size() / 2);
  for (std::size_t i = 0; i < mz_intensity.size(); i++) {
    if (i % 2 == 0) {
      cycle.mz[i / 2] = mz_intensity[i];
    } else {
      cycle.intensity[i / 2] = mz_intensity[i];
    }
  }
}

std::vector<Cycle> readCycles(const std::string &file_path) {
  std::vector<char> buffer = ReadFile(file_path);
  std::size_t data_start = FindDataStart(buffer);
  std::size_t number_of_cycles = NumberOfCycles(buffer);
  std::vector<Cycle> cycles;
  cycles.resize(number_of_cycles);
  std::size_t counter = data_start;
  for (std::size_t i = 0; i < number_of_cycles; i++) {
    if (counter >= buffer.size()) {
      throw std::runtime_error("Error extracting data");
    }
    counter += 2;
    std::size_t time = CastToUint32(buffer, counter);
    counter += 10;
    std::size_t temp = counter;
    std::size_t cycle_size = CastToUint16(buffer, counter);
    counter += 6;
    ReadCycleData(cycles[i], buffer, counter, cycle_size * 2);
    cycles[i].retention_time = static_cast<double>(time) / 60000;
    counter += cycle_size * 4;
    counter += 10;
  }
  return cycles;
}

namespace py = pybind11;

std::vector<std::map<std::string, py::object>>
py_readCycles(const std::string &file_path) {
  std::vector<Cycle> cycles = readCycles(file_path);
  std::vector<std::map<std::string, py::object>> result;
  for (const auto &cycle : cycles) {
    result.push_back(cycle.to_dict());
  }
  return result;
}

PYBIND11_MODULE(parser_ms, m) {
  m.doc() = "Chemstation MS data extraction module";
  m.def("read_cycles", &py_readCycles,
        "Extract cycles from an Chemstation MS file");
}
