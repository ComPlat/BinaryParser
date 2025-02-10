#include <Rcpp.h>
#include <cstdint>
#include <fstream>
#include <iostream>
#include <vector>

uint16_t endianSwapU16(uint16_t value) {
  return ((value & 0xFF) << 8) | ((value >> 8) & 0xFF);
}

// [[Rcpp::export]]
std::vector<uint16_t> readUint16(std::string &filepath, int offset,
                                 std::size_t n) {
  std::ifstream file(filepath, std::ios::binary | std::ios::ate);
  if (!file.is_open())
    throw std::runtime_error("Error opening file");
  size_t currentPos = 0;
  file.seekg(currentPos + offset, std::ios::beg);
  std::vector<uint16_t> data;
  data.resize(n);
  for (std::size_t i = 0; i < n; i++) {
    file.read(reinterpret_cast<char *>(&data[i]), sizeof(uint16_t));
    data[i] = endianSwapU16(data[i]);
  }
  file.close();
  return data;
}

// [[Rcpp::export]]
std::vector<uint16_t> read_mz_intensity(std::string &file_path) {
  std::ifstream file(file_path, std::ios::binary | std::ios::ate);
  if (!file.is_open())
    std::runtime_error("Error opening file");
  size_t file_size = file.tellg();
  int data_start = 266;
  int offset_correction = readUint16(file_path, data_start, 1)[0];
  int where = data_start + offset_correction * 2 - 2;
  size_t data_size = file_size - where;
  size_t n = data_size / 2;
  std::vector<uint16_t> data = readUint16(file_path, where, n);
  return data;
}

// [[Rcpp::export]]
std::vector<double> convert_mz_intensity(std::vector<uint32_t> &data) {
  std::vector<double> mz_intensity;
  mz_intensity.resize(data.size());
  for (size_t i = 0; i < data.size(); i++) {
    if (i % 2 != 0) {                     // intensity
      uint16_t head_bits = data[i] >> 14; // Shift right by 14 bits
      uint16_t tail_bits =
          data[i] & 0x3FFF; // 0x3FFF = 0011111111111111 (14 bits)
      mz_intensity[i] = std::pow(8, head_bits) * tail_bits;
    } else { // mz
      mz_intensity[i] = static_cast<double>(data[i]) / 20;
    }
  }
  return mz_intensity;
}
