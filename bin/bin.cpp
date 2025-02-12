#include <Rcpp.h>
#include <bitset>
#include <cstddef>
#include <cstdint>
#include <fstream>
#include <vector>

// [[Rcpp::export]]
int CastToUint8(std::string &buffer) {
  const char *b = buffer.c_str();
  std::uint8_t res = *reinterpret_cast<const uint8_t *>(b);
  return res;
}

uint8_t hexToByte(const std::string &hex) {
  return static_cast<uint8_t>(std::stoul(hex, nullptr, 16));
}

// [[Rcpp::export]]
int CastToUint32(Rcpp::CharacterVector buffer) {
  if (buffer.size() != 4) {
    Rcpp::stop("Expected exactly 4 hex strings representing bytes.");
  }
  uint8_t byte1 = hexToByte(Rcpp::as<std::string>(buffer[0]));
  uint8_t byte2 = hexToByte(Rcpp::as<std::string>(buffer[1]));
  uint8_t byte3 = hexToByte(Rcpp::as<std::string>(buffer[2]));
  uint8_t byte4 = hexToByte(Rcpp::as<std::string>(buffer[3]));
  uint32_t res = (byte1 << 24) | (byte2 << 16) | (byte3 << 8) | byte4;
  return res;
}

// [[Rcpp::export]]
std::uint16_t CastToUint16(Rcpp::CharacterVector buffer) {
  if (buffer.size() != 2) {
    std::cout << "size = " << buffer.size() << std::endl;
    Rcpp::stop("Expected exactly 2 hex strings representing bytes.");
  }
  uint8_t byte1 = hexToByte(Rcpp::as<std::string>(buffer[0]));
  uint8_t byte2 = hexToByte(Rcpp::as<std::string>(buffer[1]));
  uint16_t res = (byte1 << 8) | byte2;
  return res;
}

// [[Rcpp::export]]
Rcpp::IntegerMatrix uint16_to_bit_matrix(Rcpp::IntegerVector values) {
  int n = values.size();
  Rcpp::IntegerMatrix bit_matrix(n, 16);
  for (int i = 0; i < n; i++) {
    std::bitset<16> bits(values[i]);
    for (int j = 0; j < 16; j++) {
      bit_matrix(i, j) = bits[15 - j];
    }
  }
  return bit_matrix;
}
