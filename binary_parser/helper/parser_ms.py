import struct
import numpy as np


def _read_file(path):
    with open(path, "rb") as f:
        return f.read()


def _u16_be(buf, offset):
    return struct.unpack(">H", buf[offset:offset + 2])[0]


def _u32_be(buf, offset):
    return struct.unpack(">I", buf[offset:offset + 4])[0]


def _find_number_of_cycles(buf):
    return _u32_be(buf, 0x116)


def _find_data_start(buf):
    offset_correction = _u16_be(buf, 0x10A)
    return offset_correction * 2 - 2


def _convert_mz_intensity(data_u16):
    n = len(data_u16)
    n -= n % 2
    mz = np.zeros(n // 2, dtype=float)
    intensity = np.zeros(n // 2, dtype=float)

    for i in range(n):
        if (i & 1) == 0:
            # MZ
            mz[i >> 1] = data_u16[i] / 20.0
        else:
            # Intensity encoding: head = bits 14-15, tail = bits 0-13
            head = data_u16[i] >> 14
            tail = data_u16[i] & 0x3FFF
            intensity[i >> 1] = (8 ** head) * tail

    return mz, intensity


def _read_cycle(buf, start, cycle_size):
    data_u16 = []
    for i in range(cycle_size*2):
        data_u16.append(_u16_be(buf, start + i * 2))
    return _convert_mz_intensity(data_u16)


def read_cycles(path):
    buf = _read_file(path)
    data_start = _find_data_start(buf)
    num_cycles = _find_number_of_cycles(buf)

    cycles = []
    counter = data_start

    for _ in range(num_cycles):
        if counter >= len(buf):
            raise ValueError("Error extracting data")

        counter += 2  # skip?
        time = _u32_be(buf, counter)
        counter += 10

        cycle_size = _u16_be(buf, counter)
        counter += 6

        mz, intensity = _read_cycle(buf, counter, cycle_size)

        rt = time / 60000.0

        counter += cycle_size * 4
        counter += 10

        cycles.append({
            "mz": mz,
            "intensity": intensity,
            "retention_time": rt,
        })

    return cycles
