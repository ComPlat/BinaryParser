import numpy as np
import struct


def read_doubles(filepath, offset=0):
    """
    Reads the entire file (from offset) as doubles (little-endian assumed).
    """
    with open(filepath, "rb") as f:
        f.seek(0, 2)
        size_bytes = f.tell() - offset
        f.seek(offset)
        n = size_bytes // 8
        data = np.fromfile(f, dtype="<f8", count=n)
    return data


def read_floats(filepath, offset=0):
    """
    Reads the entire file (from offset) as floats (little-endian).
    """
    with open(filepath, "rb") as f:
        f.seek(0, 2)
        size_bytes = f.tell() - offset
        f.seek(offset)
        n = size_bytes // 4
        data = np.fromfile(f, dtype="<f4", count=n)
    return data


def read_int32(filepath, offset=0, count=1000):
    """
    Reads count int32 values with endian swap.
    """
    out = np.zeros(count, dtype=np.int32)
    with open(filepath, "rb") as f:
        f.seek(offset)
        raw = f.read(count * 4)

    for i in range(count):
        (v,) = struct.unpack(">i", raw[i*4:i*4+4])  # big-endian -> int32
        out[i] = v
    return out


def read_chars(filepath):
    """
    Print file content in a human-readable char + hex format.
    Matching original behavior for debugging.
    """
    with open(filepath, "rb") as f:
        buf = f.read()

    width = 8
    addr = 0

    for i in range(0, len(buf), width):
        chunk = buf[i:i+width]

        # address line (hex)
        print(" ".join(f"{addr+j:03x}" for j in range(len(chunk))))
        addr += len(chunk)

        # character view
        print(" ".join(f"'{chr(c)}'" for c in chunk))
