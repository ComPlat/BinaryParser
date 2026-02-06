import struct
import numpy as np


def _u16_be_to_host(v):
    return struct.unpack(">H", struct.pack(">H", v))[0]


def _u32_be_to_host(v):
    return struct.unpack(">I", struct.pack(">I", v))[0]


def _i16_be(stream):
    data = stream.read(2)
    if not data:
        raise EOFError
    return struct.unpack(">h", data)[0]


def _i32_be(stream):
    data = stream.read(4)
    if not data:
        raise EOFError
    return struct.unpack(">i", data)[0]


def _u32_be(stream):
    data = stream.read(4)
    if not data:
        raise EOFError
    return struct.unpack(">I", data)[0]


def readInt(filepath, offset):
    with open(filepath, "rb") as f:
        f.seek(offset)
        data = f.read(4)
        return struct.unpack("<i", data)[0]   # original code was host endian


def readDouble(filepath, offset):
    with open(filepath, "rb") as f:
        f.seek(offset)
        data = f.read(8)
        val = struct.unpack(">d", data)[0]   # inverted endian
        return val


def readUint8(filepath, offset):
    with open(filepath, "rb") as f:
        f.seek(offset)
        out = f.read(40)
        return [chr(b) for b in out]


def readTime(filepath, offset):
    out = np.zeros(2, dtype=float)
    with open(filepath, "rb") as f:
        f.seek(offset)
        for i in range(2):
            raw = _i32_be(f)
            out[i] = raw / 60000.0
    return out


def DeltaCompression(filepath, offset, n=None):
    with open(filepath, "rb") as f:
        f.seek(offset)
        res = []
        prev = 0

        while True:
            try:
                header = _i16_be(f)
            except EOFError:
                break

            # C++: if (buffer1 << 12 == 0)
            if (header << 12) == 0:
                break

            count = header & 4095

            for _ in range(count):
                try:
                    delta = _i16_be(f)
                except EOFError:
                    break

                if delta != -32768:
                    prev += delta
                    res.append(prev)
                else:
                    prev = _i32_be(f)
                    res.append(prev)

        return np.array(res, dtype=np.int32)


class UVClass:
    """
    Python-port of UVClass in pybind11 module
    """

    def __init__(self, filepath):
        self.filepath = filepath

        # read nscans
        nscans = _read_int32_be(filepath, 0x116)

        self.time = np.zeros(nscans, dtype=float)
        self.wavelengths = []
        self.ndata = []

        with open(filepath, "rb") as f:
            offset = 0x1002
            prev_buffer7 = 0

            for scan in range(nscans):
                f.seek(offset)

                # buffer1
                size = struct.unpack("<H", f.read(2))[0]
                offset += size

                # buffer2
                time_raw = struct.unpack("<I", f.read(4))[0]
                self.time[scan] = time_raw / 60000.0

                # buffer3, buffer4, buffer5
                wstart = struct.unpack("<H", f.read(2))[0]
                wstop = struct.unpack("<H", f.read(2))[0]
                wstep = struct.unpack("<H", f.read(2))[0]

                # build wavelength list
                wl = []
                for wv in range(wstart, wstop, wstep):
                    wl.append(wv / 20.0)

                for w in wl:
                    if w not in self.wavelengths:
                        self.wavelengths.append(w)

                # rotate ordering like original C++
                max_idx = np.argmax(self.wavelengths)
                wv_map = list(range(max_idx + 1, len(self.wavelengths))) + list(range(0, max_idx + 1))

                row = np.zeros(len(self.wavelengths))

                prev_buffer7 = 0
                for j in wv_map:
                    delta = struct.unpack("<h", f.read(2))[0]
                    if delta == -32768:
                        prev_buffer7 = struct.unpack("<i", f.read(4))[0]
                    else:
                        prev_buffer7 += delta
                    row[j] = prev_buffer7

                self.ndata.append(row)

        self.ndata = np.array(self.ndata, dtype=float)


    def getTime(self):
        return self.time

    def getWavelengths(self):
        return np.array(self.wavelengths, dtype=float)

    def getData(self):
        return self.ndata


# helper missing from above:
def _read_int32_be(filepath, offset):
    with open(filepath, "rb") as f:
        f.seek(offset)
        data = f.read(4)
        return struct.unpack(">i", data)[0]
