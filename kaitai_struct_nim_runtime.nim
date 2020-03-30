import streams, endians, sequtils, bitops, strutils, strformat, options

type
  KaitaiStream* = ref object
    io*: Stream
    bits*: uint64
    bitsLeft*: int
  KaitaiError* = object of Exception

converter toString(bytes: seq[byte]): string =
  result = newStringOfCap(len(bytes))
  for b in bytes:
    add(result, char(b))
converter toString(str: seq[char]): string =
  result = newStringOfCap(len(str))
  for c in str:
    add(result, c)
converter toIntOption*(n: Option[uint8]): Option[int] = some(get(n).int)
converter toUint8Option*(n: Option[int]): Option[uint8] = some(get(n).uint8)
converter toInt8Option*(n: Option[int]): Option[int8] = some(get(n).int8)

proc newKaitaiFileStream*(f: File): owned KaitaiStream =
  KaitaiStream(io: newFileStream(f), bits: 0, bitsLeft: 0)
proc newKaitaiFileStream*(filename: string): owned KaitaiStream =
  KaitaiStream(io: newFileStream(filename), bits: 0, bitsLeft: 0)
proc newKaitaiStringStream*(data: string): owned KaitaiStream =
  KaitaiStream(io: newStringStream(data), bits: 0, bitsLeft: 0)

# Stream positioning
proc close*(ks: KaitaiStream) = close(ks.io)
proc eof*(ks: KaitaiStream): bool = atEnd(ks.io)
proc seek*(ks: KaitaiStream, n: int) = setPosition(ks.io, n)
proc pos*(ks: KaitaiStream): int = getPosition(ks.io)
proc skip*(ks: KaitaiStream, n: int) = ks.seek(pos(ks) + n)
proc size*(ks: KaitaiStream): int =
  let p = getPosition(ks.io)
  result = readAll(ks.io).len
  setPosition(ks.io, p)

# Signed integer numbers
proc readS1*(ks: KaitaiStream): int8 = readInt8(ks.io)

when system.cpuEndian == bigEndian:
  proc readS2Be*(ks: KaitaiStream): int16 = readInt16(ks.io)
  proc readS4Be*(ks: KaitaiStream): int32 = readInt32(ks.io)
  proc readS8Be*(ks: KaitaiStream): int64 = readInt64(ks.io)

  proc readS2Le*(ks: KaitaiStream): int16 =
    var
      bufferIn: array[2, byte]
      bufferOut: array[2, byte]
    doAssert ks.io.readData(addr(bufferIn), 2) == 2
    swapEndian16(addr(bufferOut), addr(bufferIn))
    result = cast[int16](bufferOut)

  proc readS4Le*(ks: KaitaiStream): int32 =
    var
      bufferIn: array[4, byte]
      bufferOut: array[4, byte]
    doAssert ks.io.readData(addr(bufferIn), 4) == 4
    swapEndian32(addr(bufferOut), addr(bufferIn))
    result = cast[int32](bufferOut)

  proc readS8Le*(ks: KaitaiStream): int64 =
    var
      bufferIn: array[8, byte]
      bufferOut: array[8, byte]
    doAssert ks.io.readData(addr(bufferIn), 8) == 8
    swapEndian64(addr(bufferOut), addr(bufferIn))
    result = cast[int64](bufferOut)
else:
  proc readS2Be*(ks: KaitaiStream): int16 =
    var
      bufferIn: array[2, byte]
      bufferOut: array[2, byte]
    doAssert ks.io.readData(addr(bufferIn), 2) == 2
    swapEndian16(addr(bufferOut), addr(bufferIn))
    result = cast[int16](bufferOut)

  proc readS4Be*(ks: KaitaiStream): int32 =
    var
      bufferIn: array[4, byte]
      bufferOut: array[4, byte]
    doAssert ks.io.readData(addr(bufferIn), 4) == 4
    swapEndian32(addr(bufferOut), addr(bufferIn))
    result = cast[int32](bufferOut)

  proc readS8Be*(ks: KaitaiStream): int64 =
    var
      bufferIn: array[8, byte]
      bufferOut: array[8, byte]
    doAssert ks.io.readData(addr(bufferIn), 8) == 8
    swapEndian64(addr(bufferOut), addr(bufferIn))
    result = cast[int64](bufferOut)

  proc readS2Le*(ks: KaitaiStream): int16 = readInt16(ks.io)
  proc readS4Le*(ks: KaitaiStream): int32 = readInt32(ks.io)
  proc readS8Le*(ks: KaitaiStream): int64 = readInt64(ks.io)

# Unsigned integer numbers
proc readU1*(ks: KaitaiStream): uint8 = readUint8(ks.io)

when system.cpuEndian == bigEndian:
  proc readU2Be*(ks: KaitaiStream): uint16 = readUint16(ks.io)
  proc readU4Be*(ks: KaitaiStream): uint32 = readUint32(ks.io)
  proc readU8Be*(ks: KaitaiStream): uint64 = readUint64(ks.io)

  proc readU2Le*(ks: KaitaiStream): uint16 =
    var
      bufferIn: array[2, byte]
      bufferOut: array[2, byte]
    doAssert ks.io.readData(addr(bufferIn), 2) == 2
    swapEndian16(addr(bufferOut), addr(bufferIn))
    result = cast[uint16](bufferOut)

  proc readU4Le*(ks: KaitaiStream): uint32 =
    var
      bufferIn: array[4, byte]
      bufferOut: array[4, byte]
    doAssert ks.io.readData(addr(bufferIn), 4) == 4
    swapEndian32(addr(bufferOut), addr(bufferIn))
    result = cast[uint32](bufferOut)

  proc readU8Le*(ks: KaitaiStream): uint64 =
    var
      bufferIn: array[8, byte]
      bufferOut: array[8, byte]
    doAssert ks.io.readData(addr(bufferIn), 8) == 8
    swapEndian64(addr(bufferOut), addr(bufferIn))
    result = cast[uint64](bufferOut)
else:
  proc readU2Be*(ks: KaitaiStream): uint16 =
    var
      bufferIn: array[2, byte]
      bufferOut: array[2, byte]
    doAssert ks.io.readData(addr(bufferIn), 2) == 2
    swapEndian16(addr(bufferOut), addr(bufferIn))
    result = cast[uint16](bufferOut)

  proc readU4Be*(ks: KaitaiStream): uint32 =
    var
      bufferIn: array[4, byte]
      bufferOut: array[4, byte]
    doAssert ks.io.readData(addr(bufferIn), 4) == 4
    swapEndian32(addr(bufferOut), addr(bufferIn))
    result = cast[uint32](bufferOut)

  proc readU8Be*(ks: KaitaiStream): uint64 =
    var
      bufferIn: array[8, byte]
      bufferOut: array[8, byte]
    doAssert ks.io.readData(addr(bufferIn), 8) == 8
    swapEndian64(addr(bufferOut), addr(bufferIn))
    result = cast[uint64](bufferOut)

  proc readU2Le*(ks: KaitaiStream): uint16 = readUint16(ks.io)
  proc readU4Le*(ks: KaitaiStream): uint32 = readUint32(ks.io)
  proc readU8Le*(ks: KaitaiStream): uint64 = readUint64(ks.io)

# Floating point numbers
when system.cpuEndian == bigEndian:
  proc readF4Be*(ks: KaitaiStream): float32 = readFloat32(ks.io)
  proc readF8Be*(ks: KaitaiStream): float64 = readFloat64(ks.io)

  proc readF4Le*(ks: KaitaiStream): float32 =
    var
      bufferIn: array[4, byte]
      bufferOut: array[4, byte]
    doAssert ks.io.readData(addr(bufferIn), 4) == 4
    swapEndian32(addr(bufferOut), addr(bufferIn))
    result = cast[float32](bufferOut)

  proc readF8Le*(ks: KaitaiStream): float64 =
    var
      bufferIn: array[8, byte]
      bufferOut: array[8, byte]
    doAssert ks.io.readData(addr(bufferIn), 8) == 8
    swapEndian64(addr(bufferOut), addr(bufferIn))
    result = cast[float64](bufferOut)
else:
  proc readF4Be*(ks: KaitaiStream): float32 =
    var
      bufferIn: array[4, byte]
      bufferOut: array[4, byte]
    doAssert ks.io.readData(addr(bufferIn), 4) == 4
    swapEndian32(addr(bufferOut), addr(bufferIn))
    result = cast[float32](bufferOut)

  proc readF8Be*(ks: KaitaiStream): float64 =
    var
      bufferIn: array[8, byte]
      bufferOut: array[8, byte]
    doAssert ks.io.readData(addr(bufferIn), 8) == 8
    swapEndian64(addr(bufferOut), addr(bufferIn))
    result = cast[float64](bufferOut)

  proc readF4Le*(ks: KaitaiStream): float32 = readFloat32(ks.io)
  proc readF8Le*(ks: KaitaiStream): float64 = readFloat64(ks.io)

# Unaligned bit values
proc align_to_byte*(ks: KaitaiStream) =
  ks.bits = 0
  ks.bitsLeft = 0

proc readBitsInt*(ks: KaitaiStream, n: int): uint64 =
  proc getMaskOnes(n: int): uint64 =
    if n == 64: 0xFFFFFFFFFFFFFFFF'u64
    else: (1'u64 shl n) - 1
  let bitsNeeded = n - ks.bitsLeft
  if bitsNeeded > 0:
    var bytesNeeded = ((bitsNeeded - 1) div 8) + 1;
    var buf: array[8, byte]
    doAssert ks.io.readData(addr(buf), bytesNeeded) == bytesNeeded
    for i in 0..<bytesNeeded:
      ks.bits = ks.bits shl 8
      ks.bits = ks.bits or buf[i]
      inc(ks.bitsLeft, 8)
  let
    shiftBits = ks.bitsLeft - n
    mask = getMaskOnes(n) shl shiftBits
  result = (ks.bits and mask) shr shiftBits
  dec(ks.bitsLeft, n)
  ks.bits = ks.bits and getMaskOnes(ks.bitsLeft)

# XXX: proc readBitsArray*(ks: KaitaiStream, n: int): string =

# Byte arrays
proc readBytes*(ks: KaitaiStream, n: int): string =
  result = newString(n)
  doAssert ks.io.readData(addr(result[0]), n) == n

proc readBytesFull*(ks: KaitaiStream): string =
  const bufferSize = 1024
  var buffer {.noinit.}: array[bufferSize, char]
  while true:
    let bytesRead = ks.io.readData(addr(buffer[0]), bufferSize)
    if bytesRead == 0: break
    let prevLen = result.len
    result.setLen(prevLen + bytesRead)
    copyMem(addr(result[prevLen]), addr(buffer[0]), bytesRead)
    if bytesRead < bufferSize:
      break

proc readBytesTerm*(ks: KaitaiStream; term: byte;
                      includeTerm, consumeTerm: bool): string =
  while true:
    let c = readUint8(ks.io)
    if c == term:
      if includeTerm:
        result.add(term.char)
      if not consumeTerm:
        ks.io.setPosition(ks.io.getPosition - 1)
      break
    result.add(c.char)

proc ensureFixedContents*(ks: KaitaiStream, expected: string): string =
  result = ks.read_bytes(expected.len)
  if result != expected:
    raise newException(AssertionError, "the request to the OS failed")

proc bytesStripRight*(bytes: string, padByte: byte): string =
  var newLen = bytes.len
  while newLen > 0 and bytes[newLen - 1].byte == padByte: dec(newLen)
  result = bytes
  result.setLen(newLen)

proc bytesTerminate*(bytes: string, term: byte, includeTerm: bool): string =
  var newLen: int
  let maxLen = bytes.len
  while newLen < maxLen and bytes[newLen].byte != term: inc(newLen)
  if includeTerm and newLen < maxLen: inc(newLen)
  result = bytes
  result.setLen(newLen)

# XXX: proc bytesToStr(bytes: string, encoding: string): string =

# Byte array processing
proc processXor*(data: string, key: byte): string =
  result = data.mapIt(it.byte xor key)

proc processXor*(data, key: string): string =
  result = newString(data.len)
  var currKeyIdx: int
  for i in 0..<data.len:
    result[i] = char(data[i].byte xor key[currKeyIdx].byte)
    inc currKeyIdx
    if currKeyIdx >= key.len: currKeyIdx = 0

proc processRotateLeft*(data: string, amount: int): string =
  result = data.mapIt(rotateLeftBits(it.byte, amount.byte))

# XXX: proc process_zlib(data: string): string =

proc parseInt*(s: string, radix: int): int {.raises: [ValueError].} =
  case radix
  of 2: parseBinInt(s)
  of 8: parseOctInt(s)
  of 10: parseInt(s)
  of 16: parseHexInt(s)
  else:
    raise newException(ValueError,
      fmt"base {radix} is not supported; use base 2, 8, 10 or 16")

proc `%%%`*[T, U: SomeInteger](a: T, b: U): U =
  if a >= T(0):
    result = a.U mod b;
  else:
    let x = if b >= U(0): b else: -b
    result = x - 1 + U(a + 1) mod b;
