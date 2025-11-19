import
  streams, endians, sequtils, bitops, strutils, strformat, options, encodings

type
  KaitaiStruct* {.inheritable.} = ref object
    io*: KaitaiStream
    root*: KaitaiStruct
  KaitaiStream* = ref object
    io*: Stream
    bitsLeft*: int
    bits*: uint64
  KaitaiError* = object of Exception

proc toString(bytes: seq[byte]): string =
  result = newStringOfCap(len(bytes))
  for b in bytes:
    add(result, char(b))

converter toOption*[T: not Option](x: T): Option[T] = some(x)

proc `<=`*(x, y: seq[byte]): bool = x.toString <= y.toString
proc `<`*(x, y: seq[byte]): bool = x.toString <= y.toString
proc `-`*(n: byte): byte = byte(255'u8 - n + 1)
proc `%%%`*[T, U: SomeInteger](a: T, b: U): U =
  if a >= T(0):
    result = a.U mod b;
  else:
    let x = if b >= U(0): b else: -b
    result = x - 1 + U(a + 1) mod b;

proc newKaitaiFileStream*(f: File): KaitaiStream =
  KaitaiStream(io: newFileStream(f), bitsLeft: 0, bits: 0)
proc newKaitaiFileStream*(filename: string): KaitaiStream =
  KaitaiStream(io: newFileStream(filename), bitsLeft: 0, bits: 0)
proc newKaitaiStream*(data: seq[byte]): KaitaiStream =
  KaitaiStream(io: newStringStream(toString(data)), bitsLeft: 0, bits: 0)
proc newKaitaiStream*(data: seq[seq[byte]]): KaitaiStream =
  KaitaiStream(io: newStringStream(data.mapIt(it.toString).join("")),
               bitsLeft: 0, bits: 0)

# NOTE: alignToByte() must be defined here at the top because in Nim, you can
# only call methods that were defined earlier (i.e. you cannot call methods
# defined later in the file).
proc alignToByte*(ks: KaitaiStream) =
  ks.bitsLeft = 0
  ks.bits = 0

# Stream positioning
proc close*(ks: KaitaiStream) = close(ks.io)
proc isEof*(ks: KaitaiStream): bool = atEnd(ks.io) and ks.bitsLeft == 0
proc seek*(ks: KaitaiStream, n: int) =
  ks.alignToByte()
  setPosition(ks.io, n)
proc pos*(ks: KaitaiStream): int = getPosition(ks.io)
proc skip*(ks: KaitaiStream, n: int) = ks.seek(pos(ks) + n)
proc size*(ks: KaitaiStream): int =
  let p = getPosition(ks.io)
  result = readAll(ks.io).len + p
  setPosition(ks.io, p)

# Signed integer numbers
proc readS1*(ks: KaitaiStream): int8 =
  ks.alignToByte()
  readInt8(ks.io)

proc readS2be*(ks: KaitaiStream): int16 =
  ks.alignToByte()
  var native = readInt16(ks.io)
  bigEndian16(addr result, addr native)

proc readS4be*(ks: KaitaiStream): int32 =
  ks.alignToByte()
  var native = readInt32(ks.io)
  bigEndian32(addr result, addr native)

proc readS8be*(ks: KaitaiStream): int64 =
  ks.alignToByte()
  var native = readInt64(ks.io)
  bigEndian64(addr result, addr native)

proc readS2le*(ks: KaitaiStream): int16 =
  ks.alignToByte()
  var native = readInt16(ks.io)
  littleEndian16(addr result, addr native)

proc readS4le*(ks: KaitaiStream): int32 =
  ks.alignToByte()
  var native = readInt32(ks.io)
  littleEndian32(addr result, addr native)

proc readS8le*(ks: KaitaiStream): int64 =
  ks.alignToByte()
  var native = readInt64(ks.io)
  littleEndian64(addr result, addr native)

# Unsigned integer numbers
proc readU1*(ks: KaitaiStream): uint8 =
  ks.alignToByte()
  readUint8(ks.io)

proc readU2be*(ks: KaitaiStream): uint16 =
  ks.alignToByte()
  var native = readUint16(ks.io)
  bigEndian16(addr result, addr native)

proc readU4be*(ks: KaitaiStream): uint32 =
  ks.alignToByte()
  var native = readUint32(ks.io)
  bigEndian32(addr result, addr native)

proc readU8be*(ks: KaitaiStream): uint64 =
  ks.alignToByte()
  var native = readUint64(ks.io)
  bigEndian64(addr result, addr native)

proc readU2le*(ks: KaitaiStream): uint16 =
  ks.alignToByte()
  var native = readUint16(ks.io)
  littleEndian16(addr result, addr native)

proc readU4le*(ks: KaitaiStream): uint32 =
  ks.alignToByte()
  var native = readUint32(ks.io)
  littleEndian32(addr result, addr native)

proc readU8le*(ks: KaitaiStream): uint64 =
  ks.alignToByte()
  var native = readUint64(ks.io)
  littleEndian64(addr result, addr native)

# Floating point numbers
proc readF4be*(ks: KaitaiStream): float32 =
  ks.alignToByte()
  var native = readFloat32(ks.io)
  bigEndian32(addr result, addr native)

proc readF8be*(ks: KaitaiStream): float64 =
  ks.alignToByte()
  var native = readFloat64(ks.io)
  bigEndian64(addr result, addr native)

proc readF4le*(ks: KaitaiStream): float32 =
  ks.alignToByte()
  var native = readFloat32(ks.io)
  littleEndian32(addr result, addr native)

proc readF8le*(ks: KaitaiStream): float64 =
  ks.alignToByte()
  var native = readFloat64(ks.io)
  littleEndian64(addr result, addr native)

# Unaligned bit values
proc readBitsIntBe*(ks: KaitaiStream, n: int): uint64 =
  result = 0

  let bitsNeeded = n - ks.bitsLeft
  ks.bitsLeft = -bitsNeeded and 7 # `-bitsNeeded mod 8`

  if bitsNeeded > 0:
    # 1 bit  => 1 byte
    # 8 bits => 1 byte
    # 9 bits => 2 bytes
    let bytesNeeded = ((bitsNeeded - 1) div 8) + 1 # `ceil(bitsNeeded / 8)`
    doAssert bytesNeeded <= 8, "readBitsIntBe: more than 8 bytes requested"
    var buf: array[8, byte]
    doAssert ks.io.readData(addr(buf), bytesNeeded) == bytesNeeded
    for i in 0..<bytesNeeded:
      result = (result shl 8) or buf[i]

    let newBits = result
    result = (result shr ks.bitsLeft) or (if bitsNeeded < 64: ks.bits shl bitsNeeded else: 0) # avoid undefined behavior of `(x shl 64)`
    ks.bits = newBits # will be masked at the end of the function
  else:
    result = ks.bits shr -bitsNeeded # shift unneeded bits out

  let mask = (1'u64 shl ks.bitsLeft) - 1 # `bitsLeft` is in range 0..7, so `(1'u64 shl 64)` does not have to be considered
  ks.bits = ks.bits and mask

proc readBitsInt*(ks: KaitaiStream, n: int): uint64 {.deprecated: "use readBitsIntBe instead".} =
  ks.readBitsIntBe(n)

proc readBitsIntLe*(ks: KaitaiStream, n: int): uint64 =
  result = 0
  let bitsNeeded = n - ks.bitsLeft

  if bitsNeeded > 0:
    # 1 bit  => 1 byte
    # 8 bits => 1 byte
    # 9 bits => 2 bytes
    let bytesNeeded = ((bitsNeeded - 1) div 8) + 1 # `ceil(bitsNeeded / 8)`
    doAssert bytesNeeded <= 8, "readBitsIntLe: more than 8 bytes requested"
    var buf: array[8, byte]
    doAssert ks.io.readData(addr(buf), bytesNeeded) == bytesNeeded
    for i in 0..<bytesNeeded:
      result = result or (uint64(buf[i]) shl (i * 8))

    # NB: in Nim, using the `shl` and `shr` operators to shift by more than 64 bits
    # is undefined behavior (see https://github.com/nim-lang/Nim/pull/11555).
    # So we define our desired behavior here.
    let newBits = if bitsNeeded < 64: result shr bitsNeeded else: 0
    result = (result shl ks.bitsLeft) or ks.bits
    ks.bits = newBits
  else:
    result = ks.bits
    ks.bits = ks.bits shr n

  ks.bitsLeft = -bitsNeeded and 7 # `-bitsNeeded mod 8`

  if n < 64:
    let mask = (1'u64 shl n) - 1
    result = result and mask
  # if `n == 64`, do nothing (avoids undefined behavior of `(1'u64 shl 64)`)

# XXX: proc readBitsArray*(ks: KaitaiStream, n: int): string =

# Byte arrays
proc readBytes*(ks: KaitaiStream, n: int): seq[byte] =
  ks.alignToByte()
  if n == 0: return
  result = newSeq[byte](n)
  doAssert ks.io.readData(addr(result[0]), n) == n

proc readBytesFull*(ks: KaitaiStream): seq[byte] =
  ks.alignToByte()
  const bufferSize = 1024
  var buffer {.noinit.}: array[bufferSize, byte]
  while true:
    let bytesRead = ks.io.readData(addr(buffer[0]), bufferSize)
    if bytesRead == 0: break
    let prevLen = result.len
    result.setLen(prevLen + bytesRead)
    copyMem(addr(result[prevLen]), addr(buffer[0]), bytesRead)
    if bytesRead < bufferSize:
      break

proc readBytesTerm*(ks: KaitaiStream; term: byte;
                    includeTerm, consumeTerm, eosError: bool): seq[byte] =
  ks.alignToByte()
  while true:
    let c = readUint8(ks.io)
    if c == term:
      if includeTerm:
        result.add(term)
      if not consumeTerm:
        ks.io.setPosition(ks.io.getPosition - 1)
      break
    result.add(c)

proc readBytesTermMulti*(ks: KaitaiStream; term: seq[byte];
                         includeTerm, consumeTerm, eosError: bool): seq[byte] =
  ks.alignToByte()
  let unitSize = term.len
  var c = newSeq[byte](unitSize)
  while true:
    let numRead = ks.io.readData(addr(c[0]), unitSize)
    if numRead < unitSize:
      if eosError:
        raise newException(IOError, "end of stream reached, but no terminator found")
      result.add(c[0 ..< numRead])
      break

    if c == term:
      if includeTerm:
        result.add(c)
      if not consumeTerm:
        ks.io.setPosition(ks.io.getPosition - unitSize)
      break

    result.add(c)

proc ensureFixedContents*(ks: KaitaiStream, expected: seq[byte]): seq[byte] =
  result = ks.read_bytes(expected.len)
  if result != expected:
    raise newException(AssertionError, "the request to the OS failed")

proc bytesStripRight*(bytes: seq[byte], padByte: byte): seq[byte] =
  var newLen = bytes.len
  while newLen > 0 and bytes[newLen - 1] == padByte: dec(newLen)
  result = bytes
  result.setLen(newLen)

proc bytesTerminate*(bytes: seq[byte], term: byte, includeTerm: bool): seq[byte] =
  var newLen: int
  let maxLen = bytes.len
  while newLen < maxLen and bytes[newLen] != term: inc(newLen)
  if includeTerm and newLen < maxLen: inc(newLen)
  result = bytes
  result.setLen(newLen)

proc bytesTerminateMulti*(bytes: seq[byte], term: seq[byte], includeTerm: bool): seq[byte] =
  let unitSize = term.len
  if unitSize == 0:
    return @[]
  let len = bytes.len
  var iTerm = 0
  var iBytes = 0
  while iBytes < len:
    if bytes[iBytes] != term[iTerm]:
      iBytes += unitSize - iTerm
      iTerm = 0
      continue

    inc(iBytes)
    inc(iTerm)
    if iTerm == unitSize:
      return bytes[0 ..< iBytes - (if includeTerm: 0 else: unitSize)]

  return bytes

# XXX: proc bytesToStr(bytes: seq[byte], encoding: string): string =

proc encode*(src: seq[byte], encoding: string): string =
  when not defined(windows):
    convert(src.toString, srcEncoding = encoding)
  else:
    var encoding = encoding
    if cmpIgnoreCase(encoding, "ascii") == 0:
      encoding = "us-ascii"
    convert(src.toString, srcEncoding = encoding)

# Byte array processing
proc processXor*(data: seq[byte], key: byte): seq[byte] =
  result = data.mapIt(it xor key)

proc processXor*(data, key: seq[byte]): seq[byte] =
  result = newSeq[byte](data.len)
  var currKeyIdx: int
  for i in 0..<data.len:
    result[i] = data[i] xor key[currKeyIdx]
    inc currKeyIdx
    if currKeyIdx >= key.len: currKeyIdx = 0

proc processRotateLeft*(data: seq[byte], amount: int): seq[byte] =
  result = data.mapIt(rotateLeftBits(it, amount))

# XXX: proc process_zlib(data: seq[byte]): seq[byte] =

proc parseInt*(s: string, radix: int): int {.raises: [ValueError].} =
  case radix
  of 2: parseBinInt(s)
  of 8: parseOctInt(s)
  of 10: parseInt(s)
  of 16: parseHexInt(s)
  else:
    raise newException(ValueError,
      fmt"base {radix} is not supported; use base 2, 8, 10 or 16")
