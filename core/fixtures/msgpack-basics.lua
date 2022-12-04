assert(msgpack.unpack(string.char(0xC0)) == nil)
assert(msgpack.unpack(string.char(0xC2)) == false)
assert(msgpack.unpack(string.char(0xC3)) == true)
assert(msgpack.unpack(string.char(15)) == 15)
assert(msgpack.unpack(string.char(0xCC, 0xFF)) == 0xFF)
assert(msgpack.unpack(string.char(0xD9, 0x01, 65)) == "A")

return true
