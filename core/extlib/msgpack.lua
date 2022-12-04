#lang lua

local read_msgpack  = _ENV["#%msgpack-read"]
local msgpack_tolua = _ENV["#%msgpack-tolua"]

local call_with_input_bytes = racket.lib("racket/port", "call-with-input-bytes")

local msgpack = {}

function msgpack.unpack(str)
    return msgpack_tolua(call_with_input_bytes(str, read_msgpack))
end

return msgpack
