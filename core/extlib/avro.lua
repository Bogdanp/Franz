#lang lua

local make_codec = _ENV["#%avro-make-codec"]
local codec_read = _ENV["#%avro-codec-read"]
local avro_tolua = _ENV["#%avro-tolua"]

local bytes_to_string = racket["bytes->string/utf-8"]
local call_with_input_bytes = racket.lib("racket/port", "call-with-input-bytes")

local Codec = {}
Codec.__index = Codec

function Codec.parse(schema)
    if type(schema) ~= "string" then
        error("avro.parse: bad argument #1; expected a string")
    end

    local o = {}
    o._c = make_codec(bytes_to_string(schema))
    setmetatable(o, Codec)
    return o
end

function Codec:read(str)
    local function impl(inp)
        return codec_read(self._c, inp)
    end
    return avro_tolua(call_with_input_bytes(str, impl))
end

local avro = {}

function avro.parse(schema)
    return Codec.parse(schema)
end

return avro
