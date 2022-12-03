local Person = [[
{
  "name": "Person",
  "type": "record",
  "fields": [
    {"type": "string", "name": "Name"},
    {"type": "int", "name": "Age"}
  ]
}
]]

local codec = avro.parse(Person)
local record = codec:read(string.char(12, 66, 111, 103, 100, 97, 110, 60))
assert(record.Name == "Bogdan")
assert(record.Age == 30)


local Array = [[
{
  "type": "array",
  "items": "string"
}
]]
local codec = avro.parse(Array)
local strings = codec:read(string.char(4, 2, 65, 2, 66, 0))
assert(strings[1] == "A")
assert(strings[2] == "B")


local Union = json.encode(
    {
        type = "record",
        name = "Test",
        fields = {
            {name = "Name", type = {"null", "string"}}
        }
    }
)
local codec = avro.parse(Union)
local data = codec:read(string.char(0))
assert(data.Name == nil)
local data = codec:read(string.char(2, 2, 65))
assert(data.Name.type == "string")
assert(data.Name.value == "A")

return true
