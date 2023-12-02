local script = {}
local schema = [[
{
  "type": "record",
  "name": "Person",
  "fields": [
    {"type": "string", "name": "Name"},
    {"type": "int", "name": "Age"}
  ]
}
]]

local codec = avro.parse(schema)

function script.transform(record)
  local person = codec:read(record.value)
  record.value = person.Name .. " (" .. person.Age .. ")"
  return record
end

return script