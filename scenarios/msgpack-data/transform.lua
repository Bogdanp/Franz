local script = {}

function script.transform(record)
  local data = msgpack.unpack(record.value)
  record.value = data.string
  return record
end

return script