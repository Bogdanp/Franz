local nothing = kafka.parse_committed_offset({})
assert(nothing == nil)

local record_type, offset = kafka.parse_committed_offset(
    {
        key = string.char(0x00, 0x01, 0x00, 0x03, 65, 66, 67, 0x00, 0x03, 66, 67, 68, 0x00, 0x00, 0x00, 0x01),
        value = string.char(0x00, 0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x05)
    }
)
assert(record_type == "offset_commit")
assert(offset.group == "ABC", "wrong group")
assert(offset.topic == "BCD", "wrong topic")
assert(offset.partition_id == 1, "wrong partition id")
assert(offset.offset == 5, "wrong offset")
assert(tostring(offset) == "<OffsetCommit group=\"ABC\" topic=\"BCD\" partition_id=1 offset=5>")

return true
