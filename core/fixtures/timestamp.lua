local t = Timestamp.at(2023, 5, 29, 12, 30, 45, false)
assert(t:isostring() == "2023-05-29T12:30:45")

local now = Timestamp()
assert(t < now)

local components = t:components()
assert(components.year == 2023, "year component")
assert(components.month == 5, "month component")
assert(components.day == 29, "day component")
assert(components.hour == 12, "hour component")
assert(components.min == 30, "minute component")
assert(components.sec == 45, "second component")
assert(not components.isdst, "isdst component")

assert(t == Timestamp.fromisostring("2023-05-29T12:30:45Z"))

return true
