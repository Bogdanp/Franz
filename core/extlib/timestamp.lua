#lang lua

local seconds_to_date = racket["seconds->date"]
local date_to_seconds = racket.lib("racket/date", "date*->seconds")
local date_to_string = _ENV["#%date->isostring"]
local string_to_date = _ENV["#%isostring->date"]

local Timestamp = Class {
    name = "Timestamp",
    constructor = function(ts, localtime)
        if ts ~= nil and type(ts) ~= "number" then
            error(string.format("Timestamp: arg #1 must be a number, but got %s", type(v)))
        end
        if localtime == nil then
            localtime = true
        end
        return {
            __type = "Timestamp",
            ts = ts or os.time(),
            localtime = localtime
        }
    end
}

function Timestamp.at(year, month, day, hour, minute, second, localtime)
    if localtime == nil then
        localtime = true
    end
    for i, arg in ipairs({ year, month, day, hour, minute, second }) do
        if arg == nil then
            break
        end
        if type(arg) ~= "number" then
            error(string.format("Timestamp.at: arg #%d must be a number, but got %s", i, type(arg)))
        end
    end
    local seconds = os.time({
            year  = year,
            month = month,
            day   = day,
            hour  = hour,
            min   = minute,
            sec   = second,
            isutc = not localtime
    })
    return Timestamp(seconds, localtime)
end

function Timestamp.fromisostring(s)
    local d = string_to_date(s or "")
    if not d then
        error(string.format("Timestamp.fromisostring: invalid date '%s'", s))
    end
    return Timestamp(date_to_seconds(d, false))
end

function Timestamp:components()
    return os.date(self.localtime and "*t" or "!*t", self.ts)
end

function Timestamp:isostring()
    return date_to_string(seconds_to_date(self.ts, self.localtime))
end

function Timestamp:tolocal()
    if self.localtime then
        return self
    end
    return Timestamp(self.ts)
end

function Timestamp:toutc()
    if not self.localtime then
        return self
    end
    return Timestamp(self.ts, false)
end

function Timestamp:__eq(other)
    return self.ts == other.ts
end

function Timestamp:__le(other)
    return self.ts <= other.ts
end

function Timestamp:__lt(other)
    return self.ts < other.ts
end

return Timestamp
