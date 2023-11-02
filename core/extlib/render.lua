#lang lua

-- LineChart -----------------------------------------------------------

local LineChart = Class {
    name = "LineChart",
    constructor = function(xlabel, ylabel)
        return {
            __type = "LineChart",
            xlabel = xlabel or "x axis",
            ylabel = ylabel or "y axis",
            xs = {},
            ys = {},
        }
    end
}

function LineChart:clear()
    self.xs = {}
    self.ys = {}
    return self
end

function LineChart:push(x, y)
    table.insert(self.xs, x)
    table.insert(self.ys, y)
    return self
end

function LineChart:setxs(xs)
    self.xs = xs
    return self
end

function LineChart:setys(ys)
    self.ys = ys
    return self
end

function LineChart:__tostring()
    return string.format(
        "<LineChart xlabel=%q ylabel=%q>",
        self.xlabel, self.ylabel
    )
end

return {
    LineChart = LineChart
}
