#lang lua

local LineChart = {}
LineChart.__index = LineChart

function LineChart.make(xlabel, ylabel)
    local ob = {
        __type = "LineChart",
        xlabel = xlabel or "x axis",
        ylabel = ylabel or "y axis",
        xs = {},
        ys = {},
    }
    setmetatable(ob, LineChart)
    return ob
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
        "<LineChart xlabel=%q ylabel=%q xs=%q ys=%q>",
        self.xlabel, self.ylabel, self.xs, self.ys
    )
end

return {
    LineChart = LineChart
}
