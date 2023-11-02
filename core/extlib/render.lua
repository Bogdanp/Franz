#lang lua

local function makeChartClass(name)
    local Chart = Class {
        name = name,
        constructor = function(xlabel, ylabel)
            return {
                __type = name,
                xlabel = xlabel or "x axis",
                ylabel = ylabel or "y axis",
                xs = {},
                ys = {},
            }
        end
    }

    function Chart:clear()
        self.xs = {}
        self.ys = {}
        return self
    end

    function Chart:push(x, y)
        table.insert(self.xs, x)
        table.insert(self.ys, y)
        return self
    end

    function Chart:setxs(xs)
        self.xs = xs
        return self
    end

    function Chart:setys(ys)
        self.ys = ys
        return self
    end

    function Chart:__tostring()
        return string.format(
            "<%s xlabel=%q ylabel=%q #xs=%d #ys=%d>",
            name, self.xlabel, self.ylabel, #self.xs, #self.ys
        )
    end

    return Chart
end

return {
    BarChart = makeChartClass("BarChart"),
    LineChart = makeChartClass("LineChart")
}
