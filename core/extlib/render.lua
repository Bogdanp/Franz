#lang lua

-- Chart ---------------------------------------------------------------

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

    function Chart:sort(cmp)
        cmp = cmp or function(a, b)
            return a.x < b.x
        end
        local ps = {}
        for i, x in ipairs(self.xs) do
            table.insert(ps, {x = x, y = self.ys[i] })
        end
        table.sort(ps, cmp)
        local xs = {}
        local ys = {}
        for i, p in ipairs(ps) do
            xs[i] = p.x
            ys[i] = p.y
        end
        self.xs = xs
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

-- Table ---------------------------------------------------------------

local Table = Class {
    name = "Table",
    constructor = function(columns, ...)
        return {
            __type = "Table",
            columns = columns,
            rows = table.pack(...)
        }
    end
}

return {
    BarChart = makeChartClass("BarChart"),
    LineChart = makeChartClass("LineChart"),
    Table = Table
}
