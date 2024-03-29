#lang lua

local render = {}

local function check(who, v, typ, n)
    if type(v) ~= typ then
        if n == nil then
            error(string.format("%s: expected a %s, but got %s", who, typ, type(v)))
        else
            error(string.format("%s: arg #%d must be a %s, but got %s", who, n, typ, type(v)))
        end
    end
end

-- Chart ---------------------------------------------------------------

local function makeChartClass(name)
    local Chart = Class {
        name = name,
        constructor = function(xlabel, ylabel)
            return {
                __type = name,
                xlabel = xlabel or "x axis",
                ylabel = ylabel or "y axis",
                values = {},
            }
        end
    }

    function Chart:push(x, y)
        table.insert(self.values, { x = x; y = y })
        return self
    end

    function check_type(who, typ)
        if typ ~= nil and
            typ ~= "linear" and
            typ ~= "log" then
            error(who .. ": type must be 'linear' or 'log'")
        end
    end

    local function make_scale(who, lo, hi, typ)
        check(who, lo, "number", 1)
        check(who, hi, "number", 2)
        check_type(who, typ)
        return {
            lo = lo,
            hi = hi,
            typ = typ or "linear"
        }
    end

    function Chart:setvalues(...)
        self.values = {...}
        return self
    end

    function Chart:setxscale(...)
        check_type("Chart:setxscale", typ)
        self.xscale = make_scale("Chart:setxscale", ...)
        return self
    end

    function Chart:setyscale(...)
        check_type("Chart:setyscale", typ)
        self.yscale = make_scale("Chart:setyscale", ...)
        return self
    end

    function Chart:sort(cmp)
        cmp = cmp or function(a, b)
            return a.x < b.x
        end
        table.sort(self.values, cmp)
        return self
    end

    function Chart:__tostring()
        return string.format(
            "<%s xlabel=%q ylabel=%q #values=%d>",
            name, self.xlabel, self.ylabel, #self.values
        )
    end

    return Chart
end

render.AreaChart = makeChartClass("AreaChart")
render.BarChart = makeChartClass("BarChart")
render.CandlestickChart = makeChartClass("CandlestickChart")
render.LineChart = makeChartClass("LineChart")
render.ScatterChart = makeChartClass("ScatterChart")

function render.CandlestickChart:setwidth(w)
    check("CandlestickChart:setwidth", w, "number")
    self.candlestick_width = w
    return self
end

render.Candlestick = Class {
    name = "Candlestick",
    constructor = function(o, h, l, c)
        check("Candlestick", o, "number", 1)
        check("Candlestick", h, "number", 2)
        check("Candlestick", l, "number", 3)
        check("Candlestick", c, "number", 4)
        return {
            __type = "Candlestick",
            o = o,
            h = h,
            l = l,
            c = c
        }
    end
}

render.Timestamp = Timestamp


-- Stack ---------------------------------------------------------------

local function makeStackClass(name)
    local Stack = Class {
        name = name,
        constructor = function(...)
            return {
                __type = name,
                children = {...}
            }
        end
    }

    function Stack:__tostring()
        return string.format("<Stack #children=%d>", #self.children)
    end

    return Stack
end

render.HStack = makeStackClass("HStack")
render.VStack = makeStackClass("VStack")


-- Table ---------------------------------------------------------------

render.Table = Class {
    name = "Table",
    constructor = function(columns, ...)
        return {
            __type = "Table",
            columns = columns,
            rows = {...}
        }
    end
}


return render
