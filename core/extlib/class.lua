#lang lua

local ClassMeta = {}
ClassMeta.__index = ClassMeta

function ClassMeta:__call(...)
    return self.make(...)
end

function ClassMeta:__tostring()
    return string.format("<#%s>", self.__name)
end

function Class(params)
    if not params.constructor then
        error("'constructor' param is required")
    end

    local TheClass = {}
    TheClass.__name = params.name or "AnonymousClass"
    TheClass.__index = TheClass
    setmetatable(TheClass, ClassMeta)

    function TheClass.make(...)
        local ob = params.constructor(...)
        setmetatable(ob, TheClass)
        return ob
    end

    function TheClass:__tostring()
        return string.format("<%s>", TheClass.__name)
    end

    return TheClass
end

return Class
