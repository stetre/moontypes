#!/usr/bin/env lua
-- MoonTypes example: primitives.lua
local newgroup = require("moontypes").group

-- Create a group:
local group = newgroup()

-- Create an instance of the primitive type 'char':
local c = group:new('char')

-- Set its value:
c:pset(123)
print(c)        --> char 123
print(c:pget()) --> 123

-- Reset its value:
c:pset()
print(c)       --> char ?
print(c:pget()) --> ?
print(c:get('*')) --> ?
print(c:pget() == group:np()) --> true

-- This would cause an error because the value is out of range:
-- c:pset(300)

-- Define a custom primitive type:
group:primitive("mynumber", function(x) return type(x)=="number" and x>=0 and x<1 end)

local n = group:new('mynumber')
n:pset(0.5)
print(n)
print(n:pget())
-- This would cause an error because the value is out of range:
-- n:pset(2)

