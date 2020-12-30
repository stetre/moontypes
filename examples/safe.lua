#!/usr/bin/env lua
-- MoonTypes example: safe.lua
local types = require("moontypes")

-- Create a group:
local group = types.group()

-- Add a type definition:
group:typedef([[
typedef struct {
   boolean  b
   int      i
} mytype
]])

-- Create an instance:
local x = group:new('mytype')

-- Safely write values in the terminal fields:
x:tset("b", true)
x:tset("i", 10)

print(x)

-- This should cause an error, because the value is not valid for the field's type:
x:tset("i", true)

