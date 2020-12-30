#!/usr/bin/env lua
-- MoonTypes example: union.lua
local types = require("moontypes")

-- Create a new group of types:
local group = types.group()

-- Add some type definitions to it:
group:typedef([[
typedef union {
   boolean  b
   int      i
   char     c[6]
   double   d[2]
} myunion
]])

-- Create an instance of the type myunion:
local x = group:new('myunion')
print(x)    --> mystruct ? ? ? ? ? ?
-- The size of a union is the size of its longest element:
print("sizeof:", x:sizeof())  --> 6 ..
x:set('c.2', 123)
print(x)    --> mystruct ? 123 ? ? ? ?
x:set('b', false)
print(x)    --> mystruct false 123 ? ? ? ?
x:set('c.1', 456) -- this overrides x.b
print(x)    --> mystruct 456 123 ? ? ? ?
x:set('d.2', 3.14) -- this overrides c.2
print(x)    --> mystruct 456 3.14 ? ? ? ?
print("b:", x:get('b'))
print("i:", x:get('i'))
print("c:", x:get('c'))
print("d:", x:get('d'))

