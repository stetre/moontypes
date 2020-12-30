#!/usr/bin/env lua
-- MoonTypes example: hello.lua
local types = require("moontypes")

-- Create a new group of types:
local group = types.group()

-- Add some type definitions to it:
group:typedef([[
-- these are just aliases of primitive types:
typedef char mychar
typedef int myint

-- a structured type: 
typedef struct {
   boolean  b
   myint    i
   mychar   c[6]
   double   d
} mystruct
]])

-- Create an instance of the type mystruct:
local x = group:new('mystruct')

-- Some prints...
x:print()     --> mystruct ? ? ? ? ? ? ? ? ?
print(x)      --> mystruct ? ? ? ? ? ? ? ? ?
x:print('c')  --> mystruct.c ? ? ? ? ? ?
print(group:sizeof('mystruct'))    --> 9 2 10 struct{ ... }
print(x:sizeof())               --> 9 2 10 struct{ ... }
print(#x)     --> 10 (the size + 1 for the type name)

-- Write some fields:
x:set('b', true)
x:set('i', 123)
x:set('c', 10, 20, 30)
x:set('d', 1.0e20)

-- Read (and print) some fields:
print(x:get('*'))   --> mystruct true 123 10 20 30 ? ? ? 1e+20
print(x:get('b'))   --> true
print(x:get('i'))   --> 123
print(x:get('c.1')) --> 10
print(x:get('c.6')) --> ?
print(x:get('c'))   --> 10 20 30 ? ? ?

local c =group:new('char')
print(c)
c:set('*', 2)
print(c)
local y = x:clone()
print(x)
print(y)

print("Definitions in the whole group:\n"..group)
