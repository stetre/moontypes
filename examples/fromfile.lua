#!/usr/bin/env lua
-- MoonTypes example: fromfile.lua
local types = require("moontypes")

-- Create a group and add the types defined in the given file:
local group = types.group()
group:typedef_from_file("definitions.def")

-- Create an instance of mytype:
local x = group:new('mytype')

x:set("e2", 12, 34, 56)
x:print()
x:print("e2")
x:print("e2.a")
x:print("e2.b")
x:print("e2.c")

-- Copy field e2 to field e1
x:set("e1", x:get("e2"))
x:print()
x:print("e1")

