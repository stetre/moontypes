#!/usr/bin/env lua
-- MoonTypes example: matrix.lua
local types = require("moontypes")
local group = types.group()

local nrows, ncols = 3, 4 
-- Define the type 'nrows x ncols matrix of strings':
group:typedef(string.format("typedef string matrix[%u][%u]",ncols,nrows))

-- Instantiate a matrix:
local M = group:new('matrix')

-- Write in each element a string with its indices:
for i = 1, nrows do 
   for j = 1, ncols do
      M:tset(string.format('%s.%s',i,j),string.format('M_%u_%u',i,j))
   end
end

print(M)

-- Write something else in M(2,3):
M:tset('2.3', "hello!")
print(M)
M:print('2.3')
print(M:tget('2.3'))

