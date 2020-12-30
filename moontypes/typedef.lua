-- The MIT License (MIT)
--
-- Copyright (c) 2020 Stefano Trettel
--
-- Software repository: MoonTypes, https://github.com/stetre/moontypes
--
-- Permission is hereby granted, free of charge, to any person obtaining a copy
-- of this software and associated documentation files (the "Software"), to deal
-- in the Software without restriction, including without limitation the rights
-- to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
-- copies of the Software, and to permit persons to whom the Software is
-- furnished to do so, subject to the following conditions:
--
-- The above copyright notice and this permission notice shall be included in all
-- copies or substantial portions of the Software.
--
-- THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
-- IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
-- FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
-- AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
-- LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
-- OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
-- SOFTWARE.
-- 

-- *********************************************************************
-- DO NOT require() THIS MODULE (it is loaded automatically by MoonTypes)
-- *********************************************************************
do
local types = moontypes -- require("moontypes")


local lpeg = require("lpeg")
local re = require("re")
local P = lpeg.P
local V = lpeg.V
local S = lpeg.S
local R = lpeg.R
local Cs = lpeg.Cs
local Ct = lpeg.Ct
local C = lpeg.C

local Group = {}
Group.__index = Group

local Instance = {}
Instance.__index = Instance

--------------------------------------------------------------------------
-- Utilities
--------------------------------------------------------------------------

local fmt = string.format

local function notraceback(m) return m end -- replace debug.traceback

local function assertf(level, condition, ...)
   if condition then return condition end
   local errmsg = next({...}) and fmt(...) or "assertf failed"
   error(errmsg,level+1)
end

function deepprint(t, nonl)
-- deep print of a table (for debugs)
   local b=false
   io.write("{")
   for k, v in pairs(t) do
      if b then io.write(", ") else b = true end
      if type(v)=="table" then
         deepprint(v,true)
      else
         io.write(fmt("%s",v))
      end
   end
   io.write("}")
   if not nonl then io.write("\n") end
end

local function truncate(x,n)
-- truncate a long value (for prints)
   local x = tostring(x)
   return x:len() > n and x:sub(1,n) .." ..." or x --@@len()
end

-------------------------------------------------------------------------------
-- Type-check functions
-------------------------------------------------------------------------------
-- check_xxx(x) checks that x is of the given type xxx

local check_number = function(x) return type(x)=="number" end
local check_boolean = function(x) return type(x)=="boolean" end
local check_string = function(x) return type(x)=="string" end
local check_table = function(x) return type(x)=="table" end
local check_thread = function(x) return type(x)=="thread" end
local check_userdata = function(x) return type(x)=="userdata" end
local check_function = function(x) return type(x)=="function" end
local check_bit = function(x) return type(x)=="number" and (x==0 or x==1) end

local UCHARMIN, UCHARMAX = 0, 255
local check_uchar = function(x)
   if type(x)~="number" or x < UCHARMIN or x > UCHARMAX then return false end
   return (x % 1)==0
end

local CHARMIN, CHARMAX = -128, 127
local check_char = function(x)
   if type(x)~="number" or x < CHARMIN or x > CHARMAX then return false end
   return (x % 1) == 0
end

local USHORTMIN, USHORTMAX = 0, 65535
local check_ushort = function(x)
   if type(x)~="number" or x < USHORTMIN or x > USHORTMAX then return false end
   return (x % 1)==0
end

local SHORTMIN, SHORTMAX = -32768, 32767
local check_short = function(x)
   if type(x)~="number" or x < SHORTMIN or x > SHORTMAX then return false end
   return (x % 1)==0
end

local UINTMIN, UINTMAX = 0, 4294967295
local check_uint = function(x)
   if type(x)~="number" or x < UINTMIN or x > UINTMAX then return false end
   return (x % 1)==0
end

local INTMIN, INTMAX = -2147483648, 2147483647 -- -2^31, 2^31-1
local check_int = function(x)
   if type(x)~="number" or x < INTMIN or x > INTMAX then return false end
   return (x % 1)==0
end

local check_ulong = function(x)
   if type(x)~="number" then return false end
   return (x % 1)==0
end

local check_long = function(x)
   if type(x)~="number" then return false end
   return (x % 1)==0
end

local FLTMAX = 3.402823e+38 -- 340282346638528859811704183484516925440.000000
local check_float = function(x)
   return type(x)=="number" and x > -FLTMAX and x < FLTMAX
end

local check_double = function(x)
   return type(x)=="number"
end

local bitstrp = re.compile("[01]+ !. ")
local check_bitstr = function(x)
   return type(x)=="string" and (bitstrp:match(x) ~= nil)
end

local hexstrp = re.compile("([0-9] / [a-f] / [A-F])+ !.")
local check_hexstr = function(x)
   return type(x)=="string" and ((x:len() % 2)==0) and (hexstrp:match(x) ~= nil) --@@ len()
end


--------------------------------------------------------------------------
-- Group creator
--------------------------------------------------------------------------

local defineprimitives

types.group = function(np)
-- creates a new 'header' (a database of typedefs)
   local np = np or '?'
   assertf(2,type(np)=="string", "invalid value '%s' for 'not present' (must be a string)", np)
   local group = {}
   setmetatable(group, Group)
   group.NP = np
   group.T = {} -- types database, indexed by typename
   -- define primitive types
   group.T["void"] = { 0, 0, "void", nil, "void" }
   group:primitive("number", check_number)
   group:primitive("boolean", check_boolean)
   group:primitive("string", check_string)
   group:primitive("table", check_table)
   group:primitive("function", check_function)
   group:primitive("thread", check_thread)
   group:primitive("userdata", check_userdata)
   group:primitive("bit", check_bit)
   group:primitive("char", check_char)
   group:primitive("uchar", check_uchar)
   group:primitive("short", check_short)
   group:primitive("ushort", check_ushort)
   group:primitive("int", check_int)
   group:primitive("uint", check_uint)
   group:primitive("long", check_long)
   group:primitive("ulong", check_ulong)
   group:primitive("float", check_float)
   group:primitive("double", check_double)
   group:primitive("bitstr", check_bitstr)
   group:primitive("hexstr", check_hexstr)
   return group
end

function Group.primitive(group, name, checkfunc) 
   assertf(2, not group.T[name], "duplicated type '%s'", name)
   assertf(2, type(checkfunc)=="function", "missing check function")
   group.T[name] = { 1, 0, name, checkfunc, name }
end

function Group.np(group) return group.NP end

-------------------------------------------------------------------------------
-- Comments remover
-------------------------------------------------------------------------------

-- Removes C style comments (/* ... */)
local ansiccomments = Cs ( (re.compile([[
   S <- '/*' <C>
   C <- '*/' / . <C>]])/'' + 1 )^1 )

-- Removes C preprocessor directives (# ... )
local cpreprocessor = Cs ( (re.compile([[
   S <- %nl '#' <C>
   C <- %nl / . <C>]]
)/'' + 1 )^1 )

-- Removes C++ style comments (// ... )
local cppcomments = Cs( (re.compile([[
   S <- '//' <C>
   C <- %nl / . <C>]])/'\n' + 1 )^1 )

-- Removes Lua style one-line comments (-- ... )
local luacomments = Cs( (re.compile([[
   S <- '--' <C>
   C <- %nl / . <C>]])/'\n' + 1 )^1 )

local compressnl = Cs( (re.compile("(%nl)+")/'\n' + 1 )^1 )

local function stripcomments(s)
-- strips ANSI C comments and one-line Lua comments 
   local s = '\n' .. s
   s = ansiccomments:match(s)
   s = cpreprocessor:match(s)
   s = cppcomments:match(s)
   s = luacomments:match(s)
   return compressnl:match(s)
end

-------------------------------------------------------------------------------
-- Parser
-------------------------------------------------------------------------------

-- whitespaces:
local SP = S(' \t\n')
local SP0 = SP^0 -- SP*
local SP1 = SP^1 -- SP+
-- fields separator(s):
local SEP = (SP0 * (P',' + P';') * SP0) + SP1

local ALPHA = R('az','AZ') + P'_'
local ALPHANUM = ALPHA + R('09')
local IDENTIFIER = ALPHA * ALPHANUM^0
local NAME = IDENTIFIER
local TYPE = IDENTIFIER
local TYPEDEF = P'typedef'
local VOID = P'void' * #SP
local STRUCT = P'struct'
local UNION = P'union'

-- Splits a string containing multiple typedefs in one string per typedef:
local splittypedefs = Ct( (re.compile([[
S <- 'typedef' <C>
C <- &'typedef' / !. / . <C>]])/'%0' + SP^1)^1 )

local function Size(x) 
   local n = tonumber(x)
   if not n or n < 1 or (n%1~=0) then error(fmt("invalid array size '%s'",x),4) end
   return n
end

-- Parses a string containing one typedef, and builds the intermediate table:
local typedefpattern = P{
   "typedecl", 
   typedecl  = TYPEDEF * SP1 * V'field' * SEP^-1 * -P(1),
   field     = Ct(V'basetype' * SP1 * (NAME/'%0') * V'array'),
   array     = Ct(( SP0 * '[' * ((1 -S'[]')^1/Size) * ']')^0),
   basetype  = VOID/'%0' + Ct(V'structorunion') + TYPE/'%0',
   structorunion = (STRUCT + UNION)/'%0' * SP0 * V'fieldlist',
   fieldlist = '{' * SP0 * V'field' * (SEP * V'field')^0 * SEP^-1 * '}',
}

-------------------------------------------------------------------------------
-- Typedef
-------------------------------------------------------------------------------

local function sizeof(group, typename)
   local td = group.T[typename]
   assertf(2, td, fmt("unknown type '%s'",typename))
   return td[1]
end

local function prepend_dot(name)
   if name:sub(1,1) ~= "." then return "."..name else return name end
end 

local function fullname(typename, fieldname)
   local name = fieldname and typename .. '.' .. fieldname or typename
   if name:sub(1,1)=='.' then return name:sub(2) else return name end
end 

local function kindof(kind)
   if kind==0 then return "terminal"
   elseif kind == 1 then return "array"
   elseif kind == 2 then return "struct"
   elseif kind == 3 then return "union"
   end
   error("invalid kind=%s",kind)
end

---------------------------------------------------
-- TYPE DESCRIPTOR
-- 
-- group.T[typename] = {
--    [1] = size (no. of terminal fields)
--    [2] = kind (0=primitive, 1=array, 2 =struct, 3=union)
--    [3] = N (array size, for arrays)
--          { fieldname1, ..., fieldnameN } (for structs and unions)
--          primitive typename (for terminals)
--    [4] = check function (for primitives only)
--    [5] = description (typedef string)
--    For arrays only:
--    ["basetype"] = base_typename
--    For unions and structs only:
--    [fieldname1] = { field_typename, position }
--       ...
--    [fieldnameN] = { field_typename, position }
--    }
--
--    'position' is the index (starting from 1) where the field begins
--    within the struct or union
--
-- intermediate_table = 
-- { typename name {N1, ... Nn} } or
-- { {struct {field1} {field2} ... {fieldN}} name {N1, ... Nn} } or
-- { {union {field1} {field2} ... {fieldN}} name {N1, ... Nn} } or
--
-- fieldK = intermediate_table
-----------------------------------------------------

local function description(t, s)
-- creates a string describing a type, from the intermediate table t
   local s = s or ""
   local typ, name, arr = t[1], t[2], t[3]
   if type(typ) == "string" then
      s = s .. typ
   else -- type(typ) == "table" then
      s = s .. typ[1] .. '{ ' -- "struct" or "union"
      for i=2,#typ do
         s = s .. description(typ[i])
      end
      s = s .. '}' 
   end
-- s = s .. ' ' .. name
   if #arr > 0 then
      for i = 1,#arr do
         s = s .. fmt("[%s]",arr[i])
      end
   end
   return s .. ' '
end

local function definetype(group, typ, name, arr)
-- creates the type entry in group.T
-- returns the type name
   assertf(2, not group.T[name], fmt("duplicated typedef of '%s'",name))

   -- array ----------------------------------------------------
   if #arr > 0 then
      local n = arr[#arr]
      local td = {}
      td[2] = 1 -- "array"
      td[3] = n
      td[5] = description({typ, name, arr})
      arr[#arr]=nil
      if (#arr == 0) and type(typ)=="string" then
         local td1 = group.T[typ]
         assertf(2, td1, "unknown type '%s'",typ)
         td["basetype"] = typ
         td[1] = sizeof(group, typ)*n
      else
         --local bname =  name .. "_1"
         local bname =  name .. ".@"
         bname = prepend_dot(bname)
         definetype(group, typ, bname, arr)
         td["basetype"] = bname
         td[1] = sizeof(group, bname) * n
      end
      group.T[name] = td
      return name
   end

   -- alias -------------------------------------------------------
   if type(typ)=="string" then 
      local td = group.T[typ] 
      assertf(2, td, "unknown type '%s'",typ)
      group.T[name] = td
      return name
   end

   -- struct or union ---------------------------------------------
   local flist ={} -- create the field list
   local td = {}
   td[5] = description({typ, name, arr})
   for i=2,#typ do
      local tt = typ[i]
      local fname = tt[2]
      assertf(2, not td[fname], "duplicate field '%s'",fname)
      flist[#flist+1] = fname
      local tname =  name .."."..fname
      tname = prepend_dot(tname)
      td[fname] = { definetype(group, tt[1], tname, tt[3]) }
   end
   local kind = typ[1] == "struct" and 2 or 3
   td[2] = kind
   td[3] = flist
   -- compute size and fields positions
   if kind == 2 then --"struct"
      local pos = 0
      for _,f in ipairs(flist) do
         local sz = sizeof(group, td[f][1])
         td[f][2] = sz == 0 and pos or pos + 1
         pos = pos + sz
      end
      td[1] = pos
   elseif kind == 3 then -- kind == "union"
      local pos, maxsz = 0, 0
      for _,f in ipairs(flist) do
         local sz = sizeof(group, td[f][1])
         td[f][2] = sz == 0 and 0 or 1
         if sz > maxsz then maxsz = sz end
      end
      td[1] = maxsz
   else
      error("syntax error",2)
   end
   group.T[name] = td
   return name
end

local errlvl = 2

function Group.typedef_from_file(group, filename)
-- defines types from a file
   local file, errmsg = io.open(filename,"r")
   if not file then error(errmsg,2) end
   local s = file:read("a")
   --print(s)
   errlvl = errlvl+1
   group:typedef(s)
   errlvl = errlvl-1
end

function Group.typedef(group, defs)
   local defs = stripcomments(defs)
   -- separate the typedefs
   local defs1 = splittypedefs:match(defs)
   local names = {}
   for _,d in ipairs(defs1) do
      -- parse the definition and create the intermediate table
      local t = typedefpattern:match(d)
      if not t then error(fmt("syntax error in:\n%s\n",d),errlvl) end
      --deepprint(t)
      -- define the type:
      local ok, name = xpcall(function (...)
         return definetype(...)
      end, notraceback, group, t[1], t[2],t[3])
      if not ok then
         errmsg = name .. fmt(", in:\n%s\n",d)
         error(errmsg,errlvl) end
      names[#names+1] = name
   end
   assertf(errlvl, #names > 0, "no type definitions found")
end


-------------------------------------------------------------------------------
-- Tostring functions
-------------------------------------------------------------------------------

function Group.__tostring(group)
   local s = {}
   local ot = {} --ordered table

   for name, td in pairs(group.T) do ot[#ot+1] = { name, td } end
   table.sort(ot, function(x,y)
                     local a,b = x[1], y[1]
                     local a = a:sub(1,1) == '.' and a:sub(2) or a
                     local b = b:sub(1,1) == '.' and b:sub(2) or b
                     return a<b end)

   for _,t in ipairs(ot) do
      local name, td = t[1], t[2]
      local size, kind = td[1], td[2]
      if kind == 0 then
         s[#s+1] = fmt("%s (%s) %s", name,size, td[3])
      elseif kind == 1 then -- array then
         s[#s+1] = fmt("%s (%s) %s[%u]", name,size,td.basetype,td[3])
      else -- struct or union
         s[#s+1] = fmt("%s (%s) %s{%s}", name,size,kindof(kind),table.concat(td[3],","))
         for _,f in ipairs(td[3]) do
            local fd = td[f]
            s[#s+1] = fmt("  %4s %s (%s) %s",fd[2], f,sizeof(group,fd[1]),fd[1])
         end
      end
   end
   return table.concat(s,"\n")
end

Group.__concat = function(x, y) return tostring(x)..tostring(y) end

local function concat(x, sep)
   local sep = sep or ' '
   local s = {}
   for k,v in ipairs(x) do s[k] = tostring(v) end
   return table.concat(s, sep)
end

function Instance.tostring(s, fieldname, sep)
   if not fieldname or fieldname == '*' then return concat(s, sep) end
   local size, first, last = s:sizeof(fieldname)
   local t = { s[1] .. '.' .. fieldname }
   for i=first,last do t[#t+1] = s[i] end
   return concat(t, sep)
end

function Instance.print(s, fieldname, sep)
   print(s:tostring(fieldname, sep))
end

-- If we are just interested in the whole instance, we can use the canonical tostring():
Instance.__tostring = function(s) return s:tostring() end
Instance.__concat = function(x, y) return tostring(x)..tostring(y) end

-------------------------------------------------------------------------------
-- indicesof
-------------------------------------------------------------------------------

local function outerfield(fieldname)
-- splits a dotted fieldname in the outer field name and the rest
   if not fieldname or fieldname == '*' or fieldname == '' then return nil end
   local i = string.find(fieldname,'[.]')
   if not i then return fieldname end
   return string.sub(fieldname,1,i-1), string.sub(fieldname,i+1)
end


local function indicesof(group, typename, fieldname, pos)
-- given the field 'fieldname' in the type 'typename', 
-- returns: size, first, last, td
-- fieldname is the 'dot' field name, or nil or '' for the whole type

   local td = group.T[typename]
   if not td then error(fmt("invalid type '%s'",typename),2) end

   local pos = pos or 1 -- stay behind of one position (type may be void)
   local size = td[1]
   if not fieldname or fieldname == "*" or fieldname == '' then
      if size == 0 then return 0, 0, 0, td  end
      return size, pos+1, pos+size, td
   end

   local kind = td[2]
   local outer, subfieldname = outerfield(fieldname)

   if kind == 0 then -- type has no named fields
      error(fmt("invalid field name '%s' (%s)",outer,fullname(typename)),2)
   end

   local subtype

   if kind == 1 then -- "array"
      local N = td[3]
      local i = tonumber(outer) -- should be a number between 1 and N
      if not i or (i%1~=0) or i<1 or i>N then 
         error(fmt("invalid field name '%s' (%s)",outer,fullname(typename)),2)
      end
      subtype = td["basetype"]
      pos = pos + (i-1)*sizeof(group, subtype)
   else -- if kind == 2 ("struct") or kind == 3 ("union")
      -- local fields = td[3]
      local fd = td[outer] -- should be a valid field descriptor
      if not fd then
         error(fmt("invalid field name '%s' (%s)",outer,fullname(typename)),2)
      end
      subtype = fd[1]
      pos = pos + fd[2]-1
   end
      
   return indicesof(group, subtype, subfieldname, pos)
end

-------------------------------------------------------------------------------
-- new(), get(), set()
-------------------------------------------------------------------------------

function Group.new(group, typename)
-- Creates a new instance of the specified type, with all its fields initialized to NP
   local td = group.T[typename]
   assertf(2, td, "unknown type '%s'", typename)
   local NP = group.NP
   local s = { typename }
   for i=2, td[1]+1 do s[i]=NP end
   s.group = group
   return setmetatable(s, Instance)
end

function Group.sizeof(group, typename, fieldname)
   assertf(2, type(typename)=="string", "invalid type name")
   local ok, size, first, last, td = xpcall(function(...)
      return indicesof(...)
   end, notraceback, group, typename, fieldname)
   if not ok then error(size,2) end
   return size, first, last, td[5]
end

function Instance.sizeof(s, fieldname)
   local typename = s[1]
   local group = s.group
   assertf(2, type(typename)=="string", "invalid type name")
   local ok, size, first, last, td = xpcall(function(...)
      return indicesof(...)
   end, notraceback, group, typename, fieldname)
   if not ok then error(size,2) end
   return size, first, last, td[5]
end

function Instance.clone(s)
   assertf(2, type(s)=="table", "invalid instance")
   local group = s.group
   local len = sizeof(group, s[1])
   assertf(2, #s == (len+1), "instance has an invalid length")
   local s1 = { table.unpack(s) }
   s1.group = group
   return setmetatable(s1, Instance)
end

function Instance.set(s, fieldname, ...)
-- set field 'fieldname' of the sequence s (no type-checking)
   assertf(2, type(s)=="table", "missing or invalid instance")
   assertf(2, type(fieldname)=="string", "missing or invalid field name")
   local typename = s[1]
   local group = s.group
   local ok, size, first, last = xpcall(function(...)
      return indicesof(...)
   end, notraceback, group, typename, fieldname)
   if not ok then error(size,2) end
   if size == 0 then return end -- void type
   local val = {...}
   local nval = #val
   local j = 1
   if nval == 0 then -- reset field
      local NP = group.NP
      for i = first, last do
         s[i] = NP
         j=j+1
      end
   else -- set only the passed values
      for i = first, first + nval - 1 do 
         s[i] = val[j] j=j+1 end
   end
end

function Instance.get(s, fieldname)
-- get field 'fieldname' from the sequence s (no type-checking)
   assertf(2, type(s)=="table", "missing or invalid instance")
   assertf(2, type(fieldname)=="string", "missing or invalid field name")
   local typename = s[1]
   local group = s.group
   local ok, size, first, last = xpcall(function(...)
      return indicesof(...)
   end, notraceback, group, typename, fieldname)
   if not ok then error(size,2) end
   if size == 0 then return nil end -- void type
   local f, j = {}, 1
   for i = first, last do f[j] = s[i] j=j+1 end
   return table.unpack(f)
end

function Instance.tset(s, fieldname, val)
-- safely set a terminal field
   local errlvl = 2
   assertf(errlvl, type(s)=="table", "missing or invalid instance")
   assertf(errlvl, type(fieldname)=="string", "missing or invalid field name")
   local typename = s[1]
   local group = s.group
   local ok, size, pos, _, td = xpcall(function(...)
      return indicesof(...)
   end, notraceback, group, typename, fieldname)
   if not ok then error(size,errlvl) end
   if size == 0 then 
      error("attempt to set void field '%s'", fieldname, errlvl)
   end
   assertf(errlvl, td[2]==0, "'%s' is not a terminal field", fieldname)
   if not val or val == group.NP then s[pos] = group.NP return end
   if not td[4](val) then
      error(fmt("invalid value '%s' for terminal field '%s' (%s)"
                  ,truncate(val,20),fieldname,td[3]),errlvl)
   end
   s[pos] = val
end

function Instance.tget(s, fieldname)
-- safely get a terminal field's value
-- on error, including if fieldname is not a terminal field, it raises an error()
   local errlvl = 2
   assertf(errlvl, type(s)=="table", "missing or invalid instance")
   assertf(errlvl, type(fieldname)=="string", "missing or invalid field name")
   local typename = s[1]
   local group = s.group
   local ok, size, pos, _, td = xpcall(function(...)
      return indicesof(...)
   end, notraceback, group, typename, fieldname)
   if not ok then error(size,errlvl) end
   if size == 0 then 
      error("attempt to get void field '%s'", fieldname, errlvl)
   end
   assertf(errlvl, td[2]==0, "'%s' is not a terminal field", fieldname)
   local val = s[pos]
   if val ~= group.NP and not td[4](val) then
      error(fmt("invalid value '%s' in terminal field '%s' (%s)"
                  ,truncate(val,20),fieldname,td[3]),errlvl)
   end
   return val
end

function Instance.pset(s, val)
   local errlvl = 2
   assertf(errlvl, type(s)=="table", "missing or invalid instance")
   local typename = s[1]
   local group = s.group
   local ok, size, pos, _, td = xpcall(function(...)
      return indicesof(...)
   end, notraceback, group, typename)
   if not ok then error(size,errlvl) end
   if size == 0 then 
      error("attempt to set void field", errlvl)
   end
   assertf(errlvl, td[2]==0, "'%s' is not a primitive type", typename)
   if not val or val == group.NP then s[pos] = group.NP return end
   if not td[4](val) then
      error(fmt("invalid value '%s' for type '%s'",truncate(val,20),td[3]),errlvl)
   end
   s[pos] = val
end

function Instance.pget(s)
   local errlvl = 2
   assertf(errlvl, type(s)=="table", "missing or invalid instance")
   local typename = s[1]
   local group = s.group
   local ok, size, pos, _, td = xpcall(function(...)
      return indicesof(...)
   end, notraceback, group, typename)
   if not ok then error(size,errlvl) end
   if size == 0 then 
      error("attempt to set void field", errlvl)
   end
   assertf(errlvl, td[2]==0, "'%s' is not a primitive type", typename)
   local val = s[pos]
   if val ~= group.NP and not td[4](val) then
      error(fmt("invalid value '%s' in type '%s'",truncate(val,20),td[3]),errlvl)
   end
   return val
end

end
