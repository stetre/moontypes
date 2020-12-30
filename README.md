## MoonTypes: A C-like typedef for Lua

MoonTypes is a Lua module that provides means to define **C-like structured types** in Lua.

It runs on GNU/Linux <!-- and on Windows (MSYS2/MinGW) --> and requires 
[Lua](http://www.lua.org/) (>=5.3) and [LPeg](http://www.inf.puc-rio.br/~roberto/lpeg/).

_Author:_ _[Stefano Trettel](https://www.linkedin.com/in/stetre)_

[![Lua logo](./doc/powered-by-lua.gif)](http://www.lua.org/)

#### License

MIT/X11 license (same as Lua). See [LICENSE](./LICENSE).

#### Documentation

See the [Reference Manual](https://stetre.github.io/moontypes/doc/index.html).

#### Getting and installing

Setup the build environment as described [here](https://github.com/stetre/moonlibs), then:

```sh
$ git clone https://github.com/stetre/moontypes/
$ cd moontypes
moontypes$ make
moontypes$ sudo make install
```

#### Example

The example below shows the basic usage of MoonTypes.

Other examples can be found in the **examples/** directory.

```lua
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
print(group:sizeof('mystruct')) --> 9 2 10 struct{ ... }
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
```

The script can be executed at the shell prompt with the standard Lua interpreter:

```shell
$ lua hello.lua
```

#### See also

* [MoonLibs - Graphics and Audio Lua Libraries](https://github.com/stetre/moonlibs).

