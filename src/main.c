/* The MIT License (MIT)
 *
 * Copyright (c) 2020 Stefano Trettel
 *
 * Software repository: MoonTypes, https://github.com/stetre/moontypes
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 */

#include "internal.h"

lua_State *moontypes_L;

static void AtExit(void)
    {
    if(moontypes_L)
        {
        moontypes_L = NULL;
        }
    }
 
static int AddVersions(lua_State *L)
    {
    lua_pushstring(L, "_VERSION");
    lua_pushstring(L, "MoonTypes "MOONTYPES_VERSION);
    lua_settable(L, -3);
    return 0;
    }
 
int luaopen_moontypes(lua_State *L)
/* Lua calls this function to load the module */
    {
    moontypes_L = L;

    atexit(AtExit);

    lua_newtable(L); /* the module table */
    AddVersions(L);

    /* Add functions implemented in Lua */
    lua_pushvalue(L, -1); lua_setglobal(L, "moontypes");
    if(luaL_dostring(L, "require('moontypes.typedef')") != 0) lua_error(L);
    lua_pushnil(L);  lua_setglobal(L, "moontypes");
    return 1;
    }

