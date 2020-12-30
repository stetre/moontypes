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

#ifndef internalDEFINED
#define internalDEFINED

#include <string.h>
#include <stdlib.h>
#include <time.h>
#include "moontypes.h"

#define TOSTR_(x) #x
#define TOSTR(x) TOSTR_(x)

/* Note: all the dynamic symbols of this library (should) start with 'moontypes_' .
 * The only exception is the luaopen_moontypes() function, which is searched for
 * with that name by Lua.
 * MoonTypes's string references on the Lua registry also start with 'moontypes_'.
 */

#if 0
/* .c */
#define  moontypes_
#endif

/* main.c */
extern lua_State *moontypes_L;
int luaopen_moontypes(lua_State *L);

/*------------------------------------------------------------------------------*
 | Debug and other utilities                                                    |
 *------------------------------------------------------------------------------*/

/* DEBUG -------------------------------------------------------- */
#if defined(DEBUG)

#define DBG printf
#define TR() do { printf("trace %s %d\n",__FILE__,__LINE__); } while(0)
#define BK() do { printf("break %s %d\n",__FILE__,__LINE__); getchar(); } while(0)
#define TSTART double ts = now();
#define TSTOP do {                                          \
    ts = since(ts); ts = ts*1e6;                            \
    printf("%s %d %.3f us\n", __FILE__, __LINE__, ts);      \
    ts = now();                                             \
} while(0);

#else 

#define DBG noprintf
#define TR()
#define BK()
#define TSTART do {} while(0) 
#define TSTOP do {} while(0)    

#endif /* DEBUG ------------------------------------------------- */

#endif /* internalDEFINED */
