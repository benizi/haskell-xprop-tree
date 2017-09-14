# xprop-tree

Just a dumb thing for running `xprop` for a text property against all windows.

# Usage

Defaults to printing the first value (if present) of WM_COMMAND

``` sh
$ stack exec -- xprop-tree
0x400001   parcellite
0x600001   lxpanel
0x3200001  chromium
# ... so on, for all windows ...
```

Can specify what property to fetch

``` sh
$ stack exec -- xprop-tree _NET_WM_NAME
0x1600009   vim - s/x/s/list.c - s/x/s/xinput.c - s/x/s/xinput.h
0x3400009   NIX_PATH=~g nix-repl '<nixpkgs>' │ ~v │ bhaskell
0x8600009  ~/plan9 │ bhaskell
# ...
```

Multiple values are tab-delimited

``` sh
$ stack exec -- xprop-tree WM_CLASS
0x200001    lxpanel     Lxpanel
0x400001    parcellite  Parcellite
0x800001    nm-applet   Nm-applet
0x20001a    panel       lxpanel
# ...
```

# License

The MIT License (MIT)

Copyright (c) 2017 Benjamin R. Haskell

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
