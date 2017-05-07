# xprop-tree

Just a dumb thing for running `xprop` for a text property against all windows.

# Usage

``` sh
## Defaults to printing the first value (if present) of WM_COMMAND
$ stack exec -- xprop-tree
4194305   parcellite
6291457   lxpanel
52428801  chromium
# ... so on, for all windows ...

## Can specify what property to fetch
$ stack exec -- _NET_WM_NAME
23068681   vim - s/x/s/list.c - s/x/s/xinput.c - s/x/s/xinput.h
54525961   NIX_PATH=~g nix-repl '<nixpkgs>' │ ~v │ bhaskell
140509193  ~/plan9 │ bhaskell
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
