
# Python

From https://dlang.org/blog/2017/05/24/faster-command-line-tools-in-d/

```
       18.60 real        18.18 user         0.21 sys
```

# Pypy

From https://dlang.org/blog/2017/05/24/faster-command-line-tools-in-d/

```
        3.70 real         3.34 user         0.19 sys
```

# D (LDC)

From https://dlang.org/blog/2017/05/24/faster-command-line-tools-in-d/

```
        1.84 real         1.56 user         0.10 sys
```

# D (DMD)

From https://dlang.org/blog/2017/05/24/faster-command-line-tools-in-d/

```
        3.25 real         3.04 user         0.11 sys
```

# Nim

From https://www.euantorano.co.uk/posts/faster-command-line-tools-in-nim/

```
        1.56 real         1.48 user         0.05 sys
```

# golang

First pass:

```
        3.28 real         3.18 user         0.20 sys
```

Channels and Go routines:

```
        4.30 real        15.29 user         1.81 sys
```

Don't use strings:

```
        1.78 real         1.64 user         0.13 sys
```

Soup to nuts:

```
        0.76 real         0.61 user         0.11 sys
```

Final:

```
        0.52 real         0.40 user         0.11 sys
```

# Haskell

# Rust

