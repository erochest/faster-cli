
# Python

From https://dlang.org/blog/2017/05/24/faster-command-line-tools-in-d/

```
MIN TIME: 19.87s
PEAK MEM: 5.8Mb
```

# Pypy

From https://dlang.org/blog/2017/05/24/faster-command-line-tools-in-d/

```
MIN TIME: 3.79s
PEAK MEM: 49.1Mb
```

# D (LDC)

From https://dlang.org/blog/2017/05/24/faster-command-line-tools-in-d/

```
MIN TIME: 1.89s
PEAK MEM: 2.2Mb
```

# D (DMD)

From https://dlang.org/blog/2017/05/24/faster-command-line-tools-in-d/

```
MIN TIME: 3.30s
PEAK MEM: 1.5Mb
```

# Nim

From https://www.euantorano.co.uk/posts/faster-command-line-tools-in-nim/

```
MIN TIME: 1.40s
PEAK MEM: 1.1Mb
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
MIN TIME: 0.43s
PEAK MEM: 1.4Mb
```

# Haskell

First pass:

```
real    0m4.509s
user    0m4.112s
sys     0m0.459s
```

Later:

```
MIN TIME: 3.31s
PEAK MEM: 49.6Mb
```

# Rust

First pass:

```
MIN TIME: 6.47s
PEAK MEM: 11.3Mb
```

Second pass:

```
MIN TIME: 3.05s
PEAK MEM: 1.2Mb
```

Third:

```
MIN TIME: 2.59s
PEAK MEM: 1.2Mb
```

Fourth:

```
MIN TIME: 1.60s
PEAK MEM: 1.2Mb
```
