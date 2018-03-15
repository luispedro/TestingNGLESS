# Testing of -O2 pessimization with GHC

This code **uses linear space with -O2**, but runs in constant space without it.


`stack setup`

# Build with O2


Build with `-O2` and run


```bash
    stack build
    /usr/bin/time -v $(stack path --local-install-root)/bin/testingNGLESS
```


# Build without O2

```bash
    stack clean
    stack build --fast
    /usr/bin/time -v $(stack path --local-install-root)/bin/testingNGLESS
```

