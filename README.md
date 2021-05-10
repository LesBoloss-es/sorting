Sorting
=======

### Running Tests

```
dune test
```

### Running Benchmarks

```
dune exec --profile=release bench/arrays.exe
```

The use of the `release` profile is very important here as it compiles with
`--noassert` which removes assertions in the code.
