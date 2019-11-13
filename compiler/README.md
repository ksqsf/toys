# compiler
[Implementing a JIT compiled Language with Haskell and LLVM](http://www.stephendiehl.com/llvm/])

I followed the tutorial above and this is (or will be) the result.

Hopefully I will come back here from time to time to fix "the stupid things".

## Basic Language

The language is called Kaleidoscope. 

```python
def fib(x)
  if x < 3 then
    1
  else
    fib(x-1) + fib(x-2)
    
fib(40)
```


