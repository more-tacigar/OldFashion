# OldFashion - A Simple Script Language written in OCaml. #

イッツ、メチャメチャシンプル。

```
fn fib(n) {
  if (n <= 1) {
    return n
  } else {
    return fib(n - 1) + fib(n - 2)
  }
}

fn main() {
  print(fib(10))
}
```
