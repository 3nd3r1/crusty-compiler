fun collatz(n: Int): Unit {
    print_int(n);
    while n > 1 do {
        if n % 2 == 0 then {
            n = n / 2;
        } else {
            n = 3 * n + 1;
        }
        print_int(n);
    }
}

collatz(read_int());
input 20
prints 20
prints 10
prints 5
prints 16
prints 8
prints 4
prints 2
prints 1
---
fun f(n: Int): Int {
  if n > 5 then {
    return 5;
  }
  123
}

f(10)
prints 5
---
fun f(x: Int): Int {
    return g(x + 1);
}
fun g(x: Int): Int {
    print_int(x);
    if x < 5 then {
        return f(x);
    } else {
        return x;
    }
}
f(1) * 100
prints 2
prints 3
prints 4
prints 5
prints 500
---
fun f(x: Int): Int { return x+1; }
f(3)
prints 4
