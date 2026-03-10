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
