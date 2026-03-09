var n: Int = read_int();
print_int(n);
while n > 1 do {
    if n % 2 == 0 then {
        n = n / 2;
    } else {
        n = 3*n + 1;
    }
    print_int(n);
}
input 5
prints 5
prints 16
prints 8
prints 4
prints 2
prints 1
