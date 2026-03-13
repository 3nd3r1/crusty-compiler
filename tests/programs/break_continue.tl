var x = 7;
while true do {
    if x % 5 == 0 then {
        break;
    }
    x = x + 1;
}
x

prints 10
---
var x = 7;
while x <= 100 do {
    if x % 5 == 0 then {
        x = x * 2 + 1;
        continue;
    }
    x = x + 1;
}
x

prints 111
