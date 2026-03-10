var x = 3;
{
    var x = 4;
    x = 5;
}
x
prints 3
---
var x = { { print_int(1) } { 2 } }
x
prints 1
prints 2
---
{
    {
        {
            123
        }
    }
};
