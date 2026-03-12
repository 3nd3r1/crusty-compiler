# Bug discovered by a student
var a = false;
true or { a = true; a }
print_bool(a);
prints false
