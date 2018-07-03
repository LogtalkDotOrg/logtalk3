a(1).
a(2).
a(3).

b(1, 1).
b(1, 2).
b(1, 3).

:- op(888, xfx, foo).

a foo b.
b foo c.
c foo d.

end_of_file.

c(1, 2, 3).
