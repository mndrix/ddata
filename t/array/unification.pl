:- use_module(library(ddata/array)).

:- use_module(library(tap)).

'matching indices, variable values' :-
    % describe the first array
    nth(One, 1, a),
    nth(One, 2, b),
    nth(One, 3, c),

    % describe the second array, leaving holes for values
    nth(Two, 1, A),
    nth(Two, 2, B),
    nth(Two, 3, C),

    % unify
    One = Two,
    A == a,
    B == b,
    C == c.


identical :-
    nth(M1,42,world),
    nth(M2,42,world),
    M1 = M2.

disjoint :-
    nth(M1,3,3.14159),
    nth(M2,42,answer),
    M1 = M2,

    % unification merged the two arrays
    nth(M1,3,3.14159),
    nth(M1,42,answer),
    nth(M2,3,3.14159),
    nth(M2,42,answer).
