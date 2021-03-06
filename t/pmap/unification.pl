:- use_module(library(ddata/pmap)).

:- use_module(library(tap)).

'matching keys, variable values' :-
    % describe the first map
    pairs(One, [alpha-a,beta-b,gamma-c]),

    % describe the second map, leaving holes for values
    pairs(Two,[beta-B,alpha-A,gamma-C]),

    % unify
    One = Two,
    A == a,
    B == b,
    C == c.


identical :-
    insert(hello,world,empty,M1),
    insert(hello,world,empty,M2),
    M1 = M2.

disjoint(fail) :-
    insert(pi,3.14159,empty,M1),
    insert(alpha,a,empty,M2),
    M1 = M2.

'unification after deletion' :-
    pairs(Foo0,[alpha-a,beta-b,foo-f]),
    pairs(Bar0,[alpha-a,beta-b,bar-b]),

    % remove keys to make identical maps
    insert(foo,_,Foo,Foo0),
    insert(bar,_,Bar,Bar0),

    % the maps should unify
    Foo = Bar.
