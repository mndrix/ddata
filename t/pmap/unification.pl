:- use_module(library(ddata/pmap)).

:- use_module(library(tap)).

'matching keys, variable values' :-
    % describe the first map
    foldl(delta, [alpha,beta,gamma],[a,b,c],empty,One),

    % describe the second map, leaving holes for values
    foldl(delta, [alpha,beta,gamma],[A,B,C],empty,Two),

    % unify
    One = Two,
    A == a,
    B == b,
    C == c.


identical :-
    delta(hello,world,empty,M1),
    delta(hello,world,empty,M2),
    M1 = M2.

disjoint(fail) :-
    delta(pi,3.14159,empty,M1),
    delta(alpha,a,empty,M2),
    M1 = M2.
