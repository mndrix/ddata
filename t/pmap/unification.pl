:- use_module(library(ddata/pmap)).

:- use_module(library(tap)).

'matching keys, variable values' :-
    % describe the first map
    foldl(delta, [alpha,beta,gamma],[a,b,c],_,One),

    % describe the second map, leaving holes for values
    foldl(delta, [alpha,beta,gamma],[A,B,C],_,Two),

    % unify
    One = Two,
    A == a,
    B == b,
    C == c.


identical :-
    delta(hello,world,_,M1),
    delta(hello,world,_,M2),
    M1 = M2.

disjoint(fails) :-
    delta(pi,3.14159,_,M1),
    delta(alpha,a,_,M2),
    M1 = M2.
