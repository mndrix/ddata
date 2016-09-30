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

'unification after deletion' :-
    foldl(delta, [alpha,beta,foo],[a,b,f],empty,Foo0),
    foldl(delta, [alpha,beta,bar],[a,b,b],empty,Bar0),

    % remove keys to make identical maps
    delta(foo,_,Foo,Foo0),
    delta(bar,_,Bar,Bar0),

    % the maps should unify
    Foo = Bar.
