:- use_module(library(hmap)).

:- use_module(library(tap)).

'matching keys, variable values' :-
    % describe the first map
    kv(One, alpha, a),
    kv(One, beta, b),
    kv(One, gamma, c),

    % describe the second map, leaving holes for values
    kv(Two, alpha, A),
    kv(Two, beta, B),
    kv(Two, gamma, C),

    % unify
    One = Two,
    A == a,
    B == b,
    C == c.


identical :-
    kv(M1,hello,world),
    kv(M2,hello,world),
    M1 = M2.

disjoint :-
    kv(M1,pi,3.14159),
    kv(M2,alpha,a),
    M1 = M2,

    % unification merged the two maps
    kv(M1,pi,3.14159),
    kv(M1,alpha,a),
    kv(M2,pi,3.14159),
    kv(M2,alpha,a).
