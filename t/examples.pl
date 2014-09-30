:- use_module(library(hmap)).

:- use_module(library(tap)).

'three insertions' :-
    % declare map contents
    kv(Map, hello, world),
    kv(Map, goodbye, friends),
    kv(Map, list, [1,2,3]),

    % can we fetch the right values?
    kv(Map, hello, World),
    World == world,
    kv(Map, goodbye, Friends),
    Friends == friends,
    kv(Map, list, List),
    List == [1,2,3].


'duplicate keys'(fail) :-
    kv(Map, one, 1),
    kv(Map, one, won).


unification :-
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
