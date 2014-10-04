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


'iterate keys' :-
    kv(Map, alpha, one),
    kv(Map, beta, two),
    kv(Map, gamma, three),
    kv(Map, delta, four),

    setof(Key-Value,kv(Map,Key,Value),Pairs),
    Pairs == [ alpha-one, beta-two, delta-four, gamma-three ].

'iterate values' :-
    kv(Map, alpha, greek),
    kv(Map, beta, greek),
    kv(Map, aleph, hebrew),
    kv(Map, beth, hebrew),

    setof(Letter,kv(Map,Letter,greek),Greek),
    Greek == [alpha, beta],

    setof(Letter,kv(Map,Letter,hebrew),Hebrew),
    Hebrew == [aleph, beth].
