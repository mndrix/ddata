:- use_module(library(ddata/pmap)).

greek(Map) :-
    foldl(
        delta,
        [alpha,beta,gamma,delta],
        [one,  two, three,four ],
        empty,Map
    ).

:- use_module(library(tap)).

'three insertions' :-
    % declare map contents
    delta(hello, world, empty,Map1),
    delta(goodbye, friends, Map1,Map2),
    delta(list, [1,2,3], Map2,Map),

    % can we fetch the right values?
    kv(Map,hello, World),
    World == world,
    kv(Map, goodbye, Friends),
    Friends == friends,
    kv(Map, list, List),
    List == [1,2,3].


'duplicate keys'(fail) :-
    delta(one, 1, empty, Map1),
    delta(one, won, Map1, _).


'iterate keys' :-
    greek(Map),

    setof(Key-Value,kv(Map,Key,Value),Pairs),
    Pairs == [ alpha-one, beta-two, delta-four, gamma-three ].

'iterate values' :-
    delta(alpha, greek, empty, Map1),
    delta(beta, greek, Map1, Map2),
    delta(aleph, hebrew, Map2, Map3),
    delta(beth, hebrew, Map3, Map),

    setof(Letter,kv(Map,Letter,greek),Greek),
    Greek == [alpha, beta],

    setof(Letter,kv(Map,Letter,hebrew),Hebrew),
    Hebrew == [aleph, beth].
