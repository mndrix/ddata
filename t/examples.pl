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
