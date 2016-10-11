:- use_module(library(ddata/pmap)).

% a map's size is the number of keys it has
prop_keys_size(Map:pmap(atom,integer)) :-
    size(Map,MapSize),
    keys(Map,Keys),
    length(Keys,KeysLen),
    KeysLen == MapSize.

:- use_module(library(quickcheck)).
:- use_module(library(tap)).

quickcheck(prop_keys_size/1).
