:- module(ddata_array, [nth/3]).
:- use_module(library(ddata/map), []).

%% nth(Array,N:positive_integer,Value)
%
%
nth(Array,N,Value) :-
    var(N),
    !,
    ddata_map:unknown_key(Array,N,Value).
nth(Array,N,Value) :-
    must_be(positive_integer,N),
    ddata_map:hash_depth(N,Depth),
    ddata_map:kv(Depth,N,Array,N,Value).
