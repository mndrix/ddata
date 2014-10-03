:- set_prolog_flag(hmap_wants_collision,true).
:- use_module(library(hmap)).

:- use_module(library(tap)).

% two floats that collide under term_hash/2
one(fail) :-
    kv(Map,0.057333229583460106,_),
    kv(Map,0.0017627580259546528,_).
