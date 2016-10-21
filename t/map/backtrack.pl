:- use_module(library(ddata/map)).

:- use_module(library(tap)).

'backtracking undoes kv' :-
    kv(Map,a,one),
    kv(Map,b,two),
    DeepPairs = deep_pairs([]),
    ( kv(Map,c,three),
      kv(Map,d,four),
      pairs(Map,Pairs0),
      keysort(Pairs0,Pairs),
      nb_setarg(1,DeepPairs,Pairs),
      fail
    ; DeepPairs = deep_pairs(OldPairs),
      OldPairs == [a-one,b-two,c-three,d-four],
      pairs(Map,NewPairs0),
      keysort(NewPairs0,NewPairs),
      NewPairs == [a-one,b-two]
    ).
