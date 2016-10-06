% DCG notation can be a helpful way to describe
% persistent maps.

:- use_module(library(ddata/pmap)).
:- use_module(library(clpfd)).

pairs([K-V|Pairs]) -->
    delta(K,V),
    pairs(Pairs).
pairs([]) -->
    [].

% map[sum] = map[x] + map[y]
sum_this -->
    { Sum #= X + Y },
    kv(x,X),
    kv(y,Y),
    delta(sum,Sum).

:- use_module(library(tap)).

created :-
    phrase(pairs([x-7,y-2]),empty,Map),
    size(Map,N),
    N == 2,
    kv(Map,x,X),
    X == 7,
    kv(Map,y,Y),
    Y == 2.


summing :-
    pairs(Map0,[x-7, y-2]),
    phrase(sum_this,Map0,Map),
    kv(Map,sum,Sum),
    Sum == 9.
