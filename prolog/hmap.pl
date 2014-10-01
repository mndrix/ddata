:- module(hmap, [kv/3]).

% node(Hash,Key,Value)

kv(Map,Key,Value) :-
    term_hash(Key,Hash),
    Depth is floor(log(Hash)/log(2)),  % log2(Hash)
    Mask is 1 << Depth,
    kv(Mask,Map,Hash,Key,Value).

kv(1,node(Hash,Key,Value,_L,_R),Hash,Key,Value) :-
    !.
kv(Mask0,node(_H,_K,_V,L,R),Hash,Key,Value) :-
    Mask0 >= 1,
    ( 0 is Hash /\ Mask0 -> Child=L; Child=R ),
    Mask1 is Mask0 >> 1,
    kv(Mask1,Child,Hash,Key,Value).


show(Map) :-
    show(Map,0).

show(Map,Indent) :-
    var(Map),
    !,
    indent(Indent),
    format(".~n").
show(node(_H,K,V,L,R),Indent) :-
    indent(Indent),
    format("~p => ~p~n", [K,V]),
    succ(Indent,NextIndent),
    show(L,NextIndent),
    show(R,NextIndent).

indent(N) :-
    forall( between(1,N,_), write("    ") ).
