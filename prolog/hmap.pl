:- module(hmap, [kv/3]).

% node(Hash,Key,Value)

kv(Map,Key,Value) :-
    term_hash(Key,Hash),
    Depth is floor(log(Hash)/log(2)),  % log2(Hash)
    kv(Depth,Hash,Map,Hash,Key,Value).

kv(0,_P,node(Hash,Key,Value,_,_),Hash,Key,Value) :-
    !.
kv(Depth,P,node(H,K,V,A,B),Hash,Key,Value) :-
    Depth > 0,
    Node = node(H,K,V,A,B),
    N is 4 + (P /\ 1),
    arg(N,Node,Child),
    Depth1 is Depth - 1,
    P1 is P >> 1,
    kv(Depth1,P1,Child,Hash,Key,Value).


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
