:- module(hmap, [kv/3]).

% node(Hash,Key,Value)

kv(Map,Key,Value) :-
    term_hash(Key,Hash),
    Depth is floor(log(Hash)/log(4)),  % log4(Hash)
    kv(Depth,Hash,Map,Hash,Key,Value).

kv(0,_P,node(Hash,Key,Value,_,_,_,_),Hash,Key,Value) :-
    !.
kv(Depth,P,node(H,K,V,A,B,C,D),Hash,Key,Value) :-
    Depth > 0,
    Node = node(H,K,V,A,B,C,D),
    N is 4 + (P /\ 0b11),
    arg(N,Node,Child),
    Depth1 is Depth - 1,
    P1 is P >> 2,
    kv(Depth1,P1,Child,Hash,Key,Value).


show(Map) :-
    show(Map,0).

show(Map,Indent) :-
    var(Map),
    !,
    indent(Indent),
    format(".~n").
show(node(_H,K,V,A,B,C,D),Indent) :-
    indent(Indent),
    format("~p => ~p~n", [K,V]),
    succ(Indent,NextIndent),
    show(A,NextIndent),
    show(B,NextIndent),
    show(C,NextIndent),
    show(D,NextIndent).

indent(N) :-
    forall( between(1,N,_), write("    ") ).
