:- module(hmap, [kv/3]).

% node(Hash,Key,Value)

kv(Map,Key,Value) :-
    term_hash(Key,Hash),
    %debug(hmap,"~2r -> ~w", [Hash,Key]),
    Mask is 1 << (Hash /\ 0xF),
    Partial is Hash >> 4,
    kv(Mask,Partial,Map,Hash,Key,Value).

kv(0,_P,node(Hash,Key,Value,_L,_R),Hash,Key,Value) :-
    !.
kv(Mask0,P,node(_H,_K,_V,L,R),Hash,Key,Value) :-
    Mask0 \== 0,
    Mask1 is Mask0 >> 1,
    ( 0 is Hash /\ Mask0 -> Child=L; Child=R ),
    kv(Mask1,P,Child,Hash,Key,Value).


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
