:- module(hmap, [kv/3]).


%% hash(+Term,-Hash:integer) is det.
%
%  Calculate a hash for a ground Term.
hash(Term,Hash) :-
    must_be(ground,Term),
    term_hash(Term,Hash).


kv(Map,Key,Value) :-
    hash(Key,Hash),
    % n = Hash + 1, because hashes can be zero
    Depth is ceil(log(7*(Hash+1)+1)/log(8))-1,  % ceil(log8(7n+1))-1
    kv(Depth,Hash,Map,Key,Value).

kv(0,_P,Node,Key,Value) :-
    node(Node,ExistingKey,ExistingValue),
    !,
    ( Key = ExistingKey -> true; throw(collision(Key,ExistingKey)) ),
    Value = ExistingValue.
kv(Depth,P,Node,Key,Value) :-
    Depth > 0,
    node(Node,_,_),
    N is 3 + (P /\ 0b111),
    arg(N,Node,Child),
    Depth1 is Depth - 1,
    P1 is P >> 3,
    kv(Depth1,P1,Child,Key,Value).


node(Node,Key,Value) :-
    functor(Node,node,10 /*2+8*/),
    arg(1,Node,Key),
    arg(2,Node,Value).


show(Map) :-
    show(Map,0).

show(Map,Indent) :-
    var(Map),
    !,
    indent(Indent),
    format(".~n").
show(Node,Indent) :-
    node(Node,K,V),
    indent(Indent),
    format("~p => ~p~n", [K,V]),
    succ(Indent,NextIndent),
    forall( between(3,10,N)
          , ( arg(N,Node,X)
            , show(X,NextIndent)
            )
          ).

indent(N) :-
    forall( between(1,N,_), write("    ") ).
