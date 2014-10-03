:- module(hmap, [kv/3]).


%% hash(+Term,-Hash:integer) is det.
%
%  Calculate a hash for a ground Term.
hash(Term,Hash) :-
    must_be(ground,Term),
    variant_sha1(Term,Hex),
    hex_int(Hex,Hash).

hex_int(Hex,Int) :-
    atom_codes(Hex,Codes),
    foldl(sum_hex,Codes,0,Int).

sum_hex(Char,Accum0,Accum) :-
    hex_val(Char,N),
    Accum is (Accum0 << 4) + N.

hex_val(0'0,0).
hex_val(0'1,1).
hex_val(0'2,2).
hex_val(0'3,3).
hex_val(0'4,4).
hex_val(0'5,5).
hex_val(0'6,6).
hex_val(0'7,7).
hex_val(0'8,8).
hex_val(0'9,9).
hex_val(0'a,10).
hex_val(0'b,11).
hex_val(0'c,12).
hex_val(0'd,13).
hex_val(0'e,14).
hex_val(0'f,15).


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
