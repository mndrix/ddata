:- module(ddata_map, [kv/3]).
:- use_module(library(sha),[sha_hash/3]).

/*
This library is implemented in a very non-logical fashion.  Because
it uses hash functions (an inherently one way operation) and attributed
variables (a non-logical optimization, in this case), that's the price
we pay.

However, the publicly accessible APIs (kv/3, etc) should
present the illusion of logical operations.  As long as
functional/imperative code stays hidden beneath the covers,
we'll be fine.
*/


%% hash(+Term,-Hash:integer) is det.
%
%  Calculate a hash for a ground Term.
:- if(fail).
% for testing: term_hash/2 collides more often than SHA1
hash(Term,Hash) :-
    must_be(ground,Term),
    term_hash(Term,Hash).
:- else.
hash(Term,Hash) :-
    must_be(ground,Term),
    format(string(S),'~k',[Term]),
    sha_hash(S,Bytes,[algorithm(sha1)]),
    bytes_int(Bytes,Hash).
:- endif.


bytes_int(Bytes,N) :-
    bytes_int(Bytes,0,N).

bytes_int([],Sum,Sum).
bytes_int([Byte|Bytes],N0,N) :-
    N1 is N0 << 8 + Byte,
    bytes_int(Bytes,N1,N).


%% attr(Var, Value)
%
%  True if Var has =|ddata_map|= attribute of Value.
attr(Var,Value) :-
    ( nonvar(Var) ->
        fail
    ; get_attr(Var,ddata_map,ExistingValue) ->
        Value = ExistingValue
    ; otherwise ->
        put_attr(Var,ddata_map,Value)
    ).


attr_unify_hook(LazyKvA,VarOrVal) :-
    ( get_attr(VarOrVal,ddata_map,LazyKvB) ->
        % two lazy nodes try using same attributed variable
        ( LazyKvA = LazyKvB ->
            % same key requires no more work
            true
        ; otherwise ->
            % differing keys require recursive inserts
            node(Node,_,_),
            VarOrVal = Node,
            LazyKvA = lazy_kv(DepthA,PartialA,KeyA,ValueA),
            kv(DepthA,PartialA,Node,KeyA,ValueA),
            LazyKvB = lazy_kv(DepthB,PartialB,KeyB,ValueB),
            kv(DepthB,PartialB,Node,KeyB,ValueB)
        )
    ; var(VarOrVal) ->
        throw("eager node is a variable. should never happen")
    ; otherwise ->
        % insert our lazy key-value into this eager node
        LazyKvA = lazy_kv(Depth,P,Key,Value),
        kv(Depth,P,VarOrVal,Key,Value)
    ).


kv(Map,Key,Value) :-
    var(Key),
    !,
    unknown_key(Map,Key,Value).
kv(Map,Key,Value) :-
    hash(Key,Hash),
    % n = Hash + 1, because hashes can be zero
    hash_depth(Hash+1,Depth),
    kv(Depth,Hash,Map,Key,Value).

kv(0,_P,Node,Key,Value) :-
    node(Node,ExistingKey,ExistingValue),
    !,
    ( Key = ExistingKey -> true; throw(collision(Key,ExistingKey)) ),
    Value = ExistingValue.
kv(Depth,P,Node,Key,Value) :-
    % postpone walking the tree until later
    attr(Node,lazy_kv(Depth,P,Key,Value)),
    !.
kv(Depth,P,Node,Key,Value) :-
    Depth > 0,
    node(Node,_,_),
    N is 3 + (P /\ 0b111),
    arg(N,Node,Child),
    Depth1 is Depth - 1,
    P1 is P >> 3,
    kv(Depth1,P1,Child,Key,Value).


hash_depth(N,Depth) :-
    Depth is ceil(log(7*N+1)/log(8))-1.  % ceil(log8(7n+1))-1


node(Node,Key,Value) :-
    functor(Node,node,10 /*2+8*/),
    arg(1,Node,Key),
    arg(2,Node,Value).


% non-logical stuff is to avoid instantiating attributed variables
% while traversing the tree
unknown_key(Node,Key,Value) :-
    get_attr(Node,ddata_map,lazy_kv(_Depth,_Partial,Key,Value)),
    !.
unknown_key(Node,Key,Value) :-
    nonvar(Node),
    ( node(Node,Key,Value),
      ground(Key)
    ; between(3,10,N),
      arg(N,Node,Child),
      unknown_key(Child,Key,Value)
    ).


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
