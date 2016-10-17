:- module(ddata_array, [nth/3]).

%% nth(Array,N:positive_integer,Value)
%
%
nth(Array,N,Value) :-
    var(N),
    !,
    unknown_key(Array,N,Value).
nth(Array,N,Value) :-
    must_be(positive_integer,N),
    hash_depth(N,Depth),
    kv(Depth,N,Array,N,Value).


%% attr(Var, Value)
%
%  True if Var has =|ddata_array|= attribute of Value.
attr(Var,Value) :-
    ( nonvar(Var) ->
        fail
    ; get_attr(Var,ddata_array,ExistingValue) ->
        Value = ExistingValue
    ; otherwise ->
        put_attr(Var,ddata_array,Value)
    ).


attr_unify_hook(LazyKvA,VarOrVal) :-
    ( get_attr(VarOrVal,ddata_array,LazyKvB) ->
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
    get_attr(Node,ddata_array,lazy_kv(_Depth,_Partial,Key,Value)),
    !.
unknown_key(Node,Key,Value) :-
    nonvar(Node),
    ( node(Node,Key,Value),
      ground(Key)
    ; between(3,10,N),
      arg(N,Node,Child),
      unknown_key(Child,Key,Value)
    ).
