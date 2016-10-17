:- module(ddata_map, [kv/3]).
:- use_module(library(ddata/pmap), []).

/*
Goal:

A fast, declarative map supporting unification and compatibility with pmap.

Design:

A map is a key-value map stored as a hash array mapped trie.  A node in the trie
can be one of three kinds:

 * variable without attributes
 * variable with an attribute
 * plump

A variable without any attributes represents an empty subtree.

A variable with an attribute represents a subtree with only a single node.
Conceptually it's a lazy representation of the entire subtree where we haven't
yet performed the work of pushing the key-value pair all the way to a leaf.

A `plump` represents an internal node in the trie, containing several subtrees.

*/


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
        LazyKvA = lazy_kv(HashA,KeyA,ValueA),
        LazyKvB = lazy_kv(HashB,KeyB,ValueB),
        ( KeyA==KeyB ->
            ValueA=ValueB
        ; otherwise ->
            % differing keys require recursive inserts
            pmap:plump(Plump),
            VarOrVal = Plump,
            kv(HashA,Plump,KeyA,ValueA),
            kv(HashB,Plump,KeyB,ValueB)
        )
    ; var(VarOrVal) ->
        throw("eager node is a variable. should never happen")
    ; otherwise ->
        % insert our lazy key-value into this eager node
        LazyKvA = lazy_kv(Hash,Key,Value),
        kv(Hash,VarOrVal,Key,Value)
    ).


kv(Map,Key,Value) :-
    var(Key),
    !,
    unknown_key(Map,Key,Value).
kv(Map,Key,Value) :-
    pmap:hash(Key,Hash),
    kv(Hash,Map,Key,Value).


kv(Hash,Map,Key,Value) :-
    ( get_attr(Map,ddata_map,lazy_kv(OtherHash,OtherKey,OtherValue)) ->
        ( Key==OtherKey ->
            !,
            Value=OtherValue
        ; otherwise ->
            % push keys down to a lower level
            del_attr(Map,ddata_map),
            pmap:plump(Map),
            kv(OtherHash,Map,OtherKey,OtherValue),
            kv(Hash,Map,Key,Value)
        )
    ; var(Map) ->
        put_attr(Map,ddata_map,lazy_kv(Hash,Key,Value))
    ),
    !.
kv(Hash,Map,Key,Value) :-
    pmap:plump(Map),
    pmap:hash_residue_n(Hash,Residue,N),
    arg(N,Map,Child),
    kv(Residue,Child,Key,Value).


hash_depth(N,Depth) :-
    Depth is ceil(log(7*N+1)/log(8))-1.  % ceil(log8(7n+1))-1


node(Node,Key,Value) :-
    functor(Node,node,10 /*2+8*/),
    arg(1,Node,Key),
    arg(2,Node,Value).


% non-logical stuff is to avoid instantiating attributed variables
% while traversing the tree
unknown_key(Map,Key,Value) :-
    get_attr(Map,ddata_map,lazy_kv(_Hash,Key,Value)),
    !.
unknown_key(Map,Key,Value) :-
    nonvar(Map),
    pmap:plump(Map),
    arg(_,Map,Child),
    unknown_key(Child,Key,Value).
