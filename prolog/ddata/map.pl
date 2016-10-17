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
            kv_plump(Plump,HashA,KeyA,ValueA),
            kv_plump(Plump,HashB,KeyB,ValueB)
        )
    ; var(VarOrVal) ->
        throw("eager node is a variable. should never happen")
    ; otherwise ->
        % insert our lazy key-value into this eager node
        LazyKvA = lazy_kv(Hash,Key,Value),
        kv_plump(VarOrVal,Hash,Key,Value)
    ).


kv(Map,Key,Value) :-
    var(Key),
    !,
    unknown_key(Map,Key,Value).
kv(Map,Key,Value) :-
    pmap:hash(Key,Hash),
    put_attr(X,ddata_map,lazy_kv(Hash,Key,Value)),
    Map = X.


kv_plump(Map,Hash,Key,Value) :-
    pmap:plump(Map),
    pmap:hash_residue_n(Hash,Residue,N),
    put_attr(Child,ddata_map,lazy_kv(Residue,Key,Value)),
    arg(N,Map,Child).


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
