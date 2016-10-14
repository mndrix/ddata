:- module(pmap, [
    cons/4,
    delete/3,
    insert/4,
    keys/2,
    kv//2,
    kv/3,
    pairs/2,
    size/2
]).
:- use_module(library(ddata/map), []).

/*
Goal:

A fast, persistent map supporting unification and reversible predicates.

Design:

A pmap is a key-value map stored as a hash array mapped trie.  A node in the
tree can be one of three kinds:

  * empty
  * plump
  * trim

An `empty` node contains no key-value pairs.  A `plump` node contains N
subtrees, but doesn't contain any key-value pairs within itself.  If all the
subtrees of a `plump` node together contain only a single key-value pair, the
`plump` node can be collapsed into a `trim` node containing that key-value pair.

A `trim` node represents a `plump` subtree that's been trimmed off to a single
element. Conceptually, the key-value pair still resides at a leaf somewhere
beneath the `plump` node, but that entire portion of the tree is optimized into
a single node to avoid storing and searching the entire, deep structure.

*/

% tree configuration parameters
plump_width(8).
plump_shift(3).
plump_mask(0b111).

%% insert(+Key,?Value,+Without,?With) is semidet.
%% insert(+Key,?Value,?Without,+With) is semidet.
%% insert(?Key,?Value,?Without,+With) is multi.
%
%  True if With maps Key to Value, Without has no mapping for Key and all other
%  mappings are identical. This is the fundamental pmap operation upon which all
%  others are built.
insert(Key,Value,Without,With) :-
    % validate the mode
    ( ground(Key), nonvar(Without) -> true
    ; ground(Key), nonvar(With) -> true
    ; var(Key), nonvar(With) ->
        kv(With,Key,_)
    ; otherwise ->
        throw('Invalid mode for insert/4')
    ),

    ddata_map:hash(Key,Hash),
    insert(0,Hash,Key,Value,Without,With).


trim(trim(_,_,_,_)).
trim_depth(trim(Depth,_,_,_), Depth).
trim_hash(trim(_,Hash,_,_), Hash).
trim_key(trim(_,_,Key,_), Key).
trim_value(trim(_,_,_,Value), Value).


plump(plump(_,_,_,_,_,_,_,_)).


% true if all arguments of P are 'empty'
empty_plump(P) :-
    plump(P),
    plump_width(Width),
    empty_plump_(Width,P).

empty_plump_(0,_) :- !.
empty_plump_(N0,P) :-
    arg(N0,P,empty),
    succ(N,N0),
    empty_plump_(N,P).


% two plump nodes (A0 and B0) are identical to each other except for the child
% in position N.  A has ChildA in that position; B has ChildB.  ChildA and
% ChildB are different from one another.
differ_in_one_child(A0,B0,N,ChildA,ChildB) :-
    plump(A0),
    plump(B0),
    arg(N,A0,ChildA),
    arg(N,B0,ChildB),
    differ_in_one_child_(N,A0,B0),
    dif(ChildA,ChildB).

differ_in_one_child_(
    1,
    plump(_,A,B,C,D,E,F,G),
    plump(_,A,B,C,D,E,F,G)
).
differ_in_one_child_(
    2,
    plump(A,_,B,C,D,E,F,G),
    plump(A,_,B,C,D,E,F,G)
).
differ_in_one_child_(
    3,
    plump(A,B,_,C,D,E,F,G),
    plump(A,B,_,C,D,E,F,G)
).
differ_in_one_child_(
    4,
    plump(A,B,C,_,D,E,F,G),
    plump(A,B,C,_,D,E,F,G)
).
differ_in_one_child_(
    5,
    plump(A,B,C,D,_,E,F,G),
    plump(A,B,C,D,_,E,F,G)
).
differ_in_one_child_(
    6,
    plump(A,B,C,D,E,_,F,G),
    plump(A,B,C,D,E,_,F,G)
).
differ_in_one_child_(
    7,
    plump(A,B,C,D,E,F,_,G),
    plump(A,B,C,D,E,F,_,G)
).
differ_in_one_child_(
    8,
    plump(A,B,C,D,E,F,G,_),
    plump(A,B,C,D,E,F,G,_)
).


nth_child(N,Plump,Child) :-
    plump(Plump),
    arg(N,Plump,Child).


% insert(+Depth:nonneg,+Hash,+Key,?Value,?Without,?With)
insert(Depth,Hash,Key,Value,empty,Trim) :-
    trim_depth(Trim,Depth),
    trim_hash(Trim,Hash),
    trim_key(Trim,Key),
    trim_value(Trim,Value),
    !.
insert(Depth,Hash,K,V,Trim,With) :-
    plump(With),
    trim_depth(Trim,Depth),
    trim_hash(Trim,TrimHash),
    TrimHash \== Hash,  % implies that Trim's key \= K
    trim_as_plump(Trim,Without),
    insert_plumps(Depth,Hash,K,V,Without,With),
    !.
insert(Depth,Hash,K,V,Without,With) :-
    insert_plumps(Depth,Hash,K,V,Without,With).

insert_plumps(Depth,Hash,K,V,Without,With) :-
    plump(Without),
    plump(With),
    hash_depth_n(Hash,Depth,N),
    nth_child(N,With,ChildWith),
    nth_child(N,Without,ChildWithout),
    succ(Depth,Depth1),
    ( ground(Hash), nonvar(ChildWithout) ->
        insert(Depth1,Hash,K,V,ChildWithout,ChildWith),
        differ_in_one_child(Without,With,N,ChildWithout,ChildWith)
    ;
        differ_in_one_child(Without,With,N,ChildWithout,ChildWith),
        insert(Depth1,Hash,K,V,ChildWithout,ChildWith)
    ).


hash_depth_n(Hash,Depth,N) :-
    when((ground(Hash),ground(Depth)), hash_depth_n_(Hash,Depth,N)).

hash_depth_n_(Hash,Depth,N) :-
    plump_shift(Shift),
    plump_mask(Mask),
    N is ((Hash >> (Shift*Depth)) /\ Mask) + 1.


%% kv(+Map,+Key,?Value) is semidet.
%% kv(+Map,?Key,?Value) is multi.
%
%  True if Map maps Key to Value.
kv(Map,Key,Value) :-
    ground(Key),
    !,
    insert(Key,Value,_,Map).
kv(Map,Key,Value) :-
    kv_(Map,Key,Value).

kv_(Trim,Key,Value) :-
    trim_key(Trim,Key),
    trim_value(Trim,Value).
kv_(Plump,Key,Value) :-
    plump(Plump),
    nth_child(_,Plump,Child),
    kv_(Child,Key,Value).


%% kv(Key,Value)//
%
%  Identical to kv/3 but designed for use in a DCG with the Map as state.
kv(Key,Value,Map,Map) :-
    kv(Map,Key,Value).


% trim_as_plump(?Trim,?AsPlump)
trim_as_plump(Trim0,AsPlump) :-
    plump(AsPlump),

    % describe trim element at Depth0
    trim_depth(Trim0,Depth0),
    trim_hash(Trim0,Hash),
    trim_key(Trim0,Key),
    trim_value(Trim0,Value),

    % describe trim element at Depth + 1
    trim_depth(Trim1,Depth1),
    trim_hash(Trim1,Hash),
    trim_key(Trim1,Key),
    trim_value(Trim1,Value),
    succ(Depth0,Depth1),

    % relate empty plump to plump containing deeper trim element
    hash_depth_n(Hash,Depth0,N),
    empty_plump(Empty),
    differ_in_one_child(Empty,AsPlump,N,empty,Trim1).


:- multifile quickcheck:arbitrary/2.
quickcheck:arbitrary(pmap,Map) :-
    quickcheck:arbitrary(pmap(any,any),Map).
quickcheck:arbitrary(pmap(K,V),Map) :-
    quickcheck:arbitrary(list(K),Keys0),
    sort(Keys0,Keys), % remove duplicates
    length(Keys,Len),
    length(Vals,Len),
    maplist(quickcheck:arbitrary(V),Vals),
    foldl(insert,Keys,Vals,empty,Map).


%% size(+Map:pmap, -N:nonneg) is det.
%
%  True if Map has N keys.
size(Map,N) :-
    must_be(nonvar,Map),
    size_(Map,N).

size_(empty, 0) :-
    !.
size_(Trim, 1) :-
    trim(Trim),
    !.
size_(Plump,N) :-
    bagof(Len,N^Child^(nth_child(N,Plump,Child),size(Child,Len)),Lens),
    sumlist(Lens, N).


%% keys(+Map:pmap, -Keys:list) is det.
%% keys(?Map:pmap, +Keys:list) is det.
%
%  True if Map has a key for each of Keys.
keys(Map,Keys) :-
    ( nonvar(Keys) ->  % insert/4 can't yet handle an unbound Key
        foldl(insert,Keys,_Vals,empty,Map)
    ; nonvar(Map) ->
        findall(Key,kv(Map,Key,_),Keys)
    ; otherwise ->
        throw('In keys/2, one of the arguments must be nonvar')
    ).


%% delete(+Key,+Map0:pmap,-Map:pmap) is semidet.
%% delete(+Key,-Map0:pmap,+Map:pmap) is semidet.
%
%  True if removing Key from Map0 yields Map.  A convenience
%  wrapper around insert/4.
delete(Key,MapWith,MapWithout) :-
    insert(Key,_Val,MapWithout,MapWith).


%% cons(+Key,?Value,+Map0,-Map) is semidet.
%% cons(?Key,?Value,?Map0,+Map) is semidet.
%
%  True if Key maps to Value in Map but is absent from Map0. This is identical
%  to insert/4 but it arbitrarily chooses a single mapping to remove (instead of
%  iterating) when called without a key.  It can be helpful for incrementally
%  building and deconstructing a map.
cons(Key,Value,MapWithout,MapWith) :-
    once(insert(Key,Value,MapWithout,MapWith)).


%% pairs(+Map,-KVs:list) is det.
%% pairs(-Map,+KVs:list) is semidet.
%
%  True if the pairs in KVs represent the mappings in Map.  Although many
%  orderings of KVs would produce the same Map, one ordering is chosen
%  arbitrarily when KVs is unbound.
pairs(Map,KVs) :-
    pairs(KVs,empty,Map).

pairs([],Map,Map) :-
    !.  % optimization (trim empty choicepoint)
pairs([K-V|KVs],Map0,Map) :-
    % See Note_pipeline
    ( nonvar(Map) ->
        cons(K,V,Map1,Map),
        pairs(KVs,Map0,Map1)
    ; otherwise ->
        cons(K,V,Map0,Map1),
        pairs(KVs,Map1,Map)
    ).

/*
Note_pipeline:

It's a little annoying that we have to repeat ourselves within the two branches
of the second clause of pairs/3.  The branches are identical except for the
order of variables.  It seems like there should be a way to describe the
pipeline and allow the compiler to assign variables for us.

This is very similar to the way that DCG notation works.  If I had described
pairs/3 using DCG notation (as pairs//1), it would have generated the variable
ordering in the second branch.  Standard DCG notation has no way to generate
the ordering in the first branch.

*/
