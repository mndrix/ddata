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
plump_width(32).
plump_shift(5).
plump_mask(0b11111).

differ_clause(N,(differ_in_one_child(N,A,B,ChildA,ChildB):-dif(ChildA,ChildB))) :-
    plump_width(Width),
    functor(A,plump,Width),
    functor(B,plump,Width),
    foreach(
        between(1,Width,I),
        differ_clause_(I,N,A,B,ChildA,ChildB)
    ).

differ_clause_(I,N,A,B,ChildA,ChildB) :-
    ( I==N -> arg(I,A,ChildA),arg(I,B,ChildB) ; arg(I,A,X),arg(I,B,X) ).


% generate clauses like:
% plump(_,_,_,_).
term_expansion(plump,plump(Plump)) :-
    plump_width(Width),
    functor(Plump,plump,Width).
% generate clauses like:
% empty_plump(plump(empty,empty,empty,empty)).
term_expansion(empty_plump,empty_plump(Plump)) :-
    plump_width(Width),
    functor(Plump,plump,Width),
    foreach(between(1,Width,N),arg(N,Plump,empty)).
% generate clauses like:
% differ_in_one_child(1,plump(X,A,B,C),plump(Y,A,B,C),X,Y) :- dif(X,Y).
term_expansion(differ_in_one_child,Terms) :-
    plump_width(Width),
    findall(
        Term,
        (between(1,Width,N),differ_clause(N,Term)),
        Terms
    ).


% trigger term_expansion
plump.
empty_plump.
differ_in_one_child.


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
    insert(Without,With,Hash,Key,Value).


trim(trim(_,_,_)).
trim_hash(trim(Hash,_,_), Hash).
trim_key(trim(_,Key,_), Key).
trim_value(trim(_,_,Value), Value).


nth_child(N,Plump,Child) :-
    plump(Plump),
    arg(N,Plump,Child).


% insert(+Depth:nonneg,+Hash,+Key,?Value,?Without,?With)
insert(empty,Trim,Hash,Key,Value) :-
    trim_hash(Trim,Hash),
    trim_key(Trim,Key),
    trim_value(Trim,Value),
    !.
insert(Trim,With,Hash,K,V) :-
    plump(With),
    trim_key(Trim,TrimKey),
    TrimKey \== K,
    trim_as_plump(Trim,Without),
    insert_plumps(Hash,K,V,Without,With),
    !.
insert(Without,With,Hash,K,V) :-
    insert_plumps(Hash,K,V,Without,With).

insert_plumps(Hash,K,V,Without,With) :-
    plump(Without),
    plump(With),
    hash_residue_n(Hash,Residue,N),
    nth_child(N,With,ChildWith),
    nth_child(N,Without,ChildWithout),
    ( ground(Hash), nonvar(ChildWithout) ->
        insert(ChildWithout,ChildWith,Residue,K,V),
        differ_in_one_child(N,Without,With,ChildWithout,ChildWith)
    ;
        differ_in_one_child(N,Without,With,ChildWithout,ChildWith),
        insert(ChildWithout,ChildWith,Residue,K,V)
    ).


hash_residue_n(Hash,Residue,N) :-
    ( ground(Hash) ->
        plump_mask(Mask),
        plump_shift(Shift),
        N is (Hash /\ Mask) + 1,
        Residue is Hash >> Shift
    ; ground(Residue), ground(N) ->
        plump_shift(Shift),
        Hash is (Residue << Shift) \/ (N-1)
    ; otherwise ->
        when(
            (ground(Hash);(ground(Residue),ground(N))),
            hash_residue_n(Hash,Residue,N)
        )
    ).


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
    trim_hash(Trim0,Hash),
    trim_key(Trim0,Key),
    trim_value(Trim0,Value),

    % describe trim element at Depth + 1
    trim_hash(Trim1,Residue),
    trim_key(Trim1,Key),
    trim_value(Trim1,Value),

    % relate empty plump to plump containing deeper trim element
    hash_residue_n(Hash,Residue,N),
    empty_plump(Empty),
    differ_in_one_child(N,Empty,AsPlump,empty,Trim1).


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
