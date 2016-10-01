:- module(ddata_pmap, [delete/3,delta/4,keys/2,kv/3,size/2]).
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

%% delta(+Key,?Value,+Without,?With) is semidet.
%% delta(+Key,?Value,?Without,+With) is semidet.
%% delta(?Key,?Value,?Without,+With) is multi.
%
%  True if With maps Key to Value, Without has no mapping for Key and all other
%  mappings are identical. This is the fundamental pmap operation upon which all
%  others are built.
delta(Key,Value,Without,With) :-
    % validate the mode
    ( ground(Key), nonvar(Without) -> true
    ; ground(Key), nonvar(With) -> true
    ; var(Key), nonvar(With) ->
        kv(With,Key,_)
    ; otherwise ->
        throw('Invalid mode for delta/4')
    ),

    ddata_map:hash(Key,Hash),
    insert(0,Hash,Key,Value,Without,With).


trim(trim(_,_,_,_)).
trim_depth(trim(Depth,_,_,_), Depth).
trim_hash(trim(_,Hash,_,_), Hash).
trim_key(trim(_,_,Key,_), Key).
trim_value(trim(_,_,_,Value), Value).


plump(P) :-
    functor(P,plump,8).


empty_plump(P) :-
    plump(P),
    foreach(between(1,8,N),arg(N,P,empty)).


% two plump nodes (A0 and B0) are identical to each other except for the child
% in position N.  A has ChildA in that position; B has ChildB.  ChildA and
% ChildB are different from one another.
differ_in_one_child(A0,B0,N,ChildA,ChildB) :-
    plump(A0),
    plump(B0),
    nth_child(N,A0,ChildA),
    nth_child(N,B0,ChildB),
    map_args(differ_(N),A0,B0).

differ_(N,N,A,B) :-
    dif(A,B).
differ_(N,M,A,A) :-
    dif(N,M).


nth_child(N,Plump,Child) :-
    plump(Plump),
    arg(N,Plump,Child).


% call once(Goal(N,A,B)) for each corresponding argument of TermA and TermB. N
% is the argument's index (1-based). A is the argument for TermA.  B is the
% argument for TermB.
%
% There's no guarantee about the order in which arguments
% are traversed.  TermA and TermB must have the same functor.
map_args(Goal,TermA,TermB) :-
    functor(TermA,F,Arity),
    functor(TermB,F,Arity),
    map_args_(Arity,Goal,TermA,TermB).

map_args_(0,_,_,_) :-
    !.
map_args_(N,Goal,TermA,TermB) :-
    arg(N,TermA,A),
    arg(N,TermB,B),
    once(call(Goal,N,A,B)),
    N0 is N-1,
    map_args_(N0,Goal,TermA,TermB).



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
    dif(TrimHash,Hash),  % implies that Trim's key \= K
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
    differ_in_one_child(Without,With,N,ChildWithout,ChildWith),
    insert(Depth1,Hash,K,V,ChildWithout,ChildWith).


hash_depth_n(Hash,Depth,N) :-
    when((ground(Hash),ground(Depth)), hash_depth_n_(Hash,Depth,N)).

hash_depth_n_(Hash,Depth,N) :-
    N is ((Hash >> (3*Depth)) /\ 0b111) + 1.


%% kv(+Map,+Key,?Value) is semidet.
%% kv(+Map,?Key,?Value) is multi.
%
%  True if Map maps Key to Value.
kv(Map,Key,Value) :-
    ground(Key),
    !,
    delta(Key,Value,_,Map).
kv(Map,Key,Value) :-
    kv_(Map,Key,Value).

kv_(Trim,Key,Value) :-
    trim_key(Trim,Key),
    trim_value(Trim,Value).
kv_(Plump,Key,Value) :-
    plump(Plump),
    nth_child(_,Plump,Child),
    kv_(Child,Key,Value).


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
    foldl(delta,Keys,Vals,empty,Map).


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
    ( nonvar(Keys) ->  % delta/4 can't yet handle an unbound Key
        foldl(delta,Keys,_Vals,empty,Map)
    ; nonvar(Map) ->
        findall(Key,kv(Map,Key,_),Keys)
    ; otherwise ->
        throw('In keys/2, one of the arguments must be nonvar')
    ).


%% delete(+Key,+Map0:pmap,-Map:pmap) is semidet.
%% delete(+Key,-Map0:pmap,+Map:pmap) is semidet.
%
%  True if removing Key from Map0 yields Map.  A convenience
%  wrapper around delta/4.
delete(Key,MapWith,MapWithout) :-
    delta(Key,_Val,MapWithout,MapWith).
