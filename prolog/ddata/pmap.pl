:- module(ddata_pmap, [delta/4,kv/3]).
:- use_module(library(ddata/map), []).

/*
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

delta(Key,Value,Without,With) :-
    must_be(ground,Key),
    once( nonvar(Without)
        ; nonvar(With)
        ; throw('In delta/4, one of Without or With must be nonvar')
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


map_children(P,Goal) :-
    plump(P),
    functor(P,_,N),
    map_children_(N,P,Goal).

map_children_(0,_,_) :-
    !.
map_children_(N,Plump,Goal) :-
    N > 0,
    arg(N,Plump,Child),
    call(Goal,Child),
    N0 is N - 1,
    map_children_(N0,Plump,Goal).


nth_child(N,Plump,Child) :-
    plump(Plump),
    arg(N,Plump,Child).


map_args(Goal,TermA) :-
    functor(TermA,_,Arity),
    map_args_(Arity,Goal,TermA).

map_args_(0,_,_) :-
    !.
map_args_(N,Goal,TermA) :-
    arg(N,TermA,A),
    once(call(Goal,N,A)),
    N0 is N-1,
    map_args_(N0,Goal,TermA).


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



%% known_key(+Depth:nonneg,+Hash,+Key,?Value,?Without,?With)
insert(Depth,Hash,Key,Value,empty,Trim) :-
    trim_depth(Trim,Depth),
    trim_hash(Trim,Hash),
    trim_key(Trim,Key),
    trim_value(Trim,Value),
    !.
insert(Depth,Hash,K,V,Trim,With) :-
    trim_depth(Trim,Depth),
    trim_hash(Trim,TrimHash),
    dif(TrimHash,Hash),  % implies that Trim's key \= K
    trim_as_plump(Trim,Without),
    insert_plumps(Depth,Hash,K,V,Without,With),
    !.
insert(Depth,Hash,K,V,Without,With) :-
    insert_plumps(Depth,Hash,K,V,Without,With).

insert_plumps(Depth,Hash,K,V,Without,With) :-
    hash_depth_n(Hash,Depth,N),
    nth_child(N,With,ChildWith),
    nth_child(N,Without,ChildWithout),
    succ(Depth,Depth1),
    insert(Depth1,Hash,K,V,ChildWithout,ChildWith),
    differ_in_one_child(Without,With,N,ChildWithout,ChildWith).


hash_depth_n(Hash,Depth,N) :-
    when((ground(Hash),ground(Depth)), hash_depth_n_(Hash,Depth,N)).

hash_depth_n_(Hash,Depth,N) :-
    N is ((Hash >> (3*Depth)) /\ 0b111) + 1.


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
    between(1,8,N),
    nth_child(N,Plump,Child),
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
    succ(Depth0,Depth1),
    trim_depth(Trim1,Depth1),
    trim_hash(Trim1,Hash),
    trim_key(Trim1,Key),
    trim_value(Trim1,Value),

    % relate empty plump to plump containing deeper trim element
    hash_depth_n(Hash,Depth0,N),
    empty_plump(Empty),
    differ_in_one_child(Empty,AsPlump,N,empty,Trim1).
