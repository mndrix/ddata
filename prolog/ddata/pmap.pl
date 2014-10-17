:- module(ddata_pmap, [delta/4,kv/3]).
:- use_module(library(ddata/map), []).

delta(Key,Value,Without,With) :-
    must_be(ground,Key),
    ddata_map:hash(Key,Hash),
    insert(0,Hash,Key,Value,Without,With).


trim_depth(trim(Depth,_,_,_), Depth).
trim_hash(trim(_,Hash,_,_), Hash).
trim_key(trim(_,_,Key,_), Key).
trim_value(trim(_,_,_,Value), Value).


parent(P) :-
    functor(P,parent,8).


map_children(P,Goal) :-
    parent(P),
    functor(P,_,N),
    map_children_(N,P,Goal).

map_children_(0,_,_) :-
    !.
map_children_(N,Parent,Goal) :-
    N > 0,
    arg(N,Parent,Child),
    call(Goal,Child),
    N0 is N - 1,
    map_children_(N0,Parent,Goal).


nth_child(N,Parent,Child) :-
    parent(Parent),
    arg(N,Parent,Child).


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


unify_except_arg(ExceptN,X,Y) :-
    map_args(unify_except_arg_(ExceptN),X,Y).

unify_except_arg_(N,N,_,_).
unify_except_arg_(ExceptN,N,X,X) :-
    N \= ExceptN.


/*
%% unknown_key(-K,?V,?Without,?With)
unknown_key(K,V,empty,Trim) :-
    % With has 1 key
    trim_depth(Trim,0),
    trim_key(Trim,K),
    trim_value(Trim,V).
unknown_key(K,V,Peer0,Parent) :-
    % With has 2 keys
    trim_depth(Peer0,0),
    trim_key(Peer0,Kpeer),
    dif(K,Kpeer),
    ( % peers belong in different subtrees
      trim_pushdown(1,Peer0,Parent),
      trim_depth(Us,0),
      trim_key(Us,Key),
      trim_value(Us,Value),
      trim_pushdown(Us,Parent),
      todo("other children are empty/0")

    ; % peers belong in same subtree
      single_child(Parent,Child),
      unknown_key(Kpeer,Vpeer,Peer0,Child),
      unknown_key(K,V,Trim0,Child)
    ).
unknown_key(K,V,ParentA,ParentB) :-
    % With has >2 keys
    todo.
*/


%% known_key(+Depth:nonneg,+Hash,+Key,?Value,?Without,?With)
insert(Depth,Hash,Key,Value,empty,Trim) :-
    Trim=trim(Depth,Hash,Key,Value).
insert(Depth,Hash,K,V,Trim,Parent) :-
    trim_depth(Trim,Depth),
    parent(Parent),
    trim_pushdown(Trim,Parent0),
    insert(Depth,Hash,K,V,Parent0,Parent).
insert(Depth0,Hash,K,V,Parent0,Parent) :-
    succ(Depth0,Depth),
    hash_depth_n(Hash,Depth,N),
    nth_child(Parent0,N,Child0),
    nth_child(Parent,N,Child),
    unify_except_arg(N,Parent0,Parent),
    insert(Depth,Hash,K,V,Child0,Child).


hash_depth_n(Hash,Depth,N) :-
    N is (Hash >> (3*Depth)) /\ 0b111.


kv(Map,Key,Value) :-
    ground(Key),
    !,
    delta(Key,Value,_,Map).
kv(_Map,_Key,_Value) :-
    todo("iterate each node in the tree").


% trim_pushdown(?Trim,?AsParent)
trim_pushdown(Trim0,AsParent) :-
    Trim0 = trim(Depth,Hash,Key,Value),
    parent(AsParent),
    succ(Depth,Depth1),
    Trim = trim(Depth1,Hash,Key,Value),
    hash_depth_n(Hash,Depth,N),
    arg(N,AsParent,Trim),
    map_args(trim_pushdown_(N),AsParent).

trim_pushdown_(N,N,_).
trim_pushdown_(N,M,empty) :-
    N \= M.


/*
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
*/
