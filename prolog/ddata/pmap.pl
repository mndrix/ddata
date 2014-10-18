:- module(ddata_pmap, [delta/4,kv/3]).
:- use_module(library(ddata/map), []).

delta(Key,Value,Without,With) :-
    must_be(ground,Key),
    ddata_map:hash(Key,Hash),
    insert(0,Hash,Key,Value,Without,With).


trim(trim(_,_,_,_)).
trim_depth(trim(Depth,_,_,_), Depth).
trim_hash(trim(_,Hash,_,_), Hash).
trim_key(trim(_,_,Key,_), Key).
trim_value(trim(_,_,_,Value), Value).


parent(P) :-
    functor(P,parent,8).


empty_parent(P) :-
    parent(P),
    foreach(between(1,8,N),arg(N,P,empty)).


differ_in_one_child(A0,B0,N,ChildA,ChildB) :-
    parent(A0),
    parent(B0),
    nth_child(N,A0,ChildA),
    nth_child(N,B0,ChildB),
    map_args(differ_(N),A0,B0).

differ_(N,N,A,B) :-
    dif(A,B).
differ_(N,M,A,A) :-
    dif(N,M).


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



%% known_key(+Depth:nonneg,+Hash,+Key,?Value,?Without,?With)
insert(Depth,Hash,Key,Value,empty,Trim) :-
    Trim=trim(Depth,Hash,Key,Value).
insert(Depth,Hash,K,V,Trim,With) :-
    parent(With),
    trim_depth(Trim,Depth),
    trim_hash(Trim,TrimHash),
    ( ground(TrimHash) ->
        trim_pushdown(Trim,Without),
        insert(Depth,Hash,K,V,Without,With)
    ; otherwise ->
        parent(Without),
        insert(Depth,Hash,K,V,Without,With),
        trim_pushdown(Trim,Without)
    ).
insert(Depth0,Hash,K,V,Without,With) :-
    parent(Without),
    parent(With),
    hash_depth_n(Depth0,Hash,N),
    differ_in_one_child(Without,With,N,Old,New),
    succ(Depth0,Depth1),
    insert(Depth1,Hash,K,V,Old,New).


hash_depth_n(Hash,Depth,N) :-
    when((ground(Hash),ground(Depth)), hash_depth_n_(Hash,Depth,N)).

hash_depth_n_(Hash,Depth,N) :-
    N is ((Hash >> (3*Depth)) /\ 0b111) + 1.


kv(Map,Key,Value) :-
    ground(Key),
    !,
    delta(Key,Value,_,Map).
kv(_Map,_Key,_Value) :-
    todo("iterate each node in the tree").


% trim_pushdown(?Trim,?AsParent)
trim_pushdown(Trim0,AsParent) :-
    parent(AsParent),

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

    % relate empty parent to parent containing deeper trim element
    hash_depth_n(Hash,Depth0,N),
    empty_parent(Empty),
    differ_in_one_child(Empty,AsParent,N,empty,Trim1).
