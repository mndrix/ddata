:- module(bench, [call_ns/1,call_ns/2,compare/1]).

:- use_module(library(sweet)).

compare(Goals) :-
    maplist(run_comparison,Goals,Timings),
    keysort(Timings,Sorted),
    format("name\tns/op~n",[]),
    maplist(show_timing,Sorted).

run_comparison(Goal,Timing-Name) :-
    functor(Goal,Name,_),
    call_ns(Goal, Timing).

show_timing(Timing-Name) :-
    format("~s\t~0f~n", [Name,Timing]).


:- meta_predicate call_ns(0).
call_ns(Goal) :-
    call_ns(Goal, Ns),
    _Module:Term = Goal,
    functor(Term,Name,_),
    format("~w: ~0f ns/op~n", [Name,Ns]).


:- meta_predicate call_ns(0,-).
call_ns(Goal, NsPerOp) :-
    call_ns_(Goal, 1, NsPerOp).

:- meta_predicate call_ns_(0,+,-).
call_ns_(Goal, N, NsPerOp) :-
    get_time(Start),
    forall(between(1,N,_), call(Goal)),
    get_time(End),
    Runtime is End - Start,
    ( Runtime > 0.100 ->
        NsPerOp is Runtime/N * 1_000_000_000
    ; otherwise ->
        N1 is 2*N,
        call_ns_(Goal,N1,NsPerOp)
    ).
