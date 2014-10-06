:- use_module(library(ddata/array)).

:- use_module(library(tap)).

'three insertions' :-
    % declare array contents
    nth(Arr, 1, world),
    nth(Arr, 2, friends),
    nth(Arr, 3, [1,2,3]),

    % can we fetch the right values?
    nth(Arr, 1, World),
    World == world,
    nth(Arr, 2, Friends),
    Friends == friends,
    nth(Arr, 3, List),
    List == [1,2,3].


'duplicate indices'(fail) :-
    nth(Arr, 1, one),
    nth(Arr, 1, won).


'iterate keys' :-
    nth(Arr, 1, one),
    nth(Arr, 2, two),
    nth(Arr, 3, three),
    nth(Arr, 4, four),

    setof(N-Value,nth(Arr,N,Value),Pairs),
    Pairs == [ 1-one, 2-two, 3-three, 4-four ].

'iterate values' :-
    nth(Arr, 1, odd),
    nth(Arr, 2, even),
    nth(Arr, 3, odd),
    nth(Arr, 4, even),

    setof(N,nth(Arr,N,odd),Odds),
    Odds == [1, 3],

    setof(N,nth(Arr,N,even),Evens),
    Evens == [2, 4].
