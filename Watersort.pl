:- consult('KB2.pl').

% ************************************************************ Search ************************************************************


state(bottle1(Top1, Bottom1), bottle2(Top2, Bottom2), bottle3(Top3, Bottom3)) :-
    bottle1(Top1, Bottom1),
    bottle2(Top2, Bottom2),
    bottle3(Top3, Bottom3).

initial_state(state(bottle1(Top1, Bottom1), bottle2(Top2, Bottom2), bottle3(Top3, Bottom3))) :-
    bottle1(Top1, Bottom1),
    bottle2(Top2, Bottom2),
    bottle3(Top3, Bottom3).


is_goal_bottle(bottle1(Top, Bottom)) :-
    (Top = e) 
    ; 
    (Top = Bottom).

is_goal_bottle(bottle2(Top, Bottom)) :-
    (Top = e) 
    ; 
    (Top = Bottom).

is_goal_bottle(bottle3(Top, Bottom)) :-
    (Top = e) 
    ; 
    (Top = Bottom).

is_goal_state(state(bottle1(Top1, Bottom1), bottle2(Top2, Bottom2), bottle3(Top3, Bottom3))) :-
    is_goal_bottle(bottle1(Top1, Bottom1)),
    is_goal_bottle(bottle2(Top2, Bottom2)),
    is_goal_bottle(bottle3(Top3, Bottom3)).

% Search for the goal state
search(State, Situation, S, _) :-
    is_goal_state(State),
    S = Situation.

search(State, Situation, S, Visited) :-
    \+ member(State, Visited),
    member(pour(X, Y), [pour(1, 2), pour(1, 3), pour(2, 1), pour(2, 3), pour(3, 1), pour(3, 2)]),
    NextSituation = result(pour(X, Y), Situation),
    state(bottle1(Top1, Bottom1), bottle2(Top2, Bottom2), bottle3(Top3, Bottom3), NextSituation),
    NextState = state(bottle1(Top1, Bottom1), bottle2(Top2, Bottom2), bottle3(Top3, Bottom3)),
    search(NextState, NextSituation, S, [State|Visited]).

goal(S) :-
    initial_state(InitialState),
    search(InitialState, s0, S, []).

% ************************************************************ Successor State Axiom ************************************************************

state(bottle1(Top1, Bottom1), bottle2(Top2, Bottom2), bottle3(Top3, Bottom3), s0) :-
    bottle1(Top1, Bottom1),
    bottle2(Top2, Bottom2),
    bottle3(Top3, Bottom3).

state(bottle1(NewTop1, NewBottom1), bottle2(NewTop2, NewBottom2), bottle3(NewTop3, NewBottom3), result(A, S)) :-

    state(bottle1(Top1, Bottom1), bottle2(Top2, Bottom2), bottle3(Top3, Bottom3), S),

    (A = pour(1, 2),
        (Top1 \= e, Bottom1 \= e, Top2 = e, Bottom2 \= e ->
            NewTop1 = e, NewBottom1 = Bottom1, NewTop2 = Top1, NewBottom2 = Bottom2
        ; Top1 \= e, Bottom1 \= e, Top2 = e, Bottom2 = e ->
            NewTop1 = e, NewBottom1 = Bottom1, NewTop2 = e, NewBottom2 = Top1
        ; Top1 = e, Bottom1 \= e, Top2 = e, Bottom2 \= e ->
            NewTop1 = e, NewBottom1 = e, NewTop2 = Bottom1, NewBottom2 = Bottom2
        ; Top1 = e, Bottom1 \= e, Top2 = e, Bottom2 = e ->
            NewTop1 = e, NewBottom1 = e, NewTop2 = e, NewBottom2 = Bottom2
        ),
        NewTop3 = Top3, NewBottom3 = Bottom3
    ;

    A = pour(1, 3),
        (Top1 \= e, Bottom1 \= e, Top3 = e, Bottom3 \= e ->
            NewTop1 = e, NewBottom1 = Bottom1, NewTop3 = Top1, NewBottom3 = Bottom3
        ; Top1 \= e, Bottom1 \= e, Top3 = e, Bottom3 = e ->
            NewTop1 = e, NewBottom1 = Bottom1, NewTop3 = e, NewBottom3 = Top1
        ; Top1 = e, Bottom1 \= e, Top3 = e, Bottom3 \= e ->
            NewTop1 = e, NewBottom1 = e, NewTop3 = Bottom1, NewBottom3 = Bottom3
        ; Top1 = e, Bottom1 \= e, Top3 = e, Bottom3 = e ->
            NewTop1 = e, NewBottom1 = e, NewTop3 = e, NewBottom3 = Bottom1
        ),
        NewTop2 = Top2, NewBottom2 = Bottom2
    ;

    A = pour(2, 1),
        (Top2 \= e, Bottom2 \= e, Top1 = e, Bottom1 \= e ->
            NewTop2 = e, NewBottom2 = Bottom2, NewTop1 = Top2, NewBottom1 = Bottom1
        ; Top2 \= e, Bottom2 \= e, Top1 = e, Bottom1 = e ->
            NewTop2 = e, NewBottom2 = Bottom2, NewTop1 = e, NewBottom1 = Top2
        ; Top2 = e, Bottom2 \= e, Top1 = e, Bottom1 \= e ->
            NewTop2 = e, NewBottom2 = e, NewTop1 = Bottom2, NewBottom1 = Bottom1
        ; Top2 = e, Bottom2 \= e, Top1 = e, Bottom1 = e ->
            NewTop2 = e, NewBottom2 = e, NewTop1 = e, NewBottom1 = Bottom2
        ),
        NewTop3 = Top3, NewBottom3 = Bottom3
    ;

    A = pour(2, 3),
        (Top2 \= e, Bottom2 \= e, Top3 = e, Bottom3 \= e ->
            NewTop2 = e, NewBottom2 = Bottom2, NewTop3 = Top2, NewBottom3 = Bottom3
        ; Top2 \= e, Bottom2 \= e, Top3 = e, Bottom3 = e ->
            NewTop2 = e, NewBottom2 = Bottom2, NewTop3 = e, NewBottom3 = Top2
        ; Top2 = e, Bottom2 \= e, Top3 = e, Bottom3 \= e ->
            NewTop2 = e, NewBottom2 = e, NewTop3 = Bottom2, NewBottom3 = Bottom3
        ; Top2 = e, Bottom2 \= e, Top3 = e, Bottom3 = e ->
            NewTop2 = e, NewBottom2 = e, NewTop3 = e, NewBottom3 = Bottom2
        ),
        NewTop1 = Top1, NewBottom1 = Bottom1
    ;

    A = pour(3, 1),
        (Top3 \= e, Bottom3 \= e, Top1 = e, Bottom1 \= e ->
            NewTop3 = e, NewBottom3 = Bottom3, NewTop1 = Top3, NewBottom1 = Bottom1
        ; Top3 \= e, Bottom3 \= e, Top1 = e, Bottom1 = e ->
            NewTop3 = e, NewBottom3 = Bottom3, NewTop1 = e, NewBottom1 = Top3
        ; Top3 = e, Bottom3 \= e, Top1 = e, Bottom1 \= e ->
            NewTop3 = e, NewBottom3 = e, NewTop1 = Bottom3, NewBottom1 = Bottom1
        ; Top3 = e, Bottom3 \= e, Top1 = e, Bottom1 = e ->
            NewTop3 = e, NewBottom3 = e, NewTop1 = e, NewBottom1 = Bottom3
        ),
        NewTop2 = Top2, NewBottom2 = Bottom2
    ;

    A = pour(3, 2),
        (Top3 \= e, Bottom3 \= e, Top2 = e, Bottom2 \= e ->
            NewTop3 = e, NewBottom3 = Bottom3, NewTop2 = Top3, NewBottom2 = Bottom2
        ; Top3 \= e, Bottom3 \= e, Top2 = e, Bottom2 = e ->
            NewTop3 = e, NewBottom3 = Bottom3, NewTop2 = e, NewBottom2 = Top3
        ; Top3 = e, Bottom3 \= e, Top2 = e, Bottom2 \= e ->
            NewTop3 = e, NewBottom3 = e, NewTop2 = Bottom3, NewBottom2 = Bottom2
        ; Top3 = e, Bottom3 \= e, Top2 = e, Bottom2 = e ->
            NewTop3 = e, NewBottom3 = e, NewTop2 = e, NewBottom2 = Bottom3
        ),
        NewTop1 = Top1, NewBottom1 = Bottom1
    ).