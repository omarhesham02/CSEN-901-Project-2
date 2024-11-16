:- consult('KB2.pl').

% ************************************************************ Search ************************************************************

state(bottle1(Top1, Bottom1), bottle2(Top2, Bottom2), bottle3(Top3, Bottom3)) :-
    bottle1(Top1, Bottom1),
    bottle2(Top2, Bottom2),
    bottle3(Top3, Bottom3).

is_goal_bottle(bottle1(Top, Bottom)) :-
    (Top = e, Bottom = e)
    ;
    (Bottom \= e, Top = Bottom).

is_goal_bottle(bottle2(Top, Bottom)) :-
    (Top = e, Bottom = e)
    ;
    (Bottom \= e, Top = Bottom).

is_goal_bottle(bottle3(Top, Bottom)) :-
    (Top = e, Bottom = e)
    ;
    (Bottom \= e, Top = Bottom).

is_goal_state(state(bottle1(Top1, Bottom1), bottle2(Top2, Bottom2), bottle3(Top3, Bottom3))) :-
    is_goal_bottle(bottle1(Top1, Bottom1)),
    is_goal_bottle(bottle2(Top2, Bottom2)),
    is_goal_bottle(bottle3(Top3, Bottom3)).


    goal(S) :-
        state(B1, B2, B3, S),
        State = state(B1, B2, B3),
        is_goal_state(State).

% ************************************************************ Successor State Axiom ************************************************************

state(bottle1(Top1, Bottom1), bottle2(Top2, Bottom2), bottle3(Top3, Bottom3), s0) :-
    bottle1(Top1, Bottom1),
    bottle2(Top2, Bottom2),
    bottle3(Top3, Bottom3).

state(bottle1(NewTop1, NewBottom1), bottle2(NewTop2, NewBottom2), bottle3(NewTop3, NewBottom3), result(A, S)) :-

    state(bottle1(Top1, Bottom1), bottle2(Top2, Bottom2), bottle3(Top3, Bottom3), S),

    (
    A = pour(1, 2),
        (
            Top1 \= e, Bottom1 \= e, Top2 = e, Bottom2 \= e,
                NewTop1 = e, NewBottom1 = Bottom1, NewTop2 = Top1, NewBottom2 = Bottom2
        ;   Top1 \= e, Bottom1 \= e, Top2 = e, Bottom2 = e,
                NewTop1 = e, NewBottom1 = Bottom1, NewTop2 = e, NewBottom2 = Top1
        ;   Top1 = e, Bottom1 \= e, Top2 = e, Bottom2 \= e,
                NewTop1 = e, NewBottom1 = e, NewTop2 = Bottom1, NewBottom2 = Bottom2
        ;   Top1 = e, Bottom1 \= e, Top2 = e, Bottom2 = e,
                NewTop1 = e, NewBottom1 = e, NewTop2 = e, NewBottom2 = Bottom2
        ),
        NewTop3 = Top3, NewBottom3 = Bottom3
    ;
    
    A = pour(1, 3),
        (
            Top1 \= e, Bottom1 \= e, Top3 = e, Bottom3 \= e,
                NewTop1 = e, NewBottom1 = Bottom1, NewTop3 = Top1, NewBottom3 = Bottom3
        ;   Top1 \= e, Bottom1 \= e, Top3 = e, Bottom3 = e,
                NewTop1 = e, NewBottom1 = Bottom1, NewTop3 = e, NewBottom3 = Top1
        ;   Top1 = e, Bottom1 \= e, Top3 = e, Bottom3 \= e,
                NewTop1 = e, NewBottom1 = e, NewTop3 = Bottom1, NewBottom3 = Bottom3
        ;   Top1 = e, Bottom1 \= e, Top3 = e, Bottom3 = e,
                NewTop1 = e, NewBottom1 = e, NewTop3 = e, NewBottom3 = Bottom1
        ),
        NewTop2 = Top2, NewBottom2 = Bottom2
    ;

    A = pour(2, 1),
        (
            Top2 \= e, Bottom2 \= e, Top1 = e, Bottom1 \= e,
                NewTop2 = e, NewBottom2 = Bottom2, NewTop1 = Top2, NewBottom1 = Bottom1
        ;   Top2 \= e, Bottom2 \= e, Top1 = e, Bottom1 = e,
                NewTop2 = e, NewBottom2 = Bottom2, NewTop1 = e, NewBottom1 = Top2
        ;   Top2 = e, Bottom2 \= e, Top1 = e, Bottom1 \= e,
                NewTop2 = e, NewBottom2 = e, NewTop1 = Bottom2, NewBottom1 = Bottom1
        ;   Top2 = e, Bottom2 \= e, Top1 = e, Bottom1 = e,
                NewTop2 = e, NewBottom2 = e, NewTop1 = e, NewBottom1 = Bottom2
        ),
        NewTop3 = Top3, NewBottom3 = Bottom3
    ;

    A = pour(2, 3),
        (
            Top2 \= e, Bottom2 \= e, Top3 = e, Bottom3 \= e,
                NewTop2 = e, NewBottom2 = Bottom2, NewTop3 = Top2, NewBottom3 = Bottom3
        ;   Top2 \= e, Bottom2 \= e, Top3 = e, Bottom3 = e,
                NewTop2 = e, NewBottom2 = Bottom2, NewTop3 = e, NewBottom3 = Top2
        ;   Top2 = e, Bottom2 \= e, Top3 = e, Bottom3 \= e,
                NewTop2 = e, NewBottom2 = e, NewTop3 = Bottom2, NewBottom3 = Bottom3
        ;   Top2 = e, Bottom2 \= e, Top3 = e, Bottom3 = e,
                NewTop2 = e, NewBottom2 = e, NewTop3 = e, NewBottom3 = Bottom2
        ),
        NewTop1 = Top1, NewBottom1 = Bottom1
    ;

    A = pour(3, 1),
        (
            Top3 \= e, Bottom3 \= e, Top1 = e, Bottom1 \= e,
                NewTop3 = e, NewBottom3 = Bottom3, NewTop1 = Top3, NewBottom1 = Bottom1
        ;   Top3 \= e, Bottom3 \= e, Top1 = e, Bottom1 = e,
                NewTop3 = e, NewBottom3 = Bottom3, NewTop1 = e, NewBottom1 = Top3
        ;   Top3 = e, Bottom3 \= e, Top1 = e, Bottom1 \= e,
                NewTop3 = e, NewBottom3 = e, NewTop1 = Bottom3, NewBottom1 = Bottom1
        ;   Top3 = e, Bottom3 \= e, Top1 = e, Bottom1 = e,
                NewTop3 = e, NewBottom3 = e, NewTop1 = e, NewBottom1 = Bottom3
        ),
        NewTop2 = Top2, NewBottom2 = Bottom2
    ;

    A = pour(3, 2),
        (
            Top3 \= e, Bottom3 \= e, Top2 = e, Bottom2 \= e,
                NewTop3 = e, NewBottom3 = Bottom3, NewTop2 = Top3, NewBottom2 = Bottom2
        ;   Top3 \= e, Bottom3 \= e, Top2 = e, Bottom2 = e,
                NewTop3 = e, NewBottom3 = Bottom3, NewTop2 = e, NewBottom2 = Top3
        ;   Top3 = e, Bottom3 \= e, Top2 = e, Bottom2 \= e,
                NewTop3 = e, NewBottom3 = e, NewTop2 = Bottom3, NewBottom2 = Bottom2
        ;   Top3 = e, Bottom3 \= e, Top2 = e, Bottom2 = e,
                NewTop3 = e, NewBottom3 = e, NewTop2 = e, NewBottom2 = Bottom3
        ),
        NewTop1 = Top1, NewBottom1 = Bottom1
    ).


% ************************************************************ Experimentation ************************************************************

search(Situation, S, _) :-
    state(B1, B2, B3, Situation),
    State = state(B1, B2, B3),
    is_goal_state(State),
    S = Situation.

search(Situation, S, Visited) :-
    state(B1, B2, B3, Situation),
    State = state(B1, B2, B3),
    \+ member(State, Visited),
    A = pour(_, _),
    NextSituation = result(A, Situation),
    search(NextSituation, S, [State | Visited]).

dls_with_visited_states(Situation, S, Depth, _) :-
    Depth > 0,
    state(B1, B2, B3, Situation),
    State = state(B1, B2, B3), 
    is_goal_state(State),
    S = Situation.

dls_with_visited_states(Situation, S, Depth, Visited) :-
    Depth > 0,
    NewDepth is Depth - 1,
    state(B1, B2, B3, Situation),
    State = state(B1, B2, B3),
    \+ member(State, Visited),
    A = pour(_, _),
    NextSituation = result(A, Situation),
    dls_with_visited_states(NextSituation, S, NewDepth, [State | Visited]).

ids(Situation, S, L, MaxDepth, Visited) :-
    L =< MaxDepth,
    call_with_depth_limit(search(Situation, S, Visited), L, R),
    R \= depth_limit_exceeded.

ids(Situation, S, L, MaxDepth, _) :-
    L = MaxDepth,
    L1 is L + 1,
    ids(Situation, S, L1, MaxDepth, []).