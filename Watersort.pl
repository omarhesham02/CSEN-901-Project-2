:- consult('KB2.pl').
:- dynamic bottle1/2, bottle2/2, bottle3/2.
:- dynamic result/2.

% ************************************************************ Pour Operations ************************************************************

% Pour from bottle1 to bottle2
pour(1, 2, state(bottle1(Top1, Bottom1), bottle2(Top2, Bottom2), B3), NewState) :-
    (
        % Case 1: [Top1, Bottom1] -> [e, Bottom2]
        (Top1 \= e, Top2 = e, Bottom2 \= e,
            NewState = state(bottle1(e, Bottom1), bottle2(Top1, Bottom2), B3)
        )
        ;
        % Case 2: [Top1, Bottom1] -> [e, e]
        (Top1 \= e, Top2 = e, Bottom2 = e,
            NewState = state(bottle1(e, Bottom1), bottle2(e, Top1), B3)
        )
        ;
        % Case 3: [e, Bottom1] -> [e, Bottom2]
        (Top1 = e, Bottom1 \= e, Top2 = e, Bottom2 \= e,
            NewState = state(bottle1(e, e), bottle2(Bottom1, Bottom2), B3)
        )
        ;
        % Case 4: [e, Bottom1] -> [e, e]
        (Top1 = e, Bottom1 \= e, Top2 = e, Bottom2 = e,
            NewState = state(bottle1(e, e), bottle2(e, Bottom1), B3)
        )
    ).

% Pour from bottle1 to bottle3
pour(1, 3, state(bottle1(Top1, Bottom1), B2, bottle3(Top3, Bottom3)), NewState) :-
    (
        % Case 1: [Top1, Bottom1] -> [e, Bottom3]
        (Top1 \= e, Top3 = e, Bottom3 \= e,
            NewState = state(bottle1(e, Bottom1), B2, bottle3(Top1, Bottom3))
        )
        ;
        % Case 2: [Top1, Bottom1] -> [e, e]
        (Top1 \= e, Top3 = e, Bottom3 = e,
            NewState = state(bottle1(e, Bottom1), B2, bottle3(e, Top1))
        )
        ;
        % Case 3: [e, Bottom1] -> [e, Bottom3]
        (Top1 = e, Bottom1 \= e, Top3 = e, Bottom3 \= e,
            NewState = state(bottle1(e, e), B2, bottle3(Bottom1, Bottom3))
        )
        ;
        % Case 4: [e, Bottom1] -> [e, e]
        (Top1 = e, Bottom1 \= e, Top3 = e, Bottom3 = e,
            NewState = state(bottle1(e, e), B2, bottle3(e, Bottom1))
        )
    ).

% Pour from bottle2 to bottle1
pour(2, 1, state(bottle1(Top1, Bottom1), bottle2(Top2, Bottom2), B3), NewState) :-
    (
        % Case 1: [Top2, Bottom2] -> [e, Bottom1]
        (Top2 \= e, Top1 = e, Bottom1 \= e,
            NewState = state(bottle1(Top2, Bottom1), bottle2(e, Bottom2), B3)
        )
        ;
        % Case 2: [Top2, Bottom2] -> [e, e]
        (Top2 \= e, Top1 = e, Bottom1 = e,
            NewState = state(bottle1(e, Top2), bottle2(e, Bottom2), B3)
        )
        ;
        % Case 3: [e, Bottom2] -> [e, Bottom1]
        (Top2 = e, Bottom2 \= e, Top1 = e, Bottom1 \= e,
            NewState = state(bottle1(Bottom2, Bottom1), bottle2(e, e), B3)
        )
        ;
        % Case 4: [e, Bottom2] -> [e, e]
        (Top2 = e, Bottom2 \= e, Top1 = e, Bottom1 = e,
            NewState = state(bottle1(e, Bottom2), bottle2(e, e), B3)
        )
    ).

% Pour from bottle2 to bottle3
pour(2, 3, state(B1, bottle2(Top2, Bottom2), bottle3(Top3, Bottom3)), NewState) :-
    (
        % Case 1: [Top2, Bottom2] -> [e, Bottom3]
        (Top2 \= e, Top3 = e, Bottom3 \= e,
            NewState = state(B1, bottle2(e, Bottom2), bottle3(Top2, Bottom3))
        )
        ;
        % Case 2: [Top2, Bottom2] -> [e, e]
        (Top2 \= e, Top3 = e, Bottom3 = e,
            NewState = state(B1, bottle2(e, Bottom2), bottle3(e, Top2))
        )
        ;
        % Case 3: [e, Bottom2] -> [e, Bottom3]
        (Top2 = e, Bottom2 \= e, Top3 = e, Bottom3 \= e,
            NewState = state(B1, bottle2(e, e), bottle3(Bottom2, Bottom3))
        )
        ;
        % Case 4: [e, Bottom2] -> [e, e]
        (Top2 = e, Bottom2 \= e, Top3 = e, Bottom3 = e,
            NewState = state(B1, bottle2(e, e), bottle3(e, Bottom2))
        )
    ).

% Pour from bottle3 to bottle1
pour(3, 1, state(bottle1(Top1, Bottom1), B2, bottle3(Top3, Bottom3)), NewState) :-
    (
        % Case 1: [Top3, Bottom3] -> [e, Bottom1]
        (Top3 \= e, Top1 = e, Bottom1 \= e,
            NewState = state(bottle1(Top3, Bottom1), B2, bottle3(e, Bottom3))
        )
        ;
        % Case 2: [Top3, Bottom3] -> [e, e]
        (Top3 \= e, Top1 = e, Bottom1 = e,
            NewState = state(bottle1(e, Top3), B2, bottle3(e, Bottom3))
        )
        ;
        % Case 3: [e, Bottom3] -> [e, Bottom1]
        (Top3 = e, Bottom3 \= e, Top1 = e, Bottom1 \= e,
            NewState = state(bottle1(Bottom3, Bottom1), B2, bottle3(e, e))
        )
        ;
        % Case 4: [e, Bottom3] -> [e, e]
        (Top3 = e, Bottom3 \= e, Top1 = e, Bottom1 = e,
            NewState = state(bottle1(e, Bottom3), B2, bottle3(e, e))
        )
    ).

% Pour from bottle3 to bottle2
pour(3, 2, state(B1, bottle2(Top2, Bottom2), bottle3(Top3, Bottom3)), NewState) :-
    (
        % Case 1: [Top3, Bottom3] -> [e, Bottom2]
        (Top3 \= e, Top2 = e, Bottom2 \= e,
            NewState = state(B1, bottle2(Top3, Bottom2), bottle3(e, Bottom3))
        )
        ;
        % Case 2: [Top3, Bottom3] -> [e, e]
        (Top3 \= e, Top2 = e, Bottom2 = e,
            NewState = state(B1, bottle2(e, Top3), bottle3(e, Bottom3))
        )
        ;
        % Case 3: [e, Bottom3] -> [e, Bottom2]
        (Top3 = e, Bottom3 \= e, Top2 = e, Bottom2 \= e,
            NewState = state(B1, bottle2(Bottom3, Bottom2), bottle3(e, e))
        )
        ;
        % Case 4: [e, Bottom3] -> [e, e]
        (Top3 = e, Bottom3 \= e, Top2 = e, Bottom2 = e,
            NewState = state(B1, bottle2(e, Bottom3), bottle3(e, e))
        )
    ).

% ************************************************************ Main Logic ************************************************************

% Define the initial state from the KB
initial_state(state(bottle1(Top1, Bottom1), bottle2(Top2, Bottom2), bottle3(Top3, Bottom3))) :-
    bottle1(Top1, Bottom1),
    bottle2(Top2, Bottom2),
    bottle3(Top3, Bottom3).


is_goal_state(state(bottle1(Top1, Bottom1), bottle2(Top2, Bottom2), bottle3(Top3, Bottom3))) :-
    is_goal_bottle(bottle1(Top1, Bottom1)),
    is_goal_bottle(bottle2(Top2, Bottom2)),
    is_goal_bottle(bottle3(Top3, Bottom3)).


is_goal_bottle(bottle1(Top1, Bottom1)) :-
    (Top1 = e, Bottom1 \= e)
    ;
    (Top1 = Bottom1).

is_goal_bottle(bottle2(Top2, Bottom2)) :-
    (Top2 = e, Bottom2 \= e)
    ;
    (Top2 = Bottom2).

is_goal_bottle(bottle3(Top3, Bottom3)) :-
    (Top3 = e, Bottom3 \= e)
    ;
    (Top3 = Bottom3).


apply_pour(pour(From, To), State, NewState) :-
    pour(From, To, State, NewState).


search(State, Situation, S) :-
    is_goal_state(State),
    S = Situation.

search(State, Situation, S) :-
    pour_operation(Pour),
    apply_pour(Pour, State, NewState),
    NewSituation = result(Pour, Situation),
    search(NewState, NewSituation, S).


pour_operation(pour(1, 2)).
pour_operation(pour(1, 3)).
pour_operation(pour(2, 1)).
pour_operation(pour(2, 3)).
pour_operation(pour(3, 1)).
pour_operation(pour(3, 2)).


goal(S) :-
    initial_state(InitialState),
    ids(InitialState, S).


ids(State, S) :-
    L = 0,
    ids(State, S, L).

ids(State, S, L) :-
    call_with_depth_limit(search(State, s0, S), L, R),
    R \= depth_limit_exceeded.

ids(State, S, L) :-
    L1 is L + 1,
    ids(State, S, L1).






    
    