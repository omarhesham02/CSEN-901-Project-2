:- consult('KB2.pl').
:- dynamic bottle1/2, bottle2/2, bottle3/2.
:- dynamic result/2.


% ************************************************************ Pour Operations ************************************************************



% Pour from bottle1 to bottle2
pour(1, 2) :-
    bottle1(Top1, Bottom1),
    bottle2(Top2, Bottom2),
    (
        % Case 1: [Top1, Bottom1] -> [e, Bottom2]
        (Top1 \= e, Top2 = e, Bottom2 \= e,
            retract(bottle1(Top1, Bottom1)),
            retract(bottle2(Top2, Bottom2)),
            assert(bottle1(e, Bottom1)),
            assert(bottle2(Top1, Bottom2))
        )
        ;
        % Case 2: [Top1, Bottom1] -> [e, e]
        (Top1 \= e, Top2 = e, Bottom2 = e,
            retract(bottle1(Top1, Bottom1)),
            retract(bottle2(Top2, Bottom2)),
            assert(bottle1(e, Bottom1)),
            assert(bottle2(e, Top1))
        )
        ;
        % Case 3: [e, Bottom1] -> [e, Bottom2]
        (Top1 = e, Bottom1 \= e, Top2 = e, Bottom2 \= e,
            retract(bottle1(Top1, Bottom1)),
            retract(bottle2(Top2, Bottom2)),
            assert(bottle1(e, e)),
            assert(bottle2(Bottom1, Bottom2))
        )
        ;
        % Case 4: [e, Bottom1] -> [e, e]
        (Top1 = e, Bottom1 \= e,
            retract(bottle1(Top1, Bottom1)),
            retract(bottle2(Top2, Bottom2)),
            assert(bottle1(e, e)),
            assert(bottle2(e, Bottom1))
        )
    ).

% Pour from bottle1 to bottle3
pour(1, 3) :-
    bottle1(Top1, Bottom1),
    bottle3(Top3, Bottom3),
    (
        % Case 1: [Top1, Bottom1] -> [e, Bottom3]
        (Top1 \= e, Top3 = e, Bottom3 \= e,
            retract(bottle1(Top1, Bottom1)),
            retract(bottle3(Top3, Bottom3)),
            assert(bottle1(e, Bottom1)),
            assert(bottle3(Top1, Bottom3))
        )
        ;
        % Case 2: [Top1, Bottom1] -> [e, e]
        (Top1 \= e, Top3 = e, Bottom3 = e,
            retract(bottle1(Top1, Bottom1)),
            retract(bottle3(Top3, Bottom3)),
            assert(bottle1(e, Bottom1)),
            assert(bottle3(e, Top1))
        )
        ;
        % Case 3: [e, Bottom1] -> [e, Bottom3]
        (Top1 = e, Bottom1 \= e, Top3 = e, Bottom3 \= e,
            retract(bottle1(Top1, Bottom1)),
            retract(bottle3(Top3, Bottom3)),
            assert(bottle1(e, e)),
            assert(bottle3(Bottom1, Bottom3))
        )
        ;
        % Case 4: [e, Bottom1] -> [e, e]
        (Top1 = e, Bottom1 \= e,
            retract(bottle1(Top1, Bottom1)),
            retract(bottle3(Top3, Bottom3)),
            assert(bottle1(e, e)),
            assert(bottle3(e, Bottom1))
        )
    ).

% Pour from bottle2 to bottle1
pour(2, 1) :-
    bottle2(Top2, Bottom2),
    bottle1(Top1, Bottom1),
    (
        % Case 1: [Top2, Bottom2] -> [e, Bottom1]
        (Top2 \= e, Top1 = e, Bottom1 \= e,
            retract(bottle2(Top2, Bottom2)),
            retract(bottle1(Top1, Bottom1)),
            assert(bottle2(e, Bottom2)),
            assert(bottle1(Top2, Bottom1))
        )
        ;
        % Case 2: [Top2, Bottom2] -> [e, e]
        (Top2 \= e, Top1 = e, Bottom1 = e,
            retract(bottle2(Top2, Bottom2)),
            retract(bottle1(Top1, Bottom1)),
            assert(bottle2(e, Bottom2)),
            assert(bottle1(e, Top2))
        )
        ;
        % Case 3: [e, Bottom2] -> [e, Bottom1]
        (Top2 = e, Bottom2 \= e, Top1 = e, Bottom1 \= e,
            retract(bottle2(Top2, Bottom2)),
            retract(bottle1(Top1, Bottom1)),
            assert(bottle2(e, e)),
            assert(bottle1(Bottom2, Bottom1))
        )
        ;
        % Case 4: [e, Bottom2] -> [e, e]
        (Top2 = e, Bottom2 \= e,
            retract(bottle2(Top2, Bottom2)),
            retract(bottle1(Top1, Bottom1)),
            assert(bottle2(e, e)),
            assert(bottle1(e, Bottom2))
        )
    ).

% Pour from bottle2 to bottle3
pour(2, 3) :-
    bottle2(Top2, Bottom2),
    bottle3(Top3, Bottom3),
    (
        % Case 1: [Top2, Bottom2] -> [e, Bottom3]
        (Top2 \= e, Top3 = e, Bottom3 \= e,
            retract(bottle2(Top2, Bottom2)),
            retract(bottle3(Top3, Bottom3)),
            assert(bottle2(e, Bottom2)),
            assert(bottle3(Top2, Bottom3))
        )
        ;
        % Case 2: [Top2, Bottom2] -> [e, e]
        (Top2 \= e, Top3 = e, Bottom3 = e,
            retract(bottle2(Top2, Bottom2)),
            retract(bottle3(Top3, Bottom3)),
            assert(bottle2(e, Bottom2)),
            assert(bottle3(e, Top2))
        )
        ;
        % Case 3: [e, Bottom2] -> [e, Bottom3]
        (Top2 = e, Bottom2 \= e, Top3 = e, Bottom3 \= e,
            retract(bottle2(Top2, Bottom2)),
            retract(bottle3(Top3, Bottom3)),
            assert(bottle2(e, e)),
            assert(bottle3(Bottom2, Bottom3))
        )
        ;
        % Case 4: [e, Bottom2] -> [e, e]
        (Top2 = e, Bottom2 \= e,
            retract(bottle2(Top2, Bottom2)),
            retract(bottle3(Top3, Bottom3)),
            assert(bottle2(e, e)),
            assert(bottle3(e, Bottom2))
        )
    ).

% Pour from bottle3 to bottle1
pour(3, 1) :-
    bottle3(Top3, Bottom3),
    bottle1(Top1, Bottom1),
    (
        % Case 1: [Top3, Bottom3] -> [e, Bottom1]
        (Top3 \= e, Top1 = e, Bottom1 \= e,
            retract(bottle3(Top3, Bottom3)),
            retract(bottle1(Top1, Bottom1)),
            assert(bottle3(e, Bottom3)),
            assert(bottle1(Top3, Bottom1))
        )
        ;
        % Case 2: [Top3, Bottom3] -> [e, e]
        (Top3 \= e, Top1 = e, Bottom1 = e,
            retract(bottle3(Top3, Bottom3)),
            retract(bottle1(Top1, Bottom1)),
            assert(bottle3(e, Bottom3)),
            assert(bottle1(e, Top3))
        )
        ;
        % Case 3: [e, Bottom3] -> [e, Bottom1]
        (Top3 = e, Bottom3 \= e, Top1 = e, Bottom1 \= e,
            retract(bottle3(Top3, Bottom3)),
            retract(bottle1(Top1, Bottom1)),
            assert(bottle3(e, e)),
            assert(bottle1(Bottom3, Bottom1))
        )
        ;
        % Case 4: [e, Bottom3] -> [e, e]
        (Top3 = e, Bottom3 \= e,
            retract(bottle3(Top3, Bottom3)),
            retract(bottle1(Top1, Bottom1)),
            assert(bottle3(e, e)),
            assert(bottle1(e, Bottom3))
        )
    ).

% Pour from bottle3 to bottle2
pour(3, 2) :-
    bottle3(Top3, Bottom3),
    bottle2(Top2, Bottom2),
    (
        % Case 1: [Top3, Bottom3] -> [e, Bottom2]
        (Top3 \= e, Top2 = e, Bottom2 \= e,
            retract(bottle3(Top3, Bottom3)),
            retract(bottle2(Top2, Bottom2)),
            assert(bottle3(e, Bottom3)),
            assert(bottle2(Top3, Bottom2))
        )
        ;
        % Case 2: [Top3, Bottom3] -> [e, e]
        (Top3 \= e, Top2 = e, Bottom2 = e,
            retract(bottle3(Top3, Bottom3)),
            retract(bottle2(Top2, Bottom2)),
            assert(bottle3(e, Bottom3)),
            assert(bottle2(e, Top3))
        )
        ;
        % Case 3: [e, Bottom3] -> [e, Bottom2]
        (Top3 = e, Bottom3 \= e, Top2 = e, Bottom2 \= e,
            retract(bottle3(Top3, Bottom3)),
            retract(bottle2(Top2, Bottom2)),
            assert(bottle3(e, e)),
            assert(bottle2(Bottom3, Bottom2))
        )
        ;
        % Case 4: [e, Bottom3] -> [e, e]
        (Top3 = e, Bottom3 \= e,
            retract(bottle3(Top3, Bottom3)),
            retract(bottle2(Top2, Bottom2)),
            assert(bottle3(e, e)),
            assert(bottle2(e, Bottom3))
        )
    ).

% ************************************************************ Main Logic ************************************************************

is_goal_bottle(bottle1(Top, Bottom)) :-
    (Top = e, Bottom = e)                
    ;
    (Top = e, Bottom \= e)                 
    ;
    (Top = Bottom, Top \= e).       

is_goal_bottle(bottle2(Top, Bottom)) :-
    (Top = e, Bottom = e)                 
    ;
    (Top = e, Bottom \= e)                 
    ;
    (Top = Bottom, Top \= e).

is_goal_bottle(bottle3(Top, Bottom)) :-
    (Top = e, Bottom = e)                 
    ;
    (Top = e, Bottom \= e)                 
    ;
    (Top = Bottom, Top \= e).


s0.

s(state(bottle1(Top1, Bottom1), bottle2(Top2, Bottom2), bottle3(Top3, Bottom3))) :-
    bottle1(Top1, Bottom1),
    bottle2(Top2, Bottom2),
    bottle3(Top3, Bottom3).

is_goal_state(state(bottle1(Top1, Bottom1), bottle2(Top2, Bottom2), bottle3(Top3, Bottom3))) :-
    is_goal_bottle(bottle1(Top1, Bottom1)),
    is_goal_bottle(bottle2(Top2, Bottom2)),
    is_goal_bottle(bottle3(Top3, Bottom3)).


result(Action, Situation).


search(State, Situation, NextState) :-
    is_goal_state(State),
    NextState = Situation.

search(State, Situation, NextState) :-
    pour_operation(Pour),
    call(Pour),
    s(NewState),
    search(NewState, result(Pour, Situation), NextState).

pour_operation(pour(1, 2)).
pour_operation(pour(1, 3)).
pour_operation(pour(2, 1)).
pour_operation(pour(2, 3)).
pour_operation(pour(3, 1)).
pour_operation(pour(3, 2)).


goal(S) :-
    s(InitialState),
    search(InitialState, s0, S).