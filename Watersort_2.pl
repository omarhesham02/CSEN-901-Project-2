:- consult('KB2.pl').

% ************************************************************ Main Logic ************************************************************

% Define the initial state from the KB
initial_state(state(bottle1(Top1, Bottom1), bottle2(Top2, Bottom2), bottle3(Top3, Bottom3))) :-
    bottle1(Top1, Bottom1),
    bottle2(Top2, Bottom2),
    bottle3(Top3, Bottom3).


is_goal_state(state(bottle1(C1, C1), bottle2(C2, C2), bottle3(C3, C3))).


situation_to_state(s0, State) :-
    initial_state(State).

situation_to_state(result(pour(From, To), RestSituation), State) :-
    situation_to_state(RestSituation, IntermediateState),
    pour(From, To, IntermediateState, State).


goal(S) :-
    nonvar(S),
    situation_to_state(S, State),
    is_goal_state(State),
    !.

goal(S) :-
    var(S),
    initial_state(InitialState),
    bfs([[InitialState, s0]], [], S).


bfs([[State, Situation] | _], _, Situation) :-
    is_goal_state(State).
 
bfs([[State, Situation] | Rest], Visited, S) :-
    findall([NextState, result(pour(From, To), Situation)],
            (pour(From, To, State, NextState),
            \+ member(NextState, Visited)),
            NextStates
            ),
    append(Rest, NextStates, NewQueue),
    bfs(NewQueue, [State | Visited], S).

bfs([], _, _) :- fail.

% ************************************************************ Pour Operations ************************************************************

% Pour from bottle1 to bottle2
pour(1, 2, state(bottle1(Top1, Bottom1), bottle2(Top2, Bottom2), B3), NextState) :-
    (
        % Case 1: [Top1, Bottom1] -> [e, Bottom2]
        (
            Top1 \= e, Top2 = e, Bottom2 \= e,
            NextState = state(bottle1(e, Bottom1), bottle2(Top1, Bottom2), B3)
        )
        ;
        % Case 2: [Top1, Bottom1] -> [e, e]
        (
            Top1 \= e, Top2 = e, Bottom2 = e,
            NextState = state(bottle1(e, Bottom1), bottle2(e, Top1), B3)
        )
        ;
        % Case 3: [e, Bottom1] -> [e, Bottom2]
        (
            Top1 = e, Bottom1 \= e, Top2 = e, Bottom2 \= e,
            NextState = state(bottle1(e, e), bottle2(Bottom1, Bottom2), B3)
        )
        ;
        % Case 4: [e, Bottom1] -> [e, e]
        (
            Top1 = e, Bottom1 \= e, Top2 = e, Bottom2 = e,
            NextState = state(bottle1(e, e), bottle2(e, Bottom1), B3)
        )
    ).

% Pour from bottle1 to bottle3
pour(1, 3, state(bottle1(Top1, Bottom1), B2, bottle3(Top3, Bottom3)), NextState) :-
    (
        % Case 1: [Top1, Bottom1] -> [e, Bottom3]
        (
            Top1 \= e, Top3 = e, Bottom3 \= e,
            NextState = state(bottle1(e, Bottom1), B2, bottle3(Top1, Bottom3))
        )
        ;
        % Case 2: [Top1, Bottom1] -> [e, e]
        (
            Top1 \= e, Top3 = e, Bottom3 = e,
            NextState = state(bottle1(e, Bottom1), B2, bottle3(e, Top1))
        )
        ;
        % Case 3: [e, Bottom1] -> [e, Bottom3]
        (
            Top1 = e, Bottom1 \= e, Top3 = e, Bottom3 \= e,
            NextState = state(bottle1(e, e), B2, bottle3(Bottom1, Bottom3))
        )
        ;
        % Case 4: [e, Bottom1] -> [e, e]
        (
            Top1 = e, Bottom1 \= e, Top3 = e, Bottom3 = e,
            NextState = state(bottle1(e, e), B2, bottle3(e, Bottom1))
        )
    ).

% Pour from bottle2 to bottle1
pour(2, 1, state(bottle1(Top1, Bottom1), bottle2(Top2, Bottom2), B3), NextState) :-
    (
        % Case 1: [Top2, Bottom2] -> [e, Bottom1]
        (
            Top2 \= e, Top1 = e, Bottom1 \= e,
            NextState = state(bottle1(Top2, Bottom1), bottle2(e, Bottom2), B3)
        )
        ;
        % Case 2: [Top2, Bottom2] -> [e, e]
        (
            Top2 \= e, Top1 = e, Bottom1 = e,
            NextState = state(bottle1(e, Top2), bottle2(e, Bottom2), B3)
        )
        ;
        % Case 3: [e, Bottom2] -> [e, Bottom1]
        (
            Top2 = e, Bottom2 \= e, Top1 = e, Bottom1 \= e,
            NextState = state(bottle1(Bottom2, Bottom1), bottle2(e, e), B3)
        )
        ;
        % Case 4: [e, Bottom2] -> [e, e]
        (
            Top2 = e, Bottom2 \= e, Top1 = e, Bottom1 = e,
            NextState = state(bottle1(e, Bottom2), bottle2(e, e), B3)
        )
    ).

% Pour from bottle2 to bottle3
pour(2, 3, state(B1, bottle2(Top2, Bottom2), bottle3(Top3, Bottom3)), NextState) :-
    (
        % Case 1: [Top2, Bottom2] -> [e, Bottom3]
        (
            Top2 \= e, Top3 = e, Bottom3 \= e,
            NextState = state(B1, bottle2(e, Bottom2), bottle3(Top2, Bottom3))
        )
        ;
        % Case 2: [Top2, Bottom2] -> [e, e]
        (
            Top2 \= e, Top3 = e, Bottom3 = e,
            NextState = state(B1, bottle2(e, Bottom2), bottle3(e, Top2))
        )
        ;
        % Case 3: [e, Bottom2] -> [e, Bottom3]
        (
            Top2 = e, Bottom2 \= e, Top3 = e, Bottom3 \= e,
            NextState = state(B1, bottle2(e, e), bottle3(Bottom2, Bottom3))
        )
        ;
        % Case 4: [e, Bottom2] -> [e, e]
        (
            Top2 = e, Bottom2 \= e, Top3 = e, Bottom3 = e,
            NextState = state(B1, bottle2(e, e), bottle3(e, Bottom2))
        )
    ).

% Pour from bottle3 to bottle1
pour(3, 1, state(bottle1(Top1, Bottom1), B2, bottle3(Top3, Bottom3)), NextState) :-
    (
        % Case 1: [Top3, Bottom3] -> [e, Bottom1]
        (
            Top3 \= e, Top1 = e, Bottom1 \= e,
            NextState = state(bottle1(Top3, Bottom1), B2, bottle3(e, Bottom3))
        )
        ;
        % Case 2: [Top3, Bottom3] -> [e, e]
        (
            Top3 \= e, Top1 = e, Bottom1 = e,
            NextState = state(bottle1(e, Top3), B2, bottle3(e, Bottom3))
        )
        ;
        % Case 3: [e, Bottom3] -> [e, Bottom1]
        (
            Top3 = e, Bottom3 \= e, Top1 = e, Bottom1 \= e,
            NextState = state(bottle1(Bottom3, Bottom1), B2, bottle3(e, e))
        )
        ;
        % Case 4: [e, Bottom3] -> [e, e]
        (
            Top3 = e, Bottom3 \= e, Top1 = e, Bottom1 = e,
            NextState = state(bottle1(e, Bottom3), B2, bottle3(e, e))
        )
    ).

% Pour from bottle3 to bottle2
pour(3, 2, state(B1, bottle2(Top2, Bottom2), bottle3(Top3, Bottom3)), NextState) :-
    (
        % Case 1: [Top3, Bottom3] -> [e, Bottom2]
        (
            Top3 \= e, Top2 = e, Bottom2 \= e,
            NextState = state(B1, bottle2(Top3, Bottom2), bottle3(e, Bottom3))
        )
        ;
        % Case 2: [Top3, Bottom3] -> [e, e]
        (
            Top3 \= e, Top2 = e, Bottom2 = e,
            NextState = state(B1, bottle2(e, Top3), bottle3(e, Bottom3))
        )
        ;
        % Case 3: [e, Bottom3] -> [e, Bottom2]
        (
            Top3 = e, Bottom3 \= e, Top2 = e, Bottom2 \= e,
            NextState = state(B1, bottle2(Bottom3, Bottom2), bottle3(e, e))
        )
        ;
        % Case 4: [e, Bottom3] -> [e, e]
        (
            Top3 = e, Bottom3 \= e, Top2 = e, Bottom2 = e,
            NextState = state(B1, bottle2(e, Bottom3), bottle3(e, e))
        )
    ).
