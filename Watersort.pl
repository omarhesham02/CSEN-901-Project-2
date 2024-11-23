:- consult('KB2.pl').

% ************************************************************ Helper Predicates ************************************************************

count_occurrences(_, [], 0).
count_occurrences(X, [X|T], N) :- count_occurrences(X, T, N1), N is N1 + 1.
count_occurrences(X, [Y|T], N) :- X \= Y, count_occurrences(X, T, N).

unique_colors([], []).
unique_colors([e|T], T1) :- unique_colors(T, T1).
unique_colors([H|T], [H|T1]) :- H \= e, delete(T, H, T2), unique_colors(T2, T1).

% ************************************************************ Auxiliary Predicates ************************************************************

is_goal_state(state(bottle1(C1, C1), bottle2(C2, C2), bottle3(C3, C3), _)).

can_lead_to_solution(state(bottle1(Top1, Bottom1), bottle2(Top2, Bottom2), bottle3(Top3, Bottom3), _)) :-
    is_goal_state(state(bottle1(Top1, Bottom1), bottle2(Top2, Bottom2), bottle3(Top3, Bottom3), _)).

can_lead_to_solution(state(bottle1(Top1, Bottom1), bottle2(Top2, Bottom2), bottle3(Top3, Bottom3), _)) :-
    Layers = [Top1, Bottom1, Top2, Bottom2, Top3, Bottom3],
    unique_colors(Layers, UniqueColors),
  (
    (
        length(UniqueColors, 1),
        [Color] = UniqueColors,
        nonvar(Color),
        count_occurrences(Color, Layers, Count),
        0 is Count mod 2
    )
    ;
    (
        length(UniqueColors, 2),
        [Color1, Color2] = UniqueColors,
        nonvar(Color1), nonvar(Color2),
        count_occurrences(Color1, Layers, Count1),
        count_occurrences(Color2, Layers, Count2),
        Count1 + Count2 < 6,
        0 is Count1 mod 2,
        0 is Count2 mod 2
    )
  ).

% ************************************************************ Goal Predicate ************************************************************

goal(S) :-
    (
      state(B1I, B2I, B3I, s0),
      can_lead_to_solution(state(B1I, B2I, B3I, s0))     
    )
    ->
    (
      state(B1, B2, B3, S),
      is_goal_state(state(B1, B2, B3, S))
    )
    ,!
    .

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
          Top1 \= e, Bottom1 \= e, Top2 = e, Bottom2 = Top1,
            NewTop1 = e, NewBottom1 = Bottom1, NewTop2 = Top1, NewBottom2 = Bottom2
        ; Top1 \= e, Bottom1 \= e, Top2 = e, Bottom2 = e,
            NewTop1 = e, NewBottom1 = Bottom1, NewTop2 = e, NewBottom2 = Top1
        ; Top1 = e, Bottom1 \= e, Top2 = e, Bottom2 = Bottom1,
            NewTop1 = e, NewBottom1 = e, NewTop2 = Bottom1, NewBottom2 = Bottom2
        ; Top1 = e, Bottom1 \= e, Top2 = e, Bottom2 = e,
            NewTop1 = e, NewBottom1 = e, NewTop2 = e, NewBottom2 = Bottom1
        ),
        NewTop3 = Top3, NewBottom3 = Bottom3
    ;

    A = pour(1, 3),
        (
          Top1 \= e, Bottom1 \= e, Top3 = e, Bottom3 = Top1,
            NewTop1 = e, NewBottom1 = Bottom1, NewTop3 = Top1, NewBottom3 = Bottom3
        ; Top1 \= e, Bottom1 \= e, Top3 = e, Bottom3 = e,
            NewTop1 = e, NewBottom1 = Bottom1, NewTop3 = e, NewBottom3 = Top1
        ; Top1 = e, Bottom1 \= e, Top3 = e, Bottom3 = Bottom1,
            NewTop1 = e, NewBottom1 = e, NewTop3 = Bottom1, NewBottom3 = Bottom3
        ; Top1 = e, Bottom1 \= e, Top3 = e, Bottom3 = e,
            NewTop1 = e, NewBottom1 = e, NewTop3 = e, NewBottom3 = Bottom1
        ),
        NewTop2 = Top2, NewBottom2 = Bottom2
    ;

    A = pour(2, 1),
        (
          Top2 \= e, Bottom2 \= e, Top1 = e, Bottom1 = Top2,
            NewTop2 = e, NewBottom2 = Bottom2, NewTop1 = Top2, NewBottom1 = Bottom1
        ; Top2 \= e, Bottom2 \= e, Top1 = e, Bottom1 = e,
            NewTop2 = e, NewBottom2 = Bottom2, NewTop1 = e, NewBottom1 = Top2
        ; Top2 = e, Bottom2 \= e, Top1 = e, Bottom1 = Bottom2,
            NewTop2 = e, NewBottom2 = e, NewTop1 = Bottom2, NewBottom1 = Bottom1
        ; Top2 = e, Bottom2 \= e, Top1 = e, Bottom1 = e,
            NewTop2 = e, NewBottom2 = e, NewTop1 = e, NewBottom1 = Bottom2
        ),
        NewTop3 = Top3, NewBottom3 = Bottom3
    ;

    A = pour(2, 3),
        (
          Top2 \= e, Bottom2 \= e, Top3 = e, Bottom3 = Top2,
            NewTop2 = e, NewBottom2 = Bottom2, NewTop3 = Top2, NewBottom3 = Bottom3
        ; Top2 \= e, Bottom2 \= e, Top3 = e, Bottom3 = e,
            NewTop2 = e, NewBottom2 = Bottom2, NewTop3 = e, NewBottom3 = Top2
        ; Top2 = e, Bottom2 \= e, Top3 = e, Bottom3 = Bottom2,
            NewTop2 = e, NewBottom2 = e, NewTop3 = Bottom2, NewBottom3 = Bottom3
        ; Top2 = e, Bottom2 \= e, Top3 = e, Bottom3 = e,
            NewTop2 = e, NewBottom2 = e, NewTop3 = e, NewBottom3 = Bottom2
        ),
        NewTop1 = Top1, NewBottom1 = Bottom1
    ;

    A = pour(3, 1),
        (
          Top3 \= e, Bottom3 \= e, Top1 = e, Bottom1 = Top3,
            NewTop3 = e, NewBottom3 = Bottom3, NewTop1 = Top3, NewBottom1 = Bottom1
        ; Top3 \= e, Bottom3 \= e, Top1 = e, Bottom1 = e,
            NewTop3 = e, NewBottom3 = Bottom3, NewTop1 = e, NewBottom1 = Top3
        ; Top3 = e, Bottom3 \= e, Top1 = e, Bottom1 = Bottom3,
            NewTop3 = e, NewBottom3 = e, NewTop1 = Bottom3, NewBottom1 = Bottom1
        ; Top3 = e, Bottom3 \= e, Top1 = e, Bottom1 = e,
            NewTop3 = e, NewBottom3 = e, NewTop1 = e, NewBottom1 = Bottom3
        ),
        NewTop2 = Top2, NewBottom2 = Bottom2
    ;

    A = pour(3, 2),
        (
          Top3 \= e, Bottom3 \= e, Top2 = e, Bottom2 = Top3,
            NewTop3 = e, NewBottom3 = Bottom3, NewTop2 = Top3, NewBottom2 = Bottom2
        ; Top3 \= e, Bottom3 \= e, Top2 = e, Bottom2 = e,
            NewTop3 = e, NewBottom3 = Bottom3, NewTop2 = e, NewBottom2 = Top3
        ; Top3 = e, Bottom3 \= e, Top2 = e, Bottom2 = Bottom3,
            NewTop3 = e, NewBottom3 = e, NewTop2 = Bottom3, NewBottom2 = Bottom2
        ; Top3 = e, Bottom3 \= e, Top2 = e, Bottom2 = e,
            NewTop3 = e, NewBottom3 = e, NewTop2 = e, NewBottom2 = Bottom3
        ),
        NewTop1 = Top1, NewBottom1 = Bottom1
  ).
