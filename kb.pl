% Welcome to the brain/heart of the TIC TAC TOE game, this is where all the magic happens!

% The game is represented like a list with [board, player, state] as the variables.

% The board is a 2D List that we later convert into a GUI box style matrix.

:- use_module(library(clpfd)).

% First of all, we need to tell prolog who it and its opponent is. In other words
% We need to define the players of the game.
player(X) :- known(player, X).
opponent(X) :- known(opponent, X).

%%%%%%%%%%%%%%%% The following lines are the evaluation function, which counts the rows, columns,
% and diagonals that the player can win.
% First it counts and checks the rows in which the player can still win.
count_rows([], 0).
count_rows([R|Rs], X) :-
    opponent(Y),
    not(member(Y, R)),
    count_rows(Rs, Count),
    X is 1+Count
    ;
    count_rows(Rs, X).

% Next it counts and checks the columns in which the player can still win.
count_columns([], 0).
count_columns(Board, X) :-
    opponent(Y),
    maplist(pop, Board, Front),
    not(member(Y, Front)),
    maplist(end, Board, Board2),
    count_columns(Board2, Count),
    X is 1+Count
    ;
    maplist(end, Board, Board2),
    not(empty(Board2)),
    count_columns(Board2, X)
    ;
    X is 0
    .

% Finally, it counts and checks the diagonals to see if the player can win there.
count_diagonal_one([R|Board2], 1) :-
    pop(R, F),
    opponent(Y),
    not(F = Y),
    maplist(end, Board2, Board3),
    count_diagonal_one(Board3, Z),
    (   Z = 1;
    empty(Board3)).
count_diagonal_one(_, 0).

count_diagonal_two([R|Board2], 1) :-
    popEnd(R, F),
    opponent(Y),
    not(F = Y),
    maplist(front, Board2, Board3),
    count_diagonal_one(Board3, Z),
    (   Z = 1;
    empty(Board3)).
count_diagonal_two(_, 0).

% After it is done counting and checking the rows, columns, and diagonals
% it returns a evaluation based on the Positionsible number of ways for X to win and transforms it into a heuristic.
evaluatePosition([Board, _, game], 0, Count) :-
    count_rows(Board, X),
    count_columns(Board, Y),
    count_diagonal_one(Board, Z),
    count_diagonal_two(Board, U),
    Count is X+Y+Z+U.

%%%%%%%%%% The evaluation function ends here

% Now, we need to see if previous player won and also check the value of a certain board.
evaluatePosition([_, X, win], Depth, 100+Depth) :- opponent(X).
evaluatePosition([_, X, win], Depth, -100+Depth) :- player(X).
evaluatePosition([_, _, draw], Depth, Depth).

% This function checks if a board ends in a draw by looking for empty cells.
drawBoard(Board) :-
    flatten(Board, Board2),
    not(member(., Board2)).

% Now we need to check if all elements in a list are equal
same([L|R], P) :-
    L == P,
    same(R, P).
same([], _).

% Checking rows to see if any row is a win or not and continuing until we reach the end of the board
check_the_row([R], P) :-
    same(R, P).
check_the_row([R|Rs], P) :-
    same(R, P);
    check_the_row(Rs, P).

% Getting first or last of the 2D List.
pop([A|_], A).
popEnd([_,H|Rest], Last) :- !, popEnd([H|Rest], Last).
popEnd([Last], Last).

% After getting the first or last of a list, we need to remove it
end([_|B], B).
end([], []).
front([_], []).
front([F|End], [F|WithoutLast]) :-
    front(End, WithoutLast).

% After removing, we need to check if remaining list or sublists are all empty
empty([]).
empty([A|B]) :-
    A == [],
    empty(B).

% Checking columns to see if any column is a win or not and continuing until we reach the end of the board
check_the_column([], _).
check_the_column(Board, P) :-
    ( maplist(pop, Board, Front),
      same(Front, P)
    );
    ( maplist(end, Board, Board2),
      (   not(empty(Board2)),
      check_the_column(Board2, P))
    ).

% Checking for player-element matching in the diagonal.
diagonalOne([], _).
diagonalOne([R|Board2], P) :-
    pop(R, F),
	F = P,
    maplist(end, Board2, Board3),
    diagonalOne(Board3, P).

diagonalTwo([], _).
diagonalTwo([R|Board2], P) :-
    popEnd(R, F),
    F = P,
    maplist(front, Board2, Board3),
    diagonalTwo(Board3, P).


% Finally, checking if player P won the board with all 4 rules or not
winBoard(Board, P) :-
    check_the_row(Board, P);
    check_the_column(Board, P);
    diagonalOne(Board, P);
    diagonalTwo(Board, P).

% Now we need to determine whether to do a min or max operation in a tree
min_to_move([_, X, _]) :- opponent(X).
max_to_move([_, X, _]) :- player(X).
next(x, o).
next(o, x).

% Now we can move board in one of  three options.
% Option one - From playing to win
move([B1, P1, game], [B2, P2, win]) :-
    next(P1, P2),
    move_over(P1, B1, B2),
    winBoard(B2, P1), !.

% Option two - From playing to draw
move([B1, P1, game], [B2, P2, draw]) :-
    next(P1, P2),
    move_over(P1, B1, B2),
    drawBoard(B2), !.

% Option three - From playing to playing
move([B1, P1, game], [B2, P2, game]) :-
    next(P1, P2),
    move_over(P1, B1, B2).

make_a_move(P, [.|Bs], [P|Bs]).
make_a_move(P, [B|Bs], [B|Bs2]) :-
    make_a_move(P, Bs, Bs2).

move_over(P, [R], [R2]) :-
    make_a_move(P, R, R2).
move_over(P, [R|Rows], [R2|Rows2]) :-
    (   make_a_move(P, R, R2),
    	Rows2 = Rows);
    ( R2 = R ,
    	move_over(P, Rows, Rows2)).

%%%%%% This is where the implementation of the minimax algorithm starts

% minimax(+Position, -BestNextPosition, -Value)
% move(+Position, -NextPosition)
% evaluatePosition(+Position, -Vol)
% min_to_move(+Position)
% max_to_move(+Position)
% minimax(Position, BestNextPosition, Value)
% Position is a Position on Board, Value is its minimax value.
% Best move from a previous position leads to a position through BestNextPosition.

nextmove(Position, BestNextPosition, Value, Depth) :-
    alpha_and_beta(Position, -1000, 1000, BestNextPosition, Value, Depth).

minimax(Position, BestNextPosition, Value) :-
    % Moves that are allowed in Position produce NextPositionList
    bagof(NextPosition, move(Position, NextPosition), NextPositionList),
    best(NextPositionList, BestNextPosition, Value), !
    ;
    % Evalutate the Position if it has no successors
    evaluatePosition(Position, Value).

best([Position], Position, Value) :-
    minimax(Position, _, Value), !.

best([Position1 | PositionList], BestPosition, BestValue) :-
    minimax(Position1, _, Value1),
    best(PositionList, Position2, Value2),
    better_than(Position1, Value1, Position2, Value2, BestPosition, BestValue).

% If the previous position is better than the next one, then either Min will move to previous Position
% and Max prefers the greater value or Max moves to the previous position and Min prefers the lesser value
better_than(Position0, Value0, _, Value1, Position0, Value0) :-   % If Position0 better than Position1
    min_to_move(Position0),                         % MIN to move in Position0
    Value0 > Value1, !                             % MAX prefers the greater value
    ;
    max_to_move(Position0),                         % MAX to move in Position0
    Value0 < Value1, !.                            % MIN prefers the lesser value

better_than(_, _, Position1, Value1, Position1, Value1).        % If not then, Position1 better than Position0

alpha_and_beta(Position, Alpha, Beta, BestPosition, Value, Depth):-
	Depth > 0,
	bagof(NextPosition, move(Position, NextPosition), PositionList),
	bounded_at_its_best( PositionList, Alpha, Beta, BestPosition, Value, Depth), !
	;
	evaluatePosition(Position, Depth, Value).

bounded_at_its_best([Position1 | PositionList], Alpha, Beta, BestPosition, BestValue, Depth):-
	D1 is Depth - 1,
	alpha_and_beta(Position1, Alpha, Beta, _, Value, D1),
	good_enough(PositionList, Alpha, Beta, Position1, Value, BestPosition, BestValue, Depth).

% If there is no other candidate, then
good_enough([], _, _, Position, Value, Position, Value, _)  :-  !.

% either Max attains upper or lower bound.
good_enough(_, Alpha, Beta, Position, Value, Position, Value, _)  :-
	min_to_move( Position), Value > Beta, !              % upper bound
	;
	max_to_move( Position), Value < Alpha, !.            % lower bound

good_enough(PositionList, Alpha, Beta, Position, Value, BestPosition, BestValue, Depth)  :-
	new_bounds(Alpha, Beta, Position, Value, NewAlpha, NewBeta),    % Refining the bounds
	bounded_at_its_best(PositionList, NewAlpha, NewBeta, Position1, Value1, Depth),
	better_than( Position, Value, Position1, Value1, BestPosition, BestValue).

% Max increases lower bound
new_bounds(Alpha, Beta, Position, Value, Value, Beta)  :-
	min_to_move( Position), Value > Alpha, !.

% Min decreases upper bound
new_bounds( Alpha, Beta, Position, Value, Alpha, Value)  :-
	max_to_move( Position), Value < Beta, !.                 % Minimizer decreased upper bound

% If there is other candidate then bounds remain unchanged
new_bounds( Alpha, Beta, _, _, Alpha, Beta).

% Again, if previous position better than next one, then
better_than( Position, Value, _, Value1, Position, Value)  :-
	min_to_move( Position),
    Value > Value1, !
	;
	max_to_move( Position),
    Value < Value1, !.
% and if not then the next position is better
better_than( _, _, Position1, Value1, Position1, Value1).
