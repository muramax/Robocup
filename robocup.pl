:- dynamic field/1, ball/1, player/5, goal_position/2, score/2, ball_caught_by/1, ball_with/1, freeball/1, kicked/1.
:- use_module(library(time)). % Import time utilities

% Delay for given Seconds
delay(Seconds) :-
    sleep(Seconds).

% Define initial ball position (midfield)
initial_ball_position(position(50, 25)).
ball(position(50, 25)).  % Ball starts at midfield

% Define goal positions for each team
goal_position(team2, 100, 25).  
goal_position(team1, 0, 25). 

% Initialize scores
score(team1, 0).
score(team2, 0).

% Reset ball position at the start of each round
reset_ball :-
    initial_ball_position(Pos),
    retractall(ball(_)),  % Remove existing ball position
    assertz(ball(Pos)),   % Reset ball to midfield
    retractall(ball_caught_by(_)), % Clear goalkeeper possession
    format('Ball reset to (~w, ~w)~n', [50, 25]).

% Boundary checking predicate with new adjusted coordinate if exceeds field size
check_bounds(X, Y, SafeX, SafeY) :-
    % Check the X position and adjust X if it exceeds
    ( X < 5 -> SafeX is 5 ;
      X > 95 -> SafeX is 95 ;
      SafeX is X
    ),
    % Check the Y position and adjust Y if it exceeds
    ( Y < 5 -> SafeY is 5 ;
      Y > 45 -> SafeY is 45 ;
      SafeY is Y
    ).

% Check if the ball is within a 1-unit range of a player
ball_in_range(Team, Role, N) :-
    player(Team, Role, position(X, Y), _, _),
    ball(position(BX, BY)),
    abs(X - BX) =< N,
    abs(Y - BY) =< N.

% Move player towards the ball, with different conditions for forward and defender
move_towards_ball(Team, Role) :-
    player(Team, Role, position(X1, Y1), stamina(S), _),
    player(Team, forward, _, _, status(St)),
    ball(position(X2, Y2)),
    S > 0, % Check if still have stamina
    % Calculate Euclidean distance between player and ball
    distance(X1, Y1, X2, Y2, D),  % D is the distance between the player and the ball
    (
        Role = forward, freeball(X), X = yes -> true ; % Allow forward to move when the ball is free
        Team = team1, Role = forward, X1 < 50 -> true ; % Allow team1's forward to move only if they are on their side
        Team = team2, Role = forward, X1 > 50 -> true ; % Allow team2's forward to move only if they are on their side
        (Team = team1, Role = defender, St = no, D =< 10, X2 < 50); % Allow team1's defender to move if they are within 10 units of the ball and still on their side
        (Team = team2, Role = defender, St = no, D =< 10, X2 > 50) % Allow team2's defender to move if they are within 10 units of the ball and still on their side
    ),
    % Compute movement direction
    XDiff is X2 - X1,
    YDiff is Y2 - Y1,
    sign(XDiff, DX),
    sign(YDiff, DY),

    % IF Ball is not in player's possesion, player moves toward ball
    ((DX =\= 0 ; DY =\= 0) ->
        NewX is X1 + DX,
        NewY is Y1 + DY,
        check_bounds(NewX, NewY, SafeX, SafeY), % Boundary check

        NewS is max(0, S - 1),  % Reduce stamina with movement

        retract(player(Team, Role, position(X1, Y1), stamina(S), _)),
        assertz(player(Team, Role, position(SafeX, SafeY), stamina(NewS), _)),

        format('~w ~w moves to (~w, ~w) with stamina ~w~n', [Team, Role, NewX, NewY, NewS])
    );
    % ELSE Player is idle, player gain stamina
    gain_stamina(Team, Role).

% Sign function to normalize movement direction
sign(X, 1) :- X > 0.
sign(X, -1) :- X < 0.
sign(X, 0) :- X =:= 0.

% Kick the ball towards the opponents goal                   
kick_ball(Team, Role) :-
    opponent_team(Team, Opponent), % Get the opponents goal
    player(Opponent, goalkeeper, position(GoalX, GoalY), stamina(_),status(no)),
    player(Team, Role, position(X1, Y1), stamina(S),status(yes)),
    ball(position(X2, Y2)),
    S > 0, % Check if still have stamina                   
    NewS is max(0, S - 5),  % Reduce stamina with kick ball    

    abs(X1 - X2) =< 1, abs(Y1 - Y2) =< 1, % Ensures that the forward is near the ball before kicking the ball
    abs(X1 - GoalX) =< 8, abs(Y1 - GoalY) =< 8, % Ensures forward is within 8 units of the opponent's goal before kicking the ball to goal 
    XDiff is GoalX - X2 - 1, % XDiff calculates distance between ball and goal

    sign(XDiff, DX),

    (Team = team1 -> NewBallX is X2 + (DX * XDiff); NewBallX is X2 - (DX * XDiff)), % Ball moves toward opponent goal
    random(20, 30 , NewBallY),  % Randomly Generates (20-30) the y-coordinate of ball within the goalpost after kicked 
    check_bounds(NewBallX, NewBallY, SafeBallX, SafeBallY), % Boundary check (ball) (not neccessary)

    retract(player(Team, Role, position(X1, Y1), stamina(S),status(yes))),
    assertz(player(Team, Role, position(X1, Y1), stamina(NewS),status(no))),
    retract(ball(position(X2, Y2))),
    assertz(ball(position(SafeBallX, SafeBallY))),
    assertz(kicked(Team)),

    format('~w ~w kicks the ball to (~w, ~w)~n', [Team, Role, SafeBallX, SafeBallY]).

% Define opponent team relation
opponent_team(team1, team2).
opponent_team(team2, team1).

goalkeeper_move(Team) :-                
    opponent_team(Team, Opponent),
    player(Opponent, forward, position(XP, YP), _, status(yes)), % Opponent has the ball
    player(Team, goalkeeper, position(XG, YG), stamina(S), status(no)),
    S > 0, % Check if still have stamina
    NewS is max(0, S - 1),  % Reduce stamina   

    % Check if the opponent with the ball is within range 12
    abs(XG - XP) =< 12,
    abs(YG - YP) =< 12,

    % Ensure goalkeeper stays within field bounds (assuming 15 <= Y <= 35 for goal area)
    (Team = team1 -> NewX is max(5, XG) ; NewX is min(95, XG)),  
    (YP > YG -> NewY is min(35, YG + 1) ; NewY is max(15, YG - 1)),  % Move up or down to follow the ball

    % Ensure goalkeeper stays within bounds
    NewY >= 15, NewY =< 35,
    (Team = team1 -> NewX =< 10 ; NewX >= 90),

    % Move the goalkeeper
    retract(player(Team, goalkeeper, position(XG, YG), stamina(S), status(no))),
    assertz(player(Team, goalkeeper, position(NewX, NewY), stamina(NewS), status(no))),

    format('~w goalkeeper moves to (~w, ~w) within the goal area~n', [Team, NewX, NewY]).

% Checks if the ball is within a 2-unit range of the goalkeeper, making it catchable
catchable(X, Y, BX, BY) :-
    DX is abs(X - BX),
    DY is abs(Y - BY),
    DX =< 2, DY =< 2.

% Checks if the ball is out of the 2-unit range, making it uncatchable
uncatchable(X, Y, BX, BY) :-
    DX is abs(X - BX),
    DY is abs(Y - BY),
    (DX > 2; DY > 2).

% Goalkeeper catches the ball
catch_ball(Team) :-
    kicked(Wt), % Indicates the last team that kicked the ball.
    opponent_team(Team, Opponent),
    Wt = Opponent,
    player(Team, goalkeeper, position(X, Y), stamina(S), _),
    S > 0, % Check if still have stamina
    NewS is max(0, S - 5),  % Reduce stamina
    ball(position(BX, BY)),

    % Use helper predicate catchable to decide whether the ball is catchable
    (   catchable(X, Y, BX, BY) ->
        (Team = team1 -> BallX is X + 1 ; BallX is X - 1),  % Move the ball to the front of the goalkeeper
        retract(ball(position(BX, BY))),
        assertz(ball(position(BallX, BY))),
        retractall(ball_caught_by(_)),  % Ensure only one goalkeeper holds the ball
        retractall(kicked(_)),  % Reset kicked state
        assertz(ball_caught_by(Team)),  % Mark ball possession for the next round
        retract(player(Team, goalkeeper, position(X, Y), stamina(S), status(no))),
        assertz(player(Team, goalkeeper, position(X, Y), stamina(NewS), status(yes))),

        % Prevents the opponent player from stealing the ball
        retractall(freeball(_)),
        assertz(freeball(no)),

        format('~w goalkeeper catches the ball at (~w, ~w)!~n', [Team, X, Y])
    ).

% Goalkeeper cannot catch the ball hence the ball goes into the goal
uncatch_ball(Team) :-
    kicked(Wt), % Indicates the last team that kicked the ball.
    opponent_team(Team, Opponent),
    Wt = Opponent,
    player(Team, goalkeeper, position(X, Y), stamina(_), _),
    ball(position(BX, BY)),

    % Use helper predicate uncatchable to decide whether the ball is uncatchable
    (   uncatchable(X, Y, BX, BY) ->
        (Team = team1 -> BallX is X - 4 ; BallX is X + 4),  % Move ball towards goal in X direction
        retract(ball(position(BX, BY))),
        assertz(ball(position(BallX, BY))),
        retractall(kicked(_)),  % Reset kicked state

        format('~w goalkeeper cannot catches the ball at (~w, ~w)!~n', [Team, X, Y])
    ).

% Goalkeeper passes the ball
pass_ball_gk(Team) :-
    ball_caught_by(Team),
    player(Team, Role,position(X,Y), stamina(S), status(St)),
    Role = goalkeeper,
    St = yes,
    S > 0, % Check if still have stamina
    NewS is max(0, S - 2),  % Reduce stamina
    find_nearest_teammate(Team, forward , PX, PY), % Find the nearest forward teammate
    (
        Team = team1 -> NewPX is PX + 3;
        Team = team2 -> NewPX is PX - 3
    ),
    delay(1), % Add pausing while goalkeeper holding the ball for the ease of analysing the game-play
    retractall(ball(_)), % Remove the ball from the goalkeeper
    assertz(ball(position(NewPX, PY))), % Move ball to teammate
    retract(player(Team, Role, position(X,Y), stamina(S), status(yes))),

    % Relocate the goalkeeper to its initial position and remove status holding ball
    (Team = team1 -> assertz(player(Team,Role,position(5,25), stamina(NewS), status(no))); assertz(player(Team,Role,position(95,25), stamina(NewS), status(no)))),

    retract(ball_caught_by(Team)),
    assertz(freeball(yes)),

    format('~w goalkeeper passes the ball to teammate at (~w, ~w)!~n', [Team, PX, PY]).

% Dribble the ball so it stays 1 step ahead of the player
dribble(Team, Role) :-
    opponent_team(Team, Opponent),
    player(Team, Role, position(X1, Y1), stamina(S), status(yes)),
    player(Opponent, forward, _, _, status(no)),
    ball(position(BX, BY)),

    S > 0,

    ball_in_range(Team, Role, 1), % Ensure the ball is close
    goal_position(Opponent, GoalX, _),

    % Check for goalkeeper
    player(Opponent,goalkeeper,position(GX, _),_ , _),
    Distance is abs(X1 - GX),

    (   Distance =< 12 ->
        % Move up or down randomly
        random_between(-1, 1, RandomMove),
        NewY1 is Y1 + RandomMove
    ;
        NewY1 is Y1
    ),

    % Calculate direction towards goal
    XDiff is GoalX - X1,
    sign(XDiff, DX),

    % Move player forward
    NewX1 is X1 + DX,

    % Move ball one step ahead of the player
    BallX is NewX1 + DX,
    BallY is NewY1,

    check_bounds(NewX1, NewY1, SafeX, SafeY1), % Boundary check (player)
    check_bounds(BallX,BallY,SafeBallX, SafeBallY), % Boundary check (ball)

    % Reduce stamina
    NewS is max(0, S - 1),

    format('Moving ~w ~w to (~w, ~w) with stamina ~w and ball to (~w, ~w)~n', [Team, Role, SafeX, SafeY1, NewS, SafeBallX, SafeBallY]),

    retract(player(Team, Role, position(X1, Y1), stamina(S), status(yes))),
    assertz(player(Team, Role, position(SafeX, SafeY1), stamina(NewS), status(yes))),

    retract(ball(position(BX, BY))),
    assertz(ball(position(SafeBallX, SafeBallY))).

% Tackle opponent and attempt to take the ball
tackle(Team, Role) :-
    % Get player's position
    player(Team, Role, position(X1, Y1), stamina(S), status(St)),
    St = no,
    % Find an opponent with the ball nearby
    opponent_team(Team, Opponent),
    player(Opponent, X, position(X2, Y2), stamina(OS), status(yes)), % Opponent has the ball
    ball(position(BX, BY)),

    % Ensure the opponent is within 1-unit range
    abs(X1 - X2) =< 1,
    abs(Y1 - Y2) =< 1,
    abs(BX - X2) =< 1,
    abs(BY - Y2) =< 1,

    random(1, 11, Chance), % Generates a number from 1 to 10
    (Chance =< 5 ->
        % Successful tackle: Ball moves to tackling player
        retract(ball(position(BX, BY))),
        assertz(ball(position(X1, Y1))),

        NewS is max(0, S - 2), % Reduce stamina

        % Change possession
        retract(player(Team, Role, position(X1, Y1), stamina(S), status(no))),
        assertz(player(Team, Role, position(X1, Y1), stamina(NewS), status(yes))),

        retract(player(Opponent, X, position(X2, Y2), stamina(OS), status(yes))),
        assertz(player(Opponent, X, position(X2, Y2), stamina(OS), status(no))),

        % Move tackling player towards opponent's goal after successfully tackling
        dribble(Team, Role),
        dribble(Team, Role),
        dribble(Team, Role),
        dribble(Team, Role),
        dribble(Team, Role),
        dribble(Team, Role),

        format('~w ~w successfully tackled and took the ball at (~w, ~w)!~n', [Team, Role, X1, Y1]),

        (Role = defender -> assertz(ball_with(Team)))
    ;
        % Failed tackle:
        format('~w ~w failed to tackle ~w at (~w, ~w)!~n', [Team, Role, Opponent, X2, Y2]),
        
        % Move tackled player towards opponent's goal after fail tackling
        dribble(Opponent, X),
        dribble(Opponent, X),
        dribble(Opponent, X),
        dribble(Opponent, X)
    ).

% Defender pass ball to forward teammate
pass_ball_def(Team, Role) :-
    ball_with(Team),
    player(Team, Role, position(X, Y), Stamina, status(yes)),
    Role = defender,

    % Find the nearest forward teammate (we have only 1 forward per team)
    find_nearest_teammate(Team, forward, PX, PY),

    % Calculate distance to the forward
    distance(X, Y, PX, PY, Dist),

    (   Dist > 25 ->
        % If too far, dribble closer instead of passing
        dribble(Team, Role),
        format('~w defender dribbles closer to forward at (~w, ~w).~n', [Team, PX, PY])
    ;
        % Defender is close enough to pass
        (   Team = team1 -> NewPX is PX - 5
        ;   Team = team2 -> NewPX is PX + 5
        ),

        Low is Y - 10,
        High is Y + 10,
        random(Low, High, NewPY),
        check_bounds(NewPX, NewPY, SafePX, SafePY), % Boundary check (ball)

        % Move the ball to the forward
        retract(ball(_)),
        assertz(ball(position(SafePX, SafePY))),

        % Defender no longer has the ball
        retract(player(Team, Role, position(X, Y), Stamina, status(yes))),
        assertz(player(Team, Role, position(X, Y), Stamina, status(no))),

        % Ball is now free
        retract(ball_with(Team)),
        assertz(freeball(yes)),

        format('~w defender passes the ball to forward at (~w, ~w)!~n', [Team, SafePX, SafePY])
    ).

% Gain stamina for idle players
gain_stamina(Team, Role) :-
    player(Team, Role, position(X, Y), stamina(S), _),
    (
        S < 100 ->  % Only proceed if stamina is below 100
        NewS is min(S + 5, 100),  % Increase stamina, but cap at 100
        retract(player(Team, Role, position(X, Y), stamina(S), _)),
        assertz(player(Team, Role, position(X, Y), stamina(NewS), _)),
        format('~w ~w gains 5 stamina, new stamina is ~w~n', [Team, Role, NewS])
    ;
        % If stamina is already 100, print current player info for debugging
        format('~w ~w stays at (~w, ~w) with stamina ~w~n', [Team, Role, X, Y, S])
    ).

% Find the nearest teammate to pass the ball
find_nearest_teammate(Team, Pos, PX, PY) :-
    player(Team, Pos, position(PX, PY), _, _),  % Find any teammate
    ball(position(BX, BY)),
    distance(BX, BY, PX, PY, _).

% Compute Euclidean distance between two points
distance(X1, Y1, X2, Y2, D) :-
    DX is X2 - X1,
    DY is Y2 - Y1,
    D is sqrt(DX * DX + DY * DY).

% Check for a goal and update the score
check_goal :-
    ball(position(X, Y)),
    % Check for goal on team 2's side
    (   X < 5, Y >= 20, Y =< 29 ->
        delay(1), % Add pause during the ball is inside the goal for analysing the game-play
        update_score(team2),
        format('Goal for Team 2! Ball reached (~w, ~w). Resetting...~n', [X, Y]),
        show_goal_message('Team 2'),
        new_simulation_1, % After team 2 scores, team 1 will gets the ball
        update_display,
        delay(3) % Add pause after reset round for analysing the game-play
    ;
        % Check for goal on team 1's side
        X > 95, Y >= 20, Y =< 29 ->
        delay(1), % Add pause during the ball is inside the goal for analysing the game-play
        update_score(team1),
        format('Goal for Team 1! Ball reached (~w, ~w). Resetting...~n', [X, Y]),
        show_goal_message('Team 1'),
        new_simulation_2, % After team 1 scores, team 2 will gets the ball
        update_display,
        delay(3) % Add pause after reset round for analysing the game-play
    ).

% Update score for a team
update_score(Team) :-
    score(Team, CurrentScore),
    NewScore is CurrentScore + 1,
    retract(score(Team, CurrentScore)),
    assertz(score(Team, NewScore)),
    format('Score Update: ~w - ~w~n', [Team, NewScore]).

check_ball_status :-
    \+ player(_,_,_,_,status(yes)), % No player currently has the ball, so find a nearby player
    ball(position(BX, BY)),
    player(Team, Role, position(X, Y), stamina(S), status(no)),
    
    % Check if the player is within a range of 1 square
    abs(X - BX) =< 1,
    abs(Y - BY) =< 1,

    % If so, take possession
    retractall(freeball(_)),  % Ensure the freeball state is reset

    % Remove the player without the ball and assign possession
    retract(player(Team, Role, position(X, Y), stamina(S), status(no))),
    assertz(player(Team, Role, position(X, Y), stamina(S), status(yes))),
    (Role = defender -> assertz(ball_with(Team))),

    format('~w ~w takes possession of the ball at (~w, ~w)!~n', [Team, Role, X, Y]).

% Simulate one round of the game
simulate_round :-
    (check_goal; true),
    (pass_ball_gk(team1); pass_ball_gk(team2); true),
    (catch_ball(team1); uncatch_ball(team1); catch_ball(team2); uncatch_ball(team2); true),
    (goalkeeper_move(team1); goalkeeper_move(team2); true),
    (move_towards_ball(team1, goalkeeper); move_towards_ball(team2, goalkeeper); true),
    (check_ball_status; true),  % Check if the ball is free and assign possession

    % Players move towards the ball or dribble
    (kick_ball(team1, forward); dribble(team1, forward); move_towards_ball(team1, forward); true),
    (kick_ball(team2, forward); dribble(team2, forward); move_towards_ball(team2, forward); true),

    (pass_ball_def(team1, defender); move_towards_ball(team1, defender); true),
    (pass_ball_def(team2, defender); move_towards_ball(team2, defender); true),

    % Attempt tackles (allow players to tackle opponents with the ball)
    (tackle(team1, forward); tackle(team1, defender); true), % Team 1 defenders or forwards may tackle
    (tackle(team2, forward); tackle(team2, defender); true), % Team 2 defenders or forwards may tackle

    ball(position(BX, BY)),

    format('Ball is now at (~w, ~w)~n', [BX, BY]),
    update_display,
    delay(0.2), % Pause 0.2 seconds after running new frame
    update_score.

% Run simulation (base case)
run_simulation(0) :-
    score(team1, S1),
    score(team2, S2),
    format('Final Score - Team 1: ~w | Team 2: ~w~n', [S1, S2]),
    format('Simulation ended.~n'),
    show_end_message,
    !.

% Run simulation for N rounds
run_simulation(N) :-
    N > 0, % Ensure N is positive
    format('--- Round ~w ---~n', [N]),
    simulate_round,
    N1 is N - 1,
    run_simulation(N1).

% Initiate the game with team 1 starting the play
new_simulation_1 :-
    retractall(player(_,_,_,_,_)),
    assertz(player(team1, forward, position(49, 25), stamina(100),status(no))),
    assertz(player(team1, defender, position(20, 20), stamina(100),status(no))),
    assertz(player(team1, goalkeeper, position(5, 25), stamina(100),status(no))),

    assertz(player(team2, forward, position(65, 35), stamina(100),status(no))),
    assertz(player(team2, defender, position(80, 20), stamina(100),status(no))),
    assertz(player(team2, goalkeeper, position(95, 25), stamina(100),status(no))),

    retractall(ball_caught_by(_)),
    retractall(ball_with(_)),
    retractall(freeball(_)),
    retractall(kicked(_)),
    reset_ball.

% Initiate the game with team 2 starting the play
new_simulation_2 :-
    retractall(player(_,_,_,_,_)),
    assertz(player(team1, forward, position(35, 35), stamina(100),status(no))),
    assertz(player(team1, defender, position(20, 20), stamina(100),status(no))),
    assertz(player(team1, goalkeeper, position(5, 25), stamina(100),status(no))),

    assertz(player(team2, forward, position(51, 25), stamina(100),status(no))),
    assertz(player(team2, defender, position(80, 20), stamina(100),status(no))),
    assertz(player(team2, goalkeeper, position(95, 25), stamina(100),status(no))),

    retractall(ball_caught_by(_)),
    retractall(ball_with(_)),
    retractall(freeball(_)),
    retractall(kicked(_)),
    reset_ball.

% Start game (Team 1 goes first)
start_game :-
    new_simulation_1, start_soccer_field,
    run_simulation(450).  % 1 min and a half game-play

%=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= VISUALIZATION =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
:- use_module(library(pce)).

% Start the soccer field visualization
start_soccer_field :-
    retractall(score(_, _)), % Reset scores
    assertz(score(team1, 0)),
    assertz(score(team2, 0)),
    % Clean up any existing window
    ( retract(display_window(OldD)) ->
        ( object(OldD) -> send(OldD, free); true ),
        true
    ; true ),
    % Create window
    new(Window, picture('Prolog Soccer Field')),
    send(Window, size, size(1000, 515)),
    send(Window, open),
    assertz(display_window(Window)),
    % Draw soccer field
    send(Window, clear),
    draw_field(Window),
    draw_ball(Window),
    draw_players(Window).

:- dynamic display_window/1.

% Update the display
update_display :-
    update_ball,
    update_player,
    (display_window(Window) -> send(Window, flush) ; true).

% Draw the soccer field
draw_field(Window) :-
    % Draw field (green grass)
    send(Window, display, new(Field, box(1000, 500)), point(0, 0)),
    send(Field, fill_pattern, colour(dark_green)),
    % Draw boundary lines (white)
    send(Window, display, new(Border, box(990, 490)), point(5, 5)),
    send(Border, colour, white),
    % Draw the middle line
    send(Window, display, new(Line, line(500, 5, 500, 495))),
    send(Line, colour, white),
    % Draw center circle
    send(Window, display, new(Circle, circle(100)), point(450, 200)),
    send(Circle, colour, white),
    % Draw goal backgrounds
    send(Window, display, new(GoalBg1, box(40, 120)), point(5, 190)),
    send(Window, display, new(GoalBg2, box(40, 120)), point(955, 190)),
    send(GoalBg1, fill_pattern, colour(black)),
    send(GoalBg2, fill_pattern, colour(black)),
    % Draw goalposts
    send(Window, display, new(Goal1, box(40, 120)), point(5, 190)),
    send(Window, display, new(Goal2, box(40, 120)), point(955, 190)),
    send(Goal1, colour, white),
    send(Goal2, colour, white),
    % Draw big goalposts
    send(Window, display, new(Goal3, box(100, 240)), point(5, 130)),
    send(Window, display, new(Goal4, box(100, 240)), point(895, 130)),
    send(Goal3, colour, white),
    send(Goal4, colour, white),
    % Draw Score Box
    send(Window, display, new(T1ScoreBox, box(500, 30)), point(0, 500)),
    send(Window, display, new(T2ScoreBox, box(500, 30)), point(500, 500)),
    send(T1ScoreBox, fill_pattern, colour(red)),
    send(T2ScoreBox, fill_pattern, colour(blue)),
    % Draw Score Name
    send(Window, display, new(T1Text, text('Team 1')), point(200, 505)),
    send(Window, display, new(T2Text, text('Team 2')), point(700, 505)),
    send(T1Text, colour, white),
    send(T2Text, colour, white),
    % Draw Score
    send(Window, display, new(T1ScoreText, text('0')), point(300, 505)),
    send(Window, display, new(T2ScoreText, text('0')), point(800, 505)),
    send(T1ScoreText, colour, white),
    send(T2ScoreText, colour, white),
    % Store Score objects
    assertz(score_object(team1, T1ScoreText)),
    assertz(score_object(team2, T2ScoreText)).

% Draw the ball
draw_ball(Window) :-
    retractall(ball_object(_)),
    ball(position(X, Y)),
    PixelX is X * 10,
    PixelY is Y * 10,
    send(Window, display, new(Ball, circle(10)), point(PixelX - 5, PixelY - 5)),
    send(Ball, fill_pattern, colour(yellow)),
    assertz(ball_object(Ball)).

% Draw all players
draw_players(Window) :-
    retractall(player_object(_, _)),
    retractall(player_label(_, _)),
    retractall(player_stamina(_, _)),
    forall(player(Team, Role, position(X, Y), stamina(S), status(_B)),
        (   PixelX is X * 10,
            PixelY is Y * 10,
            (Team = team1 -> Color = red; Color = blue),
            % Create player circle
            send(Window, display, new(Player, circle(20)), point(PixelX - 10, PixelY - 10)),
            send(Player, fill_pattern, colour(Color)),
            % Create player label Role
            atom_string(Role, RoleStr),
            send(Window, display, new(Label, text(RoleStr)), point(PixelX - 10, PixelY - 50)),
            send(Label, colour, white),
            % Create player label stamina
            number_string(S, StaminaStr),
            atomic_list_concat(['(', StaminaStr, ')'], '', StaminaFormatted),
            send(Window, display, new(StaminaText, text(StaminaFormatted)), point(PixelX - 10, PixelY - 30)),
            send(StaminaText, colour, yellow),
            % Store player objects
            atom_concat(Team, '_', Tmp),
            atom_concat(Tmp, Role, Id),
            assertz(player_object(Id, Player)),
            assertz(player_label(Id, Label)),
            assertz(player_stamina(Id, StaminaText))
        )
    ),
    send(Window, flush).

% Update all players position
update_player :-
    forall(player(Team, Role, position(X, Y), stamina(S), status(_B)),
        (   atom_concat(Team, '_', Tmp),
            atom_concat(Tmp, Role, Id),
            player_object(Id, Obj),
            player_label(Id, Label),
            player_stamina(Id, StaminaText),
            ScaleX is X * 10, ScaleY is Y * 10,
            send(Obj, move, point(ScaleX - 10, ScaleY - 10)),
            send(Label, move, point(ScaleX - 10, ScaleY - 50)),
            number_string(S, StaminaStr),
            atomic_list_concat(['(', StaminaStr, ')'], '', StaminaFormatted),
            send(StaminaText, string, StaminaFormatted),
            send(StaminaText, move, point(ScaleX - 10, ScaleY - 30))
        )
    ).

% Update ball position
update_ball :-
    ball(position(NewX, NewY)),
    ball_object(Obj),
    ScaleX is NewX * 10, ScaleY is NewY * 10,
    send(Obj, move, point(ScaleX-5, ScaleY-5)).

% Update score text
update_score :-
    forall(score_object(Team, Obj),
        (   score(Team, NewScore),
            send(Obj, string, NewScore) 
        )
    ).

show_end_message :-
    display_window(Window),
    score(team1, Score1),
    score(team2, Score2),
    (   Score1 > Score2 -> Message = 'TEAM 1 WINS!'
    ;   Score2 > Score1 -> Message = 'TEAM 2 WINS!'
    ;   Message = 'IT\'S A DRAW!'
    ),
    % Draw ResultBox
    send(Window, display, new(ResultBox, box(400, 120)), point(300, 180)),
    send(ResultBox, fill_pattern, colour(white)),
    send(ResultBox, pen, 3),
    send(ResultBox, colour, black),
    % Draw ResultText
    send(Window, display, new(ResultText, text(Message, center, font(helvetica, bold, 24))), point(400, 220)),
    send(ResultText, colour, black),
    % Use XPCE timer to remove the message after 5 seconds
    new(Timer, timer(5.0, and(message(ResultBox, free), message(ResultText, free)))),
    send(Timer, start),
    % Also Remove the message if clicked
    send(ResultBox, recogniser, click_gesture(left, '', single,
        and(message(Timer, stop), message(ResultBox, free), message(ResultText, free))
    )).

show_goal_message(ScoringTeam) :-
    display_window(Window),
    score(team1, Score1),
    score(team2, Score2),
    format(atom(Message), '~w Goal!\n~d : ~d', [ScoringTeam, Score1, Score2]),
    % Draw GoalBox
    send(Window, display, new(GoalBox, box(180, 100)), point(425, 200)),
    send(GoalBox, fill_pattern, colour(yellow)),
    send(GoalBox, pen, 3),
    send(GoalBox, colour, black),
    % Draw GoalText
    send(Window, display, new(GoalText, text(Message, center, font(helvetica, bold, 20))), point(440, 225)),
    send(GoalText, colour, black),
    % Use XPCE timer to remove the message after 2 seconds
    new(Timer, timer(2.0, and(message(GoalBox, free), message(GoalText, free)))),
    send(Timer, start).


% -=-=-=-=-=--=-=-=-=-=--=-=-=-=-=- HOW TO RUN OUR SOCCER SIMULATION -=-=-=-=-=-=-=-=-=-=--=-=-=-=-=-
% 
% 1) Consult the file in SWI-Prolog.
% 2) Type "?- start_game." to begin the simulation.
%
% The game will run for *** 1 minute and 30 seconds *** (450 iterations) with Team 1 going first.
% After the game ends, the results will be displayed for 5 seconds
% You can click to dismiss the result window.
%
% If you want to continue the simulation manually, you can run:
% "?- run_simulation(N)."  % (N is the number of rounds you want to proceed)
%
% -=-=-=- Alternative Ways to Start the Simulation -=-=-=-
%
% If you want to **manually control the number of rounds**, you can start with:
% 
% - To make **Team 1 go first**:
%   """
%   ?- new_simulation_1.
%   ?- start_soccer_field.
%   ?- run_simulation(N).  % (N is the number of rounds)
%   """
%
% - To make **Team 2 go first**:
%   """
%   new_simulation_2.
%   start_soccer_field.
%   run_simulation(N).  % (N is the number of rounds)
%   """
%
% -=-=-=-=-=-=--=- Hope you enjoy our work. We really put a lot of effort into it! -=-=-=-=-=--=-=--=-