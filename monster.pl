:-module(monster,[roll_character/2,
                  start_monster_generation/0,
                  generate_monster/1,
                  monster_action/1]).

:-use_module(mtbl).
:-use_module(dungeon).
:-use_module(actions).
:-use_module(character).
:-use_module(screen).
:-use_module(visibility).

roll_character(W,creature(C,T)):-
   rolling_inst(creature(C,T),I),
   roll_character_int(W,C,I).

roll_character_int(_,_,[]).
roll_character_int(W,C,[inherit(Type)|Tail]):-
   rolling_inst(creature(C,Type),I),
   roll_character_int(W,C,I),
   !,roll_character_int(W,C,Tail).
roll_character_int(W,C,[inherit(IC,Type)|Tail]):-
   rolling_inst(creature(IC,Type),I),
   roll_character_int(W,C,I),
   !,roll_character_int(W,C,Tail).
roll_character_int(W,C,[roll(At,N,S,Ad)|T]):-
   roll_dice(N,S,Ad,V),
   attribute(W-At,_,V),
   !,roll_character_int(W,C,T).
roll_character_int(W,C,[change(At,V,E)|T]):-
   attribute(W-At,V),
   NewV is E,
   attribute(W-At,NewV),
   !,roll_character_int(W,C,T).
roll_character_int(W,C,[K=V|T]):-
   attribute(W-K,_,V),!,roll_character_int(W,C,T).
roll_character_int(W,C,[choose(A,L)|T]):-
   random_element(L,V),attribute(W-A,_,V),!,roll_character_int(W,C,T).

start_monster_generation:-
   flag(turn,T),
   monster_type(M),once(gen_int(M,I)),
   D is random(I)+T+1,
   assert(on_turn(D,generate_monster(M))),
   fail.
start_monster_generation.

generate_monster(M):-
log_stuff('generating monster ~w~n',[M]),
   attribute(player-dungeon,D),attribute(player-level,L),
   random_passable_coords(walk,D,L,R,C),
   \+(character(D,L,R,C,_)),
   flag(next_monster,N,N+1),
   W is N+1,
   attribute(W-dungeon,_,D),attribute(W-level,_,L),
   attribute(W-row,_,R),attribute(W-column,_,C),
   assert(character(D,L,R,C,W)),
   attribute(W-type,_,M),
   roll_character(W,M),
   assert(per_turn(W)),
   request_screen_update(R,C,1,1),!.

monster_action(W):-
   attribute(player-dungeon,D),
   attribute(player-level,L),
   \+((attribute(W-dungeon,D),attribute(W-level,L))).
monster_action(W):-
   attribute(player-alignment,PA),attribute(W-alignment,WA),
   align_attacks(WA,PA),
   attribute(W-row,R),attribute(W-column,C),
   visible(R,C),
   attribute(player-row,PR),attribute(player-column,PC),
   attribute(W-player_seen,_,coords(PR,PC)),
   monster_walk_towards(W,PR,PC,R,C).
monster_action(W):-
   attribute(player-alignment,PA),attribute(W-alignment,WA),
   align_attacks(WA,PA),
   attribute(W-player_seen,coords(PR,PC)),
   attribute(W-row,R),attribute(W-column,C),
   monster_walk_towards(W,PR,PC,R,C).
monster_action(W):-
   random_element([north,south,east,west],D),
   cautious_walk(W,D).

monster_walk_towards(W,PR,_,WR,_):-random(2)=:=0,PR<WR,cautious_walk(W,north).
monster_walk_towards(W,PR,_,WR,_):-random(2)=:=0,PR>WR,cautious_walk(W,south).
monster_walk_towards(W,_,PC,_,WC):-PC>WC,cautious_walk(W,east).
monster_walk_towards(W,_,PC,_,WC):-PC<WC,cautious_walk(W,west).
monster_walk_towards(W,PR,_,WR,_):-PR<WR,cautious_walk(W,north).
monster_walk_towards(W,PR,_,WR,_):-PR>WR,cautious_walk(W,south).

% FIXME updates