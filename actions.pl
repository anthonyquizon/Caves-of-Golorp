:-module(actions,[take_action/1,on_turn/2,per_turn/1,
                  cautious_walk/2,walk/2,climb/2,teleport/3,
                  attack/2,become_dead/1]).

:-dynamic on_turn/2,per_turn/1.

:-use_module(dungeon).
:-use_module(screen).
:-use_module(visibility).
:-use_module(character).
:-use_module(messages).
:-use_module(pcmd).
:-use_module(monster).
:-use_module(mtbl).

take_action(W):-
   atomic(W),attribute(W-hit_points,H),H>=100,
   become_dead(W).
take_action(W):-
   atomic(W),attribute(W-hit_points,HA),HA>0,
   attribute(W-susceptibility,S),random(S+3)<3,
   HB is HA-1,attribute(W-hit_points,_,HB),
   (W=player,request_screen_update(attributes,hit_points);true),fail.
take_action(player):-
   get_player_command(C),
   process_player_command(C).
take_action(generate_monster(M)):-
   gen_int(M,I),
   flag(turn,T),
   F is random(I)+T+1,
   assert(on_turn(F,generate_monster(M))),
   attribute(player-dungeon,D),attribute(player-level,L),
   (monster_level(M,D,L),generate_monster(M);true).
take_action(W):-
   integer(W),monster_action(W).

walk_possible(W,D,Dn,Lv,R,C,NR,NC):-
   attribute(W-row,R),attribute(W-column,C),
   player_dungeon(R,C,CS,_),passable(walk,CS),
   calc_new_location(R,C,D,NR,NC),
   player_dungeon(NR,NC,NS,_),passable(walk,NS),
   attribute(W-dungeon,Dn),attribute(W-level,Lv).

walk(W,D):-
   walk_possible(W,D,Dn,Lv,R,C,NR,NC),
   (character(Dn,Lv,NR,NC,V),attack(W,V);do_walk(W,Dn,Lv,R,C,NR,NC)).

cautious_walk(W,D):-
   walk_possible(W,D,Dn,Lv,R,C,NR,NC),
   (character(Dn,Lv,NR,NC,V),
       (attribute(W-alignment,AA),attribute(V-alignment,AB),
        align_attacks(AA,AB),attack(W,V);!,fail);
    do_walk(W,Dn,Lv,R,C,NR,NC)).

do_walk(W,Dn,Lv,R,C,NR,NC):-
   retractall(character(Dn,Lv,R,C,W)),assert(character(Dn,Lv,NR,NC,W)),
   attribute(W-row,_,NR),attribute(W-column,_,NC),
   (W=player,calculate_visibility,
    flag(requested_cursor_row,_,NR),flag(requested_cursor_col,_,NC),
    request_screen_update(pmove);true),
   request_screen_update(R,C,1,1),
   request_screen_update(NR,NC,1,1).

% FIXME handle non-player case
climb(W,Dir):-W=player,
   attribute(W-dungeon,D),attribute(W-level,L),
   attribute(W-row,R),attribute(W-column,C),
   player_dungeon(R,C,stairs(Dir,ND,NL),_),
   set_player_level(ND,NL),
   opposite_stair(Dir,NDir),
   player_dungeon(NR,NC,stairs(NDir,D,L),_),
   retractall(character(D,L,R,C,W)),assert(character(ND,NL,NR,NC,W)),
   attribute(W-row,_,NR),attribute(W-column,_,NC),
   calculate_visibility,
   aformat(I,'~w to ~w level ~w',[Dir,ND,NL]),
   svi_message(W,climb,I),
   request_screen_update(all),!.

teleport(W,NR,NC):-
   attribute(W-dungeon,D),attribute(W-level,L),
   attribute(W-row,R,NR),attribute(W-column,C,NC),
   retractall(character(D,L,R,C,W)),assert(character(D,L,NR,NC,W)),
   calculate_visibility,
   sv_message(W,teleport),
   request_screen_update(pmove),
   request_screen_update(R,C,1,1),
   request_screen_update(NR,NC,1,1).

calc_new_location(R,C,north,RR,C):-succ(RR,R).
calc_new_location(R,C,south,RR,C):-succ(R,RR).
calc_new_location(R,C,east,R,CC):-succ(C,CC).
calc_new_location(R,C,west,R,CC):-succ(CC,C).

opposite_stair(up,down).
opposite_stair(down,up).

% FIXME
attack(S,O):-
   (attribute(S-attacks,AL);attribute(S-type,ST),attack_list(ST,AL)),
   do_attacks(S,O,AL).

do_attacks(_,_,[]).
do_attacks(S,O,[H|T]):-
   do_attack(S,O,H),!,do_attacks(S,O,T).

hit_attack(hit).
hit_attack(bite).
hit_attack(get).
hit_attack(kick).
hit_attack(pinch).
hit_attack(scratch).
hit_attack(sue).

do_attack(S,O,AT):-
   hit_attack(AT),
   attribute(S-clumsiness,CA),attribute(O-clumsiness,CB),
   random(1000)<max(500*CB/(CA+1),950),
   (opt_attribute(S-hit_dice,HD);HD is 1),
   roll_dice(HD,6,6,HR),
   attribute(S-weakness,WK),attribute(O-susceptibility,SU),
   D is ceiling(HR*SU/(WK+1)),
   attribute(O-hit_points,HA),HB is HA+D,attribute(O-hit_points,_,HB),
   svo_message(S,AT,O),
   berserk_check(S,O),
   (O=player,request_screen_update(attributes,hit_points);true).
do_attack(S,O,hit):-svo_message(S,miss,O).

do_attack(S,O,chance(P,A)):-
   random(100)<P,do_attack(S,O,A).
do_attack(_,_,chance(_,_)).

% pirate special attack!  not implemented yet
% do_attack(S,O,unauth_copy):-
%    svo_message(S,'make an unauthorized copy of',O).

berserk_check(_,player).
berserk_check(S,O):-
   attribute(S-alignment,SA),attribute(O-alignment,OA),
log_stuff('berserk check ~w ~w ~n',[SA,OA]),
   \+(align_attacks(OA,SA)),
log_stuff('Berserk!!~n',[]),
   attribute(O-alignment,_,berserk),
   svi_message(O,go,berserk).
berserk_check(_,_).

% this has a slightly clumsy name so I won't use it again elsewhere by
% accident, because "die" sounds too much like some kind of library call
become_dead(W):-
   retractall(per_turn(W)),retractall(on_turn(_,W)),
   retractall(character(_,_,_,_,W)),
   attribute(W-row,R),attribute(W-column,C),
   request_screen_update(R,C,1,1),
   sv_message(W,die),
   (W=player,flag(game_over,_,1);true).
