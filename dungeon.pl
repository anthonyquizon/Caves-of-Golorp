:-module(dungeon,[dungeon/6,
                  player_dungeon/4,
                  level_type/3,
                  start_digging/2,
                  dungeon_generated/2,
                  dungeon_init/0,
                  set_player_level/2,
                  passable/2,
                  random_room_coords/4,
                  random_passable_coords/5]).

:-dynamic dungeon/6,level_type/3,player_dungeon/4,need_stairs/3.
:-index(dungeon(1,1,1,1,0,0)).
:-index(player_dungeon(1,1,0,0)).
:-thread_local current_dungeon/3,already_visited/2,room_comp_count/1.
:-index(already_visited(1,1)).
:-index(current_dungeon(1,1,0)).

dungeon_init:-
   mutex_create(dungeon_gen),mutex_create(player_level),
   SB is random(10)+5,assert(need_stairs(main,SB,stairs(up,tower,1))).

passable(walk,room).
passable(walk,corridor).
passable(walk,stairs(_,_,_)).

passable(see,room).
passable(see,corridor).
passable(see,stairs(_,_,_)).

max_level(main,_):-!,fail.

choose_level_type(D,L,T):-
   findall(PT,level_type_allowed(D,L,PT),TL),
   random_element(TL,T).

% level_type_allowed(main,L,big_room).
% level_type_allowed(main,L,tower_grid_vh).
% level_type_allowed(main,L,tower_grid_hv).
level_type_allowed(main,L,multi_room):-L=<50.
level_type_allowed(main,L,easy_maze):-L>=10,L=<30.
level_type_allowed(main,L,hard_maze):-L>=20,L=<50.
level_type_allowed(main,L,cave_maze):-L>=30.
level_type_allowed(main,L,caves):-L>=40.

level_type_allowed(tower,_,tower_grid_vh).
level_type_allowed(tower,_,tower_grid_hv).

need_stairs(main,L,stairs(up,main,LL)):-L>1,succ(LL,L).
need_stairs(main,L,stairs(down,main,LL)):-
   \+((max_level(main,ML),L>=ML)),succ(L,LL).
% we will assert:  need_stairs(main,SomeLevel,stairs(up,tower,1)).

need_stairs(tower,L,stairs(up,tower,LL)):-succ(L,LL).
need_stairs(tower,L,stairs(down,tower,LL)):-L>1,succ(LL,L).
need_stairs(tower,1,stairs(down,main,LL)):-
   need_stairs(main,LL,stairs(up,tower,1)).

random_passable_coords(W,D,L,R,C):-
   findall([Ri,Ci],(dungeon(D,L,Ri,Ci,Sq,_),passable(W,Sq)),RCL),
   random_element(RCL,[R,C]),!.

random_room_coords(D,L,R,C):-
   findall([Ri,Ci],dungeon(D,L,Ri,Ci,room,_),RCL),
   random_element(RCL,[R,C]),!.

random_room_coords(R,C):-
   findall([Ri,Ci],current_dungeon(Ri,Ci,room),RCL),
   random_element(RCL,[R,C]),!.

place_stairs(D,L):-
   need_stairs(D,L,stairs(T,OD,OL)),
   random_room_coords(R,C),
   retractall(current_dungeon(R,C,_)),
   assertz(current_dungeon(R,C,stairs(T,OD,OL))),fail.
place_stairs(_,_).

on_dungeon_block(_,_,_,0,_,_):-!.
on_dungeon_block(_,_,0,_,_,_):-!.
on_dungeon_block(R,C,1,1,What,Parms):-
   append([What,R,C],Parms,UL),
   ToCall=..UL,!,call(ToCall).
on_dungeon_block(R,C,1,Cs,What,Parms):-
   !,on_dungeon_block(R,C,1,1,What,Parms),
   succ(C,NewC),succ(NewCs,Cs),
   on_dungeon_block(R,NewC,1,NewCs,What,Parms).
on_dungeon_block(R,C,Rs,Cs,What,Parms):-
   !,on_dungeon_block(R,C,1,Cs,What,Parms),
   succ(R,NewR),succ(NewRs,Rs),
   on_dungeon_block(NewR,C,NewRs,Cs,What,Parms).

clear_for_room(R,C):- \+current_dungeon(R,C,_).

dig(R,C,_):-current_dungeon(R,C,_),!.
dig(R,C,What):-flag(dug,N,N+1),assertz(current_dungeon(R,C,What)).

build_rooms(0):-!.
build_rooms(N):-
   repeat,
   R is 1+random(20),C is random(78),
   LRs is 21-R,LCs is 78-C,
   (LRs<10->MRs=LRs;MRs=10),
   (LCs<18->MCs=LCs;MCs=18),
   Rs is 2+random(MRs),Cs is 3+random(MCs),
   on_dungeon_block(R,C,Rs,Cs,clear_for_room,[]),
   on_dungeon_block(R,C,Rs,Cs,dig,[room]),
   succ(NN,N),
   !,build_rooms(NN).

room_component_scan(R,C):-already_visited(R,C),!.
room_component_scan(R,C):-
   current_dungeon(R,C,room),
   retract(room_comp_count(N)),succ(N,NN),assert(room_comp_count(NN)),
   !,scan_from(R,C).
room_component_scan(_,_):-!.

scan_from(R,C):-already_visited(R,C),!.
scan_from(R,C):-
   current_dungeon(R,C,What),passable(walk,What),
   assert(already_visited(R,C)),
   PR is R-1,succ(R,NR),
   PC is C-1,succ(C,NC),
   scan_from(PR,C),
   scan_from(R,NC),
   scan_from(NR,C),
   scan_from(R,PC),!.
scan_from(_,_):-!.

room_components(C):-
   assert(room_comp_count(0)),
   on_dungeon_block(1,0,21,80,room_component_scan,[]),
   retractall(already_visited(_,_)),
   retract(room_comp_count(AC)),log_stuff('components: ~w.~n',[AC]),!,AC=C.

try_corridor(R,C,Rs,Cs,no):-
   on_dungeon_block(R,C,Rs,Cs,clear_for_room,[]),!.
try_corridor(R,C,Rs,Cs,yes):-
   on_dungeon_block(R,C,Rs,Cs,dig,[corridor]),!.

try_tower_corridor(R,C):-
   clear_for_room(R,C),
   PR is R-1,NR is R+1,
   PC is C-1,NC is C+1,
   (current_dungeon(PR,C,room),current_dungeon(NR,C,room);
    current_dungeon(R,PC,room),current_dungeon(R,NC,room)),
   dig(R,C,corridor),!.

connect_tower:-
   repeat,
   R is 2+random(19),
   C is 20+random(39),
   try_tower_corridor(R,C),
   room_components(1).

connect_with_corridors(_,_,_,_,yes):-room_components(1),!.
connect_with_corridors(MinR,RRange,MinC,CRange,_):-
   R is MinR+random(RRange),C is MinC+random(CRange),
   Rs is 1+random(MinR+RRange-R),
   Cs is 1+random(MinC+CRange-C),
   ((random(2)=:=0)->try_corridor(R,C,Rs,1,Didit);
     try_corridor(R,C,1,Cs,Didit)),
   !,connect_with_corridors(MinR,RRange,MinC,CRange,Didit).

connect_with_corridors(DoCheck):-connect_with_corridors(1,21,0,80,DoCheck).

can_dig_maze_from_here(coords(SR,SC),coords(ER,SC)):-
   SR>=3,ER is SR-2,\+(current_dungeon(ER,SC,room)).
can_dig_maze_from_here(coords(SR,SC),coords(ER,SC)):-
   SR=<19,ER is SR+2,\+(current_dungeon(ER,SC,room)).
can_dig_maze_from_here(coords(SR,SC),coords(SR,EC)):-
   SC>=2,EC is SC-2,\+(current_dungeon(SR,EC,room)).
can_dig_maze_from_here(coords(SR,SC),coords(SR,EC)):-
   SC=<77,EC is SC+2,\+(current_dungeon(SR,EC,room)).

dig_maze(SP,EP):-
   SP=coords(SR,SC),EP=coords(ER,EC),
   MR is (SR+ER)//2,MC is (SC+EC)//2,
   dig(MR,MC,room),dig(ER,EC,room).

easy_maze_fill([]):-!.
easy_maze_fill([SP|SPL]):-
   \+(can_dig_maze_from_here(SP,_)),!,easy_maze_fill(SPL).
easy_maze_fill(SPL):-
   random_element(SPL,SP),
   findall(E,can_dig_maze_from_here(SP,E),EPL),
   (EPL=[]->select(SP,SPL,NSPL);
    random_element(EPL,EP),dig_maze(SP,EP),NSPL=[EP|SPL]),
   !,easy_maze_fill(NSPL).

hard_maze_fill([]):-!.
hard_maze_fill([[SP,EP]|SPL]):-
   \+(can_dig_maze_from_here(SP,EP)),!,hard_maze_fill(SPL).
hard_maze_fill(SPL):-
   random_element(SPL,[SP,EP]),
   (can_dig_maze_from_here(SP,EP)->
       dig_maze(SP,EP),
       findall([EP,EEP],can_dig_maze_from_here(EP,EEP),EPL),
       append(SPL,EPL,NSPL);
    select([SP,EP],SPL,NSPL)),!,
   hard_maze_fill(NSPL).

dig_cave:-
   Rad is 1.5+random(25)/10,
   Rc is round(1+Rad+random(200-20*Rad)/10),
   Cc is round(2*Rad+random(800-40*Rad)/10),
   Rm is Rc-floor(Rad),Rr is Rc+floor(Rad)-Rm+1,
   Cm is Cc-floor(2*Rad),Cr is Cc+floor(2*Rad)-Cm+1,
   on_dungeon_block(Rm,Cm,Rr,Cr,dig_circle,[Rc,Cc,Rad,room]),!.

dig_circle(R,C,Rc,Cc,Rad,_):-
   (R-Rc)**2+((C-Cc)**2)/4>Rad**2,!.
dig_circle(R,C,_,_,_,What):-dig(R,C,What).

dig_level(multi_room):-
   N is 2+random(5),build_rooms(N),connect_with_corridors(no).
dig_level(easy_maze):-
   dig(1,0,room),
   easy_maze_fill([coords(1,0)]),!.
dig_level(hard_maze):-
   dig(1,0,room),
   hard_maze_fill([[coords(1,0),coords(1,2)],
                   [coords(1,0),coords(3,0)]]),!.
dig_level(caves):-
   dig_cave,dig_cave,dig_cave,
   repeat,(room_components(1);dig_cave,fail),!.
dig_level(cave_maze):-
   dig(1,0,room),dig(21,0,room),
   dig(1,78,room),dig(21,78,room),
   hard_maze_fill([[coords(1,0),coords(1,2)],
                   [coords(1,0),coords(3,0)],
                   [coords(21,0),coords(21,2)],
                   [coords(21,0),coords(19,0)],
                   [coords(1,78),coords(1,76)],
                   [coords(1,78),coords(3,78)],
                   [coords(21,78),coords(21,76)],
                   [coords(21,78),coords(19,78)]]),
   repeat,(room_components(1);dig_cave,fail),!.
dig_level(big_room):-
   on_dungeon_block(1,0,21,80,dig,[room]),!.
dig_level(tower_grid_vh):-
   V is random(4)+3,
   divvy_up(2,19,V,RS,RR),
   maplist(dig_tower_row(20,39),RS,RR),
   connect_tower.
dig_level(tower_grid_hv):-
   H is random(5)+3,
   divvy_up(20,39,H,CS,CR),
   maplist(dig_tower_col(2,19),CS,CR),
   connect_tower.

dig_tower_row(C,Cs,R,Rs):-
   H is random(5)+3,
   divvy_up(C,Cs,H,RCS,RCR),
   maplist(dig_tower_room(rc,R,Rs),RCS,RCR).

dig_tower_col(R,Rs,C,Cs):-
   V is random(4)+3,
   divvy_up(R,Rs,V,RRS,RRR),
   maplist(dig_tower_room(cr,C,Cs),RRS,RRR).

dig_tower_room(cr,C,Cs,R,Rs):-on_dungeon_block(R,C,Rs,Cs,dig,[room]).
dig_tower_room(rc,R,Rs,C,Cs):-on_dungeon_block(R,C,Rs,Cs,dig,[room]).

divvy_up(I,Is,1,[I],[Is]):-!.
divvy_up(I,Is,N,[I|CT],[Cs|CsT]):-
   succ(NN,N),
   X is (Is-NN)/N,
   (1000*float_fractional_part(X)>random(1000) -> Cs is floor(X)+1;
    Cs is floor(X)),
   NI is I+Cs+1,
   NIs is Is-Cs-1,
   !,divvy_up(NI,NIs,NN,CT,CsT).

dig_level_thread(D,L):-
   illegal_level(D,L),thread_exit(illegal).  
dig_level_thread(D,L):-
   log_stuff('Digging ~w level ~w.~n',[D,L]),
   statistics(cputime,Y),
   choose_level_type(D,L,T),
   dig_level(T),
   place_stairs(D,L),
   on_dungeon_block(1,0,21,80,dig,[wall]),
   (retract(current_dungeon(R,C,What)),
    assert(dungeon(D,L,R,C,What,unseen)),fail;true),
   statistics(cputime,X),Z is (X-Y)*1000.0,
   log_stuff('Finished ~w level ~w (~w ~w).~n',[D,L,T,Z]),
   thread_exit(T).

illegal_level(_,L):-L=<0,!.
illegal_level_type(D,L):-max_level(D,ML),L>ML,!.

start_digging(D,L):-
   with_mutex(dungeon_gen,try_to_start_digging(D,L)),!.

try_to_start_digging(D,L):-illegal_level(D,L),!.
try_to_start_digging(D,L):-level_type(D,L,_),!.
try_to_start_digging(D,L):-
   thread_create(dig_level_thread(D,L),ID,[]),
   asserta(level_type(D,L,working(ID))),!.

wait_for_worker(D,L):-
   level_type(D,L,working(ID)),
   thread_join(ID,exited(NewType)),
   retractall(level_type(D,L,_)),asserta(level_type(D,L,NewType)),!.
wait_for_worker(D,L):-
   level_type(D,L,_).
wait_for_worker(D,L):-
   start_digging(D,L),!,
   wait_for_worker(D,L).

dungeon_generated(D,L):-
   with_mutex(dungeon_gen,wait_for_worker(D,L)).

set_player_level(D,L):-
   with_mutex(player_level,(attribute(player-dungeon,D),
                            attribute(player-level,L))),!.
set_player_level(D,L):-
   dungeon_generated(D,L),
   with_mutex(player_level,
      (attribute(player-dungeon,OldD,D),attribute(player-level,OldL,L),
       retractall(dungeon(OldD,OldL,_,_,_,_)),
       forall(player_dungeon(YA,YB,YC,YD),
              assert(dungeon(OldD,OldL,YA,YB,YC,YD))),
       retractall(player_dungeon(_,_,_,_)),
       forall(dungeon(D,L,XA,XB,XC,XD),
              assert(player_dungeon(XA,XB,XC,XD))))),
   forall(player_dungeon(_,_,stairs(_,SD,SL),_),start_digging(SD,SL)),!.
