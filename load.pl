
% FIXME
log_init:-
   mutex_create(log_file),
   open('threads.log',write,_,[alias(the_log),buffer(line)]).
log_stuff(F,A):-
   with_mutex(log_file,format(the_log,F,A)).

aformat(A,F,L):-
   sformat(S,F,L),string_to_atom(S,A).

all_recorded_destructive(K,[H|T]):-
   recorded(K,H,R),erase(R),!,all_recorded_destructive(K,T).
all_recorded_destructive(_,[]).

attribute(player-A,V):-!,attribute(0-A,V).
attribute(O-A,V):-integer(O),I is 1000000+O,T=..[A,V],recorded(I,T),!.
attribute(O-A,0):-integer(O),I is 1000000+O,T=..[A,_],\+(recorded(I,T)).

opt_attribute(player-A,V):-!,opt_attribute(0-A,V).
opt_attribute(O-A,V):-integer(O),I is 1000000+O,T=..[A,V],recorded(I,T),!.

attribute(player-A,OldV,NewV):-!,attribute(0-A,OldV,NewV).
attribute(O-A,OldV,NewV):-
   I is 1000000+O,
   OldT=..[A,OldV],
   (recorded(I,OldT,Ref),erase(Ref);OldV=0),
   NewT=..[A,NewV],recorda(I,NewT),!.

count_matches(G,N):-
   findall(foo,G,L),length(L,N).

erase_attribute(player-A):-erase_attribute(0-A),!.
erase_attribute(O-A):-
   I is 1000000+O,
   T=..[A,_],
   (recorded(I,T,Ref),erase(Ref),fail;true),!.

flag(K,V):-flag(K,V,V).

insert_after_nth([],_,X,[X]):-!.
insert_after_nth(L,0,X,[X|L]):-!.
insert_after_nth([H|T],P,X,[H|NT]):-
   succ(NP,P),!,insert_after_nth(T,NP,X,NT).

random_element([],_,E,E):-!.
random_element([H|T],N,P,E):-
   succ(N,NN),
   (random(NN)=:=0->PP=H;PP=P),
   !,random_element(T,NN,PP,E).
random_element(L,E):-
   random_element(L,0,nothing,E).

randomly_permute([],Done,_,Done):-!.
randomly_permute([H|T],Done,DNum,Out):-
   succ(DNum,NDNum),
   Pos is random(NDNum),
   insert_after_nth(Done,Pos,H,NDone),
   !,randomly_permute(T,NDone,NDNum,Out).

randomly_permute(In,Out):-randomly_permute(In,[],0,Out).

roll_dice(0,_,Adds,Adds):-!.
roll_dice(N,Sides,Adds,Result):-
   NA is Adds+random(Sides)+1,succ(NN,N),!,roll_dice(NN,Sides,NA,Result).

roll_dice(N,Sides,Result):-roll_dice(N,Sides,0,Result).

% :-module_transparent safe_assert/1.
%
% safe_assert(X):-X,log_stuff('safe_assert caught: ~w~n',[X]),!.
% safe_assert(X):-assert(X).

var_list(N,L):-length(L,N).

var_list(0,_,[]):-!.
var_list(M,N,[L|T]):-
   succ(MM,M),var_list(N,L),
   var_list(MM,N,T).

:-use_module(main).
