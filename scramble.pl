:-module(scramble,[scramble_list/3,scramble_list/2,scrambled/3]).

:-dynamic scrambled/3.

assert_scrambled(_,[],_):-!.
assert_scrambled(Tag,[F|From],[T|To]):-
   assertz(scrambled(Tag,F,T)),
   !,assert_scrambled(Tag,From,To).

scramble_list(Tag,From,To):-
   randomly_permute(To,NTo),
   assert_scrambled(Tag,From,NTo).

scramble_list(Tag,List):-scramble_list(Tag,List,List).