:-module(mahou,[scramble_syllables/0,translate_atom/2]).

:-use_module(scramble).

ordinary_consonant(r).
ordinary_consonant(m).
ordinary_consonant(n).
ordinary_consonant(k).

marked_consonant(p).
marked_consonant(b).
marked_consonant(g).

y_consonant(y).
y_consonant(py).
y_consonant(by).
y_consonant(j).
y_consonant(gy).
y_consonant(ry).
y_consonant(my).
y_consonant(hy).
y_consonant(ny).
y_consonant(ch).
y_consonant(sh).
y_consonant(ky).

y_vowel(a).
y_vowel(u).
y_vowel(o).

vowel(a).
vowel(i).
vowel(u).
vowel(e).
vowel(o).

syllable(main,X):-
   ordinary_consonant(C),vowel(V),atom_concat(C,V,X).
syllable(main,wa).
syllable(main,wo).
syllable(main,ha).
syllable(main,hi).
syllable(main,fu).
syllable(main,he).
syllable(main,ho).
syllable(main,ta).
syllable(main,chi).
syllable(main,tsu).
syllable(main,te).
syllable(main,to).
syllable(main,sa).
syllable(main,shi).
syllable(main,su).
syllable(main,se).
syllable(main,so).

syllable(marked,X):-
   marked_consonant(C),vowel(V),atom_concat(C,V,X).
syllable(marked,da).
syllable(marked,tchi).
syllable(marked,dzu).
syllable(marked,de).
syllable(marked,do).
syllable(marked,za).
syllable(marked,ji).
syllable(marked,zu).
syllable(marked,ze).
syllable(marked,zo).

syllable(y,X):-
   y_consonant(C),y_vowel(V),atom_concat(C,V,X).

syllable(vowel,V):-vowel(V).

syllable(n,n).
syllable(n,m).

syllable(X):-syllable(_,X).

doubled_cons(S,H):-
   atom(H),!,
   sub_atom(H,0,1,LL,F),sub_atom(H,1,1,L,F),L>=1,
   sub_atom(H,1,LL,_,S),syllable(S).
doubled_cons(S,H):-
   syllable(S),sub_atom(S,0,1,L,F),L>=1,
   atom_concat(F,S,H).

syllable_split('',[]).
syllable_split(W,[H|T]):-
   syllable(H),atom_concat(H,I,W),syllable_split(I,T).
syllable_split(W,[H|T]):-
   doubled_cons(_,H),
   atom_concat(H,I,W),syllable_split(I,T).

scramble_syllables([]):-!.
scramble_syllables([T|Tail]):-
   setof(S,syllable(T,S),Ss),
   scramble_list(mahou,Ss),
   !,scramble_syllables(Tail).
scramble_syllables:-
   setof(T,X^syllable(T,X),Ts),!,scramble_syllables(Ts).

translate_list([],[]).
translate_list([HA|TA],[HB|TB]):-
   scrambled(mahou,HA,HB),!,translate_list(TA,TB).
translate_list([HA|TA],[HB|TB]):-
   atom(HA),
   doubled_cons(HHA,HA),scrambled(mahou,HHA,HHB),doubled_cons(HHB,HB),
   !,translate_list(TA,TB).
translate_list([HA|TA],[HB|TB]):-
   doubled_cons(HHB,HB),scrambled(mahou,HHA,HHB),doubled_cons(HHA,HA),
   !,translate_list(TA,TB).

translate_atom(A,B):-
   atom(A),
   syllable_split(A,AL),
   translate_list(AL,BL),
   concat_atom(BL,B).
translate_atom(A,B):-
   atom(B),
   syllable_split(B,BL),
   translate_list(AL,BL),
   concat_atom(AL,A).
