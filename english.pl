:-module(english,[plural/2,
                  sconj/2,
                  vconj/3,
                  oconj/3]).

sconj(player,'You').
sconj(S,N):-opt_attribute(S-name,N).
sconj(S,SC):-
   attribute(S-type,creature(_,T)),
   atom_concat('The ',T,SC).
sconj(X,X).

vconj(player,V,V).
vconj(S,V,V):-attribute(S-gender,plural).
vconj(_,X,Y):-plural(X,Y).

oconj(player,player,yourself).
oconj(X,X,himself):-attribute(X-gender,male).
oconj(X,X,herself):-attribute(X-gender,female).
oconj(X,X,themselves):-attribute(X-gender,plural).
oconj(X,X,itself).
oconj(_,player,you).
oconj(_,O,N):-opt_attribute(O-name,N).
oconj(_,O,OC):-
   attribute(O-type,creature(_,T)),
   atom_concat('the ',T,OC).
oconj(_,X,X).

join(_,[],[]):-!.
join(_,A,[A]).
join(C,J,[A|B]):-join(C,K,B),append(A,[C|K],J).

split(_,[],[]):-!.
split(_,[[]],[]).
split(C,[[]|W],[C|I]):-split(C,W,I).
split(C,[[NC|WT]|W],[NC|I]):-split(C,[WT|W],I).

vowel(a). vowel(e). vowel(i). vowel(o). vowel(u).
vowel(X):-char_type(X,upper(Y)),vowel(Y).

consonant(b). consonant(c). consonant(d). consonant(f). consonant(g).
consonant(h). consonant(j). consonant(k). consonant(l). consonant(m).
consonant(n). consonant(p). consonant(q). consonant(r). consonant(s).
consonant(t). consonant(v). consonant(w). consonant(x). consonant(y).
consonant(z).
consonant(X):-char_type(X,upper(Y)),consonant(Y).

pluralize_reversed([S|Rest],[s|[e|[S|Rest]]]):-
   (s=S;'S'=S;x=S;'X'=S;z=S;'Z'=S),!.
pluralize_reversed([H|[S|Rest]],[s|[e|[H|[S|Rest]]]]):-
   (s=S;'S'=S;c=S;'C'=S),(h=H;'H'=H),!.
pluralize_reversed([O|[B|Rest]],[s|[e|[O|[B|Rest]]]]):-
   consonant(B),(o=O;'O'=O),!.
pluralize_reversed([F|Rest],[s|[e|[v|Rest]]]):-
   (f=F;'F'=F),!.
pluralize_reversed([Y|[B|Rest]],[s|[e|[i|[B|Rest]]]]):-
   consonant(B),(y=Y;'Y'=Y),!.
pluralize_reversed(Rest,[s|Rest]).

pluralize(S,P):-reverse(S,RS),pluralize_reversed(RS,RP),reverse(P,RP).

plural(Sing,Plu):-oplural(Sing,Plu),!.
plural(Sing,Plu):-
   atom_chars(Sing,SC),
   split(' ',W,SC),
   length(W,NW),
   (NW>=3,[A|[B|C]]=W,length(B,NB),NB=<4->
       atom_chars(AA,A),plural(AA,PAA),atom_chars(PAA,PA),
       [PA|[B|C]]=PW;
    W=[D]->
       pluralize(D,PD),PW=[PD];
    reverse(W,[D|E]),
       atom_chars(DA,D),plural(DA,PDA),atom_chars(PDA,PD),
       reverse(PW,[PD|E])),
   join(' ',PC,PW),
   atom_chars(Plu,PC),!.

% override plurals here!
oplural(cherub,cherubim).
oplural('creme brulee','cremeaux brulee').
oplural('creme caramel','cremeaux caramel').
oplural(goombilada,goombilada).
oplural(haibane,haibane).
oplural(man,men).
oplural(octopus,octopodes).
oplural(seraph,seraphim).
oplural(shinma,shinma).
oplural(woman,women).
oplural(womyn,wimmin).
