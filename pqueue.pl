:-module(pqueue,[pqueue_empty/1,
                 pqueue_count/2,
                 pqueue_insert/4,
                 pqueue_extract_min/4]).

% FIXME
% this is probably not the most efficient way to do this because it's
% O(log n) for both insert and delete, and we can probably make it
% constant for one or the other of those.

pqueue_empty([]).

pqueue_count(pqueue(_K,_V,C,_R,_L),C).
pqueue_count(pqueue(_K,_V),1).
pqueue_count([],0).

choose_smaller_pq(A,B,A,B):-
   pqueue_count(A,AC),pqueue_count(B,BC),
   AC=<BC,!.
choose_smaller_pq(A,B,B,A):-!.

choose_smaller_kv(AK,AV,BK,BV,AK,AV,BK,BV):-AK@=<BK,!.
choose_smaller_kv(AK,AV,BK,BV,BK,BV,AK,AV):-!.

extract_key(pqueue(K,_),K).
extract_key(pqueue(K,_,_,_,_),K).

choose_smaller_key([],X,X,[]):-!.
choose_smaller_key(X,[],X,[]):-!.
choose_smaller_key(A,B,A,B):-
   extract_key(A,KA),extract_key(B,KB),
   KA@<KB,!.
choose_smaller_key(A,B,B,A):-!.

pqueue_insert(pqueue(OK,OV,OC,OR,OL),K,V,pqueue(NK,NV,NC,NR,NL)):-
   choose_smaller_kv(K,V,OK,OV,NK,NV,IK,IV),
   succ(OC,NC),
   choose_smaller_pq(OR,OL,IR,NL),
   pqueue_insert(IR,IK,IV,NR).
pqueue_insert(pqueue(OK,OV),K,V,pqueue(NK,NV,2,pqueue(IK,IV),[])):-
   choose_smaller_kv(K,V,OK,OV,NK,NV,IK,IV).
pqueue_insert([],K,V,pqueue(K,V)).

pqueue_extract_min(pqueue(K,V,2,OR,OL),K,V,NPQ):-
   choose_smaller_key(OR,OL,NPQ,_),!.
pqueue_extract_min(pqueue(K,V,OC,OR,OL),K,V,pqueue(NK,NV,NC,NR,NL)):-
   succ(NC,OC),
   choose_smaller_key(OR,OL,IR,NL),
   pqueue_extract_min(IR,NK,NV,NR).
pqueue_extract_min(pqueue(K,V),K,V,[]).
