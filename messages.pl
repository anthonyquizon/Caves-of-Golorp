:-module(messages,[messages_init/0,
                   post_message/1,
                   post_message/2,
                   sv_message/2,
                   svo_message/3,
                   svi_message/3,
                   svoi_message/4,
                   more_prompt/0,
                   ack_messages/0]).

:-use_module(screen).
:-use_module(english).
:-use_module(visibility).

messages_init:-
   mutex_create(messages).

% FIXME - word wrap
% post_message(M):-
%    atom_length(M,N),N>72
post_message(M):-
   with_mutex(messages,(post_message_int(M),
                        flag(unacked_messages,_,1))),
   request_screen_update(0,0,1,80).

post_message(F,L):-
   aformat(A,F,L),post_message(A).

post_message_int(M):-
   flag(line0,'',M),
   atom_length(M,L),
   flag(requested_cursor_row,_,0),
   flag(requested_cursor_col,_,L),!.
post_message_int(M):-
   \+(more_prompt),
   flag(line0,OL),
   atom_length(OL,OLL),
   atom_length(M,ML),
   OLL+ML=<70,
   concat_atom([OL,'  ',M],NL),
   flag(line0,_,NL),
   atom_length(NL,Len),
   flag(requested_cursor_row,_,0),
   flag(requested_cursor_col,_,Len),!.
post_message_int(M):-
   more_prompt,
   recordz(messages,M),!.
post_message_int(M):-
   flag(more_prompt,_,1),
   flag(line0,OL),
   atom_concat(OL,' [More]',NL),
   flag(line0,_,NL),!,
   post_message_int(M).

more_prompt:-flag(more_prompt,1).

ack_messages:-flag(unacked_messages,0).
ack_messages:-
   with_mutex(messages,(
      flag(line0,_,''),
      flag(unacked_messages,_,0),
      flag(more_prompt,_,0),
      flag(requested_cursor_row,_,0),
      flag(requested_cursor_col,_,0),
      gather_messages(L),
      (L=[];flag(unacked_messages,_,1),resend_messages(L))
   )),
   request_screen_update(0,0,1,80).

gather_messages([H|T]):-
   recorded(messages,H,R),
   erase(R),
   !,gather_messages(T).
gather_messages([]).

resend_messages([H|T]):-
   post_message_int(H),!,resend_messages(T).
resend_messages([]).

sv_message(S,V):-
   care_about(S),
   sconj(S,SC),
   vconj(S,V,VC),
   post_message('~w ~w.',[SC,VC]),!.
sv_message(_,_).

svo_message(S,V,O):-
   care_about(S),care_about(O),
   sconj(S,SC),
   vconj(S,V,VC),
   oconj(S,O,OC),
   post_message('~w ~w ~w.',[SC,VC,OC]),!.
svo_message(S,V,_):-
   care_about(S), % not care_about(O),
   sconj(S,SC),
   vconj(S,V,VC),
   post_message('~w ~w it.',[SC,VC]),!.
svo_message(_,V,O):-
   care_about(O), % not care_about(S),
   vconj(it,V,VC),
   oconj(it,O,OC),
   post_message('It ~w ~w.',[VC,OC]),!.
svo_message(_,_,_).

svi_message(S,V,I):-
   care_about(S),
   sconj(S,SC),
   vconj(S,V,VC),
   post_message('~w ~w ~w.',[SC,VC,I]),!.
svi_message(_,_,_).

svoi_message(S,V,O,I):-
   care_about(S),care_about(O),
   sconj(S,SC),
   vconj(S,V,VC),
   oconj(S,O,OC),
   post_message('~w ~w ~w ~w.',[SC,VC,OC,I]),!.
svoi_message(S,V,_,I):-
   care_about(S), % not care_about(O),
   sconj(S,SC),
   vconj(S,V,VC),
   post_message('~w ~w it ~w.',[SC,VC,I]),!.
svoi_message(_,V,O,I):-
   care_about(O), % not care_about(S),
   vconj(it,V,VC),
   oconj(it,O,OC),
   post_message('It ~w ~w ~w.',[VC,OC,I]),!.
svoi_message(_,_,_,_).

care_about(player).
care_about(O):-
   attribute(player-dungeon,D),attribute(O-dungeon,D),
   attribute(player-level,L),attribute(O-level,L),
   attribute(O-row,R),attribute(O-column,C),
   visible(R,C),
log_stuff('viz~n',[]).
