:-module(keyboard,[wait_for_key/0,
                   wait_for_key/1,
                   keyboard_init/0]).

:-dynamic key_translate/2.

:-use_module(screen).
:-use_module(messages).

% FIXME dynamify key translations

key_translate(['\014\'],redraw).

key_translate(['\033\','[','A'],up).
key_translate(['\033\','[','B'],down).
key_translate(['\033\','[','C'],right).
key_translate(['\033\','[','D'],left).

key_translate(['\033\','[','5','~'],page_up).
key_translate(['\033\','[','6','~'],page_down).

key_translate(['\033\',K],meta(K)):-char_type(K,alnum).

keyboard_thread(Buffer):-
   get_single_char(Code),char_code(Key,Code),
   append(Buffer,[Key],NB),
   !,process_buffer(NB).

process_buffer([]):-!,keyboard_thread([]).
process_buffer(NB):-
   key_translate(NNB,_),
   append(NB,X,NNB),X\=[],
   !,keyboard_thread(NB).
process_buffer(NB):-
   append(NBA,NBB,NB),
   key_translate(NBA,Key),
%   log_stuff('got key ~w.~n',[Key]),
   global_key_hook(Key),
   !,process_buffer(NBB).
process_buffer([K|T]):-
   log_stuff('got key ~w.~n',[K]),global_key_hook(K),!,process_buffer(T).

global_key_hook(redraw):-!,request_screen_update(redraw).
global_key_hook(meta(h)):-!,
   attribute(player-hit_points,H),
   HH is H+random(6)+1,
   attribute(player-hit_points,_,HH),
   request_screen_update(23,0,1,10).
global_key_hook(meta(t)):-!,threads.
global_key_hook(meta(l)):-
   (dungeon:player_dungeon(R,C,A,B),
    log_stuff('~w~n',[player_dungeon(R,C,A,B)]),fail;true),!.
global_key_hook(Key):-
   with_mutex(messages,(
      more_prompt,(Key=' ',ack_messages;true);
      ack_messages,thread_send_message(keystrokes,Key)
   )),!.

keyboard_init:-
   message_queue_create(keystrokes),
   thread_create(keyboard_thread([]),_,[]).

wait_for_key(Key):-
   thread_get_message(keystrokes,Key).

wait_for_key:-wait_for_key(_).
