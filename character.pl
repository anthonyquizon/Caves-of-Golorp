:-module(character,[create_character/0,character/5]).

:-use_module(screen).
:-use_module(scroll).
:-use_module(keyboard).
:-use_module(monster).

:-dynamic character/5.
:-index(character(1,1,1,1,0)).

character_attribute(a,susceptibility).
character_attribute(b,weakness).
character_attribute(c,misfortune).
character_attribute(d,clumsiness).
character_attribute(e,stupidity).
character_attribute(f,ugliness).

create_character:-
   attribute(player-type,_,creature(person,human)),
   attribute(player-adjusts_remaining,_,3),
   attribute(player-rerolls_remaining,_,3),
   attribute(player-type,PT),
   roll_character(player,PT),
   attribute(player-alignment,_,player),
   show_resource_screen(char_creation),
   char_create_loop,
   hide_resource_screen(char_creation),
   erase_attribute(player-adjusts_remaining),
   erase_attribute(player-rerolls_remaining).

char_create_loop:-
   attribute(player-adjusts_remaining,0),
   attribute(player-rerolls_remaining,0),!.
char_create_loop:-
   wait_for_key(Key),!,char_create_command(Key).

char_create_command(Key):-
   character_attribute(Key,Attr),
   attribute(player-adjusts_remaining,N),N>0,
   attribute(player-Attr,V),
   NewV is max(1,V-1-random(9)),
   attribute(player-Attr,_,NewV),
   request_screen_update(char_creation,Attr),
   NewN is N-1,
   attribute(player-adjusts_remaining,_,NewN),
   request_screen_update(char_creation,adjusts_remaining),
   !,char_create_loop.
char_create_command(g).
char_create_command(h):-
   read_scroll(cc_help),
   !,char_create_loop.
char_create_command(r):-
   attribute(player-rerolls_remaining,N),N>0,
   NewN is N-1,
   attribute(player-rerolls_remaining,_,NewN),
   request_screen_update(char_creation,rerolls_remaining),
   attribute(player-adjusts_remaining,_,3),
   request_screen_update(char_creation,adjusts_remaining),
   attribute(player-type,PT),
   roll_character(player,PT),
   (character_attribute(_,Attr),request_screen_update(char_creation,Attr),fail;
    true),
   !,char_create_loop.
char_create_command(_):-!,char_create_loop.
