:-module(main,[main_init/0,go/0]).

:-use_module(screen).
:-use_module(keyboard).
:-use_module(dungeon).
:-use_module(character).
:-use_module(actions).
:-use_module(visibility).
:-use_module(messages).
:-use_module(monster).

% FIXME load saved game
load_or_create:-start_digging(main,1),create_character.

display_splash_screen:-
   show_resource_screen(splash_screen),
   wait_for_key,
   hide_resource_screen(splash_screen).

main_init:-
   log_init, % FIXME
   messages_init,screen_init,dungeon_init,keyboard_init.

go:-
   main_init,
   display_splash_screen,
   load_or_create,
   start_play,
   play,
   end_play.

play:-
   post_message('Welcome to the Caves of Golorp.'),
   repeat,
   flag(turn,T,T+1),
   (per_turn(A);retract(on_turn(T,A))),
   once(take_action(A)),
   game_over,
   post_message('Press a key to exit.'),
   wait_for_key.

start_play:-
% put player in a random location
   set_player_level(main,1),
   random_room_coords(main,1,R,C),
   attribute(player-row,_,R),
   attribute(player-column,_,C),
   assert(character(main,1,R,C,player)),
% set up display
   flag(level_display,_,1),
   flag(attributes,_,1),
   calculate_visibility,
   request_screen_update(all),
% get ready to accept commands
   assert(per_turn(player)),
   start_monster_generation.

end_play:-put_code(27),put_char('['),put_char('0'),put_char('m'),halt.

game_over:-
   flag(game_over,1).
