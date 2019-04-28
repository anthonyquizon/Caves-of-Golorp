:-module(pcmd,[get_player_command/1,process_player_command/1]).

:-use_module(keyboard).
:-use_module(actions).
:-use_module(dungeon).

get_player_command(C):-
   wait_for_key(K),decode_command(K,C),!.

decode_command(q,quit).
decode_command(up,walk(north)).
decode_command(down,walk(south)).
decode_command(left,walk(west)).
decode_command(right,walk(east)).
decode_command('<',climb(up)).
decode_command('>',climb(down)).
decode_command(meta(d),teleport_to_down_stairs).
decode_command(K,K).

process_player_command(quit):-flag(game_over,_,1).
process_player_command(walk(D)):-walk(player,D).
process_player_command(climb(D)):-climb(player,D).
process_player_command(teleport_to_down_stairs):-
   player_dungeon(R,C,stairs(down,_,_),_),
   teleport(player,R,C).
process_player_command(_).
