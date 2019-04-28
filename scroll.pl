:-module(scroll,[read_scroll/1,
                 scroll_screen/3]).

:-use_module(keyboard).
:-use_module(screen).

fixed_column(2,'|').
fixed_column(6,'|').
fixed_column(7,' ').
fixed_column(72,' ').
fixed_column(73,'|').
fixed_column(77,'|').
break_column(3).
break_column(4).
break_column(5).
break_column(74).
break_column(75).
break_column(76).

scroll_screen(R,C,_):-(R<1;R>22;C<2;C>77),!,fail.
scroll_screen(_,C,Char):-fixed_column(C,Char),!.
scroll_screen(R,75,Char):-!,scroll_screen(R,4,Char).
scroll_screen(R,4,'O'):-
   flag(scroll_line,L),
   (L+R) mod 2 =:= 1,!.
scroll_screen(R,C,Char):-
   break_column(C),!,
   flag(scroll_line,L),
   ((L+R) mod 22 =:= 0,Char='-';Char=' ').
scroll_screen(R,C,Char):-
   flag(scroll_line,L),
   SR is R+L,
   SC is C-7,
   screen:screen_resource_char(scroll,SR,SC,Char),!.
scroll_screen(_,_,' '):-!.

read_scroll(Scroll):-
   flag(scroll_name,Scroll),
   flag(scroll_line,_,0),
   flag(reading_scroll,_,1),request_screen_update(all),
   scroll_read_loop,
   flag(reading_scroll,_,0),request_screen_update(all),!.
read_scroll(Scroll):-
   unload_screen_resource(scroll),
   open_resource(Scroll,scroll,RS),
   load_screen_resource(scroll,RS,1,1,0),
   flag(scroll_name,_,Scroll),!,read_scroll(Scroll).

scroll_read_loop:-
   wait_for_key(Key),!,scroll_read_command(Key).

scroll_up(D):-
   flag(scroll_line,L,max(min(L-D,L),0)),
   request_screen_update(1,3,22,74),
   !,scroll_read_loop.

scroll_down(D):-
   flag(max_scroll_line,ML),
   flag(scroll_line,L,max(min(ML-10,L+D),0)),
   request_screen_update(1,3,22,74),
   !,scroll_read_loop.

scroll_read_command(up):-scroll_up(1).
scroll_read_command(down):-scroll_down(1).
scroll_read_command(page_up):-scroll_up(21).
scroll_read_command(page_down):-scroll_down(21).
scroll_read_command(' '):-!,scroll_read_command(page_down).
scroll_read_command(q).
scroll_read_command('Q').
scroll_read_command('\015\').
scroll_read_command(_):-!,scroll_read_loop.

resource(cc_help,scroll,'cc_help.txt').
