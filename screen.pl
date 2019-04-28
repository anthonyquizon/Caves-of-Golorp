:-module(screen,[screen_init/0,
                 request_screen_update/1,
                 request_screen_update/2,
                 request_screen_update/4,
                 show_resource_screen/1,
                 hide_resource_screen/1,
                 load_screen_resource/5,
                 unload_screen_resource/1]).

:-dynamic actual_screen/3,
          screen_resource_char/4,
          screen_resource_info/5,
          update_char/3,
          display_decimal_int/5.
:-index(screen_resource_char(1,1,1,0)).

:-use_module(dungeon).
:-use_module(mahou).
:-use_module(scroll).
:-use_module(visibility).
:-use_module(messages).
:-use_module(character).
:-use_module(mtbl).

screen_size_okay:-
   tty_size(Rows,Columns),Rows>=24,Columns>=80,!.
screen_size_okay:-
   /* print message */ fail.

colour(creature,white,black,bold).
colour(default,white,black,normal).
colour(more_prompt,black,white,normal).
colour(number,cyan,black,normal).
colour(scroll,green,black,normal).
colour(slashes,brown,black,normal).

colour_code(foreground,black,30).
colour_code(foreground,red,31).
colour_code(foreground,green,32).
colour_code(foreground,brown,33).
colour_code(foreground,blue,34).
colour_code(foreground,magenta,35).
colour_code(foreground,cyan,36).
colour_code(foreground,white,37).
colour_code(background,black,40).
colour_code(background,red,41).
colour_code(background,green,42).
colour_code(background,brown,43).
colour_code(background,blue,44).
colour_code(background,magenta,45).
colour_code(background,cyan,46).
colour_code(background,white,47).
colour_code(intensity,normal,22).
colour_code(intensity,bold,1).
colour_code(reset,all,0).

render_square(wall,' ').
render_square(room,atch('.',black,black,bold)).
render_square(corridor,'#').
render_square(stairs(up,_,_),'<').
render_square(stairs(down,_,_),'>').
render_square(unseen,' ').
render_square(_,'?').

wall_special(no,no,' ').
wall_special(no,yes,'|').
wall_special(yes,no,'-').
wall_special(yes,yes,'+').

room_square(room).
room_square(stairs(_,_,_)).

colourize(atch(Char,Fore,Back,Int),_Class,atch(Char,Fore,Back,Int)):-!.
colourize(atch(Char,Fore,Back),Class,atch(Char,Fore,Back,Int)):-
   colour(Class,_,_,Int),!.
colourize(atch(Char,Fore),Class,atch(Char,Fore,Back,Int)):-
   colour(Class,_,Back,Int),!.
colourize(Char,Class,atch(Char,Fore,Back,Int)):-
   colour(Class,Fore,Back,Int),!.

colourize(IC,OC):-colourize(IC,default,OC),!.

render_dungeon(R,C,Char):-
   visible(R,C),
   attribute(player-dungeon,D),attribute(player-level,L),
   character(D,L,R,C,W),
   attribute(W-type,T),
   render_creature(T,At),
   colourize(At,creature,Char),!. 
render_dungeon(R,C,Char):-
   player_dungeon(R,C,_,wall),
   PR is R-1,succ(R,NR),PC is C-1,succ(C,NC),
   (player_dungeon(PR,C,_,XA),room_square(XA),V=yes;
    player_dungeon(NR,C,_,XB),room_square(XB),V=yes;
    V=no),
   (player_dungeon(R,PC,_,XC),room_square(XC),H=yes;
    player_dungeon(R,NC,_,XD),room_square(XD),H=yes;
    H=no),
   wall_special(V,H,Char),!.
render_dungeon(R,C,Char):-
   player_dungeon(R,C,_,Sq),render_square(Sq,Char).

my_render_dungeon(R,C,atch(Ch,Fo,blue,Bo)):-
   visible(R,C),!,render_dungeon(R,C,A),
   colourize(A,atch(Ch,Fo,_,Bo)).
my_render_dungeon(R,C,A):-render_dungeon(R,C,A).

display_decimal_int(attributes,22,4,3,level).

display_decimal_int(attributes,23,4,3,hit_points).
display_decimal_int(attributes,23,13,3,susceptibility).
display_decimal_int(attributes,23,22,3,weakness).
display_decimal_int(attributes,23,31,3,misfortune).
display_decimal_int(attributes,23,40,3,clumsiness).
display_decimal_int(attributes,23,49,3,stupidity).
display_decimal_int(attributes,23,58,3,ugliness).

make_di_field(Screen,R,C,Length,Var):-
   assert(display_decimal_int(Screen,R,C,Length,Var)).

resource(splash_screen,screen,'splash_screen.txt').
resource(char_creation,screen,'char_create.txt').

calc_corner(Rs,Cs,Rc,Cc):-
   Rc is floor((24-Rs)/2),
   Cc is floor((80-Cs)/2).

unload_screen_resource(Screen):-
   retractall(screen_resource_char(Screen,_R,_C,_Char)),!.
unload_screen_resource(_).

store_screen_char(Screen,Char,R,C,MC,CC,MMC):-
   (Char=' ';assert(screen_resource_char(Screen,R,C,Char))),
   succ(C,CC),MMC is max(MC,C),!.

store_screen_chars(_Screen,[],_R,C,MC,C,MC):-!.
store_screen_chars(Screen,[H|T],R,C,MC,CC,MMC):-
   store_screen_char(Screen,H,R,C,MC,ICC,IMMC),
   !,store_screen_chars(Screen,T,R,ICC,IMMC,CC,MMC).

load_word(Stream,Word):-load_word(Stream,'',Word).

load_word(Stream,Prefix,Word):-
   peek_char(Stream,Ch),char_type(Ch,alpha),get_char(Stream,Ch),
   atom_concat(Prefix,Ch,NPrefix),
   !,load_word(Stream,NPrefix,Word).
load_word(_Stream,Word,Word):-!.

load_screen_resource(Screen,RS,R,C,MC):-
   get_char(RS,Char),!,
   load_screen_resource(Char,Screen,RS,R,C,MC).

load_screen_resource('\n',Screen,RS,R,_,MC):-
   succ(R,RR),!,load_screen_resource(Screen,RS,RR,1,MC).
load_screen_resource(end_of_file,Screen,RS,R,1,MC):-
   succ(RR,R),!,load_screen_resource(end_of_file,Screen,RS,RR,override,MC).
load_screen_resource(end_of_file,scroll,RS,R,_,_):-
   close(RS),flag(max_scroll_line,_,R),!.
load_screen_resource(end_of_file,Screen,RS,R,_,MC):-
   close(RS),
   calc_corner(R,MC,Rc,Cc),
   process_fields(Rc,Cc),
   assert(screen_resource_info(Screen,Rc,Cc,R,MC)),!.
load_screen_resource('%',Screen,RS,R,C,MC):-
   peek_char(RS,'%'),get_char(RS,'%'),
   store_screen_char(Screen,'%',R,C,MC,CC,MMC),
   !,load_screen_resource(Screen,RS,R,CC,MMC).
load_screen_resource('%',Screen,RS,R,C,MC):-
   peek_char(RS,'*'),get_char(RS,'*'),
   !,load_magic_words(Screen,RS,R,C,MC).
load_screen_resource('%',Screen,RS,R,C,MC):-
   read(RS,Term),repeat,get_char(RS,Ch),Ch='%',
   Term=..[Head,Length|Tail],
   Goal=..[Head,Screen,R,C,Length|Tail],
   recordz(fields,Goal),
   CC is C+Length,MMC is max(MC,C-1),
   !,load_screen_resource(Screen,RS,R,CC,MMC).
load_screen_resource(Char,Screen,RS,R,C,MC):-
   store_screen_char(Screen,Char,R,C,MC,CC,MMC),
   !,load_screen_resource(Screen,RS,R,CC,MMC).

load_magic_words(Screen,RS,R,C,MC):-
   peek_char(RS,' '),get_char(RS,' '),
   store_screen_char(Screen,' ',R,C,MC,CC,MMC),
   !,load_magic_words(Screen,RS,R,CC,MMC).
load_magic_words(Screen,RS,R,C,MC):-
   peek_char(RS,'*'),get_char(RS,'*'),
   !,load_screen_resource(Screen,RS,R,C,MC).
load_magic_words(Screen,RS,R,C,MC):-
   load_word(RS,Word),
   translate_atom(Word,TWord),
   atom_chars(TWord,TWC),
   store_screen_chars(Screen,TWC,R,C,MC,CC,MMC),
   !,load_magic_words(Screen,RS,R,CC,MMC).

process_fields(Rc,Cc):-
   recorded(fields,Term,Ref),
   Term=..[Head,Screen,R,C,Length|Tail],
   NR is Rc+R-1,NC is Cc+C-1,
   Goal=..[Head,Screen,NR,NC,Length|Tail],
   once(Goal),
   erase(Ref),!,process_fields(Rc,Cc).
process_fields(_,_).

show_resource_screen(Screen):-
   screen_resource_info(Screen,R,C,Rs,Cs),!,
   flag(Screen,_,1),request_screen_update(R,C,Rs,Cs).
show_resource_screen(Screen):-
   open_resource(Screen,screen,RS),
   load_screen_resource(Screen,RS,1,1,0),
   !,show_resource_screen(Screen).

hide_resource_screen(Screen):-
   screen_resource_info(Screen,R,C,Rs,Cs),!,
   flag(Screen,_,0),request_screen_update(R,C,Rs,Cs).

virtual_screen(23,79,_):-!,fail.
virtual_screen(R,C,A):-
   flag(reading_scroll,1),
   scroll_screen(R,C,Char),
   colourize(Char,scroll,A),!.
virtual_screen(R,C,A):-
   display_decimal_int(Screen,R,IC,IDig,IName),
   flag(Screen,1),
   IC=<C,Digit is IC+IDig-C,Digit>0,
   attribute(player-IName,IVal),decimal_digit(IVal,Digit,Char),
   colourize(Char,number,A),!.
virtual_screen(0,C,A):-
   flag(attributes,1),
   with_mutex(messages,(
      flag(line0,RT),
      (sub_atom(RT,C,1,After,Char);Char=' ',After=10),
      (more_prompt,After<6,Style=more_prompt;Style=default)
   )),
   colourize(Char,Style,A),!.
virtual_screen(R,C,A):-
   statline(R,RA),
   flag(attributes,1),
   flag(RA,RT),
   (sub_atom(RT,C,1,_,Char);Char=' '),
   colourize(Char,A),!.
virtual_screen(R,C,A):-
   screen_resource_info(Screen,SRR,SRC,SRRs,SRCs),
   flag(Screen,1),
   Rd is R-SRR+1,Rd>=1,Rd=<SRRs,
   Cd is C-SRC+1,Cd>=1,Cd=<SRCs,
   (screen_resource_char(Screen,Rd,Cd,Char);Char=' '),
   colourize(Char,A),!.
virtual_screen(R,C,A):-
   flag(level_display,1),
   R>=1,R=<21,
   render_dungeon(R,C,Char),
   colourize(Char,A),!.
virtual_screen(R,C,A):-((R+C) mod 2)=:=0,colourize('/',slashes,A),!.
virtual_screen(_,_,A):-colourize('\\',slashes,A),!.

statline(22,line22).
statline(23,line23).

decimal_digit(0,1,'0'):-!.
decimal_digit(Num,Dig,'-'):-
   Dig>1,-Num>=10**(Dig-2),-Num<10**(Dig-1),!.
decimal_digit(Num,Dig,Char):-
   abs(Num)>=10**(Dig-1),
   CCode is 48+((abs(Num)//10**(Dig-1)) mod 10),
   char_code(Char,CCode),!.
decimal_digit(_,_,' '):-!.

start_screen_thread:-
   thread_create(screen_thread(nowhere,nowhere,default,default,normal),IDA,[]),
   assertz(thread_id(screen,IDA)).

request_screen_update(R,C,Rows,Cols):-
   with_mutex(actual_screen,
      (recordz(check_virtual_screen,check(R,C,Rows,Cols)),
       thread_id(screen,ID),thread_send_message(ID,go))).

request_screen_update(Screen,Field):-
   display_decimal_int(Screen,R,C,L,Field),
   request_screen_update(R,C,1,L).

request_screen_update(all):-request_screen_update(0,0,24,80).
request_screen_update(pmove):-
   with_mutex(actual_screen,
      (forall((player_dungeon(R,C,_,W),
               (visible(R,C)->(W=wall;\+(was_visible(R,C)));
                              was_visible(R,C))),
          recordz(check_virtual_screen,check(R,C,1,1))),
       thread_id(screen,ID),thread_send_message(ID,go))).
request_screen_update(pzone):-
   attribute(player-row,R),attribute(player-column,C),
   LR is max(1,R-1),LC is max(0,C-1),
   HR is min(21,R+1),HC is min(79,C+1),
   NR is HR-LR+1,NC is HC-LC+1,
   !,request_screen_update(LR,LC,NR,NC).
request_screen_update(redraw):-
   with_mutex(actual_screen,
      (flag(clear_screen_requested,_,1),
       thread_id(screen,ID),thread_send_message(ID,go))).

% deal with clear-screen requests
screen_update_internal(_,_,_,_,_,R,C,F,Bk,Bo):-
   flag(clear_screen_requested,1,0),
   put_code(27),put_char('['),put_char('0'),put_char('m'),
   put_code(27),put_char('['),put_char('2'),put_char('J'),
   retractall(actual_screen(_,_,_)),
   request_screen_update(all),
   !,screen_update_internal(nowhere,nowhere,default,default,normal,R,C,F,Bk,Bo).

% there's a character in the right colour at this location
screen_update_internal(R,C,F,Bk,Bo,RR,CR,FR,BkR,BoR):-
   A=atch(Char,F,Bk,Bo),
   retract(update_char(R,C,A)),
   put_char(Char),
   retractall(actual_screen(R,C,_)),
   assertz(actual_screen(R,C,A)),
   succ(C,NewC),
   !,screen_update_internal(R,NewC,F,Bk,Bo,RR,CR,FR,BkR,BoR).

% move cursor right - keeping same attributes
screen_update_internal(R,C,F,Bk,Bo,RR,CR,FR,BkR,BoR):-
   update_char(R,CA,atch(_,F,Bk,Bo)),CA>C,
   \+((update_char(R,CB,atch(_,F,Bk,Bo)),CB>C,CB<CA)),
   NC is CA-C,
   put_code(27),put_char('['),write(NC),put_char('C'),
   !,screen_update_internal(R,CA,F,Bk,Bo,RR,CR,FR,BkR,BoR).

% move cursor random - keeping same attributes
screen_update_internal(_,_,F,Bk,Bo,RR,CR,FR,BkR,BoR):-
   update_char(R,CA,atch(_,F,Bk,Bo)),
   \+((update_char(R,CB,atch(_,F,Bk,Bo)),CB<CA)),
   tty_goto(CA,R),
   !,screen_update_internal(R,CA,F,Bk,Bo,RR,CR,FR,BkR,BoR).

% there's a character in the wrong colour at this location
screen_update_internal(R,C,F,Bk,Bo,RR,CR,FR,BkR,BoR):-
   A=atch(Char,NewF,NewBk,NewBo),
   retract(update_char(R,C,A)),
   set_graphics_rendition(F,Bk,Bo,NewF,NewBk,NewBo),
   put_char(Char),
   retractall(actual_screen(R,C,_)),
   assertz(actual_screen(R,C,A)),
   succ(C,NewC),
   !,screen_update_internal(R,NewC,NewF,NewBk,NewBo,RR,CR,FR,BkR,BoR).

% move cursor - new attributes
screen_update_internal(_,_,F,Bk,Bo,RR,CR,FR,BkR,BoR):-
   update_char(R,CA,_),
   \+((update_char(R,CB,_),CB<CA)),
   tty_goto(CA,R),
   !,screen_update_internal(R,CA,F,Bk,Bo,RR,CR,FR,BkR,BoR).

% place cursor in the right location
screen_update_internal(OldR,OldC,F,Bk,Bo,RR,CR,FR,BkR,BoR):-
   flag(requested_cursor_row,R),flag(requested_cursor_col,C),
   integer(R),integer(C),
   \+((OldR=R,OldC=C)),
   tty_goto(C,R),
   !,screen_update_internal(R,C,F,Bk,Bo,RR,CR,FR,BkR,BoR).

% final - flush output and finished
screen_update_internal(R,C,F,Bk,Bo,R,C,F,Bk,Bo):-flush_output.

set_graphics_rendition(F,Bk,Bo,F,Bk,Bo).
set_graphics_rendition(F,Bk,Bo,NewF,NewBk,NewBo):-
   put_code(27),put_char('['),
   sgr_codes(F,Bk,Bo,NewF,NewBk,NewBo),
   put_char('m').

sgr_codes(F,Bk,Bo,F,Bk,Bo).
sgr_codes(F,Bk,Bo,NewF,NewBk,NewBo):-F\=NewF,
   colour_code(foreground,NewF,CC),
   write(CC),!,sgr_separator(NewF,Bk,Bo,NewF,NewBk,NewBo).
sgr_codes(F,Bk,Bo,NewF,NewBk,NewBo):-Bk\=NewBk,
   colour_code(background,NewBk,CC),
   write(CC),!,sgr_separator(F,NewBk,Bo,NewF,NewBk,NewBo).
sgr_codes(F,Bk,Bo,NewF,NewBk,NewBo):-Bo\=NewBo,
   colour_code(intensity,NewBo,CC),
   write(CC),!,sgr_separator(F,Bk,NewBo,NewF,NewBk,NewBo).

sgr_separator(F,Bk,Bo,F,Bk,Bo):-!.
sgr_separator(F,Bk,Bo,NewF,NewBk,NewBo):-
   put_char(';'),
   sgr_codes(F,Bk,Bo,NewF,NewBk,NewBo).

virtual_screen_scan_internal:-
   recorded(check_virtual_screen,check(R,C,Rows,Cols),Ref),erase(Ref),
   scan_virtual_screen(R,C,Rows,Cols,C,fail,_),
   !,virtual_screen_scan_internal.
virtual_screen_scan_internal.

scan_virtual_screen(_,_,0,_,_,IU,IU):-!.
scan_virtual_screen(R,C,Rows,Cols,Cc,IU,OU):-
   Cc>=C+Cols,
   succ(R,IR),succ(IRows,Rows),
   !,scan_virtual_screen(IR,C,IRows,Cols,C,IU,OU).
scan_virtual_screen(R,C,Rows,Cols,Cc,IU,OU):-
   scan_virtual_char(R,Cc,DidIt),
   ((IU;DidIt)->OUI=true;OUI=fail),
   succ(Cc,ICc),
   !,scan_virtual_screen(R,C,Rows,Cols,ICc,OUI,OU).

scan_virtual_char(R,C,true):-
   virtual_screen(R,C,Char),
   \+actual_screen(R,C,Char),
   retractall(update_char(R,C,_)),assertz(update_char(R,C,Char)).
scan_virtual_char(_,_,fail).

screen_thread(R,C,F,Bk,Bo):-
   thread_get_message(go),
   with_mutex(actual_screen,
      ( % log_stuff('virtual screen scan~n',[]),
       virtual_screen_scan_internal)),
   with_mutex(actual_screen,
      ( % log_stuff('screen update~n',[]),
       screen_update_internal(R,C,F,Bk,Bo,RR,CR,FR,BkR,BoR))),
   !,screen_thread(RR,CR,FR,BkR,BoR).

screen_init:-
   flag(line0,_,''),
   flag(line22,_,'Dlvl'),
   flag(line23,_,'Hits     Susc     Weak     Misf     Clum     Stup     Ugli'),
   mutex_create(actual_screen),
   start_screen_thread,
   request_screen_update(redraw),
   scramble_syllables.
