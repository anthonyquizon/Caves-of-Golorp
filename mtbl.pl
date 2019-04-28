:-module(mtbl,[monster_type/1,
               rolling_inst/2,grows_into/2,render_creature/2,gen_int/2,
               attack_list/2,monster_level/3,
               align_attacks/2]).

:-discontiguous mtbl/9,mtbl/3,grows_into/3.

% mtbl(Class,Type,Render,Interval,Dungeon,MinLvl,MaxLvl,Instructions,Attacks).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  @ - person
%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

mtbl(person,human,'@',200,tower,1,none,[
   roll(susceptibility,8,12,3),
   roll(weakness,8,12,3),
   roll(misfortune,8,12,3),
   roll(clumsiness,8,12,3),
   roll(stupidity,8,12,3),
   roll(ugliness,8,12,3),
   alignment=evil,
   gender=either
   ],[hit]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

mtbl(person,amazon,atch('@',magenta),200,tower,2,none,[
   inherit(person,human),
   roll(misfortune,6,12,3),
   roll(ugliness,10,12,3),
   gender=female
   ],[hit]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

mtbl(person,dwarf,atch('@',black),200,_,10,35,[
   inherit(person,human),
   roll(susceptibility,6,12,3),
   roll(weakness,6,12,3),
   roll(stupidity,10,12,3),
   roll(ugliness,10,12,3),
   gender=male
     % dwarves may be biologically male or female, but that's none
     % of your business; they are all *called* "he".
   ],[hit]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

mtbl(person,elf,atch('@',green),200,tower,5,none,[
   inherit(person,human),
   roll(susceptibility,10,12,3),
   roll(weakness,10,12,3),
   roll(clumsiness,6,12,3),
   roll(ugliness,6,12,3)
   ],[hit]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

mtbl(person,hobbit,atch('@',brown),200,_,6,20,[
   inherit(person,human),
   roll(susceptibility,6,12,3),
   roll(weakness,10,12,3)
   ],[hit]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

mtbl(person,ninja,atch('@',black,white,normal),200,tower,8,none,[
   inherit(person,human),
   roll(clumsiness,6,12,3)
   ],[hit]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

mtbl(person,pirate,atch('@',white,blue),2000,tower,10,none,[
   inherit(person,human),
   roll(stupidity,6,12,3)
   ],[hit]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  & - demon
%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

mtbl(demon,'boogie man','&',2000,_,100,none,[
   inherit(imp)
   ],[get]).

grows_into(demon,'boogie man',goombilada).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

mtbl(demon,goombilada,'&',2000,_,100,none,[
   inherit(imp)
   ],[hit,scratch]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

mtbl(demon,imp,atch('&',red),2000,_,100,none,[
   inherit(person,human)
   ],[bite]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

mtbl(demon,lawyer,'&',2000,_,100,none,[
   inherit(imp)
   ],[sue]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

mtbl(demon,paralegal,'&',2000,_,100,none,[
   inherit(imp)
   ],[sue]).
grows_into(demon,paralegal,lawyer).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

mtbl(demon,shinma,'&',2000,_,100,none,[
   inherit(imp)
   ],[hit]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  % - zombie
%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

mtbl(zombie,NN,I):-
   rolling_inst(creature(person,N),I),
   atom_concat(N,' zombie',NN).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  A - angel
%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Yes, it is debatable whether haibane qualify as angels.  They look more
% like people's idea of angels than the actual Judeochristian angels do,
% though, and it makes as much sense as listing koalas and pandas as "bears".

mtbl(angel,archangel,        [inherit(haibane)]).
mtbl(angel,'avenging angel',        [inherit(haibane)]).
mtbl(angel,cherub,        [inherit(haibane)]).
mtbl(angel,haibane,        [inherit(person,human)]).
mtbl(angel,'recording angel',        [inherit(haibane)]).
mtbl(angel,seraph,        [inherit(haibane)]).

grows_into(angel,cherub,archangel).
grows_into(angel,seraph,cherub).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  a - anatomy
%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

mtbl(anatomy,'finger of death',        [inherit(skeleton)]).
mtbl(anatomy,'fist of death',        [inherit(skeleton)]).
mtbl(anatomy,'floating eye',        [inherit(skeleton)]).
mtbl(anatomy,'running nose',        [inherit(skeleton)]).
mtbl(anatomy,'sinking eye',         [inherit(skeleton)]).
mtbl(anatomy,skeleton,        [inherit(person,human)]).

grows_into(anatomy,'finger of death','fist of death').

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  b - bear
%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

mtbl(bear,'black bear',             [inherit(koala)]).
mtbl(bear,'brown bear',             [inherit(koala)]).
mtbl(bear,'care bear',              [inherit(koala)]).
mtbl(bear,'grizzly bear',           [inherit(koala)]).
mtbl(bear,koala,                    [inherit(person,human)]).
mtbl(bear,panda,                    [inherit(koala)]).
mtbl(bear,'water bear',             [inherit(koala)]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

mtbl(bear,pupcub,atch('b',white,black,normal),200,main,1,5,[
   roll(susceptibility,16,12,6),
   roll(weakness,16,12,6),
   roll(misfortune,8,12,3),
   roll(clumsiness,6,12,3),
   roll(stupidity,12,12,3),
   roll(ugliness,4,12,3),
   choose(alignment,[evil,peaceful])
   ],[hit]).

grows_into(bear,pupcub,mousebear).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

mtbl(bear,larvacub,atch('b',green,black,normal),200,main,2,6,[
   roll(susceptibility,14,12,5),
   roll(weakness,14,12,5),
   roll(misfortune,8,12,3),
   roll(clumsiness,6,12,3),
   roll(stupidity,20,12,3),
   roll(ugliness,16,12,3),
   alignment=evil
   ],[hit]).

grows_into(bear,larvacub,bugbear).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

mtbl(bear,chickcub,atch('b',blue,black,normal),200,main,4,8,[
   roll(susceptibility,12,12,4),
   roll(weakness,12,12,4),
   roll(misfortune,8,12,3),
   roll(clumsiness,6,12,3),
   roll(stupidity,12,12,3),
   roll(ugliness,6,12,3),
   alignment=evil
   ],[hit,chance(25,bite)]).

grows_into(bear,chickcub,owlbear).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

mtbl(bear,kittencub,atch('b',brown,black,normal),200,main,6,10,[
   roll(susceptibility,10,12,3),
   roll(weakness,10,12,3),
   roll(misfortune,8,12,3),
   roll(clumsiness,6,12,3),
   roll(stupidity,12,12,3),
   roll(ugliness,4,12,3),
   choose(alignment,[evil,peaceful])
   ],[hit,chance(33,bite)]).

grows_into(bear,kittencub,catbear).

mtbl(bear,catbear,                  [inherit(koala)]).  % 8-none
mtbl(bear,mousebear,                [inherit(koala)]).  % 12-none
mtbl(bear,bugbear,                  [inherit(koala)]).  % 16-none
mtbl(bear,owlbear,                  [inherit(koala)]).  % 20-none


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  c - cream
%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

mtbl(cream,'Boston cream',          [inherit('hand cream')]).
mtbl(cream,'clotted cream',         [inherit('hand cream')]).
mtbl(cream,'creme brulee',          [inherit('hand cream')]).
mtbl(cream,'creme caramel',         [inherit('hand cream')]).
mtbl(cream,'Devon cream',           [inherit('hand cream')]).
mtbl(cream,'double Devon cream',    [inherit('hand cream')]).
mtbl(cream,'hand cream',            [inherit(person,human)]).
mtbl(cream,'krispy kreme',          [inherit('hand cream')]).
mtbl(cream,'sour cream',            [inherit('hand cream')]).
mtbl(cream,'triple Devon cream',    [inherit('hand cream')]).

grows_into(cream,'Devon cream','double Devon cream').
grows_into(cream,'double Devon cream','triple Devon cream').

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  G - ghost
%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

mtbl(ghost,NN,I):-
   rolling_inst(creature(person,N),I),
   atom_concat('ghost ',N,NN).
mtbl(ghost,'ghost of the genie',    [inherit(person,human)]).
mtbl(ghost,wraith,                  [inherit(person,human)]).
mtbl(ghost,wight,                   [inherit(person,human)]).
mtbl(ghost,'master lich',           [inherit(person,human)]).
mtbl(ghost,lich,                    [inherit(person,human)]).
mtbl(ghost,demilich,                [inherit(person,human)]).
mtbl(ghost,semidemilich,            [inherit(person,human)]).
mtbl(ghost,hemisemidemilich,        [inherit(person,human)]).

grows_into(ghost,hemisemidemilich,semidemilich).
grows_into(ghost,semidemilich,demilich).
grows_into(ghost,demilich,lich).
grows_into(ghost,lich,'master lich').

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  m - multileg
%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

mtbl(multileg,centipede,            [inherit(person,human)]).
mtbl(multileg,millipede,            [inherit(person,human)]).
mtbl(multileg,micropede,            [inherit(person,human)]).
mtbl(multileg,nanopede,             [inherit(person,human)]).
mtbl(multileg,picopede,             [inherit(person,human)]).

mtbl(multileg,lobster,              [inherit(person,human)]).
mtbl(multileg,'giant spider',       [inherit(person,human)]).

grows_into(multileg,centipede,millipede).
grows_into(multileg,millipede,micropede).
grows_into(multileg,micropede,nanopede).
grows_into(multileg,nanopede,picopede).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  P - pudding
%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

mtbl(pudding,'black pudding',       [inherit(person,human)]).
mtbl(pudding,'bread pudding',       [inherit(person,human)]).
mtbl(pudding,'chocolate pudding',   [inherit(person,human)]).
mtbl(pudding,'Christmas pudding',   [inherit(person,human)]).
mtbl(pudding,haggis,                [inherit(person,human)]).
mtbl(pudding,'plum pudding',        [inherit(person,human)]).
mtbl(pudding,'rice pudding',        [inherit(person,human)]).
mtbl(pudding,'spotted dick',        [inherit(person,human)]).
mtbl(pudding,'toad in the hole',    [inherit(person,human)]).
mtbl(pudding,trifle,                [inherit(person,human)]).
mtbl(pudding,'Yorkshire pudding',   [inherit(person,human)]).

grows_into(pudding,'plum pudding','Christmas pudding').

class_symbol(demon,'&').
class_symbol(zombie,'%').
class_symbol(angel,'A').
class_symbol(anatomy,'a').
class_symbol(bear,'b').
class_symbol(cream,'c').
class_symbol(ghost,'G').
class_symbol(multileg,'m').
class_symbol(pudding,'P').

render_creature(creature(C,T),A):-mtbl(C,T,A,_,_,_,_,_,_).
render_creature(creature(C,_),A):-class_symbol(C,A).
render_creature(_,'?').

gen_int(creature(C,T),I):-mtbl(C,T,_,I,_,_,_,_,_).
gen_int(_,2000).

monster_type(T):-rolling_inst(T,_).

rolling_inst(creature(C,T),I):-mtbl(C,T,I).
rolling_inst(creature(C,T),I):-mtbl(C,T,_,_,_,_,_,I,_).

attack_list(creature(C,T),L):-mtbl(C,T,_,_,_,_,_,_,L).
attack_list(_,[hit]).

monster_level(creature(C,T),D,L):-
   mtbl(C,T,_,_,D,MinL,MaxL,_,_),
   (integer(MinL)->L>=MinL;true),
   (integer(MaxL)->L=<MaxL;true).

grows_into(creature(C,TA),creature(C,TB)):-grows_into(C,TA,TB).

align_attacks(evil,player).
align_attacks(berserk,_).
align_attacks(player,_).
align_attacks(evil,good).
align_attacks(good,evil).
