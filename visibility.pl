:-module(visibility,[calculate_visibility/0,
                    visible/2,was_visible/2]).

:-use_module(dungeon).

:-dynamic visible/2,was_visible/2.

calculate_visibility:-
   retractall(was_visible(_,_)),
   forall(visible(R,C),assert(was_visible(R,C))),
   retractall(visible(_,_)),
   attribute(player-row,PRow),attribute(player-column,PCol),
   findall(checkable_square(Octant,Distance,MinSlope,MaxSlope,Row,Col,What),
      checkable_square(PRow,PCol,Row,Col,What,Octant,
                       Distance,MinSlope,MaxSlope),
      CS),
   sort(CS,SCS),
   calculate_visibility(SCS).

calculate_visibility([]).
calculate_visibility([checkable_square(_,_,MinS,_,_,_,_)|T]):-
   MinS>1,!,calculate_visibility(T).
calculate_visibility([checkable_square(O,_,MinS,MaxS,R,C,W)|T]):-
   assert(visible(R,C)),
   first_sight(R,C),
   (passable(see,W),NT=T;weed_viz_list(T,O,MinS,MaxS,NT)),
   !,calculate_visibility(NT).

first_sight(R,C):-
   retract(player_dungeon(R,C,What,unseen)),
   first_appearance(What,App),
   assert(player_dungeon(R,C,What,App)),!.
first_sight(_,_).

first_appearance(X,X).

% weed_viz_list(L,O,MinS,MaxS,NL)
% removes any checkable_squares from L that are hidden behind the specified
% octant/slope coordinates returning result in NL

% basis case - empty lists
weed_viz_list([],_,_,_,[]).

% stop scanning if we are into a new octant; this works because the list is
% sorted by octant and we're always pulling CS from the first one, so it can
% only possibly hide things in the first chunk of the list
weed_viz_list([checkable_square(O,D,MinS,MaxS,R,C,W)|T],
              OO,_,_,
              [checkable_square(O,D,MinS,MaxS,R,C,W)|T]):-OO\=O,!.

% case 1: H completely hidden by CS - delete it
% H            A---B
% CS        X---------Y
weed_viz_list([checkable_square(O,_,A,B,_,_,_)|TA],
               O,X,Y,TB):-
   A>=X,B=<Y,!,weed_viz_list(TA,O,X,Y,TB).

% case 2: CS cuts off low side of H - clip it
% H            A------B
% CS        X------Y
weed_viz_list([checkable_square(O,D,A,B,R,C,W)|TA],
              O,X,Y,
              [checkable_square(O,D,Y,B,R,C,W)|TB]):-
   A>=X,A=<Y,B>Y,!,weed_viz_list(TA,O,X,Y,TB).

% case 3: CS cuts off high side of H - clip it
% H         A------B
% CS           X------Y
weed_viz_list([checkable_square(O,D,A,B,R,C,W)|TA],
              O,X,Y,
              [checkable_square(O,D,A,X,R,C,W)|TB]):-
   A<X,B>=X,B=<Y,!,weed_viz_list(TA,O,X,Y,TB).

% case 4: CS inside H - split H in two - should never happen
% H         A---------B
% CS           X---Y
weed_viz_list([checkable_square(O,D,A,B,R,C,W)|TA],
              O,X,Y,
              [checkable_square(O,D,A,X,R,C,W),
              checkable_square(O,D,Y,B,R,C,W)|TB]):-
   A<X,B>Y,!,weed_viz_list(TA,O,X,Y,TB).

% case 5: CS misses H completely - keep it, keep scanning
weed_viz_list([H|TA],O,X,Y,[H|TB]):-
   !,weed_viz_list(TA,O,X,Y,TB).

checkable_square(PRow,PCol,Row,Col,What,Octant,Distance,MinSlope,MaxSlope):-
   player_dungeon(Row,Col,What,_),
   XA is Col-PCol,YA is PRow-Row,
   octant_coords(Octant,XA,YA,XB,YB),
   coords_legal(XB,YB),
   Distance is XB+YB,
   MinSlope is (YB-0.5)/(XB+0.5),
   MaxSlope is (YB+0.5)/(XB-0.5).
checkable_square(PRow,PCol,PRow,PCol,What,zero,0.0,0.0,0.0):-
   player_dungeon(PRow,PCol,What,_).

octant_coords(i,X,Y,X,Y).
octant_coords(ii,X,Y,Y,X).
octant_coords(iii,X,Y,Y,XP):-XP is -X.
octant_coords(iv,X,Y,XP,Y):-XP is -X.
octant_coords(v,X,Y,XP,YP):-XP is -X,YP is -Y.
octant_coords(vi,X,Y,YP,XP):-XP is -X,YP is -Y.
octant_coords(vii,X,Y,YP,X):-YP is -Y.
octant_coords(viii,X,Y,X,YP):-YP is -Y.

coords_legal(X,Y):-X>0,Y>=0,Y=<X+1.
