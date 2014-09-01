/* prolog tutorial 7_3.pl */

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Command idiom c:
%  {Please} place <put> {block} X on <onto >{block} Y, W on Z, ...
%  I want <would like> X on Y, W on Z, ...
%  I want <would like> you to put <place> ...
%  Can <could> <would> you {please} put <place> X on Y, ...

c(L) --> lead_in,arrange(L),end.

end --> ['.'] | ['?'].

lead_in --> please, place.
lead_in --> [i], [want] | [i], [would], [like], you_to_put.
lead_in --> ([can] | [could] | [would]), [you], please, place.

you_to_put --> [] | [you], [to], place.   %%% partially optional

please --> [] | [please].    %%% optional word

place --> [put] | [place].   %%% alternate words

arrange([ON]) --> on(ON).
arrange([ON|R]) --> on(ON), comma, arrange(R).

comma --> [','] | ['and'] | [','],[and].   %%% alternate words

on(on(X,Y)) --> block, [X], ([on] | [onto] | [on],[top],[of]), block, [Y].
on(on(X,table)) --> [X],([on] | [onto]), [the], [table].

block --> [] | [block].   %%% optional word

:- [read_line].

place_blocks :- 
    repeat,
    write('?? '), 
    read_line(X),
    ( c(F,X,[]), assert_list(F), write('ok.'), nl | q(F,X,[]) ),
    answer(F), nl, fail.

% Assert each item in the list.
assert_list([]).
assert_list([H|T]) :- assert_item(H), assert_list(T).

% Add a new table spot.
assert_table_spot([X,0]) :- assert(free_spot_on_table([X,0])).
assert_table_spot([X,Y]) :- Y \= 0.

% Move block A on the table.
assert_item(on(A,table)) :- 
    location(A,[X,Y]),
    not(Y is 0),
    free_spot_on_table(P), 
    YN is Y + 1,
    not(location(_,[X,YN])),
    retract(free_spot_on_table(P)), 
    retract(location(A,[X,Y])),
    assert(location(A,P)),!.
% Move block A on block B.
assert_item(on(A,B)) :- 
    B \== table,
    location(A, [XA,YA]),
    YAN is YA + 1,
    not(location(_, [XA,YAN])),
    location(B, [XB,YB]),
    YBN is YB + 1,
    not(location(_, [XB,YBN])),
    retract(location(A, [XA,YA])),
    assert_table_spot([XA,YA]), % Possibly free up spot on table.
    assert(location(A, [XB,YBN])),!.
% Handle errors.
assert_item(on(A,table)) :- 
    location(A,[_,Y]), Y is 0,
    write('Already on the table!'), nl, !, fail.
assert_item(on(_,table)) :- 
    not(free_spot_on_table(_)), 
    write('No free spots on the table!'), nl, !, fail.
assert_item(on(A,table)) :- 
    location(A,[X,Y]), YN is Y + 1, location(_,[X,YN]),
    write('Cannot move from, something is on top!'), nl, !, fail.
assert_item(on(A,_)) :-
    not(location(A, _)),
    write('Block to move does not exist!'), nl, !, fail.
assert_item(on(_,B)) :-
    B \== table, not(location(B, _)),
    write('Block to place on does not exist!'), nl, !, fail.
assert_item(on(A,B)) :- 
    B \== table, location(A, [XA,YA]), YAN is YA + 1, location(_, [XA,YAN]),
    write('Cannot move from, something is on top!'), nl, !, fail.
assert_item(on(_,B)) :- 
    B \== table, location(B, [XB,YB]), YBN is YB + 1, location(_, [XB,YBN]),
    write('Cannot move to, something is on top!'), nl, !, fail.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Question idiom q:
%   Which block is on top of X?
%   What is on top of?
%
:- op(500, xfx, 'is_on_top_of').

% the question q
q(_ is_on_top_of A) --> [which],[block],[is],[on],[top],[of],[A],end.
q(_ is_on_top_of A) --> [what],[is],[on],[top],[of],[A],end.
   
% How to answer q
B is_on_top_of A :- location(A,[X,Y]),
                    Y1 is Y+1,
                    location(B,[X,Y1]), !.
'Nothing' is_on_top_of _ .

answer(X is_on_top_of A) :- call(X is_on_top_of A),
                            say([X,is,on,top,of,A]).

say([X|R]) :- write(X), write(' '), say(R).
say([]).

%
%  positioning information
%
%  [0,3] [1,3] [2,3]
%  [0,2] [1,2] [2,2]
%  [0,1] [1,1] [2,1]
%  [0,0] [1,0] [2,0]
% -=================-   table
%   
% initially 
%
%    c
%    b
%    a     d
% -=================-

:- dynamic free_spot_on_table/1.
:- dynamic location/2.
:- dynamic on/2.

free_spot_on_table([2,0]).
 
location(c,[0,2]).
location(b,[0,1]).
location(a,[0,0]).
location(d,[1,0]).

on(A,table) :- location(A,[_,0]).
on(A,B) :- B \== table,
           location(A,[X,YA]),
           location(B,[X,YB]),
           YB is YA - 1.

