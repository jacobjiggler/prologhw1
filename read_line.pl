%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%  read_line
%%%  Similar to read_sent in Pereira and Shieber, Prolog and
%%%        Natural Language Analysis, CSLI, 1987.
%%%
%%%  Examples:
%%%           % read_line(L).
%%%           The sky was blue, after the rain.
%%%           L = [the,sky,was,blue,',',after,the,rain,'.']
%%%           % read_line(L).
%%%           Which way to the beach?
%%%           L = [which,way,to,the, beach,'?']
%%%

read_line(Words) :- get0(C),
                    read_rest(C,Words).
          
/* A period or question mark ends the input. */
read_rest(46,['.']) :- !.
read_rest(63,['?']) :- !.

/* Spaces and newlines between words are ignored. */
read_rest(C,Words) :- ( C=32 ; C=10 ) , !,
                     get0(C1),
                     read_rest(C1,Words).

/* Commas between words are absorbed. */
read_rest(44,[','|Words]) :- !,
                             get0(C1),
                             read_rest(C1,Words).

/* Otherwise get all of the next word. */
read_rest(C,[Word|Words]) :- lower_case(C,LC),
                             read_word(LC,Chars,Next),
                             name(Word,Chars),
                             read_rest(Next,Words).

/* Space, comma, newline, period or question mark separate words. */
read_word(C,[],C) :- ( C=32 ; C=44 ; C=10 ;
                         C=46 ; C=63 ) , !.

/* Otherwise, get characters, convert alpha to lower case. */
read_word(C,[LC|Chars],Last) :- lower_case(C,LC),
                                get0(Next),
                                read_word(Next,Chars,Last).

/* Convert to lower case if necessary. */
lower_case(C,C) :- ( C <  65 ; C > 90 ) , !.
lower_case(C,LC) :- LC is C + 32.


/* for reference ... 
newline(10).
comma(44).
space(32).
period(46).
question_mark(63).
*/


