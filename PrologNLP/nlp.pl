% :- module(csse403nlp,[parse/2,translate/2]).
% we should use a module,really, but it wasn't obvious how to
% make it so the that the operator => displays correctly when it
% is in a module.

% this creates a new => operator to have the same priority as +
% seems to make it behave as you would expect.
:- op(200, xfx, user:(==>)).

% ==>(foo,bar).

% your code goes below
% the tests are in another file

parse(X,statement(NT,VT)) :-
  append(N, V, X),
  noun_phrase(PoS, NT, N),
  verb_phrase(PoS, VT, V).


noun_phrase(PoS, noun(Noun), [Noun]) :- noun(PoS, Noun).
noun_phrase(PoS, noun(Noun), [X,Noun]) :- determinant(PoS, X), noun(PoS, Noun).
noun_phrase(PoS, NT, [Qualifier, Noun]) :- NT =.. [Qualifier, noun(Noun)], noun(PoS, Noun), qualifier(PoS, Qualifier).
noun_phrase(PoS, NT, [Qualifier|Relcl]) :- qualifier(PoS, Qualifier), relcl(PoS, RT, Relcl), NT =.. [Qualifier, RT].

verb_phrase(PoS, verb(Verb), [Verb]) :- int_verb(PoS, Verb).
verb_phrase(PoS, verb(Verb, NP), [Verb|N]) :- trans_verb(PoS, Verb), noun_phrase(_, NP, N).

relcl(PoS, relcl(noun(Noun), VP), [Noun, Relative|V]) :- relative(Relative), noun(PoS, Noun), verb_phrase(PoS, VP, V).
relcl(PoS, relcl(noun(Noun), NP, verb(Verb)), [Noun, Relative|Rest]) :- append(NPhrase, [Verb], Rest), relative(Relative), noun(PoS, Noun), noun_phrase(PoS2, NP, NPhrase), trans_verb(PoS2, Verb).

determinant(_, the).
% determinant(singular, a).

noun(singular, apple).
noun(plural, apples).
noun(singular, boy).
noun(plural, boys).
noun(singular, girl).
noun(plural, girls).

adjacent(boy, boys).
adjacent(apple, apples).
adjacent(girl, girls).

find_singular(Noun, Output) :- noun(singular, Noun), !, Output = Noun.
find_singular(Noun, Output) :- noun(plural, Noun), adjacent(Output, Noun).

find_verb(Verb, Output) :- int_verb(plural, Verb), !, Output = Verb.
find_verb(Verb, Output) :- trans_verb(plural, Verb), !, Output = Verb.

find_verb(Verb, Output) :- int_verb(singular, Verb), adjacent(Verb, Output).
find_verb(Verb, Output) :- trans_verb(singular, Verb), adjacent(Verb, Output).

int_verb(singular, runs).
int_verb(plural, run).
int_verb(singular, dances).
int_verb(plural, dance).

adjacent(runs, run).
adjacent(dances, dance).

trans_verb(singular, likes).
trans_verb(plural, like).
trans_verb(singular, hates).
trans_verb(plural, hate).
trans_verb(singular, respects).
trans_verb(plural, respect).

adjacent(likes, like).
adjacent(hates, hate).
adjacent(respects, respect).

qualifier(_, some).
qualifier(plural, all).

relative(that).
relative(which).

translate(statement(NT,VT), Output) :-
	translate_np(NT, 1, N2, VPF, Output),
	translate_vp(VT, 1, N2, _, VPF). 

translate_np(some(noun(Noun)), N, N1, VPF, exists(N, Restr+VPF)) :-
	N1 is N + 1,
	find_singular(Noun, Singular),
	Restr =.. [Singular, N].

translate_np(all(noun(Noun)), N, N1, VPF, all(N, Restr==>VPF)) :-
	N1 is N + 1,
	find_singular(Noun, Singular),
	Restr =.. [Singular, N].

translate_np(some(relcl(noun(Noun), VP)), N, N1, VPF, exists(N, Restr+Relcl+VPF)) :-
	N1 is N + 1,
	find_singular(Noun, Singular),
	Restr =.. [Singular, N],
	translate_vp(VP, N, N1, _, Relcl).

translate_np(all(relcl(noun(Noun), VP)), N, N1, VPF, all(N, (Restr+Relcl)==>VPF)) :-
	N1 is N + 1,
	find_singular(Noun, Singular),
	Restr =.. [Singular, N],
	translate_vp(VP, N, N1, _, Relcl).

translate_np(all(relcl(noun(Noun), NP, verb(Verb))), N, N2, VPF, all(N, (Restr+Relcl)==>VPF)) :-
	N1 is N + 1,
	find_singular(Noun, Singular),
	Restr =.. [Singular, N],
	find_verb(Verb, Output),
	VerbApp =.. [Output, N1, N],
	translate_np(NP, N1, N2, VerbApp, Relcl).

translate_np(some(relcl(noun(Noun), NP, verb(Verb))), N, N2, VPF, exists(N, Restr+Relcl+VPF)) :-
	N1 is N + 1,
	find_singular(Noun, Singular),
	Restr =.. [Singular, N],
	find_verb(Verb, Output),
	VerbApp =.. [Output, N1, N],
	translate_np(NP, N1, N2, VerbApp, Relcl).

translate_vp(verb(Verb), SubjVar, N, N, VerbF) :-
	find_verb(Verb, Output),
	VerbF =.. [Output, SubjVar].

translate_vp(verb(Verb, ObjNP), SubjVar, N, N2, ObjF) :-
	find_verb(Verb, Output),
	VerbApp =.. [Output, SubjVar, N],
	translate_np(ObjNP, N, N2, VerbApp, ObjF).

concat_char(Chars, '\n', Z) :- Z = Chars.
concat_char(Chars, X, Z) :- get_char(Y), concat_char([X|Chars], Y, Z).

get_string(X) :- get_char(Y), concat_char([], Y, Z), reverse(Z, R), atom_chars(X, R), !.

split_helper([], _, Acc, Output) :- !, reverse(Acc, Y), atom_chars(X, Y), Output = [X].
split_helper([Seperator|Chars], Seperator, Acc, Output) :- !, split_helper(Chars, Seperator, [], X), reverse(Acc, Z), atom_chars(Y, Z), Output = [Y|X].
split_helper([First|Chars], Seperator, Acc, Output) :- split_helper(Chars, Seperator, [First|Acc], Output).

split(Atom, Seperator, Output) :- atom_chars(Atom, Chars), split_helper(Chars, Seperator, [], Output).

process_input(done) :- !.
process_input(Input) :- split(Input, ' ', Words), parse(Words, Statement), translate(Statement, Output), write(Output), !, do_nlp.

do_nlp :- get_string(Input),