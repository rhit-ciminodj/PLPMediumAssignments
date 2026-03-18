% :- module(csse403nlp,[parse/2,translate/2]).
% we should use a module,really, but it wasn't obvious how to
% make it so the that the operator => displays correctly when it
% is in a module.

% this creates a new => operator to have the same priority as +
% seems to make it behave as you would expect.
% :- op(200, xfx, user:(==>)).

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

determinant(_, the).
% determinant(singular, a).

noun(singular, apple).
noun(plural, apples).
noun(singular, boy).
noun(plural, boys).
noun(singular, girl).
noun(plural, girls).

int_verb(singular, runs).
int_verb(plural, run).
int_verb(singular, dances).
int_verb(plural, dance).

trans_verb(singular, likes).
trans_verb(plural, like).
trans_verb(singular, hates).
trans_verb(plural, hate).
trans_verb(singular, respects).
trans_verb(plural, respect).

qualifier(_, some).
qualifier(plural, all).

relative(that).
relative(which).

translate(statement(NT,VT), Output).




