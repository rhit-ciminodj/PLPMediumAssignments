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
noun_phrase(PoS, NT, [Qualifier|Relcl]) :- .

verb_phrase(PoS, verb(Verb), [Verb]) :- verb(PoS, Verb).

relcl(PoS, relcl(Noun, Relative, VP), [Noun,Relative|V]) :- .

determinant(_, the).
determinant(singular, a).

noun(singular, apple).
noun(singular, boy).
noun(singular, girl).

noun(plural, apples).
noun(plural, boys).
noun(plural, girls).

verb(singular, runs).

verb(plural, run).

qualifier(_, some).
qualifier(plural, all).

relative(that).
relative(which).

translate(statement(NT,VT), Output).




