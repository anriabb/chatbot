% =====================
% MATCHER CLASS - Pattern Matching Utilities
% =====================
:- module(matcher, [
    matcher_contains_any/2,
    matcher_intersection/3
]).

% Check if tokens contain any of the words
matcher_contains_any(Tokens, Words) :-
    matcher_intersection(Tokens, Words, Match),
    Match \= [].

% Custom intersection
matcher_intersection([], _, []).
matcher_intersection([H|T], L2, [H|Res]) :- 
    member(H, L2), !, 
    matcher_intersection(T, L2, Res).
matcher_intersection([_|T], L2, Res) :- 
    matcher_intersection(T, L2, Res).
