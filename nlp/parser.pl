% =====================
% PARSER CLASS - Text Processing
% =====================
:- module(parser, [parser_normalize/2, parser_extract_name/2]).
:- use_module('utils/helpers').

% Normalize input string to tokens
parser_normalize(String, Tokens) :-
    string_lower(String, Lower),
    split_string(Lower, " .?!,;:'\"", " .?!,;:'\"", StrList),
    maplist(atom_string, Tokens, StrList).

% Extract name from tokens
parser_extract_name(Tokens, Name) :- 
    append(_, [name, is, Name], Tokens), 
    \+ helpers_is_reserved(Name), !.
parser_extract_name(Tokens, Name) :- 
    append(_, [i, am, Name], Tokens), 
    \+ helpers_is_reserved(Name), !.
parser_extract_name(Tokens, Name) :- 
    append(_, [im, Name], Tokens), 
    \+ helpers_is_reserved(Name), !.
parser_extract_name(Tokens, Name) :- 
    append(_, [call, me, Name], Tokens), 
    \+ helpers_is_reserved(Name), !.
