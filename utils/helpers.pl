% =====================
% HELPERS CLASS - Utility Functions
% =====================
:- module(helpers, [helpers_capitalize/2, helpers_is_reserved/1]).

% Capitalize first letter of atom
helpers_capitalize(Atom, Cap) :- 
    atom_chars(Atom, [H|T]), 
    upcase_atom(H, U), 
    atom_chars(Cap, [U|T]).

% Check if word is reserved
helpers_is_reserved(W) :- 
    member(W, [sad, happy, tired, hungry, angry, here, fine, ok, okay, 
               bot, feeling, good, bad, great, awful, terrible, lonely,
               stressed, anxious, depressed]).
