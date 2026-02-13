% =====================
% DETECTOR CLASS - Emotion & Topic Detection
% =====================
:- module(detector, [
    detector_is_crisis/1,
    detector_detect_emotion/2,
    detector_detect_topic/2,
    detector_is_gibberish/2
]).
:- use_module('nlp/matcher').

% Crisis detection
detector_is_crisis(Tokens) :- 
    matcher_contains_any(Tokens, [suicide, suicidal, kill, dying, die, death, 
                                   end, ending, disappear]).

% Emotion detection
detector_detect_emotion(Tokens, sad) :-
    matcher_contains_any(Tokens, [sad, lonely, depressed, hopeless, cry, crying, 
                                   awful, terrible, horrible, bad, worst, heartbroken,
                                   upset, down, unhappy, miserable, gloomy, pain, 
                                   grief, disappointed, hurt]), !.

detector_detect_emotion(Tokens, stressed) :-
    matcher_contains_any(Tokens, [stressed, anxious, overwhelmed, pressure, panic,
                                   burned, burnout, tense, worried, deadline, chaos,
                                   frustrated, busy, nervous, overloaded]), !.

detector_detect_emotion(Tokens, angry) :-
    matcher_contains_any(Tokens, [angry, mad, furious, annoyed, irritated, rage,
                                   resentful, outraged, pissed, cross]), !.

detector_detect_emotion(Tokens, tired) :-
    matcher_contains_any(Tokens, [tired, exhausted, sleepy, drained, fatigued,
                                   worn, weary]), !.

detector_detect_emotion(Tokens, happy) :-
    matcher_contains_any(Tokens, [happy, great, excited, relieved, good, joyful,
                                   cheerful, glad, awesome, fantastic, wonderful,
                                   amazing, content, blessed]), !.

detector_detect_emotion(_, neutral).

% Topic detection
detector_detect_topic(Tokens, breakup) :- 
    matcher_contains_any(Tokens, [ex, boyfriend, girlfriend, partner, relationship,
                                   dumped, left, broke, breakup, love, heart, miss,
                                   cheated, affair, girl, boy]), !.

detector_detect_topic(Tokens, failure) :- 
    matcher_contains_any(Tokens, [failed, fail, failure, stupid, dumb, useless, 
                                   mistake, exam, test, job, fired, rejected, 
                                   rejection, inadequate]), !.

detector_detect_topic(Tokens, anxiety) :- 
    matcher_contains_any(Tokens, [scared, anxious, anxiety, panic, stress, 
                                   overwhelmed, future, nervous, worry, worried,
                                   afraid, fear]), !.

detector_detect_topic(Tokens, lonely) :- 
    matcher_contains_any(Tokens, [lonely, alone, nobody, isolated, friends, 
                                   ignored, forgotten, abandoned]), !.

detector_detect_topic(Tokens, tired) :- 
    matcher_contains_any(Tokens, [tired, exhausted, burnout, drained, weary,
                                   sleep, insomnia, rest]), !.

detector_detect_topic(_, none).

% Gibberish detection - FIXED
detector_is_gibberish(Input, Tokens) :-
    string_length(Input, L),
    length(Tokens, TL),
    (
        % Case 1: Long string with very few tokens (random letters)
        (L > 10, TL < 3) ;
        % Case 2: Single token with no vowels (like "bsks", "cdc")
        (TL =:= 1, Tokens = [Token], \+ has_vowel(Token)) ;
        % Case 3: All tokens have no vowels
        (TL > 0, \+ any_token_has_vowel(Tokens))
    ).

% Check if atom has vowels
has_vowel(Atom) :-
    atom_chars(Atom, Chars),
    member(V, [a, e, i, o, u, y]),
    member(V, Chars), !.

% Check if any token has vowels
any_token_has_vowel([H|_]) :- has_vowel(H), !.
any_token_has_vowel([_|T]) :- any_token_has_vowel(T).
