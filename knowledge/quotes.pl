% =====================
% QUOTES CLASS - Motivational Quotes Database
% =====================
:- module(quotes, [quotes_get/2, quotes_get_random/2]).
:- use_module(library(random)).

% Get all quotes for an emotion
quotes_get(Emotion, Quotes) :-
    findall(Q, quote(Emotion, Q), Quotes).

% Get random quote for an emotion
quotes_get_random(Emotion, Quote) :-
    quotes_get(Emotion, Quotes),
    random_member(Quote, Quotes).

% Quote database
quote(sad, 'Sometimes when you are in a dark place you think you have been buried, but you have actually been planted.').
quote(sad, 'Even the darkest night will end and the sun will rise.').
quote(sad, 'This too shall pass.').
quote(sad, 'Pain is inevitable. Suffering is optional.').
quote(sad, 'You are allowed to be both a masterpiece and a work in progress simultaneously.').

quote(stressed, 'Do not anticipate trouble, or worry about what may never happen.').
quote(stressed, 'You cannot control what goes on outside, but you can always control what goes on inside.').
quote(stressed, 'Tension is who you think you should be. Relaxation is who you are.').
quote(stressed, 'Almost everything will work again if you unplug it for a few minutes.').

quote(angry, 'For every minute you are angry you lose sixty seconds of happiness.').
quote(angry, 'Speak when you are angry and you will make the best speech you will ever regret.').
quote(angry, 'Holding onto anger is like drinking poison and expecting the other person to die.').
quote(angry, 'Anger is an acid that can do more harm to the vessel than to anything on which it is poured.').

quote(tired, 'Almost everything will work again if you unplug it for a few minutes... including you.').
quote(tired, 'Rest and self-care are so important.').
quote(tired, 'Take rest; a field that has rested gives a bountiful crop.').
quote(tired, 'Sleep is the best meditation.').

quote(happy, 'Happiness is not something ready-made. It comes from your own actions.').
quote(happy, 'The purpose of our lives is to be happy.').
quote(happy, 'Happiness often sneaks in through a door you did not know you left open.').
quote(happy, 'Joy is what happens when we allow ourselves to recognize how good things really are.').
