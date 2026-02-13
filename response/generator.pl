% =====================
% GENERATOR CLASS - Response Generation
% =====================
:- module(generator, [
    generator_generate_initial_response/2,
    generator_give_quote_and_advice/2
]).
:- use_module(library(random)).
:- use_module('core/memory').
:- use_module('knowledge/quotes').
:- use_module('knowledge/advice').

% Generate initial empathetic response (SHORT - no quote/advice yet)
generator_generate_initial_response(sad, Reply) :-
    random_member(Reply, [
        'I hear you. 💔 That sounds really hard.\nWhat is making you feel this way?',
        'I am sorry you are going through this. 💙\nCan you tell me more?',
        'That sounds heavy. I am here to listen.\nWhat happened?'
    ]).

generator_generate_initial_response(stressed, Reply) :-
    random_member(Reply, [
        'Stress can be overwhelming. 😓\nWhat is weighing on you most?',
        'That sounds like a lot to carry.\nTell me what is going on?',
        'I hear that you are feeling the pressure.\nWhat is stressing you out?'
    ]).

generator_generate_initial_response(angry, Reply) :-
    random_member(Reply, [
        'I hear your frustration. 😠\nWhat happened?',
        'It sounds like something really upset you.\nDo you want to talk about it?',
        'Your anger is valid.\nWhat made you feel this way?'
    ]).

generator_generate_initial_response(tired, Reply) :-
    random_member(Reply, [
        'You sound exhausted. 😴\nWhat has been draining you?',
        'Being tired is tough.\nWhat is going on?',
        'That sounds like a lot.\nHow long have you felt this way?'
    ]).

generator_generate_initial_response(happy, Reply) :-
    random_member(Reply, [
        'I am so glad to hear that! 😊\nWhat happened?',
        'That is wonderful! 🌟\nTell me more!',
        'Your happiness makes me happy too! 💛\nWhat brought this on?'
    ]).

generator_generate_initial_response(breakup, Reply) :-
    random_member(Reply, [
        'Heartbreak is one of the hardest things. 💔\nHow long ago did this happen?',
        'I am sorry. Breakups hurt deeply.\nDo you want to talk about it?',
        'That is so painful. I am here for you.\nWhat happened?'
    ]).

generator_generate_initial_response(failure, Reply) :-
    random_member(Reply, [
        'Feeling like you failed is heavy. 🛑\nWhat happened?',
        'I understand that weight.\nCan you tell me about it?',
        'That sounds tough.\nWhat went wrong?'
    ]).

generator_generate_initial_response(anxiety, Reply) :-
    random_member(Reply, [
        'Anxiety can feel suffocating. 🍃\nWhat is making you anxious?',
        'I hear you. That sounds overwhelming.\nWhat are you worried about?',
        'Take a breath. I am here.\nWhat is going on?'
    ]).

generator_generate_initial_response(lonely, Reply) :-
    random_member(Reply, [
        'Loneliness is a heavy feeling. 🌍\nHow long have you felt this way?',
        'I am here with you right now.\nTell me more about how you are feeling.',
        'You are not alone in this conversation. 💙\nWhat is going on?'
    ]).

% NEW: Give quote and advice (called after 2+ emotion mentions)
generator_give_quote_and_advice(Stage, Reply) :-
    member(Stage, [sad, stressed, angry, tired, breakup, failure, anxiety, lonely]), !,
    map_stage_to_emotion(Stage, QuoteEmotion),
    quotes_get_random(QuoteEmotion, Quote),
    advice_get_random(Stage, Advice),
    format(string(Reply), '💬 "~w"\n\n💡 ~w\n\nDoes this help?', [Quote, Advice]).

generator_give_quote_and_advice(_, Reply) :-
    Reply = 'I am here with you. 💛 What would you like to talk about?'.

% Map conversation stages to quote emotions
map_stage_to_emotion(sad, sad).
map_stage_to_emotion(stressed, stressed).
map_stage_to_emotion(angry, angry).
map_stage_to_emotion(tired, tired).
map_stage_to_emotion(breakup, sad).
map_stage_to_emotion(failure, stressed).
map_stage_to_emotion(anxiety, stressed).
map_stage_to_emotion(lonely, sad).
