% =====================
% ROUTER CLASS - Response Decision Logic
% =====================
:- module(router, [router_decide_response/3]).
:- use_module(library(random)).
:- use_module('core/memory').
:- use_module('nlp/parser').
:- use_module('nlp/detector').
:- use_module('nlp/matcher').
:- use_module('response/generator').

% Main routing logic
router_decide_response(Tokens, _, Reply) :-
    member(bye, Tokens), !,
    Reply = 'Goodbye. Be gentle with yourself today. 🤍'.

router_decide_response(Tokens, _, Reply) :-
    detector_is_crisis(Tokens), !,
    Reply = '\033[1;31m🛑 I hear that you are in deep pain.\nI am a bot, so I cannot keep you safe, but there are people who can.\n\nPlease reach out immediately:\n  • National Suicide Prevention Lifeline: 988 (US)\n  • Crisis Text Line: Text HOME to 741741\n  • International: findahelpline.com\n\nYou matter. Your pain is real, but it is not permanent.\033[0m'.

router_decide_response(Tokens, _, Reply) :-
    parser_extract_name(Tokens, Name), !,
    memory_set_name(Name),
    helpers_capitalize(Name, Cap),
    format(string(Reply), 'It is wonderful to meet you, ~w. 🤝\nHow are you feeling right now?', [Cap]).

router_decide_response(Tokens, _, Reply) :-
    matcher_contains_any(Tokens, [hello, hi, hey, greetings, sup, wassup]), !,
    (memory_get_name(N) -> 
        helpers_capitalize(N, C), 
        format(string(R), 'Hello again, ~w. 🤍', [C]) 
    ; 
        R = 'Hello there. 🤍'
    ),
    string_concat(R, ' What is on your mind today?', Reply).

router_decide_response(Tokens, _, Reply) :-
    matcher_contains_any(Tokens, [thanks, thank, thx, appreciate, grateful]), !,
    random_member(Reply, [
        'You are very welcome. 💛 I am glad we could talk.',
        'Of course — it is good to share these things.',
        'Anytime. I am here whenever you need to talk. 🤍'
    ]).

router_decide_response(Tokens, Input, Reply) :-
    detector_is_gibberish(Input, Tokens), !,
    Reply = 'Hmm 🤔 I did not quite understand that. Could you rephrase?'.

router_decide_response(Tokens, _, Reply) :-
    matcher_contains_any(Tokens, [okay, ok, yes, yeah, yep, sure, alright, yeas]),
    length(Tokens, L),
    L < 3,
    \+ member(nothing, Tokens),
    \+ member(no, Tokens), !,
    memory_get_emotion_mentions(Count),
    memory_get_conversation_stage(Stage),
    (Count >= 2, Stage \= initial ->
        generator_give_quote_and_advice(Stage, Reply)
    ;
        random_member(Reply, [
            'I am listening. Tell me more.',
            'Go on, I am here. 💛',
            'What else would you like to share?'
        ])
    ).

router_decide_response(Tokens, _, Reply) :-
    (member(nothing, Tokens) ; member(nothin, Tokens)),
    (member(else, Tokens) ; length(Tokens, L), L < 3), !,
    memory_get_conversation_stage(Stage),
    (Stage \= initial ->
        generator_give_quote_and_advice(Stage, Reply)
    ;
        Reply = 'That is okay. Take your time. 🤍'
    ).

router_decide_response(Tokens, _, Reply) :-
    matcher_contains_any(Tokens, [sad, hard, difficult, struggling, failed]),
    matcher_contains_any(Tokens, [but, trying, try, hope, still]), !,
    Reply = 'I see pain in your words, but I also see resilience. ✨\nWhat is happening right now?'.

router_decide_response(Tokens, _, Reply) :- 
    detector_detect_topic(Tokens, Topic),
    Topic \= none, !,
    memory_track_mood(Topic),
    memory_set_conversation_stage(Topic),
    memory_increment_emotion_mentions,
    generator_generate_initial_response(Topic, Reply).

router_decide_response(Tokens, _, Reply) :-
    detector_detect_emotion(Tokens, Emotion),
    Emotion \= neutral, !,
    memory_track_mood(Emotion),
    memory_set_conversation_stage(Emotion),
    memory_increment_emotion_mentions,
    memory_get_emotion_mentions(Count),
    (Count >= 2 ->
        generator_give_quote_and_advice(Emotion, Reply)
    ;
        generator_generate_initial_response(Emotion, Reply)
    ).

router_decide_response(Tokens, _, Reply) :-
    matcher_contains_any(Tokens, [need, want, wanna, gotta]),
    matcher_contains_any(Tokens, [talk, speak, share, tell, say]), !,
    random_member(Reply, [
        'I am here for you. What is on your mind?',
        'Of course. I am listening.',
        'Please share. I am here with you. 💛'
    ]).

router_decide_response(_, _, Reply) :-
    memory_get_conversation_stage(Stage),
    memory_get_emotion_mentions(Count),
    (Count >= 2, Stage \= initial ->
        generator_give_quote_and_advice(Stage, Reply)
    ;
        random_member(Reply, [
            'I am listening. Please go on.',
            'Tell me more about that.',
            'What else is on your mind?',
            'I am here. What is happening?'
        ])
    ).
