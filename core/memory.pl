% =====================
% MEMORY CLASS - Dynamic State Management
% =====================
:- module(memory, [
    memory_init/0,
    memory_reset/0,
    memory_set_name/1,
    memory_get_name/1,
    memory_track_mood/1,
    memory_increment_depth/0,
    memory_get_depth/1,
    memory_get_conversation_stage/1,
    memory_set_conversation_stage/1,
    memory_increment_emotion_mentions/0,
    memory_get_emotion_mentions/1
]).

:- use_module(library(random)).

% Dynamic predicates
:- dynamic user_name/1.
:- dynamic mood_history/1.
:- dynamic conversation_depth/1.
:- dynamic conversation_stage/1.
:- dynamic emotion_mention_count/1.

% Initialize memory system
memory_init :-
    get_time(T), 
    S is round(T), 
    set_random(seed(S)),
    memory_reset.

% Reset all memory
memory_reset :-
    retractall(user_name(_)),
    retractall(mood_history(_)),
    retractall(conversation_depth(_)),
    retractall(conversation_stage(_)),
    retractall(emotion_mention_count(_)),
    assertz(conversation_depth(0)),
    assertz(conversation_stage(initial)),
    assertz(emotion_mention_count(0)).

% Set user name
memory_set_name(Name) :-
    retractall(user_name(_)),
    assertz(user_name(Name)).

% Get user name
memory_get_name(Name) :-
    user_name(Name).

% Track mood
memory_track_mood(Emotion) :-
    assertz(mood_history(Emotion)).

% Increment conversation depth
memory_increment_depth :-
    retract(conversation_depth(N)),
    N1 is N + 1,
    assertz(conversation_depth(N1)).

% Get conversation depth
memory_get_depth(N) :-
    conversation_depth(N).

% Get conversation stage
memory_get_conversation_stage(Stage) :-
    conversation_stage(Stage).

% Set conversation stage
memory_set_conversation_stage(Stage) :-
    retractall(conversation_stage(_)),
    assertz(conversation_stage(Stage)).

% Increment emotion mentions
memory_increment_emotion_mentions :-
    retract(emotion_mention_count(N)),
    N1 is N + 1,
    assertz(emotion_mention_count(N1)).

% Get emotion mention count
memory_get_emotion_mentions(N) :-
    emotion_mention_count(N).
