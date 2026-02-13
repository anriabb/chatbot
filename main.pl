% =====================
% MAIN ENTRY POINT
% =====================
:- use_module(library(random)).
:- use_module(library(lists)).

% Load all modules
:- consult('core/bot.pl').
:- consult('core/memory.pl').
:- consult('nlp/parser.pl').
:- consult('nlp/detector.pl').
:- consult('nlp/matcher.pl').
:- consult('knowledge/quotes.pl').
:- consult('knowledge/advice.pl').
:- consult('response/router.pl').
:- consult('response/generator.pl').
:- consult('utils/helpers.pl').

% Start the bot
:- initialization(bot_start).
