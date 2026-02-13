% =====================
% BOT CLASS - Core Bot Functionality
% =====================
:- module(bot, [bot_start/0, bot_loop/0, bot_shutdown/0]).
:- use_module('core/memory').
:- use_module('nlp/parser').
:- use_module('response/router').

% Initialize and start the bot
bot_start :-
    memory_init,
    bot_display_welcome,
    bot_loop.

% Display welcome screen
bot_display_welcome :-
    writeln("\033[1;36m╔════════════════════════════════════╗"),
    writeln("║                                    ║"),
    writeln("║          ／l、                     ║"),
    writeln("║        （ﾟ､ ｡ ７                   ║"),
    writeln("║          l、 ~ヽ                   ║"),
    writeln("║          じしf_, )ノ               ║"),
    writeln("║                                    ║"),
    writeln("║          PrologBot <3              ║"),
    writeln("║                                    ║"),
    writeln("╚════════════════════════════════════╝\033[0m"),
    writeln("Type anything to start chatting. Type 'bye' to exit."),
    nl.

% Main conversation loop
bot_loop :-
    write("\033[1;32m> \033[0m"),
    read_line_to_string(user_input, InputRaw),
    (
        InputRaw = end_of_file -> 
            bot_shutdown
    ;   
        parser_normalize(InputRaw, Tokens),
        memory_increment_depth,
        router_decide_response(Tokens, InputRaw, Response),
        % Print bot response in pink color
        write("\033[1;35m"),  % Pink color
        writeln(Response),
        write("\033[0m"),     % Reset color
        nl,
        (member(bye, Tokens) -> bot_shutdown ; bot_loop)
    ).

% Shutdown the bot
bot_shutdown :-
    write("\033[1;35m"),  % Pink color
    writeln("Goodbye. Be gentle with yourself today. 🤍"),
    write("\033[0m"),     % Reset color
    halt.
