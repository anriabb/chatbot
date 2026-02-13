% =====================
% ADVICE CLASS - Practical Advice Database
% =====================
:- module(advice, [advice_get/2, advice_get_random/2]).
:- use_module(library(random)).

% Get all advice for a topic
advice_get(Topic, Advices) :-
    findall(A, advice_item(Topic, A), Advices).

% Get random advice for a topic
advice_get_random(Topic, Advice) :-
    advice_get(Topic, Advices),
    random_member(Advice, Advices).

% Advice database
advice_item(sad, 'Try this: Set a timer for 15 minutes and allow yourself to feel everything fully. Then, do one small thing that brings comfort.').
advice_item(sad, 'Name your emotion: I am sad because... Naming reduces the intensity and helps you process it.').
advice_item(sad, 'Move your body gently—walk outside, stretch, or dance to one song. Movement shifts emotional energy.').
advice_item(sad, 'Reach out to just one person with a simple text: Having a rough day. Connection helps more than you think.').

advice_item(stressed, 'Brain dump: Write down everything worrying you. Getting it out of your head and onto paper creates mental space.').
advice_item(stressed, 'Try the Pomodoro technique: Work for 25 minutes, then take a 5-minute break. This prevents overwhelm.').
advice_item(stressed, 'Ask yourself: What is the ONE thing I can do right now that would make the biggest difference? Then do just that.').
advice_item(stressed, 'Take a 2-minute breathing break: Inhale for 4, hold for 4, exhale for 4, hold for 4. Repeat 5 times.').

advice_item(angry, 'Pause before responding. Count to 10 slowly. Anger makes us say things we cannot take back.').
advice_item(angry, 'Physical release helps: Punch a pillow, go for a run, or scream into a pillow. Get the energy OUT safely.').
advice_item(angry, 'Ask: What am I really feeling underneath this anger? Often it is hurt, fear, or disappointment.').
advice_item(angry, 'If you need to respond, use this: When you ___, I felt ___ because ___. Facts, not attacks.').

advice_item(tired, 'Sleep hygiene matters: Go to bed at the same time tonight, keep your room cool and dark, no screens 1 hour before bed.').
advice_item(tired, 'Take a 10-minute power nap (set an alarm!). Longer than 20 minutes will make you groggier.').
advice_item(tired, 'Check the basics: Are you eating enough? Drinking water? Low energy often comes from these simple things.').
advice_item(tired, 'Say no to one thing today. Every yes to something is a no to rest.').

advice_item(breakup, 'No contact is crucial. Each time you check their social media, you reset your healing clock. Block or mute them.').
advice_item(breakup, 'Write a letter you will never send. Get everything out on paper—all the hurt, anger, love. Then keep it or burn it.').
advice_item(breakup, 'It takes roughly half the length of the relationship to heal. Be patient with yourself. Healing is not linear.').
advice_item(breakup, 'Reconnect with who you were before them. What did you love doing? What made you laugh? Rediscover yourself.').

advice_item(failure, 'Do a failure autopsy: What happened? What can you learn? What was outside your control? This creates wisdom.').
advice_item(failure, 'Reframe it: Every successful person has failed. This is data for growth, not a verdict on your worth.').
advice_item(failure, 'Create a wins list right now: Write 10 things you have succeeded at, big or small. You are more capable than you feel.').
advice_item(failure, 'Take one small action today toward improvement. Even tiny momentum breaks the paralysis of failure.').

advice_item(anxiety, 'Try the 5-4-3-2-1 technique: Name 5 things you see, 4 you feel, 3 you hear, 2 you smell, 1 you taste. It grounds you.').
advice_item(anxiety, 'Schedule worry time: 15 minutes daily to write down worries. Outside this time, postpone anxious thoughts.').
advice_item(anxiety, 'Fact-check your thoughts: Is this true? Do I have evidence? What is the MOST LIKELY outcome? Often anxiety lies.').
advice_item(anxiety, 'Move for 10 minutes. Walk, dance, stretch. Physical movement significantly reduces anxiety chemicals in your brain.').

advice_item(lonely, 'Reach out first. Text someone: Hey, just thinking of you. Connection is a two-way street and most people appreciate it.').
advice_item(lonely, 'Join something with regular meetings: book club, class, volunteer work. Shared activities naturally build bonds.').
advice_item(lonely, 'Practice micro-connections: Smile at a stranger, chat with a cashier, comment genuinely online. Small interactions add up.').
advice_item(lonely, 'Be a friend to yourself first. Take yourself on a date—coffee, movie, park. Learn to enjoy your own company.').

