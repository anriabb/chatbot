# 🤖 PrologBot

A logic-driven emotional support chatbot that listens, tracks emotional intensity, and provides timely advice based on user input.

---

## 🎯 What Does It Do?
**PrologBot** is a supportive companion designed to detect emotional cues and situational keywords. It utilizes a unique **Counter System** to ensure it listens to the user fully before offering advice, striking a balance between empathy and actionable guidance.

---

## 🗣️ Key Emotion Detection
The bot monitors conversations for specific emotional triggers across five main categories:

| Category | Keywords |
| :--- | :--- |
| **Sad** | sad, lonely, depressed, hopeless, cry, awful, terrible, bad, heartbroken, hurt, pain, grief, down, upset, miserable |
| **Stressed** | stressed, anxious, overwhelmed, pressure, panic, worried, tense, nervous, frustrated, busy |
| **Angry** | angry, mad, furious, annoyed, irritated, rage, pissed |
| **Tired** | tired, exhausted, drained, sleepy, worn, weary |
| **Happy** | happy, great, excited, good, joyful, glad, awesome, fantastic, wonderful |

---

## 📍 Topic Keywords
PrologBot recognizes specific life situations to provide contextual empathy:

* 💔 **Breakup:** ex, boyfriend, girlfriend, partner, relationship, dumped, breakup, love.
* 📉 **Failure:** failed, fail, stupid, dumb, useless, mistake, exam, rejected.
* 😨 **Anxiety:** scared, anxious, panic, worry, afraid, fear.
* 👤 **Lonely:** lonely, alone, nobody, isolated, friends, ignored.

---

## ⚡ Key Triggers & Logic
The bot responds dynamically based on the following input triggers:

| You Say | Bot Action |
| :--- | :--- |
| `hello` / `hi` | Greets you and asks what's on your mind. |
| `my name is [X]` | Remembers and uses your name in conversation. |
| `sad` / `lonely` | Increments **Counter +1** and shows empathy. |
| `okay` / `yeah` | Checks counter; if **≥ 2**, triggers advice. |
| `nothing else` | Overrides counter and triggers advice immediately. |
| `thanks` | Sends a "You're welcome" response. |
| `[gibberish]` | Requests clarification ("I didn't understand"). |
| `bye` | Ends the session with a warm goodbye. |

---

## 🔢 The Brain: Counting System
To prevent the bot from giving unsolicited advice too early, it uses a simple threshold logic to ensure the user has "vented" enough.

### Logic Flow
1.  **Initialization:** `counter = 0`
2.  **Detection:** Every time an **Emotion Word** is detected, `counter` increases by 1.
3.  **Threshold:** Advice is unlocked once `counter >= 2`.
4.  **Delivery:** Advice is delivered when the user confirms with a "filler" word (e.g., "yeah") or signals they are finished.

### Example Interaction
> **User:** "I'm feeling really **sad**." `[Counter: 1]`
>
> **Bot:** "I'm so sorry to hear that. I'm here for you."
>
> **User:** "I'm just so **lonely** lately." `[Counter: 2]`
>
> **Bot:** "That sounds really tough to deal with alone."
>
> **User:** "Yeah..."
>
> **Bot:** 💡 **[ADVICE GIVEN]** "Sometimes reaching out to one person you trust can change your whole perspective..."

---

## 🛠️ Installation & Usage
1. Clone the repository.
2. Ensure you have a **Prolog** environment (like SWI-Prolog) installed.
3. Load the file:
   ```prolog
   consult('prologbot.pl').
