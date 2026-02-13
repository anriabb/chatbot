                                                                                     🤖 PrologBot

## 🎯 **What Does It Do?**

Emotional support chatbot that listens and gives advice.

---

## 🗣️ **Key Emotion Words (What Bot Detects)**

### **Sad (18 words)**
sad, lonely, depressed, hopeless, cry, awful, terrible, bad, heartbroken, hurt, pain, grief, down, upset, miserable

### **Stressed (14 words)**
stressed, anxious, overwhelmed, pressure, panic, worried, tense, nervous, frustrated, busy

### **Angry (10 words)**
angry, mad, furious, annoyed, irritated, rage, pissed

### **Tired (7 words)**
tired, exhausted, drained, sleepy, worn, weary

### **Happy (13 words)**
happy, great, excited, good, joyful, glad, awesome, fantastic, wonderful

---

## 📍 **Topic Keywords (Specific Situations)**

### **Breakup**
ex, boyfriend, girlfriend, partner, relationship, dumped, left, broke, breakup, love, heart, girl, boy

### **Failure**
failed, fail, stupid, dumb, useless, mistake, exam, rejected

### **Anxiety**
scared, anxious, panic, worry, afraid, fear

### **Lonely**
lonely, alone, nobody, isolated, friends, ignored

---

## ⚡ **Key Triggers (What Makes Bot Respond)**

| **You Say** | **Bot Does** |
|-------------|--------------|
| hello / hi / hey | Greets you + asks what's on your mind |
| my name is [X] | Remembers your name |
| sad / lonely / depressed | Count +1, shows empathy |
| okay / yeah / yes | Checks count → gives advice if ≥2 |
| nothing else | Triggers advice immediately |
| thanks / thank you | Says you're welcome |
| bsks / cdc (gibberish) | "I didn't understand" |
| bye | Says goodbye |

---

## 🔢 **Counting System (The Brain)**

### **How Counter Works:**

```
START: counter = 0

User says "sad" 
→ counter = 1 
→ Bot: empathy only

User says "lonely" 
→ counter = 2 
→ Bot: ready to give advice

User says "okay" 
→ check: counter ≥ 2? YES! 
→ Bot: 💬 quote + 💡 advice
```

### **Visual Example:**

```
Turn 1: "I'm sad"          [Counter: 0→1] ❌ No advice yet
Turn 2: "about a girl"     [Counter: 1→2] ✅ Ready!
Turn 3: "yeah"             [Counter: 2]   ✅ ADVICE GIVEN!
```

---
