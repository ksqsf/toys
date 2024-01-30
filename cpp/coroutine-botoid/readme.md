# Botoid

Botoid is a telegram bot framework built on top of C++ coroutines. It can be a basis to implement very fancy program logic. For example, to track a conversation history, you can simply say:

```c++
Botoid<> sum_bot(Bot* bot, Message last_msg)
{
    last_msg = bot->reply(last_msg, "give me some integers?");
    int sum = 0;
    while (true) {
        auto msg = co_await bot->get_reply_to(last_msg);
        sum += std::stoi(msg["text"].get<std::string>());
        last_msg = bot->reply(msg, fmt("the sum so far is {}", sum));
    }
}

// Compare it to https://github.com/teloxide/teloxide?tab=readme-ov-file#dialogues-management
Botoid<> greet_bot(Bot* bot, Message last_msg)
{
    int chat_id = last_msg["chat"]["id"];
    
    bot->send_message(chat_id, "What's your name?");
    std::string name = (co_await bot->get_message_in_chat(chat_id))["text"];
    
    bot->send_message(chat_id, "What's your age?");
    std::string age = (co_await bot->get_message_in_chat(chat_id))["text"];
    
    bot->send_message(chat_id, "What's your location");
    std::string location = (co_await bot->get_message_in_chat(chat_id))["text"];
    
    bot->send_message(chat_id, fmt("Name: {}\nAge: {}\nLocation: {}\n", name, age, location));
}
```

Note that even though we are following a conversation, we remain in the same coroutine. This highlights that with coroutines we can organize the concurrent program logic as we wish. Also it's noteworthy that the `await` keyword is only added to Telegram-related operations, as opposed to I/O operations traditionally. In a complete implementation, I/O should be considered as well.

There are many more exciting possibilities:

```c++
// Generator
for (auto& msg : bot->in_the_same_thread_of(initial_msg)) {
    // handle follow-ups to a thread
}

// Persistence
Botoid<> restartable_bot(Bot* bot, std::string tag) {
    InternalStates& states = load_saved_data(tag);
    auto something = co_await bar();  // automatically saves states
}

// Conversation "fork"
Botoid<> forking_sum_bot(Bot* bot, Message last_msg)
    last_msg = bot->reply(last_msg, "give me some integers?");
    int sum = 0;
    while (true) {
        // 'fork' will create a new coroutine based on the current state (i.e. clone it).
        // The original 'sum_bot' only responds to a reply to the latest message sent by the bot.
        // 'forking_sum_bot' will responds to every message that was sent by the bot, and 
        // it will act as if newer messages weren't there.
        auto msg = co_await bot->fork(bot->get_reply_to(last_msg));  // Hypothetical
        sum += std::stoi(msg["text"].get<std::string>());
        last_msg = bot->reply(msg, fmt("the sum so far is {}", sum));
    }
}
```

The code is just for learning purposes. It doesn't handle exceptions, for example. It is perfectly functional, however.

## Build

Install `curlpp` and `nlohmann-json` first, then do:

```bash
cmake -S . -B build
cmake --build build

# run
./build/example
```

