// This is just an example program.
#include "botoid.hpp"
#include <cstdio>
#include <exception>
#include <iostream>
#include <string>
#include <fstream>
#include <format>

using namespace std::literals::string_literals;
using namespace botoid;

namespace botoid {
int counter = 0;
}

Botoid<> sum_bot(observer_ptr<Bot> bot, Message msg);
Botoid<> greet_bot(observer_ptr<Bot> bot, Message msg);
Botoid<> restartable_bot(observer_ptr<Bot> bot, Message last_msg);

int main()
{
    Bot bot{"6713492415:AAEUzeOoQa1jZZsgyFT5WqeUWga-rsGt06M"};
    bot.command("sum", sum_bot);
    bot.command("greet", greet_bot);
    bot.command("foo", restartable_bot);
    bot.run();
}

Botoid<> sum_bot(observer_ptr<Bot> bot, Message last_msg)
{
    try {
        last_msg = bot->reply(last_msg, "give me some integers?");
    } catch(std::exception& e) {
        std::cerr << "got some error: " << e.what() << "\n";
    }
    int sum = 0;
    while (true) {
        auto msg = co_await bot->get_reply_to(last_msg);
        try {
            int value = std::stoi(msg["text"].get<std::string>());
            sum += value;
        } catch(const std::invalid_argument& e) {
            try {
                std::string errmsg {e.what()};
                errmsg += " ";
                errmsg += msg["text"];
                last_msg = bot->reply(msg, errmsg);
            } catch (const std::exception& e) {
                std::cerr << "cannot send message: " << e.what() << "\n";
            }
            continue;
        }
        try {
            last_msg = bot->reply(msg, std::format("the sum so far is {}", sum));
        } catch(const std::exception& e) {
            std::cerr << "cannot send message: " << e.what() << "\n";
        }
    }
}

// Compare it to https://github.com/teloxide/teloxide?tab=readme-ov-file#dialogues-management
Botoid<> greet_bot(observer_ptr<Bot> bot, Message last_msg)
{
    int chat_id = last_msg["chat"]["id"];

    bot->sendMessage(chat_id, "What's your name?");
    std::string name = (co_await bot->get_message_in_chat(chat_id))["text"];

    bot->sendMessage(chat_id, "What's your age?");
    std::string age = (co_await bot->get_message_in_chat(chat_id))["text"];

    bot->sendMessage(chat_id, "What's your location");
    std::string location = (co_await bot->get_message_in_chat(chat_id))["text"];

    bot->sendMessage(chat_id, std::format("Name: {}\nAge: {}\nLocation: {}\n", name, age, location));
}

struct States {
    std::string tag;
    int sum;

    // Restore data from file
    States(std::string tag): tag(tag) {
        std::ifstream is(tag);
        if (is) {
            is >> sum;
            std::cerr << "states loaded for " << tag << "!\n";
        } else {
            std::cerr << "states initialized\n";
            sum = 0;
        }
    }

    ~States() {
        save();
    }

    void save() {
        std::ofstream os(tag);
        os << sum;
        std::cerr << "states saved for " << tag << "!\n";
    }
};

Botoid<> restartable_bot(observer_ptr<Bot> bot, Message last_msg)
try {
    // Restore states.
    States states(std::to_string(last_msg["chat"]["id"].get<int>()));
    try {
        last_msg = bot->reply(last_msg, "give me some integers?");
    } catch(std::exception& e) {
        std::cerr << "got some error: " << e.what() << "\n";
    }
    while (true) {
        auto msg = co_await bot->get_reply_to(last_msg);
        try {
            int value = std::stoi(msg["text"].get<std::string>());
            states.sum += value;
        } catch(const std::invalid_argument& e) {
            try {
                std::string errmsg {e.what()};
                errmsg += " ";
                errmsg += msg["text"];
                last_msg = bot->reply(msg, errmsg);
            } catch (const std::exception& e) {
                std::cerr << "cannot send message: " << e.what() << "\n";
            }
            continue;
        }
        try {
            last_msg = bot->reply(msg, std::format("the sum so far is {}", states.sum));
            // Only save states when the user is notified of the updated result.
            states.save();
        } catch(const std::exception& e) {
            std::cerr << "cannot send message: " << e.what() << "\n";
        }
    }
}
catch(std::exception &e) {
    std::cerr << "restartable_bot: " << e.what() << "\n";
}
