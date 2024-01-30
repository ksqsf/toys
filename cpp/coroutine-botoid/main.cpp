// This is just an example program.
#include "botoid.hpp"
#include <cstdio>
#include <exception>
#include <iostream>
#include <string>

using namespace std::literals::string_literals;
using namespace botoid;

namespace botoid {
int counter = 0;
}

Botoid<> sum_bot(observer_ptr<Bot> bot, Message msg);
Botoid<> greet_bot(observer_ptr<Bot> bot, Message msg);

int main()
{
    Bot bot{"BOT TOKEN"};
    bot.command("sum", sum_bot);
    bot.command("greet", greet_bot);
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
            char buf[1024];
            snprintf(buf, sizeof(buf), "the sum so far is %d\n", sum);
            last_msg = bot->reply(msg, buf);
        } catch(const std::exception& e) {
            std::cerr << "cannot send message: " << e.what() << "\n";
        }
    }
}

Botoid<> greet_bot(observer_ptr<Bot> bot, Message last_msg)
{
    int chat_id = last_msg["chat"]["id"];

    bot->sendMessage(chat_id, "What's your name?");
    std::string name = (co_await bot->get_message_in_chat(chat_id))["text"];

    bot->sendMessage(chat_id, "What's your age?");
    std::string age = (co_await bot->get_message_in_chat(chat_id))["text"];

    bot->sendMessage(chat_id, "What's your location");
    std::string location = (co_await bot->get_message_in_chat(chat_id))["text"];

    char buf[1024];
    snprintf(buf, 1024, "Name: %s\nAge: %s\nLocation: %s", name.c_str(), age.c_str(), location.c_str());
    bot->sendMessage(chat_id, buf);
}
