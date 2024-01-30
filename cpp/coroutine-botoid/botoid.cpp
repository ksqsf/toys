#include "botoid.hpp"
#include <curlpp/cURLpp.hpp>
#include <curlpp/Easy.hpp>
#include <curlpp/Options.hpp>
#include <sstream>
#include <iostream>
#include <stdexcept>

using namespace std::literals::string_literals;

namespace botoid {

void Bot::run()
{
    while (true) {
        try {
            auto updates = getUpdates();
            for (auto& update : updates) {
                std::cerr << "Processing update " << update["update_id"] << std::endl;
                nextUpdateId_ = std::max(update["update_id"].get<unsigned long>() + 1, nextUpdateId_);
                trigger(update);
            }
        } catch (std::exception& e) {
            std::cerr << e.what() << std::endl;
        }
    }
}

nlohmann::json Bot::makeRequest(const std::string& methodName, const nlohmann::json& body)
try {
    std::string api = "https://api.telegram.org/bot"s + token_ + "/"s + methodName;
    std::cerr << "Making request to " << methodName << " with body " << body.dump(2) << std::endl;
    curlpp::Easy request;
    std::list<std::string> headers {"Content-Type: application/json"};
    std::ostringstream responseStream;
    auto bodyStr = body.dump();
    request.setOpt(curlpp::options::Url(api));
    request.setOpt(new curlpp::options::HttpHeader(headers));
    request.setOpt(new curlpp::options::PostFields(bodyStr));
    request.setOpt(new curlpp::options::PostFieldSize(static_cast<long>(bodyStr.length())));
    request.setOpt(new curlpp::options::WriteStream(&responseStream));
    request.perform();
    std::string responseStr = responseStream.str();
    nlohmann::json j = nlohmann::json::parse(responseStr);
    if (!j["ok"]) {
        throw std::runtime_error("telegram error: "s + j["description"].get<std::string>());
    }
    return j["result"];
} catch (curlpp::RuntimeError& e) {
    throw std::runtime_error("net error: "s + e.what());
} catch (curlpp::LogicError& e) {
    throw std::logic_error("net error: "s + e.what());
} catch (nlohmann::json::parse_error& e) {
    throw std::runtime_error("parse error: "s + e.what());
}

std::vector<nlohmann::json> Bot::getUpdates()
{
    return makeRequest("getUpdates", {{"timeout", 60}, {"offset", nextUpdateId_}});
}

void Bot::command(const std::string& cmd, std::function<Botoid<>(observer_ptr<Bot>, Message)> fac) {
    regex(std::regex{"/"s + cmd}, fac);
}

void Bot::regex(const std::regex& re, std::function<Botoid<>(observer_ptr<Bot>, Message)> fac)
{
    triggers_.emplace_front([=](const Update& update){
        auto msg = update["message"];
        const auto& msg_text = msg["text"].get_ref<const std::string&>();
        if (std::regex_search(msg_text, re)) {
            auto botoid = fac(this, msg);
            auto botoid_ptr = std::make_unique<Botoid<>>(std::move(botoid));
            botoids_.emplace(botoid_ptr->id, std::move(botoid_ptr));
            return TriggerResult::Stop;
        }
        return TriggerResult::Continue;
    });
}

void Bot::trigger(nlohmann::json& update)
{
    for (auto it = triggers_.begin(); it != triggers_.end(); ++it) {
        auto result = (*it)(update);
        if (result == TriggerResult::Stop) {
            return;
        } else if (result == TriggerResult::StopAndComplete) {
            triggers_.erase(it);
            return;
        } else if (result == TriggerResult::Continue) {
            continue;
        } else if (result == TriggerResult::ContinueAndComplete) {
            triggers_.erase(it);
            continue;
        }
    }
}

Botoid<Message>& Bot::get_first_msg(std::function<bool(const Message&)> f)
{
    auto botoid_ptr = std::make_unique<Botoid<>>();
    Botoid<>& botoid = *botoid_ptr;
    botoids_.emplace(botoid.id, std::move(botoid_ptr));
    triggers_.emplace_front([f, &botoid, this](const Update& update) {
        auto& new_msg = update["message"];
        if (f(new_msg)) {
            botoid.set_value(new_msg);
            botoid.parent_.resume();
            botoids_.erase(botoid.id);
            return TriggerResult::StopAndComplete;
        } else {
            return TriggerResult::Continue;
        }
    });
    return botoid;
}

Botoid<Message>& Bot::get_reply_to(const Message& awaited)
{
    auto awaited_id = awaited["message_id"];
    return get_first_msg([awaited_id](const Message& new_msg) {
        if (!new_msg.contains("reply_to_message"))
            return false;
        auto& replied = new_msg["reply_to_message"];
        return replied["message_id"] == awaited_id;
    });
}

Botoid<Message>& Bot::get_message_in_chat(int chat_id) {
    return get_first_msg([chat_id](const Message& new_msg) {
        int new_chat_id = new_msg["chat"]["id"];
        return new_chat_id == chat_id;
    });
}

}
