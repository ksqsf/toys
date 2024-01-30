#pragma once

#include <list>
#include <coroutine>
#include <string>
#include <filesystem>
#include <regex>
#include <nlohmann/json.hpp>
#include <string_view>
#include <unordered_map>
#include <iostream>
#include <memory>

namespace botoid {

template<typename T>
using observer_ptr = T*;

class Bot;
// class Botoid;
using Message = nlohmann::json;
using Update = nlohmann::json;

extern int counter;

enum class TriggerResult {
    Continue,
    Stop,
    ContinueAndComplete,
    StopAndComplete,
};

/// The coroutine type for bot
template<typename YieldType = Message>
class Botoid {
public:
    struct promise_type;
    using handle_type = std::coroutine_handle<promise_type>;
    int id = 0;
    handle_type handle;
    std::coroutine_handle<> parent_;
    std::optional<YieldType> value_;
    
    // Default init: no coroutine frame.
    Botoid(): id(counter++) {
        std::cerr << "new botoid id=" << id << "\n";
    }
    Botoid(handle_type handle) : id(counter++), handle(handle) {
        std::cerr << "new botoid with handle id=" << id << "\n";
    }

    Botoid(const Botoid &) = delete;
    
    Botoid &operator=(const Botoid &) = delete;
    
    Botoid(Botoid&& rhs) : id(rhs.id), handle(rhs.handle), parent_(rhs.parent_), value_(rhs.value_) {
        rhs.handle = nullptr;
        rhs.id = -1;
        rhs.parent_ = {};
        rhs.value_ = {};
    }
    
    Botoid& operator=(Botoid&& rhs) {
        if (handle)
            handle.destroy();
        handle = rhs.handle;
        rhs.handle = nullptr;
        return *this;
    }
    
    ~Botoid() {
        if (id>=0)
            std::cerr << "botoid id=" << id << " died\n";
        if (handle) {
            handle.destroy();
            std::cerr << "   ... handle destroyed\n";
        }
    }
    
    void set_value(YieldType value) {
        std::cerr << "botoid id " << id << " got value\n";
        value_ = value;
    }
    
    // The 'waiter' is waiting for me.
    // e.g., in `co_await get_reply_to()`, 
    // `get_reply_to` creates a Botoid
    // `co_await` will pass the current continuation here.
    //
    // `void` means transfer control to the last resumer.
    void await_suspend(std::coroutine_handle<> waiter) {
        std::cerr << "botoid id " << id <<  " await_suspend\n";
        parent_ = waiter;
    }
    
    // If the value_ is already ready, don't suspend.
    bool await_ready() {
        return (bool) value_;
    }
    
    // `co_await` returns (called when parent_ is resumed from co_await).
    YieldType await_resume() {
        return value_.value();
    }
    
    struct promise_type {
        Botoid get_return_object() {
            auto handle = Botoid::handle_type::from_promise(*this);
            return Botoid{handle};
        }
        
        std::suspend_never initial_suspend() { return {}; }
        
        std::suspend_always final_suspend() noexcept { return {}; }
        
        void unhandled_exception() {}
    };
};

/// Manages the lifecycle of a telegram bot.
///
/// Events may cause a botoid to create, resume, or die.
class Bot {
public:
    Bot(const std::string& token): token_(token) {}
    
    ~Bot() {}
    
    /// Start the bot and listen to telegram events.
    void run();
    
    /// Enable persistent states.
    ///
    /// If the directory already contains states, also restore from it.
    void enablePersistency(std::filesystem::path& stateDir);
    
    /// Register a botoid trigger from a regular expression.
    void regex(const std::regex& re, std::function<Botoid<>(observer_ptr<Bot>, Message)> fac);
    
    /// Register a command trigger.
    void command(const std::string& cmd, std::function<Botoid<>(observer_ptr<Bot>, Message)> fac);
    
private:
    std::string token_;
    unsigned long nextUpdateId_ = 0;
    std::list<std::function<TriggerResult(Update& update)>> triggers_;
    std::unordered_map<int, std::unique_ptr<Botoid<>>> botoids_;
    void trigger(nlohmann::json& update);
    
public: // bot API
    nlohmann::json makeRequest(const std::string& methodName, const nlohmann::json& body);
    std::vector<nlohmann::json> getUpdates();
    nlohmann::json sendMessage(int chat_id, const std::string& text) {
        return makeRequest("sendMessage", {
                {"chat_id", chat_id},
                {"text", text}
            });
    }
    nlohmann::json reply(const Message& msg, const std::string &text) {
        return makeRequest("sendMessage", {
                {"chat_id", msg["chat"]["id"]},
                {"text", text},
                {"reply_parameters", {{"message_id", msg["message_id"]}}}
            });
    }
    
public: // botoid special API
    Botoid<Message>& get_first_msg(std::function<bool(const Message&)> f);
    Botoid<Message>& get_reply_to(const Message& msg);
    Botoid<Message>& get_message_in_chat(int chat_id);
};

} // namespace telec
