#include <thread>
#include <atomic>
#include <vector>
using namespace std;

constexpr int N = 3;
int cnt = 0;

struct MCS {
    atomic<MCS*> next;
    atomic<bool> locked;
} global = { .next { nullptr }, .locked { false } };

void worker()
{
    MCS local = { .next { nullptr }, .locked { true } };
    
    for (int i = 0; i < 10000; ++i) {
        //
        // lock
        //
        auto oldnext = global.next.exchange(&local);
        global.locked.store(true);
        local.locked = oldnext != nullptr;
        if (oldnext != nullptr) {
            oldnext->next.store(&local);
        }
        while (local.locked.load()) {
            __asm__("PAUSE");
        }
        
        //
        // cs
        //
        cnt += 1;
        printf("%d\n", cnt);
        
        //
        // unlock
        //
        oldnext = local.next.load();
        local.locked.store(true);
        local.next.store(nullptr);
        if (oldnext != nullptr) {
            oldnext->locked.store(false);
        } else {
            global.locked.store(false);
            global.next.store(nullptr);
        }
    }
}


int main()
{
    vector<thread> ts;
    for (int i = 0; i < N; ++i) {
        thread t(worker);
        ts.push_back(std::move(t));
    }
    for (auto& t : ts) {
        t.join();
    }
}
