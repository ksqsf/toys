#include <cstdio>
#include <atomic>
#include <memory>
#include <algorithm>
#include <thread>
#include <vector>
using namespace std;

constexpr auto rlx = memory_order_relaxed;
constexpr auto acq = memory_order_acquire;
constexpr auto rel = memory_order_release;

constexpr bool nonpreemptive = false;
constexpr int n_readers = 4;
constexpr int n_writers = 1;
constexpr int max_threads = std::max(n_readers, n_writers);
atomic<int> n_threads {0};

struct Node {
    Node* next;
    Node* prev;
} *gp = nullptr;

thread_local int tid;

template<class T>
T rcu_deref(T* ptr) {
    T val = *ptr;
    atomic_thread_fence(acq); // FIXME
    return val;
}

template<class T>
void rcu_assign(T* ptr, T val) {
    atomic_thread_fence(rel); // FIXME
    *ptr = val;
}

void rcu_read_lock() {
    if constexpr(nonpreemptive) {
        return;
    }
    // TODO
}

void rcu_read_unlock() {
    if constexpr(nonpreemptive) {
        return;
    }
    // TODO
}

void rcu_sync() {
}

thread spawn() {
    return thread([] {
        tid = n_threads.fetch_add(1, rlx);
        if (!rcu_deref(&gp)) {
            printf("no\n");
        }
        printf("%d\n", tid);
    });
}

int main() {
    vector<thread> ts;
    for (int i = 0; i < max_threads; ++i) {
        ts.push_back(spawn());
    }
    for (auto& t : ts) {
        t.join();
    }
}
