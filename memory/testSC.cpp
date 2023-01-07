// This file is a test of Total Store Ordering, which is believed to
// be consistent with x86 processors.

#include <iostream>
#include <thread>
#include <condition_variable>
#include <atomic>

#include <pthread.h>
#include <mach/thread_policy.h>

constexpr int NEW = 0xdeadbeef;
constexpr int INITIAL = 0;

// Globals
volatile int x = INITIAL, y = INITIAL; // Volatile forces compiler to generate memory accesses.

bool checkStoreStore()
{
    x = INITIAL;
    y = INITIAL;
    
    volatile int result = 0xbad;
    
    std::thread t1([&](){
        std::atomic_thread_fence(std::memory_order_seq_cst);

        // W(x, NEW)
        x = NEW;
        // W(y, NEW)
        y = NEW;
    });
    std::thread t2([&](){
        std::atomic_thread_fence(std::memory_order_seq_cst);
        
        int r1, r2;
        // r1 <- R(y)
        L1: r1 = y;
        // if (r1 != NEW) goto L1;
        if (r1 != NEW) goto L1;
        // r2 <- R(x)
        r2 = x; // (1)
        result = r2; // (2) is ordered after (1) because of data dependence.

        std::atomic_thread_fence(std::memory_order_release); // [REL], (2) is ordered before [REL]
    });
    
    // Set affinity
    // set_affinity(t1, 1);
    // set_affinity(t1, 2);
    
    // Run
    t1.join();
    t2.join();
    
    // Read result
    while (result == 0xbad);
    std::atomic_thread_fence(std::memory_order_acquire); // [ACQ]
    // By now, [ACQ] syncs with [REL], and we should see (2). y is guaranteed to be NEW here.
    assert(y == NEW);
    // SC is violated if y = NEW but x = INITIAL.
    return x == INITIAL;
}

bool checkStoreLoad()
{
    x = INITIAL;
    y = INITIAL;
    
    std::thread t1([]{
        std::atomic_thread_fence(std::memory_order_seq_cst);
        y = NEW;
        x = NEW;
    });
    
    std::thread t2([]{
        std::atomic_thread_fence(std::memory_order_seq_cst);
        int r1, r2;
        L1: r1 = x;
        if (r1 == INITIAL) goto L1;
        r2 = y; // Can r2 read INITIAL?
        std::atomic_thread_fence(std::memory_order_seq_cst);
        result = r2;
    });
    
    t1.join();
    t2.join();
    
    
}

int main()
{
    int N = 1000;
    int n_StoreStore_violate = 0;
    for (int i = 0; i < N; ++i) {
        n_StoreStore_violate += checkStoreStore();
    }
    printf("Observed StoreStore reorder: %d\n", n_StoreStore_violate);
    
    int n_StoreLoad_violate = 0;
    for (int i = 0; i < N; ++i) {
        n_StoreLoad_violate += checkStoreLoad();
    }
    printf("Observed StoreLoad reorder: %d\n", n_StoreStore_violate);
}
