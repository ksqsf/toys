use std::sync::atomic::Ordering::{Acquire, Release, Relexed};
use std::ptr;

use crossbeam::mem::epoch::{self, Atomic, Owned};

pub struct TreiberStack<T> {
    head: Atomic<Node<T>>
}

struct Node<T> {
    data: T,
    next: Atomic<Node<T>>
}

impl<T> TreiberStack<T> {
    pub fn new() -> TreiberStack<T> {
        TreiberStack {
            head: Atomic::new()
        }
    }

    pub fn push(&self, t: T) {
        // allocate the node via Owned
        let mut n = Owned::new(Node {
            data: t,
            next: Atomic::new()
        });

        // become active
        let guard = epoch::pin();

        loop {
            // snapshot the current head
            let head = self.head.load(Relaxed, &guard);

            // update `next` pointer with snapshot
            n.next.store_shared(head, Relaxed);

            // if snapshot is still good, link in the new node
            match self.head.cas_and_ref(head, n, Release, &guard) {
                Ok(_) => return,
                Err(owned) => n = owned,
            }
        }
    }

    pub fn pop(&self) -> Option<T> {
        let guard = epoch::pin();

        loop {
            match self.head.load(Relaxed, &guard) {
                None => return None,
                Some(head) => {
                    let next = head.next.load(Relaxed, &guard);
                    if self.head.cas_shared(Some(head), next, Release) {
                        unsafe {
                            guard.unlinked(head);
                            return Some(ptr::read(&(*head).data))
                        }
                    }
                }
            }
        }
    }
}
