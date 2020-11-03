use std::ptr::{self, null_mut};
use std::sync::atomic::AtomicPtr;
use std::sync::atomic::Ordering::{Relaxed, Release, Acquire};

pub struct Stack<T> {
    head: AtomicPtr<Node<T>>
}

pub struct Node<T> {
    data: T,
    next: *mut Node<T>
}

impl<T> Stack<T> {
    pub fn new() -> Stack<T> {
        Stack {
            head: AtomicPtr::new(null_mut())
        }
    }

    pub fn pop(&self) -> Option<T> {
        loop {
            // take a snapshot
            // acquire: not to reorder before it
            let head = self.head.load(Acquire);

            // we observed the stack empty
            if head == null_mut() {
                return None
            } else {
                let next = unsafe { (*head).next };

                // if snapshot is still good, update from `head` to `next`
                // release: not to reorder after it
                if self.head.compare_and_swap(head, next, Release) == head {
                    // extract out the data from the now-unlinked node
                    let ret = Some(unsafe { ptr::read(&(*head).data) });

                    // NOTE: free the node -- WRONG
                    //
                    // could cause use-after-free because other
                    // threads could have snapshots of head!
                    //
                    // mem::drop(Box::from_raw(head));

                    // NOTE: leaks the node!
                    return ret
                }
            }
        }
    }

    pub fn push(&self, t: T) {
        // allocate the node and immediately turn it into a *mut pointer
        let n = Box::into_raw(Box::new(Node {
            data: t,
            next: null_mut()
        }));
        loop {
            // snapshot current head
            let head = self.head.load(Relaxed);

            // update `next` pointer with snapshot
            unsafe { (*n).next = head; }

            // if snapshot is still good, link in new node
            if self.head.compare_and_swap(head, n, Release) == head {
                break
            }
        }
    }
}

fn main() {
    
}
