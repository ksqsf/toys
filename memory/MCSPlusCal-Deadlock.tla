----------------------------- MODULE MCSPlusCal -----------------------------

EXTENDS TLC, Sequences, Integers, FiniteSets

CONSTANT NULL

N == 2

(*
--algorithm MCS
variables 
    global = [ locked|->FALSE, next|->NULL ]; 
    local  = [ proc \in ProcSet |-> [ wait|->TRUE, next|->NULL ] ];

\* Each process executes: while(true) { lock(); unlock(); }
process Proc \in 1..N
  variable oldnext; oldlocked;
  begin
    start: while TRUE
            do skip;
    \* Try to acquire the lock.
    acquire: skip;
       a1: \* FAS. enqueue self.
           oldnext := global.next;
           global.next := self;
       a2: \* FAS. announce locked.
           oldlocked := global.locked;
           global.locked := TRUE;
       a3: \* set up local info.
           local[self] := [ wait|->oldlocked, next|->oldnext ];
       a4: \* set oldnext's next.
           if oldnext # NULL
            then a5: local[oldnext].next := self;
           end if;
       a6: \* wait for the lock.
           await local[self].wait = FALSE;
    \* release the lock.
    release: skip;
       r1: oldnext := local[self].next;
       r2: \* reset local lock
           local[self] := [ wait|->TRUE, next|->NULL ];
       r3: if oldnext # NULL
            then \* transfer the lock to oldnext
                 r4: local[oldnext].wait := FALSE;
            else \* release the whole 
                 r5: global.locked := FALSE;
           end if;
   end while;
end process
end algorithm
*)
\* BEGIN TRANSLATION (chksum(pcal) = "b92d7587" /\ chksum(tla) = "e1f060bb")
CONSTANT defaultInitValue
VARIABLES global, local, pc, oldnext, oldlocked

vars == << global, local, pc, oldnext, oldlocked >>

ProcSet == (1..N)

Init == (* Global variables *)
        /\ global = [ locked|->FALSE, next|->NULL ]
        /\ local = [ proc \in ProcSet |-> [ wait|->TRUE, next|->NULL ] ]
        (* Process Proc *)
        /\ oldnext = [self \in 1..N |-> defaultInitValue]
        /\ oldlocked = [self \in 1..N |-> defaultInitValue]
        /\ pc = [self \in ProcSet |-> "start"]

start(self) == /\ pc[self] = "start"
               /\ TRUE
               /\ pc' = [pc EXCEPT ![self] = "acquire"]
               /\ UNCHANGED << global, local, oldnext, oldlocked >>

acquire(self) == /\ pc[self] = "acquire"
                 /\ TRUE
                 /\ pc' = [pc EXCEPT ![self] = "a1"]
                 /\ UNCHANGED << global, local, oldnext, oldlocked >>

a1(self) == /\ pc[self] = "a1"
            /\ oldnext' = [oldnext EXCEPT ![self] = global.next]
            /\ global' = [global EXCEPT !.next = self]
            /\ pc' = [pc EXCEPT ![self] = "a2"]
            /\ UNCHANGED << local, oldlocked >>

a2(self) == /\ pc[self] = "a2"
            /\ oldlocked' = [oldlocked EXCEPT ![self] = global.locked]
            /\ global' = [global EXCEPT !.locked = TRUE]
            /\ pc' = [pc EXCEPT ![self] = "a3"]
            /\ UNCHANGED << local, oldnext >>

a3(self) == /\ pc[self] = "a3"
            /\ local' = [local EXCEPT ![self] = [ wait|->oldlocked[self], next|->oldnext[self] ]]
            /\ pc' = [pc EXCEPT ![self] = "a4"]
            /\ UNCHANGED << global, oldnext, oldlocked >>

a4(self) == /\ pc[self] = "a4"
            /\ IF oldnext[self] # NULL
                  THEN /\ pc' = [pc EXCEPT ![self] = "a5"]
                  ELSE /\ pc' = [pc EXCEPT ![self] = "a6"]
            /\ UNCHANGED << global, local, oldnext, oldlocked >>

a5(self) == /\ pc[self] = "a5"
            /\ local' = [local EXCEPT ![oldnext[self]].next = self]
            /\ pc' = [pc EXCEPT ![self] = "a6"]
            /\ UNCHANGED << global, oldnext, oldlocked >>

a6(self) == /\ pc[self] = "a6"
            /\ local[self].wait = FALSE
            /\ pc' = [pc EXCEPT ![self] = "release"]
            /\ UNCHANGED << global, local, oldnext, oldlocked >>

release(self) == /\ pc[self] = "release"
                 /\ TRUE
                 /\ pc' = [pc EXCEPT ![self] = "r1"]
                 /\ UNCHANGED << global, local, oldnext, oldlocked >>

r1(self) == /\ pc[self] = "r1"
            /\ oldnext' = [oldnext EXCEPT ![self] = local[self].next]
            /\ pc' = [pc EXCEPT ![self] = "r2"]
            /\ UNCHANGED << global, local, oldlocked >>

r2(self) == /\ pc[self] = "r2"
            /\ local' = [local EXCEPT ![self] = [ wait|->TRUE, next|->NULL ]]
            /\ pc' = [pc EXCEPT ![self] = "r3"]
            /\ UNCHANGED << global, oldnext, oldlocked >>

r3(self) == /\ pc[self] = "r3"
            /\ IF oldnext[self] # NULL
                  THEN /\ pc' = [pc EXCEPT ![self] = "r4"]
                  ELSE /\ pc' = [pc EXCEPT ![self] = "r5"]
            /\ UNCHANGED << global, local, oldnext, oldlocked >>

r4(self) == /\ pc[self] = "r4"
            /\ local' = [local EXCEPT ![oldnext[self]].wait = FALSE]
            /\ pc' = [pc EXCEPT ![self] = "start"]
            /\ UNCHANGED << global, oldnext, oldlocked >>

r5(self) == /\ pc[self] = "r5"
            /\ global' = [global EXCEPT !.locked = FALSE]
            /\ pc' = [pc EXCEPT ![self] = "start"]
            /\ UNCHANGED << local, oldnext, oldlocked >>

Proc(self) == start(self) \/ acquire(self) \/ a1(self) \/ a2(self)
                 \/ a3(self) \/ a4(self) \/ a5(self) \/ a6(self)
                 \/ release(self) \/ r1(self) \/ r2(self) \/ r3(self)
                 \/ r4(self) \/ r5(self)

Next == (\E self \in 1..N: Proc(self))

Spec == Init /\ [][Next]_vars

\* END TRANSLATION 

LockHolders == { p \in ProcSet : local[p].wait = FALSE }
LockHoldersNum == Cardinality(LockHolders)

\* WeakSafety asserts there is no more than one lock holder.
\* Deadlock is weak-safe too.
WeakSafety == LockHoldersNum <= 1

\* StrongSafety rules out deadlock.
NoDeadLock == ~ global.locked => LockHoldersNum = 0

=============================================================================
