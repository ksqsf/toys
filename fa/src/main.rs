type StateId = usize;

/// A struct representing a state.  However, different from a "pure"
/// state, this struct also maintains how states are transitioned,
/// ie. the transition function is embedded here.
struct State {
    accept: bool,
    transitions: Vec<(char, StateId)>,
}

impl State {
    fn new(accept: bool) -> State {
        State { accept, transitions: vec![] }
    }

    fn with_transitions(accept: bool, transitions: Vec<(char, StateId)>) -> State {
        State { accept, transitions }
    }

    /// Try to transition to the next state according to input and the
    /// transition function.  If no transition is defined, 0 is
    /// returned.
    fn transition(&self, c: char) -> StateId {
        // FIXME: linear time
        self.transitions.iter().find(|(x, _)| c == *x)
            .map_or(0, |(_, id)| *id)
    }
}

/// A struct representing a DFA.
struct DFA<'s> {
    state: StateId,
    states: &'s Vec<State>,
}

impl<'s> DFA<'s> {
    fn new(initial_state: StateId, states: &'s Vec<State>) -> DFA<'s> {
        DFA { state: initial_state, states }
    }

    /// Run DFA on input and check if it's accepted.
    ///
    /// The state of the DFA is modified.
    fn run(&mut self, s: &str) -> bool {
        let mut cur = self.state;
        for c in s.chars() {
            cur = self.states[cur].transition(c);
        }
        self.state = cur;
        self.states[cur].accept
    }

    /// Run DFA on input and check if it's accepted.
    ///
    /// The state of the DFA is not modified.
    fn check(&self, s: &str) -> bool {
        let mut cur = self.state;
        for c in s.chars() {
            cur = self.states[cur].transition(c);
        }
        self.states[cur].accept
    }
}

#[cfg(test)]
#[test]
fn test_simple() {
    let states = vec![
        State::new(false),
        State::with_transitions(false, vec![('a', 2), ('b', 1)]),
        State::with_transitions(false, vec![('b', 3)]),
        State::new(true),
    ];
    let mut dfa = DFA::new(1, &states);
    assert_eq!(true, dfa.check("ab"));
    assert_eq!(false, dfa.check("abcdefg"));
    assert_eq!(false, dfa.check("b"));
    assert_eq!(false, dfa.run("b"));
    assert_eq!(true, dfa.run("bbab"));
    assert_eq!(false, dfa.run("ab"));
}

fn main() -> std::io::Result<()> {
    use std::io;
    use std::io::{BufRead, BufReader};

    let states = vec![
        State::new(false),
        State::with_transitions(false, vec![('a', 2), ('b', 1)]),
        State::with_transitions(false, vec![('b', 3)]),
        State::new(true),
    ];
    let dfa = DFA::new(1, &states);

    let mut reader = BufReader::new(io::stdin());
    let mut buf = String::new();
    loop {
        buf.clear();
        reader.read_line(&mut buf)?;
        if buf.len() == 0 {
            break
        }
        let s = &buf[0..buf.len()-1];
        println!("'{}' accepted? {}", s, dfa.check(s));
    }

    Ok(())
}
