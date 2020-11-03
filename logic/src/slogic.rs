//! Static logic encoded in the Rust typing system.
//!
//! This may seem cool at first, for that it doesn't do any dynamic
//! allocation, and there's no copying in constructing new
//! propositions.  However, it doesn't work at runtime.
//!
//! ```
//! let operators = Stack::new();
//! let operands = Stack::new();  // Don't know type actual type here!
//! 
//! match user_input() {
//!     Kind::Atom(n) => operands.push(Atom(n)),
//!     Kind::Imply => {
//!         let a = operands.pop(); // which type is a?
//!         let b = operands.pop(); // which type is b?
//!         operands.push(a.imply(b));
//!     }
//!     _ => unimplemented!()
//! }
//! ```
//!
//! OK, now that static approach is not feasible, let's turn to
//! a dynamic approach.

use std::fmt;

/// Traits of a proposition.
pub trait Prop {
    /// Construct a new implication whose antecedent is self and
    /// whose seccedent is `s`.
    fn imply<S>(self, s: S) -> Imply<Self, S>
    where
        Self: Sized,
        S: Prop
    {
        Imply { antecedent: self, seccedent: s }
    }

    /// Construct a new proposition with a negation in front of it.
    fn negate(self) -> Negate<Self>
    where
        Self : Sized
    {
        Negate { origin: self }
    }
}

/// An atom proposition.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Atom(pub u32);

/// Implication.
///
/// Instances can only be obtained from [`Prop::imply`][imply].
///
/// [imply]: trait.Prop.html#method.imply
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Imply<T, U>
where
    T: Prop,
    U: Prop
{
    antecedent: T,
    seccedent: U
}

/// Negation.
///
/// Instances can only be obtained from [`Prop::negate`][negate].
///
/// [negate]: trait.Prop.html#method.negate
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Negate<T>
where
    T: Prop
{
    origin: T
}

impl Prop for Atom {}
impl<T: Prop, U: Prop> Prop for Imply<T, U> {}
impl<T: Prop> Prop for Negate<T> {}

impl fmt::Display for Atom {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "x{}", self.0)
    }
}

impl<T, U> fmt::Display for Imply<T, U>
where
    T: Prop + fmt::Display,
    U: Prop + fmt::Display
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "({}→{})", self.antecedent, self.seccedent)
    }
}

impl<T> fmt::Display for Negate<T>
where
    T: Prop + fmt::Display
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "¬{}", self.origin)
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn simple() {
        let x1 = Atom(1);
        let x2 = Atom(2);
        let x = x1.clone().imply(x2.clone());
        let x = x.negate().imply(x1).imply(x2);
        assert_eq!("((¬(x1→x2)→x1)→x2)", x.to_string());
    }
}
