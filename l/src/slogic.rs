//! Static logic encoded in the Rust typing system.
use std::fmt;

/// Traits of a proposition.
pub trait Prop {
    fn imply<S>(self, s: S) -> Imply<Self, S>
    where
        Self: Sized,
        S: Prop
    {
        Imply { antecedent: self, seccedent: s }
    }

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

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Imply<T, U>
where
    T: Prop,
    U: Prop
{
    antecedent: T,
    seccedent: U
}

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
