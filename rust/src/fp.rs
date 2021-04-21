// https://www.fpcomplete.com/blog/monads-gats-nightly-rust/

trait MonoFunctor {
    type Unwrapped;
    fn map<F>(self, f: F) -> Self
    where
        F: FnMut(Self::Unwrapped) -> Self::Unwrapped;
}

impl<A> MonoFunctor for Option<A> {
    type Unwrapped = A;
    // Note: f must be A->A, which is undesirable
    fn map<F: FnMut(A) -> A>(self, mut f: F) -> Option<A> {
        match self {
            Some(a) => Some(f(a)),
            None => None,
        }
    }
}

trait Functor {
    type Unwrapped;
    type Wrapped<B>: Functor;

    fn map<F, B>(self, f: F) -> Self::Wrapped<B>
    where
        F: FnMut(Self::Unwrapped) -> B;
}

impl<A> Functor for Option<A> {
    type Unwrapped = A;
    type Wrapped<B> = Option<B>; // Self::Wrapped 相当于 Option<_>

    fn map<F: FnMut(A) -> B, B>(self, mut f: F) -> Option<B> {
        match self {
            Some(x) => Some(f(x)),
            None => None,
        }
    }
}

trait Pointed: Functor {
    fn wrap<T>(t: T) -> Self::Wrapped<T>;
}

impl<A> Pointed for Option<A> {
    fn wrap<T>(t: T) -> Option<T> {
        Some(t)
    }
}

trait Applicative: Pointed {
    fn lift_a2<F, B, C>(self, b: Self::Wrapped<B>, f: F) -> Self::Wrapped<C>
    where
        F: FnMut(Self::Unwrapped, B) -> C;
}

impl<A> Applicative for Option<A> {
    fn lift_a2<F, B, C>(self, b: Self::Wrapped<B>, mut f: F) -> Self::Wrapped<C>
    where
        F: FnMut(Self::Unwrapped, B) -> C,
    {
        Some(f(self?, b?))
    }
}

trait Semigroup {
    fn append(self, rhs: Self) -> Self;
}

impl Semigroup for String {
    fn append(mut self, rhs: Self) -> Self {
        self += &rhs;
        self
    }
}

impl<T> Semigroup for Vec<T> {
    fn append(mut self, mut rhs: Self) -> Self {
        Vec::append(&mut self, &mut rhs);
        self
    }
}

impl Semigroup for () {
    fn append(self, (): ()) -> () {}
}

#[derive(PartialEq, Debug)]
enum Validation<A, E> {
    Ok(A),
    Err(E),
}

impl<A, E> Functor for Validation<A, E> {
    type Unwrapped = A;
    type Wrapped<B> = Validation<B, E>;

    fn map<F: FnMut(A) -> B, B>(self, mut f: F) -> Self::Wrapped<B> {
        match self {
            Validation::Ok(a) => Validation::Ok(f(a)),
            Validation::Err(e) => Validation::Err(e),
        }
    }
}

impl<A, E> Pointed for Validation<A, E> {
    fn wrap<T>(t: T) -> Validation<T, E> {
        Validation::Ok(t)
    }
}

impl<A, E: Semigroup> Applicative for Validation<A, E> {
    fn lift_a2<F, B, C>(self, b: Self::Wrapped<B>, mut f: F) -> Self::Wrapped<C>
    where
        F: FnMut(Self::Unwrapped, B) -> C,
    {
        match (self, b) {
            (Validation::Ok(a), Validation::Ok(b)) => Validation::Ok(f(a, b)),
            (Validation::Err(e), Validation::Ok(_)) => Validation::Err(e),
            (Validation::Ok(_), Validation::Err(e)) => Validation::Err(e),
            (Validation::Err(e1), Validation::Err(e2)) => Validation::Err(e1.append(e2)),
        }
    }
}

trait Monad: Applicative {
    fn bind<B, F>(self, f: F) -> Self::Wrapped<B>
    where
        F: FnMut(Self::Unwrapped) -> Self::Wrapped<B>;
}

impl<A> Monad for Option<A> {
    fn bind<B, F>(self, f: F) -> Option<B>
    where
        F: FnMut(A) -> Option<B>,
    {
        self.and_then(f)
    }
}

struct IdentityT<M>(M);

impl<M: Functor> Functor for IdentityT<M> {
    type Unwrapped = M::Unwrapped;
    type Wrapped<A> = IdentityT<M::Wrapped<A>>;

    fn map<F, B>(self, f: F) -> Self::Wrapped<B>
    where
        F: FnMut(M::Unwrapped) -> B,
    {
        IdentityT(self.0.map(f))
    }
}

impl<M: Pointed> Pointed for IdentityT<M> {
    fn wrap<T>(t: T) -> IdentityT<M::Wrapped<T>> {
        IdentityT(M::wrap(t))
    }
}

impl<M: Applicative> Applicative for IdentityT<M> {
    fn lift_a2<F, B, C>(self, b: Self::Wrapped<B>, f: F) -> Self::Wrapped<C>
    where
        F: FnMut(Self::Unwrapped, B) -> C,
    {
        IdentityT(self.0.lift_a2(b.0, f))
    }
}

impl<M: Monad> Monad for IdentityT<M> {
    fn bind<B, F>(self, mut f: F) -> Self::Wrapped<B>
    where
        F: FnMut(Self::Unwrapped) -> Self::Wrapped<B>,
    {
        IdentityT(self.0.bind(|x| f(x).0))
    }
}

trait MonadTrans {
    type Base: Monad;
    fn lift(base: Self::Base) -> Self;
}

impl<M: Monad> MonadTrans for IdentityT<M> {
    type Base = M;

    fn lift(base: M) -> Self {
        IdentityT(base)
    }
}
