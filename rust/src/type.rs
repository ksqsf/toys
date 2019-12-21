#![recursion_limit="100000"]
use std::collections::VecDeque;

fn main() {
    println!("{}", N8::number());
    println!("{}", N24::number());
    println!("{}", <N24 as Sub<N8>>::Result::number());

    println!("{}", <N24 as Div<Five>>::Result::number());

    println!("{:?}", <(Zero, (One, (Two, (Three, ()))))>::to_vec());
    println!("{}", <Zero as Fib>::Result::number());
    println!("{}", <One as Fib>::Result::number());
    println!("{}", <Two as Fib>::Result::number());
    println!("{}", <Three as Fib>::Result::number());
    println!("{}", <Four as Fib>::Result::number());
    println!("{}", <Five as Fib>::Result::number());
    
    println!("{:?}", Rev::<<N12 as FibList>::Result>::to_vec());
    
    println!("{}", <Succ<Five> as Fact>::Result::number());
}

use std::marker::PhantomData;

struct Succ<T> {
    _marker: PhantomData<T>
}
struct Zero;
type One = Succ<Zero>;
type Two = Succ<One>;
type Three = Succ<Two>;
type Four = Succ<Three>;
type Five = Succ<Four>;
type N8 = <Five as Add<Three>>::Result;
type N12 = <Three as Mult<Four>>::Result;
type N24 = <Three as Mult<N8>>::Result;

trait Number {
    fn number() -> u32;
}
impl Number for Zero {
    fn number() -> u32 { 0 }
}
impl<T: Number> Number for Succ<T> {
    fn number() -> u32 { 1 + T::number() }
}

trait Prev {
    type Result;
}
impl Prev for Zero {
    type Result = Zero;
}
impl<T> Prev for Succ<T> {
    type Result = T;
}

trait Add<B> {
    type Result;
}
impl<B> Add<B> for Zero {
    type Result = B;
}
impl<B, A: Number + Add<B>> Add<B> for Succ<A> {
    type Result = Succ<<A as Add<B>>::Result>;
}

trait Mult<B> {
    type Result;
}
impl<B> Mult<B> for Zero {
    type Result = Zero;
}
impl<A, B, AB, Ans> Mult<B> for Succ<A>
where
    A: Mult<B, Result=AB>,
    AB: Add<B, Result=Ans>,
{
    type Result = Ans;
}

trait Sub<B> {
    type Result;
}
impl<B> Sub<B> for Zero {
    type Result = Zero;
}
impl<B, A: Sub<B>> Sub<Succ<B>> for Succ<A> {
    type Result = <A as Sub<B>>::Result;
}
impl<A> Sub<Zero> for Succ<A> {
    type Result = Succ<A>;
}

trait Div<D> {
    type Result;
}
impl<D> Div<D> for Zero {
    type Result = Zero;
}
impl<A: Number, D, ND, R> Div<D> for Succ<A>
where
    Succ<A>: Sub<D, Result=ND>,
    ND: Div<D, Result=R>,
{
    type Result = Succ<R>;
}

struct True;
struct False;
trait If<A, B> {
    type Result;
}
impl<A,B> If<A,B> for True {
    type Result = A;
}
impl<A,B> If<A,B> for False {
    type Result = B;
}

trait Car {
    type Result;
}
impl<A,B> Car for (A,B) {
    type Result = A;
}
trait Cdr {
    type Result;
}
impl<A,B> Cdr for (A,B) {
    type Result = B;
}
trait Length { type Result; }
impl Length for () {
    type Result = Zero;
}
impl<A,B: Length> Length for (A,B) {
    type Result = Succ<<B as Length>::Result>;
}
trait ToVec {
    fn to_vec() -> VecDeque<u32>;
}
impl ToVec for () {
    fn to_vec() -> VecDeque<u32> {
        VecDeque::new()
    }
}
impl<A:Number, B: ToVec> ToVec for (A,B) {
    fn to_vec() -> VecDeque<u32> {
        let mut v = B::to_vec();
        v.push_front(A::number());
        v
    }
}

trait Fib {
    type Result;
}
impl Fib for Zero {
    type Result = Zero;
}
impl Fib for Succ<Zero> {
    type Result = Succ<Zero>;
}
impl<A,FA,FB> Fib for Succ<Succ<A>>
where
    A: Fib<Result=FA>,
    Succ<A>: Fib<Result=FB>,
    FA: Add<FB>,
{
    type Result = <FA as Add<FB>>::Result;
}

trait FibList {
    type Result;
}
impl FibList for Zero {
    type Result = ();
}
impl<A,FA,LA> FibList for Succ<A>
where
    A: FibList<Result=LA> + Fib<Result=FA>,
{
    type Result = (FA, LA);
}

struct Rev<L> {
    _marker: PhantomData<L>
}
impl<L: ToVec> ToVec for Rev<L> {
    fn to_vec() -> VecDeque<u32> {
        L::to_vec().into_iter().rev().collect()
    }
}

trait IsZero {
    type Result;
}
impl IsZero for Zero {
    type Result = True;
}
impl<A> IsZero for Succ<A> {
    type Result = False;
}

trait Fact {
    type Result;
}
impl Fact for Zero {
    type Result = One;
}
impl<A: Number, FA, Ans> Fact for Succ<A>
where
    A: Fact<Result=FA>,
    FA: Mult<Succ<A>, Result=Ans>,
{
    type Result = Ans;
}
