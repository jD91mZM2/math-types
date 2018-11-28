use std::{
    cmp::{self, Ordering},
    fmt,
    iter,
    mem,
    ops::{Add, AddAssign, Div, DivAssign, Mul, MulAssign, Sub, SubAssign},
    str::FromStr
};

const SMALL_DENOMINATOR: i32 = -10_000;
const BIG_DENOMINATOR: i32 = 10_000;

/// A number stored in fraction form instead of actually calculating the
/// result. This ensures (10/3) * 3 is actually 10 and not 9.99998.
#[derive(Clone, Copy, PartialEq, Eq, Hash, Ord)]
pub struct Fraction {
    numerator: i32,
    denominator: i32
}
impl Default for Fraction {
    fn default() -> Self {
        Self::new(0, 1)
    }
}
impl Fraction {
    /// Construct a new fraction instance
    ///
    /// ## Panics
    /// Panics if the denominator is 0
    pub fn new(numerator: i32, denominator: i32) -> Self {
        assert_ne!(denominator, 0, "denominator is 0");
        Self { numerator, denominator }
    }
    /// Return the numerator
    pub fn numerator(self) -> i32 {
        self.numerator
    }
    /// Return the denominator
    pub fn denominator(self) -> i32 {
        self.denominator
    }
    /// Change the denominator and scale the numerator to reflect the changes.
    /// Note that this fraction type only works with whole numbers so some
    /// denominators may cause issues.
    pub fn with_denominator(mut self, denominator: i32) -> Self {
        assert_ne!(denominator, 0, "denominator is 0");
        let scale = denominator / self.denominator;
        self.numerator *= scale;
        self.denominator = denominator;
        self
    }
    /// Find a common denominator with other. Note that this does NOT find the
    /// lowest possible denominator. Instead you should call `simplify`
    /// afterwards if you need a small denominator.
    pub fn common_denominator(mut self, mut other: Self) -> (Self, Self) {
        let denominator = if self.denominator == other.denominator {
            self.denominator
        } else {
            if self.denominator >= BIG_DENOMINATOR || self.denominator <= SMALL_DENOMINATOR
                    || other.denominator >= BIG_DENOMINATOR || other.denominator <= SMALL_DENOMINATOR {
                self = self.simplify();
                other = other.simplify();
            }
            self.denominator * other.denominator
        };
        (self.with_denominator(denominator), other.with_denominator(denominator))
    }
    /// Find the lowest possible denominator for fraction
    pub fn simplify(mut self) -> Self {
        while self.numerator % 2 == 0 && self.denominator % 2 == 0 {
            self.numerator /= 2;
            self.denominator /= 2;
        }
        let upper = cmp::max(self.numerator, self.denominator);
        // Biggest first
        //for i in iter::once(self.denominator)
        //            .chain((3..=(self.denominator as f32).sqrt().ceil() as i32).rev().step_by(2)) {
        for i in iter::once(self.denominator).chain((0..=upper-3).map(|i| upper - i)) {
            if self.numerator % i == 0 && self.denominator % i == 0 {
                self.numerator /= i;
                self.denominator /= i;
                break;
            }
        }
        self
    }
    /// Same thing as in mathematics taking the power of -1:
    /// In this case also 1/n.
    ///
    /// ## Panics
    /// Panics if the numerator (new denominator) is 0
    pub fn invert(mut self) -> Self {
        mem::swap(&mut self.numerator, &mut self.denominator);
        assert_ne!(self.denominator, 0, "new denominator is 0");
        self
    }
    /// Returns true if this fraction is 1. This differs from comparison with
    /// 1/1 in that there is no need to simplify first.
    pub fn is_one(self) -> bool {
        self.numerator == self.denominator
    }
    /// Calculates the decimal result of this fraction
    pub fn decimal(&self) -> f64 {
        self.numerator as f64 / self.denominator as f64
    }
}
impl PartialOrd for Fraction {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        let (me, other) = self.common_denominator(*other);
        me.numerator.partial_cmp(&other.numerator)
    }
}
impl fmt::Display for Fraction {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}/{}", self.numerator, self.denominator)
    }
}
impl fmt::Debug for Fraction {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fmt::Display::fmt(&self.simplify(), f)
    }
}
macro_rules! impl_from {
    ($($int:ident),*) => {
        $(impl From<$int> for Fraction {
            fn from(i: $int) -> Self {
                Self {
                    numerator: i as _,
                    denominator: 1
                }
            }
        })*
    }
}
impl_from!(u8, u16, i8, i16, i32);

impl<'a, T: Copy + Into<Fraction>> From<&'a T> for Fraction {
    fn from(other: &'a T) -> Self {
        (*other).into()
    }
}

impl<T: Into<Fraction>> Mul<T> for Fraction {
    type Output = Fraction;
    fn mul(mut self, other: T) -> Self {
        let mut other = other.into();
        if self.denominator >= BIG_DENOMINATOR || self.denominator <= SMALL_DENOMINATOR
                || other.denominator >= BIG_DENOMINATOR || other.denominator <= SMALL_DENOMINATOR {
            self = self.simplify();
            other = other.simplify();
        }
        Self {
            numerator: self.numerator * other.numerator,
            denominator: self.denominator * other.denominator
        }
    }
}
impl<T: Into<Fraction>> Div<T> for Fraction {
    type Output = Fraction;
    fn div(self, other: T) -> Self {
        self * other.into().invert()
    }
}
macro_rules! impl_op {
    ($($trait:ident $fn:ident = ($op:tt)),* --- $($trait_assign:ident $fn_assign:ident = ($op_assign:tt)),*) => {
        $(impl<T: Into<Fraction>> $trait<T> for Fraction {
            type Output = Self;
            fn $fn(self, other: T) -> Self {
                let (mut me, other) = self.common_denominator(other.into());
                me.numerator $op other.numerator;
                me
            }
        })*
        $(impl<T: Into<Fraction>> $trait_assign<T> for Fraction {
            fn $fn_assign(&mut self, other: T) {
                *self = *self $op_assign other;
            }
        })*
    }
}
impl_op! {
    Add add = (+=),
    Sub sub = (-=)
    ---
    AddAssign add_assign = (+),
    SubAssign sub_assign = (-),
    MulAssign mul_assign = (*),
    DivAssign div_assign = (/)
}

impl FromStr for Fraction {
    type Err = ::std::num::ParseIntError;
    fn from_str(input: &str) -> Result<Self, Self::Err> {
        let mut parts = input.splitn(2, '/');
        let mut numerator = parts.next().unwrap_or_default().parse()?;
        let mut denominator = parts.next().map(|part| part.parse()).unwrap_or(Ok(1))?;
        if denominator == 0 {
            numerator = 0;
            denominator = 1;
        }
        Ok(Self {
            numerator,
            denominator
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn add() {
        assert_eq!((Fraction::new(3, 5) + Fraction::new(4, 10)).simplify(), Fraction::new(1, 1));
        for i in 0..25 {
            for j in 1..25 {
                for k in 0..25 {
                    for l in 1..25 {
                        let x = (Fraction::new(i, j) + Fraction::new(k, l)).decimal();
                        let y = (i as f64/j as f64) + (k as f64/l as f64);
                        assert!(x - y <= 1.0, "not the same thing: {} and {}", x, y);
                    }
                }
            }
        }
    }
    #[test]
    fn sub() {
        assert_eq!((Fraction::new(3, 5) - Fraction::new(4, 10)).simplify(), Fraction::new(1, 5));
        assert_eq!((Fraction::new(-1, 2) - Fraction::new(-1, 4)).simplify(), Fraction::new(-1, 4));
        for i in 0..25 {
            for j in 1..25 {
                for k in 0..25 {
                    for l in 1..25 {
                        let x = (Fraction::new(i, j) - Fraction::new(k, l)).decimal();
                        let y = (i as f64/j as f64) - (k as f64/l as f64);
                        assert!(x - y <= 1.0, "not the same thing: {} and {}", x, y);
                    }
                }
            }
        }
    }
    #[test]
    fn mul() {
        assert_eq!((Fraction::new(3, 5) * Fraction::new(4, 10)).simplify(), Fraction::new(6, 25));
        assert_eq!((Fraction::new(10, 3) * 3).simplify(), Fraction::new(10, 1));
        for i in 0..25 {
            for j in 1..25 {
                for k in 0..25 {
                    for l in 1..25 {
                        let x = (Fraction::new(i, j) * Fraction::new(k, l)).decimal();
                        let y = (i as f64/j as f64) * (k as f64/l as f64);
                        assert!(x - y <= 1.0, "not the same thing: {} and {}", x, y);
                    }
                }
            }
        }
    }
    #[test]
    fn div() {
        assert_eq!((Fraction::new(3, 5) / Fraction::new(2, 10)).simplify(), Fraction::new(3, 1));
        for i in 0..25 {
            for j in 1..25 {
                for k in 1..25 {
                    for l in 1..25 {
                        let x = (Fraction::new(i, j) / Fraction::new(k, l)).decimal();
                        let y = (i as f64/j as f64) / (k as f64/l as f64);
                        assert!(x - y <= 1.0, "not the same thing: {} and {}", x, y);
                    }
                }
            }
        }
    }
    #[test]
    fn simplify() {
        assert_eq!(Fraction::new(81, 54).simplify(), Fraction::new(3, 2));
    }
}
