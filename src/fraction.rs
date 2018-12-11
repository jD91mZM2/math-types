use std::{
    cmp::Ordering,
    fmt,
    mem,
    ops::{Add, AddAssign, Div, DivAssign, Mul, MulAssign, Neg, Sub, SubAssign},
    str::FromStr
};

const SMALL_DENOMINATOR: i32 = -10_000;
const BIG_DENOMINATOR: i32 = 10_000;

/// Find the greatest common divisor of two numbers
pub fn gcd(mut x: u32, mut y: u32) -> u32 {
    // https://en.wikipedia.org/wiki/Greatest_common_divisor#Binary_method
    // https://en.wikipedia.org/wiki/Binary_GCD_algorithm
    let mut common = 0;

    // Otherwise this will loop forever since 0/2 is also even
    if x == 0 {
        return y;
    }
    if y == 0 {
        return x;
    }

    // gcd(4, 6) = gcd(2, 3) * 2
    while x % 2 == 0 && y % 2 == 0 {
        x /= 2;
        y /= 2;
        common += 1;
    }

    // gcd(2, 3) = gcd(1, 3)
    while x % 2 == 0 {
        x /= 2;
    }

    loop {
        // Same thing for y
        while y % 2 == 0 {
            y /= 2;
        }

        // Ensure x <= y because it avoids the need to do some checks below
        if x > y {
            mem::swap(&mut x, &mut y);
        }

        // gcd(1, 3) = gcd(1, 2) = gcd(1, 1) = gcd(1, 0) = 1
        y -= x;

        // We found it!
        if y == 0 {
            break;
        }
    }

    // Restore the previously removed common multiples by multiplying with 2 **
    // common, aka bitshifting to the left with common
    x << common
}

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
    /// Construct a new fraction instance from a float. It's represented by
    /// multiplying the float enough times until it's a whole number.
    /// 0.5 is represented as 5/10. It returns None on overflow.
    pub fn from_float(mut numerator: f64) -> Option<Self> {
        let mut denominator: i32 = 1;
        while numerator % 1.0 > 0.0 {
            numerator *= 10.0;
            denominator = denominator.checked_mul(10)?;
        }
        if numerator < i32::min_value() as f64 || numerator > i32::max_value() as f64 {
            return None;
        }
        Some(Self {
            numerator: numerator as i32,
            denominator
        })
    }
    /// Construct a new fraction instance by parsing a string
    pub fn from_str_radix(input: &str, radix: u16) -> Option<Self> {
        let mut input = input.chars().peekable();

        let radix_fraction = Self::from(radix as i32);
        let mut result = Self::default();

        while let Some(digit) = input.peek().and_then(|c| c.to_digit(radix as u32)) {
            input.next().unwrap();
            result = result.checked_mul(radix_fraction)?;
            result = result.checked_add(Self::from(digit as i32))?;
        }

        if let Some('.') = input.peek() {
            input.next().unwrap();

            let mut decimal = Self::from(1);

            while let Some(digit) = input.peek().and_then(|c| c.to_digit(radix as u32)) {
                input.next().unwrap();
                decimal = decimal.checked_div(radix_fraction)?;
                result = result.checked_add(decimal.checked_mul(Self::from(digit as i32))?)?;
            }
        }

        if input.peek().is_some() {
            // Trailing input!
            None
        } else {
            Some(result)
        }
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
    fn sign_to_numerator(&mut self) {
        if self.denominator < 0 {
            self.numerator = -self.numerator;
            self.denominator = -self.denominator;
        }
    }
    /// Find a common denominator with other. Note that this does NOT find the
    /// lowest possible denominator. Instead you should call `simplify`
    /// afterwards if you need a small denominator. Returns None on overflow.
    pub fn common_denominator(mut self, mut other: Self) -> Option<(Self, Self)> {
        self.sign_to_numerator();
        other.sign_to_numerator();
        let denominator = if self.denominator == other.denominator {
            self.denominator
        } else {
            if self.denominator >= BIG_DENOMINATOR || self.denominator <= SMALL_DENOMINATOR
                    || other.denominator >= BIG_DENOMINATOR || other.denominator <= SMALL_DENOMINATOR {
                self = self.simplify();
                other = other.simplify();
            }
            self.denominator.checked_mul(other.denominator)?
        };
        Some((self.with_denominator(denominator), other.with_denominator(denominator)))
    }
    /// Find the lowest possible denominator for fraction
    pub fn simplify(mut self) -> Self {
        self.sign_to_numerator();
        let gcd = gcd(self.numerator.abs() as u32, self.denominator.abs() as u32) as i32;
        Self {
            numerator: self.numerator / gcd,
            denominator: self.denominator / gcd
        }
    }
    /// Return this value with a positive sign, no matter if it's negative or
    /// already positive.
    /// abs of 1/2 is 1/2.
    /// abs of -1/2 is 1/2.
    pub fn abs(self) -> Self {
        Self {
            numerator: self.numerator.abs(),
            denominator: self.denominator.abs()
        }
    }
    /// Same thing as in mathematics taking the power of -1.
    /// In this case also 1/n.
    ///
    /// ## Panics
    /// Panics if the numerator (new denominator) is 0
    pub fn inverse(mut self) -> Self {
        mem::swap(&mut self.numerator, &mut self.denominator);
        assert_ne!(self.denominator, 0, "new denominator is 0");
        self
    }
    /// Returns true if this fraction is 0. This differs from comparison with
    /// 0/1 in that there is no need to simplify first.
    pub fn is_zero(self) -> bool {
        self.numerator == 0
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

    /// Does addition, but returns None on overflow
    pub fn checked_add(self, other: Self) -> Option<Self> {
        let (mut left, right) = self.common_denominator(other.into())?;
        left.numerator = left.numerator.checked_add(right.numerator)?;
        Some(left)
    }
    /// Does subtraction, but returns None on overflow
    pub fn checked_sub(self, other: Self) -> Option<Self> {
        let (mut left, right) = self.common_denominator(other.into())?;
        left.numerator = left.numerator.checked_sub(right.numerator)?;
        Some(left)
    }
    /// Does multiplication, but returns None on overflow
    pub fn checked_mul(mut self, mut other: Self) -> Option<Self> {
        if self.denominator >= BIG_DENOMINATOR || self.denominator <= SMALL_DENOMINATOR
                || other.denominator >= BIG_DENOMINATOR || other.denominator <= SMALL_DENOMINATOR {
            self = self.simplify();
            other = other.simplify();
        }
        Some(Self {
            numerator: self.numerator.checked_mul(other.numerator)?,
            denominator: self.denominator.checked_mul(other.denominator)?
        })
    }
    /// Does division, but returns None on overflow
    pub fn checked_div(self, other: Self) -> Option<Self> {
        self.checked_mul(other.inverse())
    }
}
impl PartialOrd for Fraction {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        let (me, other) = self.common_denominator(*other).expect("integer overflow");
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
impl Neg for Fraction {
    type Output = Self;

    fn neg(mut self) -> Self::Output {
        self.numerator = -self.numerator;
        self
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

macro_rules! impl_op {
    ($($trait:ident $fn:ident = $call:ident),* --- $($trait_assign:ident $fn_assign:ident = ($op_assign:tt)),*) => {
        $(impl<T: Into<Fraction>> $trait<T> for Fraction {
            type Output = Self;
            fn $fn(self, other: T) -> Self {
                self.$call(other.into()).expect("integer overflow")
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
    Add add = checked_add,
    Sub sub = checked_sub,
    Mul mul = checked_mul,
    Div div = checked_div
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
        assert_eq!(Fraction::new(9, 27).simplify(), Fraction::new(1, 3));
        assert_eq!(Fraction::new(200, 400).simplify(), Fraction::new(1, 2));
        assert_eq!(Fraction::new(-200, 400).simplify(), Fraction::new(-1, 2));
    }

    #[test]
    fn ordering() {
        assert!(Fraction::new(1, 2) > Fraction::new(1, 3));
        assert!(Fraction::new(-12, 3) < Fraction::new(-6, -2));
    }

    #[test]
    fn parse() {
        assert_eq!(Fraction::from_str_radix("1.23456", 10).unwrap().simplify(), Fraction::new(3858, 3125));
        assert_eq!(Fraction::from_str_radix("1.01", 2).unwrap().simplify(), Fraction::new(5, 4));
        assert_eq!(Fraction::from_str_radix("12.3", 10).unwrap().simplify(), Fraction::new(123, 10));
    }
}
