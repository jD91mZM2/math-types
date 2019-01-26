use num_traits::*;
use std::{
    cmp::Ordering,
    fmt,
    mem,
    ops::*,
    str::FromStr
};

/// Find the greatest common divisor of two numbers
pub fn gcd<N: Unsigned + NumAssignRef + From<u8> + PartialOrd + ShlAssign<usize> + Clone>(mut x: N, mut y: N) -> N {
    // https://en.wikipedia.org/wiki/Greatest_common_divisor#Binary_method
    // https://en.wikipedia.org/wiki/Binary_GCD_algorithm
    let mut common = 0usize;

    // Otherwise this will loop forever since 0/2 is also even
    if x.is_zero() {
        return y;
    }
    if y.is_zero() {
        return x;
    }

    // gcd(4, 6) = gcd(2, 3) * 2
    while (x.clone() % N::from(2)).is_zero() && (y.clone() % N::from(2)).is_zero() {
        x /= &N::from(2);
        y /= &N::from(2);
        common += 1;
    }

    // gcd(2, 3) = gcd(1, 3)
    while (x.clone() % N::from(2)).is_zero() {
        x /= &N::from(2);
    }

    loop {
        // Same thing for y
        while (y.clone() % N::from(2)).is_zero() {
            y /= &N::from(2);
        }

        // Ensure x <= y because it avoids the need to do some checks below
        if x > y {
            mem::swap(&mut x, &mut y);
        }

        // gcd(1, 3) = gcd(1, 2) = gcd(1, 1) = gcd(1, 0) = 1
        y -= &x;

        // We found it!
        if y.is_zero() {
            break;
        }
    }

    // Restore the previously removed common multiples by multiplying with 2 **
    // common, aka bitshifting to the left with common
    x <<= common;
    x
}

pub trait FractionNumber: Clone + Signed + CheckedAdd + CheckedSub + CheckedMul + CheckedDiv + CheckedRem + CheckedNeg + PartialOrd + Ord + fmt::Debug + From<i8> {
    type UnsignedType: Unsigned + NumAssignRef + From<u8> + PartialOrd + ShlAssign<usize> + Clone;

    /// See the documentation on `big_denominator`. By default this function is
    /// the negated value of that.
    fn small_denominator() -> Self {
        -Self::big_denominator()
    }
    /// Specifies when a denominator is so big that it should automatically
    /// simplify before attempting an operation.
    ///
    /// This makes sure that
    /// 1. Fractions don't needlessly overflow when there's a simplier one available
    /// 2. Fractions don't get big enough to become slow to simplify
    fn big_denominator() -> Self;

    /// Return the unsigned absolute value for this type.
    /// Make sure to handle MIN!
    fn unsigned_abs(&self) -> Self::UnsignedType;
    /// Return the signed value for an unsigned one.
    /// Notes:
    /// - signed(MIN.unsigned_abs()) must be MIN.
    /// - Panic on error, this should never be called on an invalid value.
    fn signed(unsigned: Self::UnsignedType) -> Self;
}
macro_rules! impl_fracnum {
    ($(signed: $signed_type:ident, unsigned: $unsigned_type:ident, big: $big:expr),*) => {
        $(impl FractionNumber for $signed_type {
            type UnsignedType = $unsigned_type;

            fn big_denominator() -> Self {
                $big
            }

            fn unsigned_abs(&self) -> Self::UnsignedType {
                self.wrapping_abs() as Self::UnsignedType
            }
            fn signed(unsigned: Self::UnsignedType) -> Self {
                unsigned as $signed_type
            }
        })*
    }
}
impl_fracnum! {
    signed: i8, unsigned: u8, big: 32,
    signed: i16, unsigned: u16, big: 2_500,
    signed: i32, unsigned: u32, big: 10_000,
    signed: i64, unsigned: u64, big: 100_000,
    signed: i128, unsigned: u128, big: 100_000
}

/// A number stored in fraction form instead of actually calculating the
/// result. This ensures (10/3) * 3 is actually 10 and not 9.99998.
#[derive(Clone, Hash, Eq)]
pub struct Fraction<N: FractionNumber = i32> {
    numerator: N,
    denominator: N
}
impl<N: FractionNumber> Default for Fraction<N> {
    fn default() -> Self {
        Self::zero()
    }
}
impl<N: FractionNumber + Copy> Copy for Fraction<N> {}
impl<N: FractionNumber> From<N> for Fraction<N> {
    fn from(numerator: N) -> Self {
        Self::new(numerator, N::one())
    }
}
impl<N: FractionNumber + Copy + Into<Fraction<N>>> From<&N> for Fraction<N> {
    fn from(other: &N) -> Self {
        (*other).into()
    }
}
impl<N: FractionNumber> Fraction<N> {
    /// Construct a new fraction instance
    ///
    /// ## Panics
    /// Panics if the denominator is 0
    pub fn new(numerator: N, denominator: N) -> Self {
        assert!(!denominator.is_zero(), "denominator is 0");
        Self { numerator, denominator }
    }
    /// Construct a new fraction instance from a float. It's represented by
    /// multiplying the float enough times until it's a whole number.
    /// 0.5 is represented as 5/10. It drops precision on overflow.
    pub fn from_float(mut numerator: f64) -> Self
        where
            N: Copy + Bounded + 'static + AsPrimitive<f64>,
            f64: AsPrimitive<N>
    {
        let mut denominator = 10f64.powi(std::f64::MAX_10_EXP);
        loop {
            let new = numerator * denominator;
            if new >= N::min_value().as_() && new <= N::max_value().as_() {
                numerator = new.floor();
                while numerator % 10.0 < std::f64::EPSILON && denominator % 10.0 < std::f64::EPSILON {
                    numerator /= 10.0;
                    denominator /= 10.0;
                }
                break;
            }
            denominator = (denominator / 10.0).floor();
            if denominator < 10.0 {
                numerator = new;
                if numerator < N::min_value().as_() {
                    numerator = N::min_value().as_();
                } else if numerator > N::max_value().as_() {
                    numerator = N::max_value().as_();
                }
                break;
            }
        }
        Self {
            numerator: numerator.as_(),
            denominator: denominator.as_()
        }
    }
    /// Return the numerator
    pub fn numerator(&self) -> &N {
        &self.numerator
    }
    /// Return the denominator
    pub fn denominator(&self) -> &N {
        &self.denominator
    }
    /// Change the denominator and scale the numerator to reflect the changes.
    /// Note that this fraction type only works with whole numbers so some
    /// denominators may cause issues. Returns None on overflow.
    pub fn with_denominator(mut self, denominator: N) -> Option<Self> {
        assert!(!denominator.is_zero(), "denominator is 0");
        let scale = denominator.clone() / self.denominator;
        self.numerator = self.numerator.checked_mul(&scale)?;
        self.denominator = denominator;
        Some(self)
    }
    /// Makes sure the denominator is a positive number by moving the sign to
    /// the numerator or cancelling the signs out. -1/-2 becomes 1/2, 1/-2
    /// becomes -1/2. Returns None on overflow so you can easily use the `?` if
    /// you want the function to fail if this fails.
    pub fn sign_to_numerator(&mut self) -> Option<()> {
        if self.denominator.is_negative() {
            if let Some(numerator) = self.numerator.checked_neg() {
                if let Some(denominator) = self.denominator.checked_neg() {
                    self.numerator = numerator;
                    self.denominator = denominator;
                }
            }
        }
        Some(())
    }
    /// Find a common denominator with other. Note that this does NOT find the
    /// lowest possible denominator. Instead you should call `simplify`
    /// afterwards if you need a small denominator. Returns None on overflow.
    pub fn common_denominator(mut self, mut other: Self) -> Option<(Self, Self)> {
        self.sign_to_numerator()?;
        other.sign_to_numerator()?;
        let denominator = if self.denominator == other.denominator {
            self.denominator.clone()
        } else {
            if self.denominator >= N::big_denominator() || self.denominator <= N::small_denominator()
                    || other.denominator >= N::big_denominator() || other.denominator <= N::small_denominator() {
                self = self.simplify();
                other = other.simplify();
            }
            self.denominator.checked_mul(&other.denominator)?
        };
        Some((self.with_denominator(denominator.clone())?, other.with_denominator(denominator)?))
    }
    /// Find the lowest possible denominator for fraction
    pub fn simplify(mut self) -> Self {
        let gcd = N::signed(gcd(self.numerator.unsigned_abs(), self.denominator.unsigned_abs()));
        self = Self {
            numerator: self.numerator / gcd.clone(),
            denominator: self.denominator / gcd
        };
        // Prefer having the numerator negative, if possible without overflow
        let _ = self.sign_to_numerator();
        self
    }
    /// Return this value with a positive sign, no matter if it's negative or
    /// already positive.
    /// abs of 1/2 is 1/2.
    /// abs of -1/2 is 1/2.
    ///
    /// # Panics
    /// Panics if any component of the fraction is -i128::MIN, just like normal
    /// abs
    pub fn abs(self) -> Self {
        Self {
            numerator: self.numerator.abs(),
            denominator: self.denominator.abs()
        }
    }
    /// Calculates the decimal result of this fraction as an f64
    pub fn as_decimal(&self) -> f64
        where N: AsPrimitive<f64>
    {
        self.as_generic_decimal::<f64>()
    }
    /// Calculates the decimal result of this fraction as any generic type
    pub fn as_generic_decimal<T: 'static + Copy + Div<T, Output = T>>(&self) -> T
        where N: AsPrimitive<T>
    {
        self.numerator.as_() / self.denominator.as_()
    }
    /// Calculates the decimal integer result of this fraction
    pub fn floor(self) -> N {
        self.numerator / self.denominator
    }

    /// Calculates addition, but returns None on overflow
    pub fn checked_add(self, other: Self) -> Option<Self> {
        let (mut left, right) = self.common_denominator(other.into())?;
        left.numerator = left.numerator.checked_add(&right.numerator)?;
        Some(left)
    }
    /// Calculates subtraction, but returns None on overflow
    pub fn checked_sub(self, other: Self) -> Option<Self> {
        let (mut left, right) = self.common_denominator(other.into())?;
        left.numerator = left.numerator.checked_sub(&right.numerator)?;
        Some(left)
    }
    /// Calculates multiplication, but returns None on overflow
    pub fn checked_mul(mut self, mut other: Self) -> Option<Self> {
        if self.denominator >= N::big_denominator() || self.denominator <= N::small_denominator()
                || other.denominator >= N::big_denominator() || other.denominator <= N::small_denominator() {
            self = self.simplify();
            other = other.simplify();
        }
        Some(Self {
            numerator: self.numerator.checked_mul(&other.numerator)?,
            denominator: self.denominator.checked_mul(&other.denominator)?
        })
    }
    /// Calculates division, but returns None on overflow or if other is 0
    pub fn checked_div(self, other: Self) -> Option<Self> {
        if other.numerator.is_zero() {
            return None;
        }
        self.checked_mul(other.inv())
    }
    /// Calculates remainder, but returns None on overflow or if other is 0
    pub fn checked_rem(self, other: Self) -> Option<Self> {
        let (mut left, right) = self.common_denominator(other.into())?;
        left.numerator = left.numerator.checked_rem(&right.numerator)?;
        Some(left)
    }
}
impl<N: FractionNumber> PartialEq for Fraction<N> {
    fn eq(&self, other: &Self) -> bool {
        self.cmp(other) == Ordering::Equal
    }
}
impl<N: FractionNumber> Num for Fraction<N> {
    type FromStrRadixErr = ();

    fn from_str_radix(input: &str, radix: u32) -> Result<Self, Self::FromStrRadixErr> {
        assert!(radix >= 2 && radix <= 36, "invalid radix");
        let mut input = input.chars().peekable();

        let negative = input.peek() == Some(&'-');
        if negative {
            input.next().unwrap();
        }

        let radix_fraction = Self::from(N::from(radix as i8));
        let mut result = Self::default();

        while let Some(digit) = input.peek().and_then(|c| c.to_digit(radix)) {
            input.next().unwrap();
            result = result.checked_mul(radix_fraction.clone()).ok_or(())?;
            result = result.checked_add(Self::from(N::from(digit as i8))).ok_or(())?;
        }

        if let Some('.') = input.peek() {
            input.next().unwrap();

            let mut decimal = Self::from(N::one());

            while let Some(digit) = input.peek().and_then(|c| c.to_digit(radix)) {
                input.next().unwrap();
                decimal = decimal.checked_div(radix_fraction.clone()).ok_or(())?;
                result = result.checked_add(
                    decimal.clone().checked_mul(Self::from(N::from(digit as i8))).ok_or(())?
                ).ok_or(())?;
            }
        }

        if input.peek().is_some() {
            // Trailing input!
            Err(())
        } else {
            if negative {
                result = -result;
            }
            Ok(result)
        }
    }
}
impl<N: FractionNumber> Zero for Fraction<N> {
    fn zero() -> Self {
        Self::from(N::zero())
    }
    fn is_zero(&self) -> bool {
        self.numerator.is_zero()
    }
}
impl<N: FractionNumber> One for Fraction<N> {
    fn one() -> Self {
        Self::from(N::one())
    }
    fn is_one(&self) -> bool {
        self.numerator == self.denominator
    }
}
impl<N: FractionNumber> Inv for Fraction<N> {
    type Output = Self;
    fn inv(mut self) -> Self {
        mem::swap(&mut self.numerator, &mut self.denominator);
        assert!(!self.denominator.is_zero(), "new denominator is 0");
        self
    }
}
impl<N: FractionNumber> Signed for Fraction<N> {
    fn abs(&self) -> Self {
        self.clone().abs()
    }
    fn abs_sub(&self, other: &Self) -> Self {
        if self >= other {
            self.clone() - other.clone()
        } else {
            other.clone() - self.clone()
        }
    }
    fn signum(&self) -> Self {
        if self.is_zero() {
            Self::zero()
        } else if self.is_positive() {
            Self::one()
        } else /* if self.is_negative() */ {
            -Self::one()
        }
    }
    fn is_negative(&self) -> bool {
        !self.is_zero() && self.numerator.signum() != self.denominator.signum()
    }
    fn is_positive(&self) -> bool {
        !self.is_zero() && self.numerator.signum() == self.denominator.signum()
    }
}
impl<N: FractionNumber + Ord> Ord for Fraction<N> {
    fn cmp(&self, other: &Self) -> Ordering {
        let (me, other) = self.clone().common_denominator(other.clone()).expect("integer overflow");
        me.numerator.cmp(&other.numerator)
    }
}
impl<N: FractionNumber> PartialOrd for Fraction<N> {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        let (me, other) = self.clone().common_denominator(other.clone()).expect("integer overflow");
        me.numerator.partial_cmp(&other.numerator)
    }
}
impl<N: fmt::Display + FractionNumber + Pow<u32, Output = N> + NumRef + NumAssignRef> fmt::Display for Fraction<N> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut me = self.clone().simplify();
        if me.is_negative() {
            let _ = me.sign_to_numerator();
        }
        if let Some(precision) = f.precision() {
            // TODO: When specialization comes, implement From<usize> for Big*
            // and require usize here. This disgusting hack fails miserably
            // where precision > u32::MAX
            let exp = N::from(10).pow(precision as u32);
            if let Some(numerator) = self.numerator.checked_mul(&exp) {
                let (mut div, rem) = (numerator.clone() / &self.denominator, numerator % &self.denominator);
                if rem.is_zero() {
                    let floored = self.numerator.clone() / &self.denominator;
                    if f.sign_plus() {
                        write!(f, "{:+}", floored)?;
                    } else {
                        write!(f, "{}", floored)?;
                    }
                    div -= floored.clone() * exp;
                    return if !div.is_zero() {
                        let ten = N::from(10);
                        let mut padding = precision;
                        while div.clone() % &ten == N::from(0) {
                            div /= &ten;
                            padding -= 1;
                        }
                        write!(f, ".{:0pad$}", div, pad=padding)
                    } else {
                        Ok(())
                    };
                }
            }
        }
        if f.sign_plus() {
            write!(f, "{:+}/{}", me.numerator, me.denominator)
        } else {
            write!(f, "{}/{}", me.numerator, me.denominator)
        }
    }
}
// TODO: specialization
//impl<N: fmt::Display + FractionNumber + AsPrimitive<f64>> fmt::Display for Fraction<N> {
//    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
//        let mut me = self.simplify();
//        if me.is_negative() {
//            let _ = me.sign_to_numerator();
//        }
//        if let Some(precision) = f.precision() {
//            if me.as_decimal() * 10f64.powi(precision as i32) % 1.0 < std::f64::EPSILON {
//                return if f.sign_plus() {
//                    write!(f, "{:+}", me.as_decimal())
//                } else {
//                    write!(f, "{}", me.as_decimal())
//                };
//            }
//        }
//        if f.sign_plus() {
//            write!(f, "{:+}/{}", me.numerator, me.denominator)
//        } else {
//            write!(f, "{}/{}", me.numerator, me.denominator)
//        }
//    }
//}
impl<N: fmt::Debug + FractionNumber> fmt::Debug for Fraction<N> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut me = self.clone();
        if me.is_negative() {
            let _ = me.sign_to_numerator();
        }
        if f.sign_plus() {
            write!(f, "{:+?}/{:?}", me.numerator, me.denominator)
        } else {
            write!(f, "{:?}/{:?}", me.numerator, me.denominator)
        }
    }
}
impl<N: FractionNumber> Neg for Fraction<N> {
    type Output = Self;

    fn neg(mut self) -> Self::Output {
        if let Some(numerator) = self.numerator.checked_neg() {
            self.numerator = numerator;
        } else if let Some(denominator) = self.denominator.checked_neg() {
            self.denominator = denominator;
        } else {
            // Both variables are MIN.
            // -MIN/MIN == -1/1
            self.numerator = -N::one();
            self.denominator = N::one();
        }
        self
    }
}
impl<N: FractionNumber + From<i128>> FloatConst for Fraction<N> {
    fn E() -> Self {
        Self::new(N::from(27182818284590452353602874713526624977i128),  N::from(10000000000000000000000000000000000000i128))
    }
    fn FRAC_1_PI() -> Self {
        Self::new(N::from(031830988618379067153776752674502872406i128), N::from(100000000000000000000000000000000000000i128))
    }
    fn FRAC_1_SQRT_2() -> Self {
        Self::new(N::from(070710678118654752440084436210484903928i128), N::from(100000000000000000000000000000000000000i128))
    }
    fn FRAC_2_PI() -> Self {
        Self::new(N::from(063661977236758134307553505349005744814i128), N::from(100000000000000000000000000000000000000i128))
    }
    fn FRAC_2_SQRT_PI() -> Self {
        Self::new(N::from(11283791670955125738961589031215451716i128),  N::from(10000000000000000000000000000000000000i128))
    }
    fn FRAC_PI_2() -> Self {
        Self::new(N::from(15707963267948966192313216916397514420i128),  N::from(10000000000000000000000000000000000000i128))
    }
    fn FRAC_PI_3() -> Self {
        Self::new(N::from(10471975511965977461542144610931676280i128),  N::from(10000000000000000000000000000000000000i128))
    }
    fn FRAC_PI_4() -> Self {
        Self::new(N::from(078539816339744830961566084581987572104i128), N::from(100000000000000000000000000000000000000i128))
    }
    fn FRAC_PI_6() -> Self {
        Self::new(N::from(052359877559829887307710723054658381402i128), N::from(100000000000000000000000000000000000000i128))
    }
    fn FRAC_PI_8() -> Self {
        Self::new(N::from(039269908169872415480783042290993786052i128), N::from(100000000000000000000000000000000000000i128))
    }
    fn LN_10() -> Self {
        Self::new(N::from(23025850929940456840179914546843642076i128),  N::from(10000000000000000000000000000000000000i128))
    }
    fn LN_2() -> Self {
        Self::new(N::from(069314718055994530941723212145817656807i128), N::from(100000000000000000000000000000000000000i128))
    }
    fn LOG10_E() -> Self {
        Self::new(N::from(043429448190325182765112891891660508229i128), N::from(100000000000000000000000000000000000000i128))
    }
    fn LOG2_E() -> Self {
        Self::new(N::from(14426950408889634073599246810018921374i128),  N::from(10000000000000000000000000000000000000i128))
    }
    fn PI() -> Self {
        Self::new(N::from(31415926535897932384626433832795028841i128),  N::from(10000000000000000000000000000000000000i128))
    }
    fn SQRT_2() -> Self {
        Self::new(N::from(14142135623730950488016887242096980785i128),  N::from(10000000000000000000000000000000000000i128))
    }
}

macro_rules! impl_op {
    ($($trait:ident $fn:ident = $call:ident),* --- $($trait_assign:ident $fn_assign:ident = ($op_assign:tt)),*) => {
        $(impl<N: FractionNumber, T: Into<Fraction<N>>> $trait<T> for Fraction<N> {
            type Output = Self;
            fn $fn(self, other: T) -> Self {
                self.$call(other.into()).expect("integer overflow or other math error")
            }
        })*
        $(
            impl<N: FractionNumber> $trait_assign<&Self> for Fraction<N> {
                fn $fn_assign(&mut self, other: &Self) {
                    self.$fn_assign(other.clone());
                }
            }
            impl<N: FractionNumber, T: Into<Fraction<N>>> $trait_assign<T> for Fraction<N> {
                fn $fn_assign(&mut self, other: T) {
                    let mut me = Fraction::default();
                    mem::swap(self, &mut me);
                    me = me $op_assign other;
                    mem::swap(self, &mut me);
                }
            }
        )*
    }
}
impl_op! {
    Add add = checked_add,
    Sub sub = checked_sub,
    Mul mul = checked_mul,
    Div div = checked_div,
    Rem rem = checked_rem
    ---
    AddAssign add_assign = (+),
    SubAssign sub_assign = (-),
    MulAssign mul_assign = (*),
    DivAssign div_assign = (/),
    RemAssign rem_assign = (%)
}

impl<N: FractionNumber> FromStr for Fraction<N> {
    type Err = N::FromStrRadixErr;
    fn from_str(input: &str) -> Result<Self, Self::Err> {
        let mut parts = input.splitn(2, '/');
        let numerator = N::from_str_radix(parts.next().unwrap_or_default(), 10)?;
        let mut denominator = parts.next().map(|part| {
            N::from_str_radix(part, 10)
        }).unwrap_or(Ok(N::one()))?;
        if denominator.is_zero() {
            denominator = N::one();
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
                        let x = (Fraction::new(i, j) + Fraction::new(k, l)).as_decimal();
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
                        let x = (Fraction::new(i, j) - Fraction::new(k, l)).as_decimal();
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
                        let x = (Fraction::new(i, j) * Fraction::new(k, l)).as_decimal();
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
                        let x = (Fraction::new(i, j) / Fraction::new(k, l)).as_decimal();
                        let y = (i as f64/j as f64) / (k as f64/l as f64);
                        assert!(x - y <= 1.0, "not the same thing: {} and {}", x, y);
                    }
                }
            }
        }
    }

    #[test]
    fn rem() {
        assert_eq!((Fraction::new(3, 5) / Fraction::new(2, 10)).simplify(), Fraction::new(3, 1));
        for i in 0..25 {
            for j in 1..25 {
                for k in 1..25 {
                    for l in 1..25 {
                        let x = (Fraction::new(i, j) % Fraction::new(k, l)).as_decimal();
                        let y = (i as f64/j as f64) % (k as f64/l as f64);
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
        assert_eq!(Fraction::new(100, std::i128::MIN).simplify(), Fraction::new(-25, 42535295865117307932921825928971026432));
        assert_eq!(Fraction::new(1, std::i128::MIN).simplify(), Fraction::new(1, std::i128::MIN));
    }

    #[test]
    fn ordering() {
        assert!(Fraction::new(1, 2) > Fraction::new(1, 3));
        assert!(Fraction::new(1, 2) == Fraction::new(2, 4));
        assert!(Fraction::new(-12, 3) < Fraction::new(-6, -2));
    }

    #[test]
    fn from_float() {
        assert_eq!(Fraction::from_float(1.23456), Fraction::new(123456, 100000));
        assert_eq!(Fraction::from_float(std::f64::MAX), Fraction::new(std::i128::MAX, 1));
        assert_eq!(Fraction::from_float(3.1415926535897932384626433832), Fraction::new(314159265, 100000000));
    }

    #[test]
    fn parse() {
        assert_eq!(Fraction::from_str_radix("1.23456", 10).unwrap().simplify(), Fraction::new(3858, 3125));
        assert_eq!(Fraction::from_str_radix("1.01", 2).unwrap().simplify(), Fraction::new(5, 4));
        assert_eq!(Fraction::from_str_radix("12.3", 10).unwrap().simplify(), Fraction::new(123, 10));
        assert_eq!(Fraction::from_str_radix("-12.3", 10).unwrap().simplify(), Fraction::new(-123, 10));
    }

    #[test]
    fn format() {
        {
            use crate::BigInt;
            type Fraction = super::Fraction<BigInt>;
            assert_eq!(format!("{}", Fraction::new(BigInt::new(2), BigInt::new(4))), "1/2");
            assert_eq!(format!("{:?}", Fraction::new(BigInt::new(2), BigInt::new(4))), "2/4");
            assert_eq!(format!("{:+?}", Fraction::new(BigInt::new(2), BigInt::new(4))), "+2/4");
            assert_eq!(format!("{:?}", Fraction::new(BigInt::new(2), -BigInt::new(4))), "-2/4");
            assert_eq!(format!("{:.1}", Fraction::new(BigInt::new(1), BigInt::new(2))), "0.5");
            assert_eq!(format!("{:.10}", Fraction::new(BigInt::new(1), BigInt::new(2))), "0.5");
            assert_eq!(format!("{:+.1}", Fraction::new(BigInt::new(1), BigInt::new(2))), "+0.5");
            assert_eq!(format!("{:.1}", Fraction::new(BigInt::new(1), BigInt::new(4))), "1/4");
            assert_eq!(format!("{:.2}", Fraction::new(BigInt::new(1), BigInt::new(4))), "0.25");
            assert_eq!(format!("{:.2}", Fraction::new(BigInt::new(1), BigInt::new(3))), "1/3");
            assert_eq!(format!("{}", Fraction::new(BigInt::new(1), BigInt::new(std::i32::MIN))), "-1/2147483648");
            assert_eq!(format!("{:.10}", Fraction::new(BigInt::new(1), BigInt::new(1))), "1");
            assert_eq!(format!("{:.10}", Fraction::new(BigInt::new(100), BigInt::new(4))), "25");
            assert_eq!(format!("{:.10}", Fraction::new(BigInt::new(1), BigInt::new(100))), "0.01");
            assert_eq!(format!("{:.10}", Fraction::new(BigInt::new(1), BigInt::new(10_000))), "0.0001");
        }
        // TODO: specialization
        //{
        //    assert_eq!(format!("{}", Fraction::new(2, 4)), "1/2");
        //    assert_eq!(format!("{:?}", Fraction::new(2, 4)), "2/4");
        //    assert_eq!(format!("{:+?}", Fraction::new(2, 4)), "+2/4");
        //    assert_eq!(format!("{:?}", Fraction::new(2, -4)), "-2/4");
        //    assert_eq!(format!("{:.10}", Fraction::new(1, 2)), "0.5");
        //    assert_eq!(format!("{:+.1}", Fraction::new(1, 2)), "+0.5");
        //    assert_eq!(format!("{:.1}", Fraction::new(1, 4)), "1/4");
        //    assert_eq!(format!("{:.2}", Fraction::new(1, 4)), "0.25");
        //    assert_eq!(format!("{:.2}", Fraction::new(1, 3)), "1/3");
        //    assert_eq!(format!("{}", Fraction::new(1, std::i32::MIN)), "1/-2147483648");
        //}
    }

    #[test]
    fn consts() {
        assert!(Fraction::<i128>::PI().as_decimal() - std::f64::consts::PI < 0.000000000000001);
    }
}
