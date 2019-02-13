use super::BigUint;

use num_traits::*;
use std::{
    cmp,
    fmt,
    mem,
    ops::*
};

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum Sign {
    Negative,
    Positive
}
impl Default for Sign {
    fn default() -> Self {
        Sign::Positive
    }
}
impl Neg for Sign {
    type Output = Self;
    fn neg(self) -> Self::Output {
        match self {
            Sign::Negative => Sign::Positive,
            Sign::Positive => Sign::Negative
        }
    }
}
#[derive(Clone, Default, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct BigInt {
    sign: Sign,
    number: BigUint
}
impl BigInt {
    /// Alias of `from`
    pub fn new<T: Into<Self>>(value: T) -> Self {
        value.into()
    }
    /// Return the sign
    pub fn sign(&self) -> Sign {
        self.sign
    }
    /// Borrow the inner absolute number
    pub fn unsigned(&self) -> &BigUint {
        &self.number
    }
    /// Mutably borrow the inner absolute number
    pub fn unsigned_mut(&mut self) -> &mut BigUint {
        &mut self.number
    }
    /// Extract the inner absolute number
    pub fn into_unsigned(self) -> BigUint {
        self.number
    }

    /// Create an i128 from this value, truncating it if it's larger.
    /// ```rust
    /// use math_types::BigInt;
    /// assert_eq!(BigInt::new(5).as_i128(), 5i128);
    /// assert_eq!((BigInt::new(std::i128::MAX) * 3).as_i128(), std::i128::MAX);
    /// ```
    // TODO: TryInto
    pub fn as_i128(&self) -> i128 {
        match self.sign {
            Sign::Positive => cmp::min(self.number.as_u128(), std::i128::MAX as u128) as i128,
            Sign::Negative => -(cmp::min(self.number.as_u128(), std::i128::MIN as u128) as i128),
        }
    }
    /// A version of `self.clone() & 1 == 0` that doesn't need to clone anything
    pub fn is_even(&self) -> bool {
        self.number.is_even()
    }
}
impl Num for BigInt {
    type FromStrRadixErr = ();

    fn from_str_radix(mut input: &str, radix: u32) -> Result<Self, Self::FromStrRadixErr> {
        let sign = if input.starts_with("-") {
            input = &input[1..];
            Sign::Negative
        } else if input.starts_with("+") {
            input = &input[1..];
            Sign::Positive
        } else {
            Sign::Positive
        };
        Ok(Self {
            sign,
            number: BigUint::from_str_radix(input, radix)?
        })
    }
}
impl Zero for BigInt {
    fn zero() -> Self {
        Self {
            sign: Sign::Positive,
            number: BigUint::zero()
        }
    }
    fn is_zero(&self) -> bool {
        self.number.is_zero()
    }
}
impl One for BigInt {
    fn one() -> Self {
        Self {
            sign: Sign::Positive,
            number: BigUint::one()
        }
    }
    fn is_one(&self) -> bool {
        self.sign == Sign::Positive && self.number.is_one()
    }
}
impl Signed for BigInt {
    fn abs(&self) -> Self {
        let mut clone = self.clone();
        clone.sign = Sign::Positive;
        clone
    }
    fn abs_sub(&self, other: &Self) -> Self {
        if self > other {
            Self {
                sign: Sign::Positive,
                number: self.clone().number - &other.number
            }
        } else {
            Self {
                sign: Sign::Positive,
                number: other.clone().number - &self.number
            }
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
    fn is_positive(&self) -> bool {
        !self.is_zero() && self.sign == Sign::Positive
    }
    fn is_negative(&self) -> bool {
        self.sign == Sign::Negative
    }
}
//impl<T: Unsigned + Into<BigUint>> From<T> for BigInt {
//    fn from(i: T) -> Self {
//        Self {
//            sign: Sign::Positive,
//            number: i.into()
//        }
//    }
//}
// TODO use the `default impl` specialization
/* default */ impl<T: Into<i128>> From<T> for BigInt {
    fn from(i: T) -> Self {
        let i = i.into();
        Self {
            sign: if i < 0 { Sign::Negative } else { Sign::Positive },
            number: (i.wrapping_abs() as u128).into()
        }
    }
}
impl From<BigUint> for BigInt {
    fn from(number: BigUint) -> Self {
        Self {
            sign: Sign::Positive,
            number
        }
    }
}
impl Neg for BigInt {
    type Output = Self;
    fn neg(mut self) -> Self::Output {
        if self.number != 0u8 {
            self.sign = -self.sign;
        }
        self
    }
}
impl CheckedNeg for BigInt {
    fn checked_neg(&self) -> Option<Self> {
        Some(-self.clone())
    }
}
impl Add<&Self> for BigInt {
    type Output = Self;
    fn add(self, other: &Self) -> Self::Output {
        if self.sign == other.sign {
            // Such as (1 + 3 = 4) or (-1 + -3 = -4)
            Self {
                sign: self.sign,
                number: self.number + &other.number
            }
        } else if self.number == other.number {
            // (4 + -4 = 0) or (-4 + 4 = 0)
            // Needs special casing simply because of the sign
            Self::zero()
        } else if self.number > other.number {
            // Such as (4 + -3 = 1) or (-4 + 3 = -1)
            Self {
                sign: self.sign,
                number: self.number - &other.number
            }
        } else {
            // Such as (3 + -4 = -1) or (-3 + 4 = 1)
            Self {
                sign: -self.sign,
                number: other.number.clone() - &self.number
            }
        }
    }
}
impl Sub<&Self> for BigInt {
    type Output = Self;
    fn sub(self, other: &Self) -> Self::Output {
        let other = other.clone();
        self + -other
    }
}
impl Mul<&Self> for BigInt {
    type Output = Self;
    fn mul(self, other: &Self) -> Self::Output {
        Self {
            sign: if self.sign == other.sign { Sign::Positive } else { Sign::Negative },
            number: self.number * &other.number
        }
    }
}
impl Div<&Self> for BigInt {
    type Output = Self;
    fn div(self, other: &Self) -> Self::Output {
        Self {
            sign: if self.sign == other.sign { Sign::Positive } else { Sign::Negative },
            number: self.number / &other.number
        }
    }
}
impl Rem<&Self> for BigInt {
    type Output = Self;
    fn rem(self, other: &Self) -> Self::Output {
        Self {
            sign: if self.sign == other.sign { Sign::Positive } else { Sign::Negative },
            number: self.number % &other.number
        }
    }
}
impl fmt::Binary for BigInt {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if self.sign == Sign::Negative {
            write!(f, "-")?;
        }
        write!(f, "{:b}", self.number)
    }
}
impl fmt::Display for BigInt {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if self.sign == Sign::Negative {
            write!(f, "-")?;
        } else if f.sign_plus() {
            write!(f, "+")?;
        }
        fmt::Display::fmt(&self.number, f)
    }
}
impl fmt::Debug for BigInt {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fmt::Display::fmt(self, f)
    }
}
impl<T: Into<BigUint>> Pow<T> for BigInt {
    type Output = Self;

    fn pow(mut self, other: T) -> BigInt {
        // Somewhat hacky way to implement exponent for BigInt: Reuse the
        // BigUint definition, and correct the sign after.
        let other = other.into();
        let positive = self.sign == Sign::Positive || other.is_even();
        self.number = self.number.pow(other);
        self.sign = if positive { Sign::Positive } else { Sign::Negative };
        self
    }
}

macro_rules! impl_op {
    ($($assign_trait:ident $assign_fn:ident, $checked_trait:ident $checked_fn:ident = $op_trait:ident $op_fn:ident),*) => {
        $(
            impl $checked_trait for BigInt {
                fn $checked_fn(&self, other: &Self) -> Option<Self> {
                    Some(self.clone().$op_fn(other))
                }
            }
            impl<T: Into<Self>> $op_trait<T> for BigInt {
                type Output = Self;
                fn $op_fn(self, other: T) -> Self::Output {
                    self.$op_fn(&other.into())
                }
            }
            impl $assign_trait<&Self> for BigInt {
                fn $assign_fn(&mut self, other: &Self) {
                    let mut me = BigInt::default();
                    mem::swap(self, &mut me);
                    me = me.$op_fn(other);
                    mem::swap(self, &mut me);
                }
            }
            impl<T: Into<Self>> $assign_trait<T> for BigInt {
                fn $assign_fn(&mut self, other: T) {
                    self.$assign_fn(&other.into());
                }
            }
        )*
    }
}
impl_op! {
    AddAssign add_assign, CheckedAdd checked_add = Add add,
    SubAssign sub_assign, CheckedSub checked_sub = Sub sub,
    DivAssign div_assign, CheckedDiv checked_div = Div div,
    MulAssign mul_assign, CheckedMul checked_mul = Mul mul,
    RemAssign rem_assign, CheckedRem checked_rem = Rem rem
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn format() {
        assert_eq!(format!("{:b}", BigInt::new(-5i8)), "-101");
        assert_eq!(format!("{:b}", BigInt::new(0u8)), "0");
        assert_eq!(format!("{}", BigInt::new(0u8)), "0");
        assert_eq!(format!("{}", BigInt::new(12345u16)), "12345");
        assert_eq!(format!("{:+}", BigInt::new(12345u16)), "+12345");
        assert_eq!(format!("{:+}", BigInt::new(-12345i16)), "-12345");
        assert_eq!(format!("{}", BigInt::new(-18446744073709551616i128)), "-18446744073709551616");
    }
    #[test]
    fn parse() {
        assert_eq!(BigInt::from_str_radix("-1234", 10).unwrap(), BigInt::new(-1234i16));
        assert_eq!(BigInt::from_str_radix("18446744073709551616", 10).unwrap(), BigInt::new(18446744073709551616i128));
        assert_eq!(BigInt::from_str_radix("+101010", 2).unwrap(), BigInt::new(42u8));
    }
    #[test]
    fn add_sub() {
        assert_eq!(BigInt::new(1) + 1, BigInt::new(2));
        assert_eq!(BigInt::new(-2) + 1, BigInt::new(-1));
        assert_eq!(BigInt::new(-2) + -1, BigInt::new(-3));
        assert_eq!(BigInt::new(-1) - 1, BigInt::new(-2));
        assert_eq!(BigInt::new(1) - -1, BigInt::new(2));
        assert_eq!(BigInt::new(1) - 5, BigInt::new(-4));
        assert_eq!(BigInt::new(129) - 1, BigInt::new(128));
        assert_eq!(BigInt::new(std::i64::MIN) + (BigInt::new(std::i64::MAX) + BigInt::new(1)), BigInt::new(0));
    }
    #[test]
    fn mul() {
        assert_eq!(BigInt::new(-3) * 3, BigInt::new(-9));
        assert_eq!(BigInt::new(123) * 2, BigInt::new(246));
    }
    #[test]
    fn div() {
        assert_eq!(BigInt::new(-81) / 9, BigInt::new(-9));
        assert_eq!(BigInt::new(27) / -9, BigInt::new(-3));
        assert_eq!(BigInt::new(246) / 2, BigInt::new(123));
    }
    #[test]
    fn pow() {
        assert_eq!(BigInt::new(5).pow(3u8), BigInt::new(125));
        assert_eq!(BigInt::new(-5).pow(3u8), BigInt::new(-125));
        assert_eq!(BigInt::new(-2).pow(8u8), BigInt::new(256));
    }
}
