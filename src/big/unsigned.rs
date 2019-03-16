use num_traits::*;
use smallvec::SmallVec;
use std::{
    borrow::{Borrow, BorrowMut},
    cmp::{self, Ordering},
    fmt,
    iter,
    ops::*
};

pub const BITS: usize = 32;

#[derive(Clone, Default, PartialEq, Eq, Hash)]
pub struct BigUint {
    digits: SmallVec<[u32; 8]>
}
impl BigUint {
    /// Alias to `from`
    pub fn new<T: Into<Self>>(value: T) -> Self {
        value.into()
    }
    /// Return an iterator over all bits
    pub fn bits(&self) -> BitIter<&Self> {
        BitIter::new(self)
    }
    /// Return an iterator over all bits, but allow changing bits
    pub fn bits_mut(&mut self) -> BitIter<&mut Self> {
        BitIter::new(self)
    }
    fn trim_end(&mut self) {
        if self.digits.is_empty() {
            return;
        }
        let mut i = self.digits.len() - 1;
        while self.digits[i] == 0 {
            self.digits.pop();
            match i.checked_sub(1) {
                Some(new) => i = new,
                None => break
            }
        }
    }
    fn common_size(&mut self, other: &Self) {
        if other.digits.len() > self.digits.len() {
            self.digits.extend(iter::repeat(0).take(other.digits.len() - self.digits.len()));
        }
    }
    /// Return this digit as a u128, or u128::MAX if larger than possible
    pub fn as_u128(&self) -> u128 {
        if self.digits.len() > 4 {
            std::u128::MAX
        } else {
            let mut copy: [u32; 4] = [0; 4];
            let len = cmp::min(self.digits.len(), copy.len());
            copy[..len].copy_from_slice(&self.digits[..len]);
            ((copy[3] as u128) << 32*3)
                + ((copy[2] as u128) << 32*2)
                + ((copy[1] as u128) << 32)
                + copy[0] as u128
        }
    }
    /// Performs subtraction and returns false on overflow
    pub fn subtract(&mut self, other: &Self) -> bool {
        self.common_size(other);

        let mut carry = false;

        let mut digits2 = other.digits.iter();
        for digit in &mut self.digits {
            let other = *digits2.next().unwrap_or(&0);
            let (new, mut underflow) = digit.overflowing_sub(other);
            *digit = new;
            if carry {
                let (new2, underflow2) = digit.overflowing_sub(1);
                *digit = new2;
                underflow |= underflow2;
            }
            carry = underflow;
        }

        self.trim_end();

        // If we still have a carry value we've underflowed
        !carry
    }
    /// Performs division and remainder in one step
    pub fn divide(&self, other: &Self) -> Option<(Self, Self)> {
        // Tackling this in binary simplifies the problem significantly. The
        // algorithm is similar to long division on paper, and works like
        // this:
        // - Take a digit
        // - If the new number is less than the divisor, write 0 and repeat.
        // - Divide the new number, write 1, and repeat calculation on the
        // remainder.
        //
        // In binary, the number we divide is always less than 2x the
        // denominator, which means we can just subtract once to get the
        // remainder. I'm unsure if this is faster or slower than using the
        // normal base (which would require parts of the code to be the
        // traditional and slow subtract-while-bigger way)
        if other.is_zero() {
            return None;
        }
        if self.is_zero() {
            return Some((BigUint::zero(), BigUint::zero()));
        }
        let mut bits = self.bits();
        bits.goto_end();

        while bits.get() == Some(0) {
            bits.backwards(1);
        }

        let mut me = Self::default();
        let mut result = Self::default();

        loop {
            let next = bits.get().unwrap();
            me <<= 1;
            me += &next.into();
            result <<= 1;

            if me >= *other {
                result += &Self::new(1u8);
                me -= &*other;
            }
            if bits.pos() == 0 {
                break;
            }
            bits.backwards(1);
        }
        Some((result, me))
    }
    /// A version of `self.clone() & 1 == 0` that doesn't need to clone anything
    pub fn is_even(&self) -> bool {
        self.digits.get(0).unwrap_or(&0) & 1 == 0
    }
    fn bitop<F>(&mut self, other: &Self, mut op: F)
        where F: FnMut(&mut u32, u32)
    {
        self.common_size(other);

        for (ldigit, rdigit) in self.digits.iter_mut().zip(other.digits.iter().chain(iter::repeat(&0))) {
            op(ldigit, *rdigit);
        }

        self.trim_end();
    }
}
impl Num for BigUint {
    type FromStrRadixErr = ();

    fn from_str_radix(input: &str, radix: u32) -> Result<Self, Self::FromStrRadixErr> {
        let mut input = input.chars().peekable();
        let mut result = Self::default();
        while let Some(digit) = input.peek().and_then(|digit| digit.to_digit(radix)) {
            result *= radix;
            result += digit;
            input.next().unwrap();
        }
        if input.peek().is_some() {
            return Err(());
        }
        Ok(result)
    }
}
impl Zero for BigUint {
    fn zero() -> Self {
        Self {
            digits: SmallVec::new()
        }
    }
    fn is_zero(&self) -> bool {
        self.digits.is_empty()
    }
}
impl One for BigUint {
    fn one() -> Self {
        let mut digits = SmallVec::new();
        digits.push(1);
        Self { digits }
    }
    fn is_one(&self) -> bool {
        self.digits.len() == 1 && self.digits[0] == 1
    }
}
impl Unsigned for BigUint {}
// TODO: specialization for int <=32
//impl<N: Into<u32>> From<N> for BigUint {
//    fn from(i: N) -> Self {
//        let mut digits = SmallVec::new();
//        let mut i = i.into();
//        if i > 0 {
//            digits.push(i as u32);
//        }
//        Self { digits }
//    }
//}
impl<N: Into<u128>> From<N> for BigUint {
    fn from(i: N) -> Self {
        let mut i = i.into();
        let mut digits = SmallVec::new();
        while i > 0 {
            digits.push((i & std::u32::MAX as u128) as u32);
            i >>= BITS;
        }
        Self { digits }
    }
}
impl AddAssign<&Self> for BigUint {
    fn add_assign(&mut self, other: &Self) {
        self.common_size(other);

        let mut carry = false;

        let mut digits2 = other.digits.iter();
        for digit in &mut self.digits {
            let other = *digits2.next().unwrap_or(&0);
            let (new, mut overflow) = digit.overflowing_add(other);
            *digit = new;
            if carry {
                let (new2, overflow2) = digit.overflowing_add(1);
                *digit = new2;
                overflow |= overflow2;
            }
            carry = overflow;
        }

        if carry {
            self.digits.push(1);
        }
    }
}
impl SubAssign<&Self> for BigUint {
    fn sub_assign(&mut self, other: &Self) {
        assert!(self.subtract(other), "BigUint underflow: going below 0");
    }
}
impl CheckedSub for BigUint {
    fn checked_sub(&self, other: &Self) -> Option<Self> {
        let mut me = self.clone();
        if me.subtract(other) {
            return Some(me);
        } else {
            return None;
        }
    }
}
impl ShlAssign<usize> for BigUint {
    fn shl_assign(&mut self, shift: usize) {
        if shift <= BITS {
            // Use fast shift that can only shift between two neighbor digits
            let mut overflow = 0;
            for digit in &mut self.digits {
                let new_overflow = ((*digit as u64) << shift >> BITS) as u32;
                *digit <<= shift;
                *digit += overflow;
                overflow = new_overflow;
            }
            if overflow > 0 {
                self.digits.push(overflow);
            }
        } else {
            let amount = 1 + shift / BITS;
            self.digits.extend(iter::repeat(0).take(amount));
            let mut bits = self.bits_mut();
            bits.goto_end();

            while let Some(_) = bits.get() {
                let bit = bits.peek_backwards(shift).unwrap_or_default();
                bits.set(bit);

                if bits.pos() == 0 {
                    break;
                }
                bits.backwards(1);
            }

            self.trim_end();
        }
    }
}
impl Shl<usize> for BigUint {
    type Output = Self;
    fn shl(mut self, shift: usize) -> Self::Output {
        self <<= shift;
        self
    }
}
impl ShrAssign<usize> for BigUint {
    fn shr_assign(&mut self, shift: usize) {
        if shift < BITS {
            // Use fast shift that can only shift between two neighbor digits
            let mut overflow = 0;
            for digit in self.digits.iter_mut().rev() {
                let next_overflow = *digit & ((1 << shift) - 1);
                *digit >>= shift;
                *digit += overflow << (BITS-shift);
                overflow = next_overflow;
            }
        } else {
            let mut bits = self.bits_mut();

            while let Some(_) = bits.get() {
                let bit = bits.peek_forwards(shift).unwrap_or_default();
                bits.set(bit);

                bits.forwards(1);
            }

            self.trim_end();
        }
    }
}
impl Shr<usize> for BigUint {
    type Output = Self;
    fn shr(mut self, shift: usize) -> Self::Output {
        self >>= shift;
        self
    }
}
impl Mul<Self> for &BigUint {
    type Output = BigUint;
    fn mul(self, other: Self) -> Self::Output {
        let mut result = BigUint::zero();

        for (i, &digit) in self.digits.iter().enumerate() {
            let mut carry = 0;
            // MAX squared in N-bit int results in 2*MAX less than the MAX of a
            // 2N-bit int
            // 255 * 255 + 2*255 = 65535
            for (j, &digit2) in other.digits.iter().enumerate() {
                let mut multiplied = digit as u64 * digit2 as u64;
                multiplied += carry;
                carry = multiplied / (std::u32::MAX as u64 + 1);

                let mut part = BigUint::zero();
                part += multiplied & std::u32::MAX as u64;
                part.digits.insert_many(0, iter::repeat(0).take(i+j));
                result += part;
            }

            if carry > 0 {
                let mut part = BigUint::zero();
                part += carry;
                part.digits.insert_many(0, iter::repeat(0).take(i+other.digits.len()));
                result += part;
            }
        }

        result
    }
}
impl Div<Self> for &BigUint {
    type Output = BigUint;
    fn div(self, other: Self) -> Self::Output {
        let (res, _) = self.divide(other).expect("division by 0");
        res
    }
}
impl CheckedDiv for BigUint {
    fn checked_div(&self, other: &Self) -> Option<Self> {
        self.divide(other).map(|(res, _)| res)
    }
}
impl Rem<Self> for &BigUint {
    type Output = BigUint;
    fn rem(self, other: Self) -> Self::Output {
        let (_, rem) = self.divide(other).expect("division by 0");
        rem
    }
}
impl CheckedRem for BigUint {
    fn checked_rem(&self, other: &Self) -> Option<Self> {
        self.divide(other).map(|(_, rem)| rem)
    }
}
impl BitAndAssign<&Self> for BigUint {
    fn bitand_assign(&mut self, other: &Self) {
        self.bitop(other, |ldigit, rdigit| *ldigit &= rdigit);
    }
}
impl BitOrAssign<&Self> for BigUint {
    fn bitor_assign(&mut self, other: &Self) {
        self.bitop(other, |ldigit, rdigit| *ldigit |= rdigit);
    }
}
impl BitXorAssign<&Self> for BigUint {
    fn bitxor_assign(&mut self, other: &Self) {
        self.bitop(other, |ldigit, rdigit| *ldigit ^= rdigit);
    }
}
impl Ord for BigUint {
    fn cmp(&self, other: &Self) -> Ordering {
        let cmp = self.digits.len().cmp(&other.digits.len());
        if cmp != Ordering::Equal {
            return cmp;
        }
        self.digits.iter().rev().cmp(other.digits.iter().rev())
    }
}
impl PartialOrd for BigUint {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}
impl<T: Into<BigUint> + Copy> PartialEq<T> for BigUint {
    fn eq(&self, other: &T) -> bool {
        *self == (*other).into()
    }
}
impl<T: Into<BigUint> + Copy> PartialOrd<T> for BigUint {
    fn partial_cmp(&self, other: &T) -> Option<Ordering> {
        self.partial_cmp(&(*other).into())
    }
}
impl fmt::Binary for BigUint {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if *self == 0u8 {
            return write!(f, "0");
        }
        let mut bits = self.bits();
        bits.goto_end();
        while bits.get() == Some(0) {
            bits.backwards(1);
        }
        while let Some(bit) = bits.get() {
            write!(f, "{}", bit)?;
            if bits.pos() == 0 {
                break;
            }
            bits.backwards(1);
        }
        Ok(())
    }
}
impl fmt::Display for BigUint {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if *self == 0u8 {
            return if f.sign_aware_zero_pad() {
                write!(f, "{:0pad$}", 0, pad=f.width().unwrap_or(0))
            } else {
                write!(f, "0")
            };
        }
        let mut denominator = BigUint::new(1u8);
        let mut precision = 0;
        while denominator <= *self {
            denominator *= 10u8;
            precision += 1;
        }
        if f.sign_aware_zero_pad() && precision < f.width().unwrap() {
            write!(f, "{:0>pad$}", "", pad = f.width().unwrap() - precision)?;
        }
        while denominator > 1u8 {
            denominator /= 10u8;
            write!(f, "{}", (self / &denominator % 10u8).as_u128())?;
        }
        Ok(())
    }
}
impl fmt::Debug for BigUint {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fmt::Display::fmt(self, f)
    }
}
impl<T: Into<Self>> Pow<T> for BigUint {
    type Output = Self;

    fn pow(mut self, other: T) -> Self {
        let mut other = other.into();
        if other == 0u8 {
            return Self::one();
        }

        let mut extra = Self::one();
        while other > 1u8 {
            if other.clone() & 1u8 == 0u8 {
                other >>= 1;
                self *= self.clone();
            } else {
                other -= 1u8;
                extra *= &self;
            }
        }
        self * extra
    }
}

macro_rules! impl_op {
    (
        $($op_trait:ident $op_fn:ident$(, $checked_trait:ident $checked_fn:ident)? = $assign_trait:ident $assign_fn:ident),*
        ---
        $($op_trait2:ident $op_fn2:ident$(, $checked_trait2:ident $checked_fn2:ident)? = $assign_trait2:ident $assign_fn2:ident),*) => {
        $(
            $(impl $checked_trait for BigUint {
                fn $checked_fn(&self, other: &Self) -> Option<Self> {
                    Some(self.clone().$op_fn(other))
                }
            })?
            impl<T: Into<Self>> $assign_trait<T> for BigUint {
                fn $assign_fn(&mut self, other: T) {
                    self.$assign_fn(&other.into())
                }
            }
            impl $op_trait<&Self> for BigUint {
                type Output = Self;
                fn $op_fn(mut self, other: &Self) -> Self::Output {
                    self.$assign_fn(other);
                    self
                }
            }
            impl<T: Into<Self>> $op_trait<T> for BigUint {
                type Output = Self;
                fn $op_fn(mut self, other: T) -> Self::Output {
                    self.$assign_fn(other);
                    self
                }
            }
        )*
        $(
            $(impl $checked_trait2 for BigUint {
                fn $checked_fn2(&self, other: &Self) -> Option<Self> {
                    Some(self.clone().$op_fn2(other.clone()))
                }
            })?
            impl $op_trait2<&Self> for BigUint {
                type Output = BigUint;
                fn $op_fn2(self, other: &Self) -> Self::Output {
                    (&self).$op_fn2(&*other)
                }
            }
            impl<T: Into<Self>> $op_trait2<T> for BigUint {
                type Output = BigUint;
                fn $op_fn2(self, other: T) -> Self::Output {
                    self.$op_fn2(&other.into())
                }
            }
            impl<T: Into<Self>> $assign_trait2<T> for BigUint {
                fn $assign_fn2(&mut self, other: T) {
                    *self = (&*self).$op_fn2(&other.into());
                }
            }
            impl $assign_trait2<&Self> for BigUint {
                fn $assign_fn2(&mut self, other: &Self) {
                    *self = (&*self).$op_fn2(other);
                }
            }
        )*
    }
}
impl_op! {
    Add add, CheckedAdd checked_add = AddAssign add_assign,
    Sub sub = SubAssign sub_assign,
    BitAnd bitand = BitAndAssign bitand_assign,
    BitOr bitor = BitOrAssign bitor_assign,
    BitXor bitxor = BitXorAssign bitxor_assign
    ---
    Mul mul, CheckedMul checked_mul = MulAssign mul_assign,
    Div div = DivAssign div_assign,
    Rem rem = RemAssign rem_assign
}

#[derive(Clone)]
pub struct BitIter<T: Borrow<BigUint>> {
    num: T,
    bit: usize
}
impl<T: Borrow<BigUint> + Copy> Copy for BitIter<T> {}
impl<T: Borrow<BigUint>> BitIter<T> {
    pub fn new(num: T) -> Self {
        Self {
            num,
            bit: 0
        }
    }
    pub fn get(&mut self) -> Option<u8> {
        let digit = self.num.borrow().digits.get(self.bit / BITS)?;
        Some((digit >> (self.bit & BITS-1) & 1) as u8)
    }
    pub fn pos(&mut self) -> usize {
        self.bit
    }
    pub fn peek_backwards(&mut self, offset: usize) -> Option<u8> {
        let old = self.bit;
        self.bit = old.checked_sub(offset)?;
        let value = self.get();
        self.bit = old;
        value
    }
    pub fn peek_forwards(&mut self, offset: usize) -> Option<u8> {
        let old = self.bit;
        self.bit = old.checked_add(offset)?;
        let value = self.get();
        self.bit = old;
        value
    }
    pub fn forwards(&mut self, offset: usize) {
        self.bit += offset;
    }
    pub fn backwards(&mut self, offset: usize) {
        self.bit -= offset;
    }
    pub fn goto_end(&mut self) {
        self.bit = self.num.borrow().digits.len() * BITS - 1;
    }
}
impl<T: BorrowMut<BigUint>> BitIter<T> {
    pub fn set(&mut self, i: u8) {
        assert!(i < 2);
        let num = self.num.borrow_mut();
        let digit = num.digits.get_mut(self.bit / BITS).expect("BitIter::set failed because current digit was invalid");
        *digit &= !(1 << (self.bit & BITS-1));
        *digit |= (i as u32) << (self.bit & BITS-1);
    }
}
impl<T: Borrow<BigUint>> Iterator for BitIter<T> {
    type Item = u8;
    fn next(&mut self) -> Option<Self::Item> {
        let val = self.get()?;
        self.bit += 1;
        Some(val)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn bits_mut() {
        let mut num = BigUint::new(42u8);
        let mut bits = num.bits_mut();
        assert_eq!(bits.next().unwrap(), 0);
        assert_eq!(bits.next().unwrap(), 1);
        assert_eq!(bits.get().unwrap(), 0);
        bits.set(1);
        assert_eq!(bits.next().unwrap(), 1);
        assert_eq!(bits.next().unwrap(), 1);
        assert_eq!(bits.next().unwrap(), 0);
        assert_eq!(bits.next().unwrap(), 1);
    }
    #[test]
    fn ordering() {
        assert!(BigUint::new(4u8) < BigUint::new(5u8));
        assert!(BigUint::new(6u8) > BigUint::new(5u8));
        assert!(BigUint::new(5u8) == BigUint::new(5u8));
        assert!(BigUint::new(18446744073709551616u128) > BigUint::new(2u8));
        // This has caused issues because I was accidentally using the built-in Ord
        assert!(BigUint::new(100000000000000000000000000000000u128) > 1u8);
    }
    #[test]
    fn is_even() {
        assert!(!BigUint::new(std::u128::MAX).is_even());
        assert!((BigUint::new(std::u128::MAX) + 1u8).is_even());
    }
    #[test]
    fn format() {
        assert_eq!(format!("{:b}", BigUint::new(5u8)), "101");
        assert_eq!(format!("{:b}", BigUint::new(0u8)), "0");
        assert_eq!(format!("{}", BigUint::new(0u8)), "0");
        assert_eq!(format!("{}", BigUint::new(12345u16)), "12345");
        assert_eq!(format!("{}", BigUint::new(18446744073709551616u128)), "18446744073709551616");
        assert_eq!(format!("{}", BigUint::new(99999999999999999999999999999999u128)), "99999999999999999999999999999999");
        assert_eq!(format!("{:04}", BigUint::new(1u8)), "0001");
        assert_eq!(format!("{:04}", BigUint::new(10u8)), "0010");
        assert_eq!(format!("{:04}", BigUint::new(1000u16)), "1000");
        assert_eq!(format!("{:04}", BigUint::new(10000u16)), "10000");
        assert_eq!(format!("{:04}", BigUint::new(0u8)), "0000");
    }
    #[test]
    fn parse() {
        assert_eq!(BigUint::from_str_radix("1234", 10).unwrap(), BigUint::new(1234u16));
        assert_eq!(BigUint::from_str_radix("18446744073709551616", 10).unwrap(), BigUint::new(18446744073709551616u128));
        assert_eq!(BigUint::from_str_radix("101010", 2).unwrap(), BigUint::new(42u8));
        assert_eq!(BigUint::from_str_radix("99999999999999999999999999999999", 10).unwrap(), BigUint::new(99999999999999999999999999999999u128));
    }
    #[test]
    fn add() {
        assert_eq!(BigUint::new(1u8) + 2u8, BigUint::new(3u8));
        assert_eq!(BigUint::new(7u8) + 3u8, BigUint::new(10u8));
        assert_eq!(BigUint::new(15u8) + 30u8, BigUint::new(45u8));
        assert_eq!(BigUint::new(std::u64::MAX) + 1u8, BigUint::new(18446744073709551616u128));
    }
    #[test]
    fn sub() {
        assert_eq!(BigUint::new(15u8) - 2u8, BigUint::new(13u8));
        assert_eq!(BigUint::new(59u8) - 42u8, BigUint::new(17u8));
        assert_eq!(BigUint::new(18446744073709551616u128) - 1u8, BigUint::new(std::u64::MAX));
        assert_eq!(BigUint::new(1u8) - 1u8, BigUint::new(0u8));
    }
    #[test]
    fn shl() {
        assert_eq!(BigUint::new(1u8) << 70usize, BigUint::new(1180591620717411303424u128));
        assert_eq!(BigUint::new(std::u64::MAX) << 1usize, BigUint::new((std::u64::MAX as u128) << 1));
    }
    #[test]
    fn shr() {
        assert_eq!(BigUint::new(11u8) >> 2usize, BigUint::new(2u8));
        assert_eq!(BigUint::new(std::u64::MAX) >> 1usize, BigUint::new(std::u64::MAX >> 1));
        assert_eq!(BigUint::new(1180591620717411303424u128) >> 70usize, BigUint::new(1u8));
    }
    #[test]
    fn mul() {
        assert_eq!(BigUint::new(5u8) * 3u8, BigUint::new(15u8));
        BigUint::new(std::u64::MAX);
        BigUint::new(340282366920938463426481119284349108225u128);
        assert_eq!(BigUint::new(std::u64::MAX) * 2u8, BigUint::new(std::u64::MAX as u128 * 2));
        assert_eq!(BigUint::new(std::u64::MAX) * std::u64::MAX, BigUint::new(340282366920938463426481119284349108225u128));
        assert_eq!(BigUint::new(std::u64::MAX) * (std::u64::MAX-1), BigUint::new(340282366920938463408034375210639556610u128));
        assert_eq!(BigUint::new(99999999999999999999999999999999u128) * 10u8, BigUint::new(999999999999999999999999999999990u128));
        assert_eq!(BigUint::new(10000000000000000000000000000000u128) * 10u8, BigUint::new(100000000000000000000000000000000u128));
    }
    #[test]
    fn div() {
        assert_eq!(BigUint::new(8u8) / 2u8, BigUint::new(4u8));
        assert_eq!(BigUint::new(42u8) / 3u8, BigUint::new(14u8));
        assert_eq!(BigUint::new(std::u64::MAX) / 2u8, BigUint::new(std::u64::MAX / 2));
        assert_eq!(BigUint::new(999999u32) / 5u8, BigUint::new(199999u32));
        assert_eq!(BigUint::new(3u8) % 2u8, BigUint::new(1u8));
        assert_eq!(BigUint::new(14u8) % 10u8, BigUint::new(4u8));
        assert_eq!(BigUint::new(0u8) % 10u8, BigUint::new(0u8));
    }
    #[test]
    fn bitwise() {
        assert_eq!(BigUint::new(0b101u8) | 0b011u8, BigUint::new(0b111u8));
        assert_eq!(BigUint::new(0b101u8) & 0b011u8, BigUint::new(0b001u8));
        assert_eq!(BigUint::new(0b101u8) ^ 0b011u8, BigUint::new(0b110u8));
    }
    #[test]
    #[should_panic]
    fn underflow() {
        let _ = BigUint::new(1u8) - 2u8;
    }
    #[test]
    fn pow() {
        assert_eq!(BigUint::new(2u8).pow(128u8), BigUint::new(std::u128::MAX) + 1u8);
        assert_eq!(BigUint::new(8u8).pow(5u8), BigUint::new(32768u16));
    }
}
