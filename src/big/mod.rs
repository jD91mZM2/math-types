mod signed;
mod unsigned;

pub use self::signed::*;
pub use self::unsigned::*;

impl super::FractionNumber for BigInt {
    type UnsignedType = BigUint;

    fn big_denominator() -> Self {
        BigInt::new(100_000)
    }

    fn unsigned_abs(&self) -> Self::UnsignedType {
        self.clone().into_unsigned()
    }
    fn signed(unsigned: Self::UnsignedType) -> Self {
        BigInt::new(unsigned)
    }
}
