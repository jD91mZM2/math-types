use crate::Fraction;
use std::{
    fmt,
    ops::{Add, AddAssign, Div, DivAssign, Mul, MulAssign, Sub, SubAssign}
};

#[derive(Clone)]
enum MatrixRepr<'a, T: 'a + Clone> {
    Borrowed {
        borrow: &'a Matrix<'a, T>,
        row_off: usize,
        col_off: usize
    },
    Owned(Box<[T]>)
}
/// A matrix is a 2D array with numbers. It has special rules for what happens
/// when you add, subtract and multiply it. The lifetime of this type is
/// because you can get a subwindow of a matrix. Most of the time it's static.
#[derive(Clone)]
pub struct Matrix<'a, T: 'a + Clone> {
    inner: MatrixRepr<'a, T>,
    cols: usize,
    rows: usize
}
impl<T: Clone> Matrix<'static, T> {
    /// Create a new matrix that's just filled with the specified cell
    pub fn fill(cell: T, rows: usize, cols: usize) -> Self {
        Self::from_inner(vec![cell; cols * rows], rows, cols)
    }
    /// Create a new matrix from the inner slice of linear memory
    pub fn from_inner<I: Into<Box<[T]>>>(slice: I, rows: usize, cols: usize) -> Self {
        let slice = slice.into();
        assert_eq!(slice.len(), cols * rows, "incorrect cols/rows specified for internal vector");
        Self {
            inner: MatrixRepr::Owned(slice),
            rows,
            cols
        }
    }
}
impl<'a, T: Clone> Matrix<'a, T> {
    fn deref(&self, row: &mut usize, col: &mut usize) -> &Self {
        let mut me = self;
        if let MatrixRepr::Borrowed { borrow, row_off, col_off } = me.inner {
            me = borrow;
            *row += row_off;
            *col += col_off;
        }
        me
    }
    fn inner_mut(&mut self) -> &mut Box<[T]> {
        match self.inner {
            MatrixRepr::Borrowed { borrow, mut row_off, mut col_off } => {
                let mut clone = Vec::new();
                let borrow = borrow.deref(&mut row_off, &mut col_off);
                let slice = match borrow.inner {
                    MatrixRepr::Owned(ref slice) => slice,
                    _ => unreachable!("shouldn't be a borrow because it is dereferenced")
                };
                for row in row_off..row_off+self.rows {
                    let start = row * borrow.cols + col_off;
                    clone.extend_from_slice(&slice[start..start+self.cols])
                }
                self.inner = MatrixRepr::Owned(clone.into_boxed_slice());
                match self.inner {
                    MatrixRepr::Owned(ref mut slice) => slice,
                    _ => unreachable!("explicitly set to owned and shouldn't be a borrow")
                }
            },
            MatrixRepr::Owned(ref mut slice) => slice
        }
    }
    /// Get the internal linear memory
    pub fn into_inner(mut self) -> Box<[T]> {
        self.inner_mut();
        match self.inner {
            MatrixRepr::Owned(slice) => slice,
            _ => unreachable!("self shouldn't be a borrow after inner_mut call")
        }
    }
    /// Get the element at the specified row and column
    pub fn get(&self, mut row: usize, mut col: usize) -> &T {
        let borrow = self.deref(&mut row, &mut col);
        match borrow.inner {
            MatrixRepr::Owned(ref slice) => &slice[row * borrow.cols + col],
            _ => unreachable!("shouldn't be borrowed after a deref")
        }
    }
    /// Get a mutable rereference to the element at the specified row and column
    pub fn get_mut(&mut self, row: usize, col: usize) -> &mut T {
        let index = row * self.cols + col;
        &mut self.inner_mut()[index]
    }
    /// Swap the element at the first specified row and column with the element
    /// at the second specified row and column. Similarly to mem::swap or
    /// slice::swap, this does not invoke any destructors.
    pub fn swap(&mut self, row1: usize, col1: usize, row2: usize, col2: usize) {
        let index1 = row1 * self.cols + col1;
        let index2 = row2 * self.cols + col2;
        self.inner_mut().swap(index1, index2);
    }
    /// Return the number of rows
    pub fn rows(&self) -> usize {
        self.rows
    }
    /// Return the number of columns
    pub fn cols(&self) -> usize {
        self.cols
    }
    /// Return a submatrix starting at the specified position and going a
    /// specified size. This does not clone any data until mutated.
    pub fn window(&self, mut row: usize, mut col: usize, rows: usize, cols: usize) -> Matrix<T> {
        assert!(row + rows <= self.rows, "rows outside of range");
        assert!(col + cols <= self.cols, "cols outside of range");
        let borrow = self.deref(&mut row, &mut col);
        Matrix {
            inner: MatrixRepr::Borrowed {
                borrow,
                row_off: row,
                col_off: col
            },
            rows,
            cols
        }
    }
}
impl<T: NumericalCell> Matrix<'static, T> {
    /// Return the identity matrix for the specified size
    pub fn identity(size: usize) -> Self {
        let mut identity = Self::fill(T::zero(), size, size);
        for x in 0..size {
            *identity.get_mut(x, x) = T::one();
        }
        identity
    }
}
impl<'a, T: NumericalCell> Matrix<'a, T> {
    /// Apply Gauss Jordan Elimination on this matrix and the specified result.
    /// This returns true if it was successful. If so, this changes the matrix
    /// to the identity matrix. If not, the content of this matrix is
    /// undefined.
    pub fn gauss_jordan_eliminate(&mut self, result: &mut Matrix<T>) -> bool {
        // https://en.wikipedia.org/wiki/Gaussian_elimination
        // Helpful: https://www.codewithc.com/c-program-for-gauss-jordan-method/

        assert_eq!(self.rows, result.rows, "result must have an equal amount of rows");

        // For each column to eliminate
        for col in 0..self.cols {
            // For each row in that column, apply an operation:
            for row in 0..self.rows {
                if self.get(col, col).is_zero() {
                    // The diagonal element is zero, let's swap this row with some other row.
                    let mut swap = None;
                    for row2 in row..self.rows {
                        if !self.get(row2, col).is_zero() {
                            swap = Some(row2);
                            break;
                        }
                    }
                    match swap {
                        None => return false,
                        Some(swap) => {
                            for col2 in 0..self.cols {
                                self.swap(col, col2, swap, col2);
                            }
                            for col2 in 0..result.cols {
                                result.swap(col, col2, swap, col2);
                            }
                        }
                    }
                }

                if col == row {
                    // Divide this row by itself so the first column is 1.
                    let scale = *self.get(row, col);
                    for col2 in 0..self.cols {
                        *self.get_mut(row, col2) /= scale;
                    }
                    for col2 in 0..result.cols {
                        *result.get_mut(row, col2) /= scale;
                    }
                } else {
                    // TODO check correctness of the swapping
                    // Subtract each row with the row with the diagonal element,
                    // but scale it such that first column is 0.
                    let scale = *self.get(row, col) / *self.get(col, col);
                    for col2 in 0..self.cols {
                        *self.get_mut(row, col2) -= scale * *self.get(col, col2);
                    }
                    for col2 in 0..result.cols {
                        *result.get_mut(row, col2) -= scale * *result.get(col, col2);
                    }
                }
            }
        }
        true
    }
    /// A convenience function that applies Gauss Jordan Elimination on this
    /// matrix and its identity matrix in order to get the inverted matrix.
    /// Returns None if invertion fails.
    pub fn invert(&self) -> Option<Matrix<'static, T>> {
        let mut identity = Matrix::identity(self.cols);
        if self.clone().gauss_jordan_eliminate(&mut identity) {
            Some(identity)
        } else {
            None
        }
    }
}
impl<'a, T: Clone + Eq> Eq for Matrix<'a, T> {}
impl<'a, T: Clone + PartialEq> PartialEq for Matrix<'a, T> {
    fn eq(&self, other: &Self) -> bool {
        if self.rows != other.rows || self.cols != other.cols {
            return false;
        }
        for row in 0..self.rows {
            for col in 0..self.cols {
                if *self.get(row, col) != *self.get(row, col) {
                    return false;
                }
            }
        }
        true
    }
}
impl<'a, T: Clone + AddAssign<&'a T>> AddAssign<&'a Self> for Matrix<'a, T> {
    fn add_assign(&mut self, other: &'a Self) {
        assert_eq!(self.cols, other.cols);
        assert_eq!(self.rows, other.rows);
        for row in 0..self.rows {
            for col in 0..self.cols {
                *self.get_mut(row, col) += other.get(row, col);
            }
        }
    }
}
impl<'a, T: Clone + AddAssign<&'a T>> Add<&'a Self> for Matrix<'a, T> {
    type Output = Self;
    fn add(mut self, other: &'a Self) -> Self::Output {
        self += other;
        self
    }
}
impl<'a, T: Clone + SubAssign<&'a T>> SubAssign<&'a Self> for Matrix<'a, T> {
    fn sub_assign(&mut self, other: &'a Self) {
        assert_eq!(self.cols, other.cols);
        assert_eq!(self.rows, other.rows);
        for row in 0..self.rows {
            for col in 0..self.cols {
                *self.get_mut(row, col) -= other.get(row, col);
            }
        }
    }
}
impl<'a, T: Clone + SubAssign<&'a T>> Sub<&'a Self> for Matrix<'a, T> {
    type Output = Self;
    fn sub(mut self, other: &'a Self) -> Self::Output {
        self -= other;
        self
    }
}
impl<'a, T: 'static + Copy + Default + AddAssign<T> + Mul<T, Output = T>> Mul<Self> for &'a Matrix<'a, T> {
    type Output = Matrix<'static, T>;
    fn mul(self, other: Self) -> Self::Output {
        assert_eq!(self.cols, other.rows);

        let mut output = Vec::new();
        for row1 in 0..self.rows {
            for col2 in 0..other.cols {
                let mut value = T::default();
                for col1row2 in 0..self.cols {
                    value += *self.get(row1, col1row2) * *other.get(col1row2, col2);
                }
                output.push(value);
            }
        }

        Matrix::from_inner(output, self.rows, other.cols)
    }
}
impl<'a, T: 'static + Copy + Default + AddAssign<T> + Mul<T, Output = T>> MulAssign<&'a Self> for Matrix<'a, T> {
    fn mul_assign(&mut self, other: &'a Self) {
        *self = &*self * other;
    }
}
impl<'a, T: Clone + fmt::Debug> fmt::Debug for Matrix<'a, T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "[[")?;
        for row in 0..self.rows {
            if row > 0 {
                write!(f, "], [")?;
            }
            for col in 0..self.cols {
                if col > 0 {
                    write!(f, ", ")?;
                }
                write!(f, "{:?}", self.get(row, col))?;
            }
        }
        write!(f, "]]")?;
        Ok(())
    }
}

/// A trait for numerical types
pub trait NumericalCell: Copy + Div<Self, Output = Self> + DivAssign<Self> + Mul<Self, Output = Self> + SubAssign<Self> {
    /// Return zero for this type
    fn zero() -> Self;
    /// Return one for this type
    fn one() -> Self;
    /// Return true if this value is zero
    fn is_zero(self) -> bool;
}
macro_rules! impl_numcell {
    ($($int:ident),* --- $($float:ident),*) => {
        $(impl NumericalCell for $int {
            fn zero() -> Self {
                0
            }
            fn one() -> Self {
                1
            }
            fn is_zero(self) -> bool {
                self == 0
            }
        })*
        $(impl NumericalCell for $float {
            fn zero() -> Self {
                0.0
            }
            fn one() -> Self {
                1.0
            }
            fn is_zero(self) -> bool {
                self == 0.0
            }
        })*
    }
}
impl_numcell! {
    i8, i16, i32, i64, i128,
    u8, u16, u32, u64, u128
    ---
    f32, f64
}
impl NumericalCell for Fraction {
    fn zero() -> Self {
        Fraction::new(0, 1)
    }
    fn one() -> Self {
        Fraction::new(1, 1)
    }
    fn is_zero(self) -> bool {
        self.numerator() == 0
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    fn fraction_matrix<I, F>(input: &[I], cols: usize, rows: usize, callback: F) -> Vec<Fraction>
        where
            I: Into<Fraction> + Copy,
            F: FnOnce(&mut Matrix<Fraction>)
    {
        let mut matrix = Matrix::from_inner(
            input.iter()
                .map(|&n| n.into())
                .collect::<Vec<_>>(),
            cols,
            rows
        );
        callback(&mut matrix);
        matrix.into_inner()
            .into_iter()
            .map(|n| n.simplify())
            .collect::<Vec<_>>()
    }
    #[test]
    fn mul() {
        assert_eq!(
            &Matrix::from_inner(vec![
                1, 2, 3, 4, 5,
                6, 7, 8, 9, 10,
                11, 12, 13, 14, 15,
                16, 17, 18, 19, 20,
                21, 22, 23, 24, 25
            ], 5, 5) * &Matrix::from_inner(vec![
                2, 2, 2, 2, 2,
                2, 2, 2, 2, 2,
                2, 2, 2, 2, 2,
                2, 2, 2, 2, 2,
                2, 2, 2, 2, 2
            ], 5, 5),
            Matrix::from_inner(vec![
                30, 30, 30, 30, 30,
                80, 80, 80, 80, 80,
                130, 130, 130, 130, 130,
                180, 180, 180, 180, 180,
                230, 230, 230, 230, 230
            ], 5, 5)
        );
    }
    #[test]
    fn add() {
        assert_eq!(
            Matrix::from_inner(vec![
                1, 2, 3,
                4, 5, 6,
                7, 8, 9
            ], 3, 3) + &Matrix::from_inner(vec![
                3, 2, 1,
                6, 5, 4,
                9, 8, 7
            ], 3, 3),
            Matrix::from_inner(vec![
                 4,  4,  4,
                10, 10, 10,
                16, 16, 16
            ], 3, 3)
        );
    }
    #[test]
    fn sub() {
        assert_eq!(
            Matrix::from_inner(vec![
                1, 2, 3,
                4, 5, 6,
                7, 8, 9
            ], 3, 3) - &Matrix::from_inner(vec![
                1, 4, 7,
                2, 5, 8,
                3, 6, 9
            ], 3, 3),
            Matrix::from_inner(vec![
                0, -2, -4,
                2,  0, -2,
                4,  2,  0
            ], 3, 3)
        );
    }
    #[test]
    fn invert() {
        assert_eq!(
            fraction_matrix(
                &[
                     2, -1,  0,
                    -1,  2, -1,
                     0, -1,  2
                ],
                3, 3,
                |matrix| *matrix = matrix.invert().unwrap()
            ),

            vec![
                Fraction::new(3, 4), Fraction::new(1, 2), Fraction::new(1, 4),
                Fraction::new(1, 2), Fraction::new(1, 1), Fraction::new(1, 2),
                Fraction::new(1, 4), Fraction::new(1, 2), Fraction::new(3, 4)
            ]
        );
        assert_eq!(
            fraction_matrix(
                &[
                    4, 7,
                    2, 6
                ],
                2, 2,
                |matrix| *matrix = matrix.invert().unwrap()
            ),

            vec![
                Fraction::new(3, 5), Fraction::new(-7, 10),
                Fraction::new(-1, 5), Fraction::new(2, 5)
            ]
        );
        assert_eq!(
            fraction_matrix(
                &[
                    Fraction::new(1, 2), Fraction::new(-1, 2), Fraction::new(0, 1),
                    Fraction::new(0, 1), Fraction::new(1, 2), Fraction::new(-1, 2),
                    Fraction::new(-1, 2), Fraction::new(0, 1), Fraction::new(1, 1)
                ],
                3, 3,
                |matrix| *matrix = matrix.invert().unwrap()
            ),

            vec![
                Fraction::new(4, 1), Fraction::new(4, 1), Fraction::new(2, 1),
                Fraction::new(2, 1), Fraction::new(4, 1), Fraction::new(2, 1),
                Fraction::new(2, 1), Fraction::new(2, 1), Fraction::new(2, 1)
            ]
        );
        assert_eq!(
            fraction_matrix(
                &[
                    0, 1, 1,
                    1, 0, 1,
                    1, 1, 0
                ],
                3, 3,
                |matrix| *matrix = matrix.invert().unwrap()
            ),

            vec![
                Fraction::new(-1, 2), Fraction::new(1, 2), Fraction::new(1, 2),
                Fraction::new(1, 2), Fraction::new(-1, 2), Fraction::new(1, 2),
                Fraction::new(1, 2), Fraction::new(1, 2), Fraction::new(-1, 2)
            ]
        );
    }
    #[test]
    fn window() {
        let matrix = Matrix::from_inner(vec![
            1, 2, 3,
            4, 5, 6,
            7, 8, 9
        ], 3, 3);
        let mut window = matrix.window(1, 1, 1, 2);
        assert_eq!(format!("{:?}", window), "[[5, 6]]");
        {
            let window2 = window.window(0, 1, 1, 1);
            assert_eq!(format!("{:?}", window2), "[[6]]");
        }
        *window.get_mut(0, 1) = 7; // Copies on write
        assert_eq!(format!("{:?}", window), "[[5, 7]]");
    }
}
