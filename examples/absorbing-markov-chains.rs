extern crate math_types;

use math_types::{Fraction, Matrix};
use num_traits::*;

use std::io::{self, prelude::*};

fn main() -> Result<(), Box<std::error::Error>> {
    println!("Enter an input matrix of all states:");
    println!("Example:");
    println!("> Size: 2");
    println!("> 1/2\t1/2");
    println!("> 0\t1");
    println!();

    let stdin = io::stdin();
    let mut stdin = stdin.lock();

    print!("Size: ");
    io::stdout().flush()?;

    let mut buf = String::new();
    stdin.read_line(&mut buf)?;
    let size = buf.trim().parse()?;

    let mut matrix = Matrix::fill(Fraction::new(0, 1), size, size);

    for row in 0..size {
        buf.clear();
        stdin.read_line(&mut buf)?;
        let parts = buf.trim().split_whitespace();
        let mut col = 0;
        let mut total = Fraction::new(0, 1);
        for cell in parts {
            let cell = cell.parse()?;
            total += cell;
            if col >= size {
                eprintln!("too many columns for matrix of specified size");
                return Ok(());
            }
            *matrix.get_mut(row, col) = cell;
            col += 1;
        }
        if col < size {
            eprintln!("too few columns for matrix of specified size");
            return Ok(());
        }
        if !total.is_one() {
            eprintln!("Total value of all cells is not 1/1");
            return Ok(());
        }
    }

    println!();
    println!("Detecting absorbing states...");
    let mut end = size;
    let mut diagonal = 0;
    while diagonal < end {
        if matrix.get(diagonal, diagonal).is_one() {
            end -= 1;
            println!("Row #{}", diagonal + (size - end));
            for col in 0..size {
                matrix.swap(diagonal, col, end, col);
            }
            for row in 0..size {
                matrix.swap(row, diagonal, row, end);
            }
        } else {
            diagonal += 1;
        }
    }
    if end == size {
        eprintln!("No absorbing states!");
        return Ok(());
    }

    println!("{:?}", matrix);
    println!();
    let i = Matrix::identity(end);
    let q = matrix.window(0, 0, end, end);
    println!("({:?} - {:?})^-1", i, q);
    println!("{:?}^-1", i.clone() - &q);
    let n = (i - &q).inv().unwrap();
    println!("N = (I-Q)^-1 = {:?}", n);

    println!();
    println!("Expected numer of iterations to reach absorbing state: N1");
    println!("{:?}", &n * &Matrix::fill(Fraction::new(1, 1), end, 1));

    println!();
    println!("Chances to reach each absorbing state: NR");
    println!("{:?}", &n * &matrix.window(0, end, end, size - end));

    println!();

    Ok(())
}
