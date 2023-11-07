use std::cmp::{max, min};
use std::fmt::Display;
use std::ops::{Add, Mul};

use itertools::EitherOrBoth::*;
use itertools::Itertools;
use num_traits::Zero;

#[derive(Debug, Clone)]
pub struct Polynomial<T> {
    pub coef: Vec<T>,
}

#[macro_export]
macro_rules! poly {
    ( $( $x:expr ),* ) => {
        Polynomial { coef: vec![ $( $x ),* ] }
    };
}

impl<T: PartialEq + Zero> PartialEq for Polynomial<T> {
    fn eq(&self, other: &Self) -> bool {
        self.coef
            .iter()
            .zip_longest(other.coef.iter())
            .all(|pair| match pair {
                Both(a, b) => a == b,
                Left(a) => a.is_zero(),
                Right(b) => b.is_zero(),
            })
    }
}

impl Display for Polynomial<i32> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut is_first_term = true;

        for (&c, e) in self.coef.iter().zip(0..self.coef.len()).rev() {
            if c == 0 {
                continue;
            }
            // add a conjunctive plus sign if not the first term
            // and the coefficient does not have a sign
            if is_first_term {
                is_first_term = false;
            } else if c > 0 {
                write!(f, "+")?;
            }
            // constant term can be directly printed
            if e == 0 {
                write!(f, "{c}")?;
                continue;
            }
            // coefficient +-1 or exponent 1 can be omitted
            if c == -1 {
                write!(f, "-")?;
            } else if c != 1 {
                write!(f, "{c}*")?;
            }
            write!(f, "x")?;
            if e != 1 {
                write!(f, "^{e}")?;
            }
        }

        // if all terms are zero, output a zero coefficient
        if is_first_term {
            write!(f, "0")?;
        }

        Ok(())
    }
}

impl<T: Add<Output = T>> Add for Polynomial<T> {
    type Output = Polynomial<T>;

    fn add(self, rhs: Polynomial<T>) -> Self::Output {
        Polynomial {
            coef: self
                .coef
                .into_iter()
                .zip_longest(rhs.coef)
                .map(|pair| match pair {
                    Both(a, b) => a + b,
                    Left(a) => a,
                    Right(b) => b,
                })
                .collect(),
        }
    }
}

impl<T> Mul for Polynomial<T>
where
    T: Add<Output = T>,
    for<'a> &'a T: Mul<Output = T>,
{
    type Output = Polynomial<T>;

    fn mul(self, rhs: Polynomial<T>) -> Self::Output {
        let (l1, l2) = (self.coef.len(), rhs.coef.len());
        Polynomial {
            coef: (0..l1 + l2 - 1)
                .map(|n| {
                    (max(0, n as isize - l2 as isize + 1) as usize..=min(l1 - 1, n))
                        .map(|i| &self.coef[i] * &rhs.coef[n - i])
                        .reduce(|a, b| a + b)
                        .unwrap() // iterator guaranteed not empty
                })
                .collect(),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn display() {
        let cases = [
            (poly![0], "0"),
            (poly![1], "1"),
            (poly![-1], "-1"),
            (poly![1, 1], "x+1"),
            (poly![1, -1], "-x+1"),
            (poly![1, 2], "2*x+1"),
            (poly![1, 0, 1], "x^2+1"),
            (poly![1, 0, 2], "2*x^2+1"),
        ];
        for (p, s) in cases {
            assert_eq!(p.to_string(), s);
        }
    }

    #[test]
    fn add() {
        let cases = [
            (poly![0], poly![1], poly![1]),
            (poly![1], poly![0, 1], poly![1, 1]),
            (poly![1], poly![-1, 1], poly![0, 1]),
            (poly![1, 1], poly![0, -1], poly![1]),
        ];
        for (a, b, c) in cases {
            assert_eq!(a + b, c);
        }
    }

    #[test]
    fn mul() {
        let cases = [
            (poly![0], poly![0, 1], poly![0]),
            (poly![1], poly![0, 1], poly![0, 1]),
            (poly![0, 1], poly![0, 1], poly![0, 0, 1]),
            (poly![1, 1], poly![1, 1], poly![1, 2, 1]),
        ];
        for (a, b, c) in cases {
            assert_eq!(a * b, c);
        }
    }
}
