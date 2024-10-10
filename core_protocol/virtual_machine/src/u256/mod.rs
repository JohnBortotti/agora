use std::ops::{Add, Sub, Mul, Div, Rem};
use std::fmt;
use serde::Serialize;

#[derive(Serialize, Copy, Clone, PartialEq, Eq, Debug, PartialOrd)]
pub struct U256(pub u128, pub u128);

impl U256 {
  pub fn zero() -> U256 {
    U256(0, 0)
  }

  pub fn one() -> U256 {
    U256(0, 1)
  }
}

impl Add for U256 {
  type Output = U256;

  fn add(self, rhs: U256) -> U256 {
    let (low, carry) = self.1.overflowing_add(rhs.1);
    let high = self.0.wrapping_add(rhs.0).wrapping_add(carry as u128);
    U256(high, low)
  }
}

impl Sub for U256 {
  type Output = U256;

  fn sub(self, rhs: U256) -> U256 {
    let (low, borrow) = self.1.overflowing_sub(rhs.1);
    let high = self.0.wrapping_sub(rhs.0).wrapping_sub(borrow as u128);
    U256(high, low)
  }
}

impl Mul for U256 {
  type Output = U256;

  fn mul(self, rhs: U256) -> U256 {
    let U256(a_high, a_low) = self;
    let U256(b_high, b_low) = rhs;

    let low = a_low as u128 * b_low as u128;
    let high = a_high as u128 * b_high as u128;

    U256(high, low)
  }
}

impl Div for U256 {
  type Output = U256;

  fn div(self, rhs: U256) -> U256 {
    if rhs.0 == 0 && rhs.1 == 0 {
      panic!("Division by zero!");
    }

    if self.0 == 0 && rhs.0 == 0 {
      return U256(0, self.1 / rhs.1);
    }

    let lhs = self.0 as u128 * u128::MAX + self.1;
    let rhs_value = rhs.0 as u128 * u128::MAX + rhs.1;
    U256(0, lhs / rhs_value)
  }
}

impl Rem for U256 {
  type Output = U256;

  fn rem(self, rhs: U256) -> U256 {
    if rhs.0 == 0 && rhs.1 == 0 {
      panic!("Modulo by zero!");
    }

    if self.0 == 0 && rhs.0 == 0 {
      return U256(0, self.1 % rhs.1);
    }

    let lhs = self.0 as u128 * u128::MAX + self.1;
    let rhs_value = rhs.0 as u128 * u128::MAX + rhs.1;
    U256(0, lhs % rhs_value)
  }
}

impl fmt::Display for U256 {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    if self.0 == 0 {
      write!(f, "{}", self.1)
    } else {
      write!(f, "{}{}", self.0, self.1)
    }
  }
}

impl From<u8> for U256 {
  fn from(value: u8) -> U256 {
    U256(0, value as u128)
  }
}

impl From<u16> for U256 {
  fn from(value: u16) -> U256 {
    U256(0, value as u128)
  }
}

impl From<u32> for U256 {
  fn from(value: u32) -> U256 {
    U256(0, value as u128)
  }
}

impl From<u64> for U256 {
  fn from(value: u64) -> U256 {
    U256(0, value as u128)
  }
}

impl From<u128> for U256 {
  fn from(value: u128) -> U256 {
    U256(0, value)
  }
}

// Implement TryFrom for signed integer types
impl TryFrom<i8> for U256 {
  type Error = &'static str;

  fn try_from(value: i8) -> Result<U256, Self::Error> {
    if value < 0 {
      Err("Cannot convert negative value to U256")
    } else {
      Ok(U256(0, value as u128))
    }
  }
}

impl TryFrom<i16> for U256 {
  type Error = &'static str;

  fn try_from(value: i16) -> Result<U256, Self::Error> {
    if value < 0 {
      Err("Cannot convert negative value to U256")
    } else {
      Ok(U256(0, value as u128))
    }
  }
}

impl TryFrom<i32> for U256 {
  type Error = &'static str;

  fn try_from(value: i32) -> Result<U256, Self::Error> {
    if value < 0 {
      Err("Cannot convert negative value to U256")
    } else {
      Ok(U256(0, value as u128))
    }
  }
}

impl TryFrom<i64> for U256 {
  type Error = &'static str;

  fn try_from(value: i64) -> Result<U256, Self::Error> {
    if value < 0 {
      Err("Cannot convert negative value to U256")
    } else {
      Ok(U256(0, value as u128))
    }
  }
}

impl TryFrom<i128> for U256 {
  type Error = &'static str;

  fn try_from(value: i128) -> Result<U256, Self::Error> {
    if value < 0 {
      Err("Cannot convert negative value to U256")
    } else {
      Ok(U256(0, value as u128))
    }
  }
}
