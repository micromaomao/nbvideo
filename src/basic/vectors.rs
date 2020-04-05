//! Vector utilities: used to represent points, pixel position, direction, etc.
//!
//! Floating point components can't be infinite or null. They can either be "unconstrained", which
//! means that they can take any value, or "discrete" as in [`DiscreteVec2f`], in which they can only
//! take values that are multiples of a certain step number.

use std::hash::{Hash, Hasher};

/// Vector with two unconstrained floating point components.
///
/// Neither ones are allowed to be infinite or NaN.
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Vec2f(f32, f32);

/// Vector with three unconstrained floating point components.
///
/// Neither ones are allowed to be infinite or NaN.
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Vec3f(f32, f32, f32);

/// Vector with three discrete floating point components.
///
/// Neither ones are allowed to be infinite or NaN. All must be a multiple of `step`.
/// If they are not, they are rounded to the nearest `step` multiple.
///
/// Equality comparison does not check step property.
#[derive(Debug, Clone, Copy)]
pub struct DiscreteVec2f {
	inner: Vec2f,
	step: f32
}

impl From<DiscreteVec2f> for Vec2f {
	fn from(v: DiscreteVec2f) -> Vec2f {
		v.inner
	}
}

impl DiscreteVec2f {
	fn round(number: f32, step: f32) -> f32 {
		(number / step).round() * step
	}

	pub fn new(vec: (f32, f32), step: f32) -> Self {
		let inner: Vec2f = vec.into();
		if step <= 0f32 {
			panic!("step must be positive.");
		}
		Self{
			inner: (Self::round(inner.0, step), Self::round(inner.1, step)).into(),
			step
		}
	}

	pub fn x(&self) -> f32 {
		self.inner.0
	}

	pub fn y(&self) -> f32 {
		self.inner.1
	}
}

impl PartialEq for DiscreteVec2f {
	fn eq(&self, other: &Self) -> bool {
		let eplison = 1e-10f32 * self.step;
		(self.x() - other.x()).abs() < eplison && (self.y() - other.y()).abs() < eplison
	}
}

impl Eq for DiscreteVec2f {}

impl From<DiscreteVec2f> for (f32, f32) {
	fn from(v: DiscreteVec2f) -> Self {
		(v.inner.0, v.inner.1)
	}
}

impl Hash for DiscreteVec2f {
	fn hash<H: Hasher>(&self, state: &mut H) {
		let (ix, iy) = (self.x() * self.step, self.y() * self.step);
		const EPLISON: f32 = 1e-10;
		if (ix - ix.round()).abs() > EPLISON || (iy - iy.round()).abs() > EPLISON {
			unreachable!();
		}
		let ix = ix.round() as i32;
		let iy = iy.round() as i32;
		state.write_i32(ix);
		state.write_i32(iy);
	}
}

#[test]
fn test_discrete_round() {
	assert_eq!(DiscreteVec2f::round(0f32, 0.5f32), 0f32);
	assert_eq!(DiscreteVec2f::round(1f32, 0.5f32), 1f32);
	assert_eq!(DiscreteVec2f::round(-2.5f32, 0.5f32), -2.5f32);
	assert_eq!(DiscreteVec2f::round(-2.501f32, 0.5f32), -2.5f32);
	assert_eq!(DiscreteVec2f::round(-2.499f32, 0.5f32), -2.5f32);
	assert_eq!(DiscreteVec2f::round(2.499f32, 0.5f32), 2.5f32);
}

/// 2d vector with integer coordinates
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Vec2i(i32, i32);

/// 3d vector with integer coordinates
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Vec3i(i32, i32, i32);

macro_rules! if_float_check_valid {
	(f32,$val:expr) => {
		if $val.is_infinite() {
			panic!("One of the component contains an infinite number, which is not allowed.");
		}
		if $val.is_nan() {
			panic!("One of the component contains NaN, which is not allowed.");
		}
	};
	($type:ident,$val:expr) => {}
}

use std::fmt::{Formatter, Debug, Display, Result as FmtResult};

macro_rules! __print_vec_write_value {
	($first:ident, $f:ident, $val:expr) => {
		if !$first {
			write!($f, ",")?;
		}
		$first = false;
		write!($f, "{}", $val)?;
	}
}

macro_rules! impl_vec_rest {
	{$comp0:ident,$comp1:ident} => {
		pub fn x(&self) -> $comp0 {
			self.0
		}

		pub fn y(&self) -> $comp1 {
			self.1
		}
	};
	{$comp0:ident,$comp1:ident,$comp2:ident} => {
		impl_vec_rest!{$comp0,$comp1}
		pub fn z(&self) -> $comp2 {
			self.2
		}
	}
}

macro_rules! component_num_to_letter {
	(0,$x:ident,$y:ident,$z:ident) => { $x };
	(1,$x:ident,$y:ident,$z:ident) => { $y };
	(2,$x:ident,$y:ident,$z:ident) => { $z };
}

macro_rules! impl_vec {
	{$vec_type:ident,$($num:tt : $comp_type:ident),+} => {
		impl $vec_type {
			pub fn new($(component_num_to_letter!($num,x,y,z): $comp_type),+) -> Self {
				Self($(component_num_to_letter!($num,x,y,z)),+)
			}
		}

		impl From<($($comp_type),+)> for $vec_type {
			fn from(truple: ($($comp_type),+)) -> Self {
				$(if_float_check_valid!($comp_type,truple.$num);)+
				Self($(truple.$num),+)
			}
		}

		impl From<$vec_type> for ($($comp_type),+) {
			fn from(v: $vec_type) -> Self {
				($(v.$num),+)
			}
		}

		impl Eq for $vec_type {}

		impl Display for $vec_type {
			#[allow(unused_assignments)]
			fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
				write!(f, concat!(stringify!($vec_type), "("))?;
				let mut first = true;
				$(__print_vec_write_value!(first, f, self.$num);)+
				write!(f, ")")
			}
		}

		impl $vec_type {
			impl_vec_rest!{$($comp_type),+}
		}

		impl Hash for $vec_type {
			fn hash<H: Hasher>(&self, state: &mut H) {
				$(state.write(&self.$num.to_ne_bytes());)+
			}
		}
	};
}

impl_vec!{Vec2f, 0:f32, 1:f32}
impl_vec!{Vec3f, 0:f32, 1:f32, 2:f32}
impl_vec!{Vec2i, 0:i32, 1:i32}
impl_vec!{Vec3i, 0:i32, 1:i32, 2:i32}

#[test]
fn test_truple_convert_nopanic() {
	assert_eq!(Vec2f::from((0f32, 1f32)), Vec2f(0f32, 1f32));
	assert_eq!(Vec3f::from((-1.5f32, 1f32, 1e20f32)), Vec3f(-1.5f32, 1f32, 1e20f32));
	assert_eq!(Into::<Vec2i>::into((1i32,2i32)), Vec2i(1,2));
	assert_eq!(Into::<Vec3i>::into((2i32,2i32,1i32)), Vec3i(2,2,1));

	let t: (i32, i32) = Vec2i(1,2).into();
	assert_eq!(t, (1i32, 2i32));

	assert_ne!(Vec2i(2,1), Vec2i(1,2));
	assert_ne!(Vec2f(2.0f32,1.0f32), Vec2f(1.0f32,2.0f32));
}

#[test]
#[should_panic]
fn test_convert_should_panic_1() {
	Vec2f::from((f32::INFINITY, 0f32));
}

#[test]
#[should_panic]
fn test_convert_should_panic_2 () {
	Vec3f::from((0f32, f32::NAN, -1f32));
}

#[test]
fn test_display() {
	assert_eq!(format!("{}", &Vec2f(-0.0,3.14)), "Vec2f(0,3.14)");
}

#[test]
fn test_discrete_2f() {
	assert_eq!(DiscreteVec2f::new((1.51f32, 0.45f32), 0.5f32), DiscreteVec2f{
		inner: Vec2f(1.5f32, 0.5f32), step: 0.25f32
	});
	assert_eq!(DiscreteVec2f{
		inner: Vec2f(1.500000000000000001f32, 0.5f32), step: 0.25f32
	}, DiscreteVec2f{
		inner: Vec2f(1.5f32, 0.5f32), step: 0.25f32
	});
	assert_ne!(DiscreteVec2f{
		inner: Vec2f(1.7f32, 0.5f32), step: 0.25f32
	}, DiscreteVec2f{
		inner: Vec2f(1.5f32, 0.5f32), step: 0.25f32
	});
}
