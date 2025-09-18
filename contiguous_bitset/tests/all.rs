#![allow(unused_mut)]

use contiguous_bitset::*;

#[test]
fn test_insert_remove_contains() {
  let mut s = ContiguousBitset64::<bool>::new();
  dbg!("contains");
  assert!(!s.contains(false));
  assert!(!s.contains(true));

  dbg!("insert/remove false");
  assert!(!s.insert(false));
  assert!(s.contains(false));
  assert!(s.insert(false)); // double insert
  assert!(s.remove(false));
  assert!(!s.contains(false));

  dbg!("insert/remove true");
  assert!(!s.insert(true));
  assert!(s.contains(true));
  assert!(s.insert(true)); // double insert
  assert!(s.remove(true));
  assert!(!s.contains(true));
}

#[test]
fn test_iter() {
  let mut s = ContiguousBitset64::<bool>::new();

  let x: &[bool] = &[];
  let tmp: Vec<bool> = s.iter().collect();
  assert_eq!(x, tmp.as_slice());

  s.insert(true);
  let tmp: Vec<bool> = s.iter().collect();
  assert_eq!(&[true], tmp.as_slice());

  s.insert(false);
  let tmp: Vec<bool> = s.iter().collect();
  assert_eq!(&[false, true], tmp.as_slice());

  s.remove(true);
  let tmp: Vec<bool> = s.iter().collect();
  assert_eq!(&[false], tmp.as_slice());

  s.remove(false);
  let tmp: Vec<bool> = s.iter().collect();
  assert_eq!(x, tmp.as_slice());
}

#[test]
fn test_debug() {
  let mut s = ContiguousBitset64::<bool>::new();

  assert_eq!("{}", format!("{s:?}").as_str());

  s.insert(true);
  assert_eq!("{true}", format!("{s:?}").as_str());

  s.insert(false);
  assert_eq!("{false, true}", format!("{s:?}").as_str());

  s.remove(true);
  assert_eq!("{false}", format!("{s:?}").as_str());

  s.remove(false);
  assert_eq!("{}", format!("{s:?}").as_str());
}
