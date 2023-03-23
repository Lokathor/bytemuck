
## `bytemuck_derive` changelog

## 1.4.1

* Move the `syn` dependency to use version 2.
  This should not affect the public API in any way.

## 1.4.0

* ByteEq and ByteHash derives will make Eq and Hash impls that treat the value as a &[u8]. This provides a large codegen improvement for some types.
* Derives of repr(int) enums should now accept byte literal values as the discriminant.

## 1.3.0

* Allow `repr(transparent)` to be used generically in `derive(Pod)`.

## 1.2.1

* Fixed a regression of the `align(N)` attribute that occurred during otherwise
  routine cleanup.

## 1.2.0

* Apparently our minimum required version of `syn` went up without anyone
  noticing for a while. Because of a bump in our `syn` requirements, we're also
  issuing this minor version bump in the `bytemuck_derive` crate. Because it's
  possible to *reduce* the minimum required version of a dep in only a patch
  release, I'm going to ratchet the required version of `syn` all the way up to
  "curret" (1.0.99). If absolutely necessary we could probably reduce the
  minimum `syn` version again in a patch release for 1.2, but I don't want to
  play this dance too much so I'd rather make each jump as big as can possibly
  be. [Issue 122](https://github.com/Lokathor/bytemuck/issues/122). **Note:**
  While the core `bytemuck` crate continues to keep building on rustc-1.34.0,
  the `bytemuck_derive` crate is considered an opt-in bonus feature (which
  doesn't do anything you couldn't trivially do yourself) and so it does not
  support a specific MSRV.

## 1.1.1

* Adjusted the license files to use full files rather than symlinks.
  [PR](https://github.com/Lokathor/bytemuck/pull/118)
  The license is unchanged, just no more symlinks.

## 1.1.0

* Updated to work with `bytemuck-1.9.0`

## 1.0.1

* [yanchith](https://github.com/yanchith) fixed the derive checks code to make clippy more happy.
[PR 45](https://github.com/Lokathor/bytemuck/pull/45)

## 1.0.0

* Initial stable release.
