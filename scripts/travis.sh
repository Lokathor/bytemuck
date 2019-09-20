#!/bin/bash

set -e

rustup component add clippy
cargo clippy

if [[ "$TARGET" != "" ]]; then rustup target install $TARGET; fi

if [[ "$TARGET" == "wasm32-"* && "$TARGET" != "wasm32-wasi" ]]; then
  cargo-web --version || cargo install cargo-web
  cargo web test --no-default-features $FLAGS --target=$TARGET
  cargo web test $FLAGS --target=$TARGET
  cargo web test --all-features $FLAGS --target=$TARGET

elif [[ "$TARGET" == *"-linux-android"* ]]; then
  export PATH=/usr/local/android-sdk/ndk-bundle/toolchains/llvm/prebuilt/linux-x86_64/bin:$PATH
  pushd linux-android
    cargo build --no-default-features --target=$TARGET $FLAGS
    cargo build --target=$TARGET $FLAGS
    cargo build --all-features --target=$TARGET $FLAGS
    # Don't test, can't run android emulators successfully on travis currently
  popd

elif [[ "$TARGET" == *"-apple-ios" || "$TARGET" == "wasm32-wasi" ]]; then
  cargo build --no-default-features --target=$TARGET $FLAGS
  cargo build --target=$TARGET $FLAGS
  cargo build --all-features --target=$TARGET $FLAGS
  # Don't test
  #   iOS simulator setup/teardown is complicated
  #   cargo-web doesn't support wasm32-wasi yet, nor can wasm-pack test specify a target

elif [[ "$TARGET" == *"-unknown-linux-gnueabihf" ]]; then
  #sudo apt-get update
  #sudo apt-get install -y gcc-arm-linux-gnueabihf g++-arm-linux-gnueabihf
  pushd generic-cross
    cargo build --no-default-features --target=$TARGET $FLAGS
    cargo build --target=$TARGET $FLAGS
    cargo build --all-features --target=$TARGET $FLAGS
    # Don't test
  popd

elif [[ "$TARGET" != "" ]]; then
  pushd generic-cross
    cargo test --no-default-features --target=$TARGET $FLAGS
    cargo test --target=$TARGET $FLAGS
    cargo test --all-features --target=$TARGET $FLAGS
  popd

else
  # Push nothing, target host CPU architecture
  cargo test --no-default-features $FLAGS
  cargo test $FLAGS
  cargo test --all-features $FLAGS
fi
