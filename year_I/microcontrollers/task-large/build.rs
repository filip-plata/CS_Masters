extern crate cmake;
use cmake::Config;

fn main()
{
    let dst = Config::new("libconfig").cflag("-nostartfiles").build();

    println!("cargo:rustc-link-search=native={}", dst.display());
    println!("cargo:rustc-link-lib=static=config");
}
