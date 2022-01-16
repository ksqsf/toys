use image::io::Reader as ImageReader;
use image::Rgba;
use std::env;
use std::io::Result;

fn main() {
    let args = env::args().collect::<Vec<_>>();
    let path = &args[1];
    println!("open file: {}", path);
    let mut img = ImageReader::open(path).unwrap().decode().unwrap();
    let mut buf = img.as_mut_rgba8().unwrap();
    let pixels = buf.pixels_mut();
    for Rgba([r, g, b, a]) in pixels {
        *r = ((*r as f64 / 256.0).powf(1.5) * 256.0) as u8;
        *g = ((*g as f64 / 256.0).powf(0.8) * 256.0) as u8;
        *b = ((*b as f64 / 256.0).powf(1.5) * 256.0) as u8;
    }
    img.save("/tmp/rust.png").unwrap();
}
