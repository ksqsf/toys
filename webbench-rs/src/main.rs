//! webbench-rs is a Rust port of the original web-bench tool by ksqsf.

extern crate clap;
extern crate crossbeam;
extern crate http;
extern crate url;

use clap::{App, Arg, ArgMatches};
use std::io::Read;
use std::io::Write;
use std::net::TcpStream;
use std::process;
use std::str::FromStr;
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::mpsc;
use std::thread;
use std::time::Duration;
use url::Url;

struct Report {
    nbytes: usize,
    npages: usize,
    failed: usize,
}

struct Request {
    dest: Url,
    buf: Vec<u8>,
}

fn main() {
    let matches = get_matches();
    let url = matches.value_of("URL").unwrap();
    let clients: u32 = matches.value_of("clients").unwrap().parse().unwrap();
    let time: u64 = matches.value_of("time").unwrap().parse().unwrap();
    let force = matches.is_present("force");
    let reload = matches.is_present("reload");
    let http = matches.value_of("http").unwrap();
    let method = matches.value_of("method").unwrap();

    eprintln!("webbench-rs v0.1.0");
    let request = match build_request(url, reload, method, http) {
        Ok(r) => r,
        Err(e) => {
            eprintln!("{}", e);
            process::exit(1);
        }
    };

    // Show info
    print!(
        "Running info: {} client{}, running {} sec",
        clients,
        if clients == 1 { "" } else { "s" },
        time
    );
    if force {
        print!(", early socket close")
    }
    if reload {
        print!(", forcing reload")
    }
    println!(".");

    // Run benchmarks
    bench(request, time, clients, force);
}

/// Build HTTP request from CLI options.
fn build_request<'url>(
    url: &'url str,
    reload: bool,
    method: &str,
    http: &str,
) -> Result<Request, String> {
    let mut req: Vec<u8> = Vec::with_capacity(256);
    let mut http = match http {
        "0.9" => 0,
        "1.0" => 1,
        "1.1" => 2,
        _ => unreachable!(),
    };

    if method == "head" && http < 1 {
        http = 1
    }
    if method == "options" && http < 2 {
        http = 2
    }
    if method == "trace" && http < 2 {
        http = 2
    }

    req.write(method.to_uppercase().as_bytes()).expect("method");
    req.write(b" ").expect("delim");

    let url: Url = Url::parse(url).expect("parse");

    if url.scheme().to_lowercase() != "http" {
        return Err("Only HTTP is directly supported, set --proxy for others.".to_string());
    }
    req.write(url.path().as_bytes()).expect("path");

    if http == 1 {
        req.write(b" HTTP/1.0\r\n").expect("httpver");
    } else if http == 2 {
        req.write(b" HTTP/1.1\r\n").expect("httpver");
    }

    if http > 0 {
        req.write(b"User-Agent: WebBenchRs 0.1.0\r\n").expect("ua");
    }
    if reload {
        req.write(b"Pragma: no-cache\r\n").expect("reload");
    }
    if http > 1 {
        req.write(b"Connection: close\r\n").expect("close");
    }
    if http > 0 {
        req.write(b"\r\n").expect("end of req");
    }

    println!(
        "\nRequest:\n{}",
        String::from_utf8(req.clone()).expect("req")
    );

    Ok(Request {
        dest: url,
        buf: req,
    })
}

/// Run benchmarks
fn bench(req: Request, seconds: u64, num_clients: u32, force: bool) {
    let hostname = req.dest.host_str().unwrap_or("localhost");
    let port = req.dest.port().unwrap_or(80);

    // Check availability of the target
    if let Err(e) = TcpStream::connect((hostname, port)) {
        eprintln!("Cannot connect to server because {}, aborting", e);
        process::exit(1);
    }

    // Start
    let expired = AtomicBool::new(false);
    let (sender, receiver) = mpsc::channel();

    crossbeam::scope(|scope| {
        for _ in 0..num_clients {
            // Make the borrow checker happy :-)
            let (sender, expired, req) = (sender.clone(), &expired, &req);

            scope.spawn(move || {
                let mut failed = 0;
                let mut nbytes = 0;
                let mut npages = 0;
                let mut buf = Vec::with_capacity(1024);

                // Within the while body, continue means next try, and break means abort this client
                while !expired.load(Ordering::Relaxed) {
                    // Open connection
                    let conn = TcpStream::connect((hostname, port));
                    if let Err(e) = conn {
                        eprintln!("Cannot connect to server because {}.", e);
                        failed += 1;
                        break;
                    }
                    let mut conn = conn.unwrap();

                    // Send request
                    if let Err(e) = conn.write_all(req.buf.as_slice()) {
                        eprintln!("Cannot write because {}", e);
                        failed += 1;
                        break;
                    }

                    // Wait for reply if force = false
                    if force {
                        npages += 1
                    } else {
                        buf.clear();
                        match conn.read_to_end(&mut buf) {
                            Ok(n) => {
                                nbytes += n;
                                npages += 1
                            }
                            Err(_) => {
                                failed += 1;
                                continue;
                            }
                        }
                    }
                }

                sender
                    .send(Report {
                        nbytes,
                        npages,
                        failed,
                    })
                    .expect("send")
            });
        }

        thread::sleep(Duration::from_secs(seconds));
        expired.store(true, Ordering::Relaxed);

        // Statistics
        let mut nbytes = 0;
        let mut npages = 0;
        let mut failed = 0;
        for _ in 0..num_clients {
            let report = receiver.recv().expect("recv");
            nbytes += report.nbytes;
            npages += report.npages;
            failed += report.failed;
        }

        println!("");
        println!(
            "Speed={:.3} pages/min, {:.3} bytes/s",
            ((npages + failed) as f64) / (60.0 * seconds as f64),
            (nbytes as f64) / (seconds as f64)
        );
        println!("Requests: {} succeeded, {} failed", npages, failed)
    });
}

/// Parse command line arguments.
fn get_matches<'a>() -> ArgMatches<'a> {
    App::new("webbench")
        .version("0.1.0")
        .about("Benchmark WWW servers")
        .author("ksqsf")
        .arg(Arg::with_name("URL").required(true).validator(|v: String| {
            match Url::parse(v.as_str()) {
                Ok(_) => Ok(()),
                Err(e) => Err(e.to_string()),
            }
        }))
        .arg(
            Arg::with_name("force")
                .short("f")
                .long("force")
                .help("Don't wait for reply from server."),
        )
        .arg(
            Arg::with_name("reload")
                .short("r")
                .long("reload")
                .help("Send reload request - Pragma: no-cache."),
        )
        .arg(
            Arg::with_name("time")
                .short("t")
                .long("time")
                .default_value("30")
                .value_name("SEC")
                .validator(parse_validator::<u64>)
                .help("Run benchmark for <SEC> seconds."),
        )
        .arg(
            Arg::with_name("clients")
                .short("c")
                .long("clients")
                .default_value("1")
                .value_name("N")
                .validator(parse_validator::<u32>)
                .help("Run <N> HTTP clients at once"),
        )
        .arg(
            Arg::with_name("http")
                .short("h")
                .long("http")
                .default_value("1.1")
                .value_name("VER")
                .validator(|v: String| match v.as_str() {
                    "0.9" | "1.0" | "1.1" => Ok(()),
                    _ => Err("Only 0.9, 1.0, 1.1 are supported by --http".to_string()),
                })
                .help("HTTP version (0.9, 1.0, 1.1)."),
        )
        .arg(
            Arg::with_name("method")
                .short("m")
                .long("method")
                .default_value("get")
                .value_name("METHOD")
                .validator(|v: String| match v.as_str() {
                    "get" | "head" | "options" | "trace" => Ok(()),
                    _ => {
                        Err("Only get, head, options, trace are supported by --method".to_string())
                    }
                })
                .help("Which method to use (get, head, options, trace)."),
        )
        .get_matches()
}

/// Check whether s can be parsed as T
fn parse_validator<T>(s: String) -> Result<(), String>
where
    T: FromStr,
    <T as FromStr>::Err: ToString,
{
    match s.parse::<T>() {
        Ok(_) => Ok(()),
        Err(e) => Err(e.to_string()),
    }
}
