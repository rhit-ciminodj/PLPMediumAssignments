use std::io::{Read, Write};
use std::net::{TcpListener, TcpStream};
use std::sync::{Arc, Mutex};
use std::time::Duration;
use std::{env, thread};

mod fortune;
mod urlmatcher;
use urlmatcher::UrlMatcher;

use crate::fortune::FortuneReader;
use crate::urlmatcher::{AggMatcher, EmptyMatcher, FixedWidthNum, StringAndThen};

// adapted from https://gist.github.com/mjohnsullivan/e5182707caf0a9dbdf2d

fn handle_read(mut stream: &TcpStream) -> Option<String> {
    let mut buf = [0u8; 4096];
    match stream.read(&mut buf) {
        Ok(_) => {
            let req_str = String::from_utf8_lossy(&buf);
            println!("{}", req_str);
            // HTTP request line looks like: "GET /some/path HTTP/1.1"
            // We want the path, which is the second whitespace-delimited token
            let first_line = req_str.lines().next()?;
            let path = first_line.split_whitespace().nth(1)?;
            Some(path.to_string())
        }
        Err(e) => {
            println!("Unable to read stream: {}", e);
            None
        }
    }
}

fn handle_write(mut stream: TcpStream, reader_mutex: Arc<Mutex<FortuneReader>>) {
    let fortune = {
        let mut reader = reader_mutex.lock().unwrap();
        reader
            .next_fortune()
            .expect("oh my god NO FORTUNES? we're COMPLETELY screwed :( :( )")
    };
    let response = format!("HTTP/1.1 200 OK\r\nContent-Type: text/html; charset=UTF-8\r\n\r\n<html><body>{fortune}</body></html>\r\n");
    let response_bytes = response.as_bytes();
    match stream.write(response_bytes) {
        Ok(_) => println!("Response sent"),
        Err(e) => println!("Failed sending response: {}", e),
    }
}

struct UrlHandler<T> {
    matcher: Box<dyn UrlMatcher<T>>,
    handle_write: Box<dyn Fn(T) -> String>,
}

impl<T> UrlHandler<T> {
    pub fn new<U, W>(matcher: U, handle_write: W) -> Self
    where
        U: UrlMatcher<T> + 'static,
        W: Fn(T) -> String + 'static,
    {
        Self {
            matcher: Box::from(matcher),
            handle_write: Box::from(handle_write),
        }
    }
}

fn make_matcher<T>(handler: UrlHandler<T>) -> Box<dyn Fn(&str) -> Option<String>>
where
    T: 'static,
{
    Box::from(move |path: &str| {
        let (vals, _) = handler.matcher.do_match(path)?;
        Some((handler.handle_write)(vals))
    })
}

fn handle_client(mut stream: TcpStream, reader_mutex: Arc<Mutex<FortuneReader>>) {
    let path = handle_read(&stream).expect("request should have path");
    if let Some(response) = compute_response(&path) {
        stream.write(response.as_bytes()).unwrap();
    } else {
        handle_write(stream, reader_mutex);
    }
}

fn thread(listener: TcpListener, reader_mutex: Arc<Mutex<FortuneReader>>) {
    println!("Listening for connections on port {}", 8080);

    for stream in listener.incoming() {
        match stream {
            Ok(stream) => {
                handle_client(stream, reader_mutex.clone());
                thread::sleep(Duration::from_secs(1));
            }
            Err(e) => {
                println!("Unable to connect: {}", e);
            }
        }
    }
}

fn main() {
    let args: Vec<String> = env::args().collect();
    let arg = args.get(1).expect("you didnt pass an ARG you STUPID");
    let n_threads: i32 = arg.parse().expect("bro that aint an int");

    let reader = FortuneReader::new().unwrap();
    let reader_mutex = Arc::new(Mutex::new(reader));

    let listener = TcpListener::bind("127.0.0.1:8080").unwrap();

    for _ in 0..n_threads {
        let new_listener = listener.try_clone().expect("listener should let us clone");
        let reader_mutex_clone = reader_mutex.clone();
        thread::spawn(|| thread(new_listener, reader_mutex_clone));
    }
    loop {}
}

// Step 4: Implement this function to match URLs and return content.
// Return None if the URL can't be matched (webserver should fall back to fortune).
fn compute_response(path: &str) -> Option<String> {
    let mut handlers = vec![];
    handlers.push(make_matcher(UrlHandler::new(
        StringAndThen::new("/contact-us".to_string(), EmptyMatcher {}),
        |_| {
            "Contact using our emails: steigeo@rose-hulman.edu, ciminodj@rose-hulman.edu, and wanghuk@rose-hulman.edu".to_string()
        },
    )));
    handlers.push(make_matcher(UrlHandler::new(
        StringAndThen::new(
            "/calendar/".to_string(),
            AggMatcher::new(
                FixedWidthNum { width: 2 },
                StringAndThen::new("/".to_string(), FixedWidthNum { width: 4 }),
            ),
        ),
        |(month_num, year)| {
            let month = match month_num {
                1 => "January",
                2 => "February",
                3 => "March",
                4 => "April",
                5 => "May",
                6 => "June",
                7 => "July",
                8 => "August",
                9 => "September",
                10 => "October",
                11 => "November",
                12 => "December",
                _ => "bro wtf",
            };
            format!("Calendar for {month} {year}").to_string()
        },
    )));
    for handler in handlers {
        let response = (handler)(path);
        if response.is_some() {
            return response;
        }
    }
    None
}

// Step 4 tests

#[test]
fn test_contact_us() {
    let result = compute_response("/contact-us").unwrap();
    assert!(result.starts_with("Contact us"));
}

#[test]
fn test_calendar() {
    assert_eq!(
        compute_response("/calendar/04/2026"),
        Some("Calendar for April 2026".to_string())
    );
    assert_eq!(
        compute_response("/calendar/01/2000"),
        Some("Calendar for January 2000".to_string())
    );
    assert_eq!(
        compute_response("/calendar/12/1999"),
        Some("Calendar for December 1999".to_string())
    );
}

#[test]
fn test_no_match() {
    assert_eq!(compute_response("/"), None);
    assert_eq!(compute_response("/nonexistent"), None);
    assert_eq!(compute_response("/calendar/4/2026"), None);
}
