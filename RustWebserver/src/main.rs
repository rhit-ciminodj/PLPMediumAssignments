use std::io::{Read, Write};
use std::net::{TcpListener, TcpStream};
use std::sync::{Arc, Mutex};
use std::time::Duration;
use std::{env, thread};

mod fortune;
mod urlmatcher;
use urlmatcher::UrlMatcher;

use crate::fortune::FortuneReader;

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

fn handle_client(stream: TcpStream, reader_mutex: Arc<Mutex<FortuneReader>>) {
    let _path = handle_read(&stream);
    handle_write(stream, reader_mutex);
}

fn thread(listener: TcpListener, reader_mutex: Arc<Mutex<FortuneReader>>) {
    println!("Listening for connections on port {}", 8080);

    for stream in listener.incoming() {
        thread::sleep(Duration::from_secs(2));
        match stream {
            Ok(stream) => {
                handle_client(stream, reader_mutex.clone());
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
fn compute_response(_path: &str) -> Option<String> {
    None // TODO: implement URL matching routes
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
