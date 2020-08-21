use std::net::TcpListener;

fn main() -> std::io::Result<()> {
    let listener = TcpListener::bind("127.0.0.1:7878")?;
    
    for stream in listener.incoming() {
        match stream {
            Ok(_) => println!("Connected!"),
            Err(_) => println!("Connection error!")
        }
    }
    Ok(())
}
