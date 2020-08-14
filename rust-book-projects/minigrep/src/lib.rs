use std::error::Error;
use std::fs;

pub struct Config {
    pub query: String,
    pub filename: String,
}

impl Config {
    pub fn new(args: &[String]) -> Result<Config, &str> {
        if args.len() < 3 {
            return Err("missing arguments");
        }

        let query = args[1].clone();
        let filename = args[2].clone();

        Ok(Config { query, filename })
    }
}

pub fn run(cfg: Config) -> Result<(), Box<dyn Error>> {
    let contents = fs::read_to_string(cfg.filename)?;
    let results = search(&cfg.query, &contents)?;
    for result in results {
       println!("{}", result);
    }
    Ok(())
}

fn search<'a>(query: &str, contents: &'a str) -> Result<Vec<&'a str>, Box<dyn Error>> {
    let mut results =  Vec::new();
    for line in contents.lines() {
        if line.contains(query) {
            results.push(line);
        }
    }
    Ok(results)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn basic_test() {
        let query = "你好";
        let contents = "\
hello world
你好 world
hola world";
        match search(&query, &contents) {
            Ok(res) => assert_eq!(vec!["你好 world"], res),
            Err(_) => panic!("Test failed!")
        }
    }

}
