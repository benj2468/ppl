use week9::*;

#[test]
pub fn test_timer() {
    // Should print the time to calculate 10!
    assert_eq!(timer1(10), 3628800)
}

#[test]
pub fn test_timer_file() {
    timer_file()
}

#[test]
pub fn test_timer_and_field() {
    // Should print a timestamp, AND should perform the mod calculation
    assert_eq!(timer(2), 4);
    assert_eq!(timer(3), 9 % 5);
}

#[test]
pub fn field() {
    assert_eq!(some_calc(4), 0);
    assert_eq!(timer(2), 4)
}

#[test]
pub fn test_cacher() {
    // If you run this once, it should create a new file with the cached values, and print FETCHING FROM CACHE only on two of the indicated calls.
    // If you run it again without changing the cacher or foobar functions, then it should print FETCHING FROM CACHE on each call.
    cacher(2);
    cacher(3);
    // This should print: FETCHING FROM CACHE: ./.cacher_cacher.json
    cacher(2);
    // This should print: FETCHING FROM CACHE: ./.cacher_cacher.json
    cacher(3);

    foobar(3, "Benjamin".into());
    foobar(2, "Benjamin".into());
}

#[time_it]
pub fn timer1(a: u32) -> u64 {
    let mut fact = 1;
    for i in 1..(a + 1) {
        fact *= i as u64
    }
    fact
}

// This illustrates how both can be used for the same function.
#[time_it]
#[in_prime_field(5)]
pub fn timer(a: u32) -> u32 {
    #[field]
    let r = a.pow(2);
    println!("{:?}", r);
    let plus = a + 5;
    println!("{:?}", plus);
    // let b = r == ((a + 5) * (a + 5));
    // println!("{:?}", b);
    r
}

#[time_it("timer_file.txt")]
pub fn timer_file() {
    let mut i = 0;
    for _ in 0..100000 {
        i += 1;
    }
    assert!(i > 0)
}
#[cached]
pub fn cacher(a: u32) -> u32 {
    a % 2
}

#[cached]
pub fn foobar(a: u32, b: String) -> String {
    if a % 2 == 0 {
        b.to_uppercase()
    } else {
        b.to_lowercase()
    }
}

#[in_prime_field(5)]
pub fn some_calc(x: u32) -> u32 {
    let res = some_other_calc(x + 1) + 5;

    res
}

pub fn some_other_calc(x: u32) -> u32 {
    x
}
