pub trait UrlMatcher<T> {
    fn do_match<'a>(&self, s: &'a str) -> Option<(T, &'a str)>;
}

// here's the FixedWithNum struct to get you started.  You'll have to
// build the rest yourself!
pub struct FixedWidthNum {
    pub width: usize,
}

impl UrlMatcher<u64> for FixedWidthNum {
    fn do_match<'a>(&self, s: &'a str) -> Option<(u64, &'a str)> {
        if self.width > s.len() {
            return None;
        }
        let out: &'a str = &s[0..self.width];
        let num: u64 = out.parse().ok()?;
        Some((num, &s[self.width..]))
    }
}

pub struct AlphaMatcher {}

impl UrlMatcher<String> for AlphaMatcher {
    fn do_match<'a>(&self, s: &'a str) -> Option<(String, &'a str)> {
        let mut index = 0;
        for char in s.chars() {
            if char.is_alphabetic() {
                index += 1;
            } else {
                break;
            }
        }
        if index == 0 {
            return None;
        }
        let out = String::from(&s[0..index]);
        let rest: &'a str = &s[index..];
        Some((out, rest))
    }
}

pub struct StringAndThen<T> {
    pub string: String,
    pub matcher: Box<dyn UrlMatcher<T>>,
}

impl<T> StringAndThen<T> {
    pub fn new<U>(string: String, matcher: U) -> StringAndThen<T>
    where
        U: UrlMatcher<T> + 'static,
    {
        StringAndThen::<T> {
            string: string,
            matcher: Box::from(matcher),
        }
    }
}

impl<T> UrlMatcher<T> for StringAndThen<T> {
    fn do_match<'a>(&self, s: &'a str) -> Option<(T, &'a str)> {
        if !s.starts_with(&self.string) {
            return None;
        }
        let remaining = &s[self.string.len()..];
        self.matcher.do_match(remaining)
    }
}

pub struct EmptyMatcher {}

impl UrlMatcher<()> for EmptyMatcher {
    fn do_match<'a>(&self, s: &'a str) -> Option<((), &'a str)> {
        if s.is_empty() {
            Some(((), s))
        } else {
            None
        }
    }
}

pub struct AggMatcher<T, U> {
    pub matcher1: Box<dyn UrlMatcher<T>>,
    pub matcher2: Box<dyn UrlMatcher<U>>,
}

impl<T, U> AggMatcher<T, U> {
    pub fn new<V, W>(matcher1: V, matcher2: W) -> AggMatcher<T, U>
    where
        V: UrlMatcher<T> + 'static,
        W: UrlMatcher<U> + 'static,
    {
        AggMatcher::<T, U> {
            matcher1: Box::from(matcher1),
            matcher2: Box::from(matcher2),
        }
    }
}

impl<T, U> UrlMatcher<(T, U)> for AggMatcher<T, U> {
    fn do_match<'a>(&self, s: &'a str) -> Option<((T, U), &'a str)> {
        let (result1, rest) = self.matcher1.do_match(s)?;
        let (result2, rest) = self.matcher2.do_match(rest)?;
        Some(((result1, result2), rest))
    }
}

#[test]
fn test_fixed_width_num() {
    {
        // I put these test cases in their own scopes so I don't have
        // to keep making up new variable names

        let matcher = FixedWidthNum { width: 4 };
        let (a, b) = matcher.do_match("1234hello").unwrap();

        assert_eq!(a, 1234);
        assert_eq!(b, "hello");
    }
    {
        let matcher = FixedWidthNum { width: 3 };
        let (a, b) = matcher.do_match("1234hello56").unwrap();

        assert_eq!(a, 123);
        assert_eq!(b, "4hello56");
    }
    {
        let matcher = FixedWidthNum { width: 3 };
        let (a, b) = matcher.do_match("0014hello56").unwrap();

        assert_eq!(a, 1);
        assert_eq!(b, "4hello56");
    }

    {
        let matcher = FixedWidthNum { width: 3 };
        let (a, b) = matcher.do_match("123").unwrap();

        assert_eq!(a, 123);
        assert_eq!(b, "");
    }
    {
        let matcher = FixedWidthNum { width: 3 };
        let result = matcher.do_match("hello");

        assert_eq!(result, None);
    }

    {
        let matcher = FixedWidthNum { width: 3 };
        let result = matcher.do_match("12");

        assert_eq!(result, None);
    }
    {
        let matcher = FixedWidthNum { width: 3 };
        let result = matcher.do_match("12h3ello");

        assert_eq!(result, None);
    }
}
// UNCOMMENT THESE OTHER TESTS AS YOU GO!

#[test]
fn test_alpha_matcher() {
    {
        let matcher = AlphaMatcher {};
        let (a, b) = matcher.do_match("hello1234").unwrap();
        assert_eq!(a, "hello");
        assert_eq!(b, "1234");
    }

    {
        let matcher = AlphaMatcher {};
        let (a, b) = matcher.do_match("q1234").unwrap();
        assert_eq!(a, "q");
        assert_eq!(b, "1234");
    }

    {
        let matcher = AlphaMatcher {};
        let (a, b) = matcher.do_match("longlonglong").unwrap();
        assert_eq!(a, "longlonglong");
        assert_eq!(b, "");
    }

    {
        let matcher = AlphaMatcher {};
        let result = matcher.do_match("1234");
        assert_eq!(result, None);
    }

    {
        let matcher = AlphaMatcher {};
        let result = matcher.do_match("");
        assert_eq!(result, None);
    }
}

#[test]
fn test_string_and_then_matcher() {
    {
        let matcher = StringAndThen::new("http://foo.com/".to_string(), AlphaMatcher {});
        let (a, b) = matcher.do_match("http://foo.com/hello1234").unwrap();
        assert_eq!(a, "hello");
        assert_eq!(b, "1234");
    }
    {
        let matcher = StringAndThen::new("http://foo.com/".to_string(), AlphaMatcher {});
        let result = matcher.do_match("XXXXXXXXXXXXXXhello1234");
        assert_eq!(result, None);
    }
    {
        let matcher = StringAndThen::new("http://foo.com/".to_string(), AlphaMatcher {});
        let result = matcher.do_match("foo.com");
        assert_eq!(result, None);
    }

    {
        // this one fails cause the alphamatcher fails
        let matcher = StringAndThen::new("http://foo.com/".to_string(), AlphaMatcher {});
        let result = matcher.do_match("http://foo.com/1234");
        assert_eq!(result, None);
    }
}

#[test]
fn test_agg_matcher() {
    {
        let matcher = FixedWidthNum { width: 4 };
        let matcher2 = AlphaMatcher {};
        let matcher3 = AggMatcher::new(matcher, matcher2);
        let ((a3, a4), b3) = matcher3.do_match("1234hello5").unwrap();
        assert_eq!(1234, a3);
        assert_eq!("hello", a4);
        assert_eq!("5", b3);
    }

    {
        let matcher = FixedWidthNum { width: 4 };
        let matcher2 = AlphaMatcher {};
        let matcher3 = AggMatcher::new(matcher, matcher2);
        assert_eq!(None, matcher3.do_match("hello"));
    }

    {
        let matcher = FixedWidthNum { width: 4 };
        let matcher2 = AlphaMatcher {};
        let matcher3 = AggMatcher::new(matcher, matcher2);
        assert_eq!(None, matcher3.do_match("333344444"));
    }

    {
        // a bigger aggregate!
        let matcher = StringAndThen::new("/product_id/".to_string(), FixedWidthNum { width: 4 });
        let matcher2 = StringAndThen::new("/state_code/".to_string(), AlphaMatcher {});
        let matcher3 = StringAndThen::new(
            "http://foobar.com".to_string(),
            AggMatcher::new(matcher, matcher2),
        );
        let ((a3, a4), b3) = matcher3
            .do_match("http://foobar.com/product_id/1234/state_code/hello")
            .unwrap();
        assert_eq!(1234, a3);
        assert_eq!("hello", a4);
        assert_eq!("", b3);
    }
}

#[test]
fn test_empty_matcher() {
    {
        let matcher = EmptyMatcher {};
        let result = matcher.do_match("hello");
        assert_eq!(result, None);
    }
    {
        let matcher = EmptyMatcher {};
        let (a, b) = matcher.do_match("").unwrap();
        assert_eq!(a, ());
        assert_eq!(b, "");
    }
    {
        let matcher = StringAndThen::new("/contact-us".to_string(), EmptyMatcher {});
        let (a, b) = matcher.do_match("/contact-us").unwrap();
        assert_eq!(a, ());
        assert_eq!(b, "");
    }
    {
        let matcher = StringAndThen::new("/contact-us".to_string(), EmptyMatcher {});
        let result = matcher.do_match("/contact-us/extra");
        assert_eq!(result, None);
    }
    {
        let matcher = StringAndThen::new("/contact-us".to_string(), EmptyMatcher {});
        let result = matcher.do_match("/other-page");
        assert_eq!(result, None);
    }
}
