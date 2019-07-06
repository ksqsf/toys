use std;
use rfyl;

pub(crate) fn roll_expression(spec: &str) -> Result<i32, Box<dyn std::error::Error>> {
    rfyl::roll(spec.to_owned())
        .map(|v| v.get_result())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn roll_works() {
        assert!(roll_expression("1d10").is_ok());
        assert!(roll_expression("lady luck").is_err());
    }
}
