use itertools::Itertools;
use rayon;

struct Response {
    right_color: u8,
    right_place: u8,
}
impl Response {
    fn new(right_color: u8, right_place: u8) -> Response {
        Response {
            right_color,
            right_place,
        }
    }
    fn right_color(&self) -> u8 {
        self.right_color
    }
    fn right_place(&self) -> u8 {
        self.right_place
    }
}

#[derive(Clone)]
struct Row {
    vals: Vec<u8>,
    num_colors: u8,
    num_pins: u8,
}
impl Row {
    fn new(vals: Vec<u8>, num_colors: u8) -> Row {
        Row {
            vals: vals.clone(),
            num_colors,
            num_pins: vals.len() as u8,
        }
    }
    fn get_vals(&self) -> Vec<u8> {
        self.vals.clone()
    }
    fn get_response(&self, password: Row) -> Response {
        let pass = password.get_vals();
        let num_matching_pins: usize = self
            .vals
            .iter()
            .zip(pass.iter())
            .filter(|&(a, b)| a == b)
            .map(|(&a, _)| a)
            .count();
        let mut num_matching_colors: usize = (1..=self.num_colors)
            .into_iter()
            .map(|n| {
                [
                    self.vals.iter().filter(|&v| *v == n).count(),
                    pass.iter().filter(|&v| *v == n).count(),
                ]
                .into_iter()
                .min()
                .unwrap()
            })
            .sum();
        num_matching_colors -= num_matching_pins;
        let num_matching_pins = self
            .vals
            .iter()
            .zip(pass.iter())
            .filter(|&(a, b)| a == b)
            .map(|(&a, _)| a)
            .count();
        Response::new(num_matching_colors as u8, num_matching_pins as u8)
    }
}

struct Space {
    current_space: Vec<Row>,
    full_space: Vec<Row>,
    num_colors: u8,
    num_pins: u8,
}
impl Space {
    fn build(num_colors: u8, num_pins: u8) -> Space {
        let original_space: Vec<Row> = std::iter::repeat((1..=num_colors).into_iter())
            .take(num_pins.into())
            .multi_cartesian_product()
            .map(|a| Row::new(a, num_colors))
            .collect();
        Space {
            current_space: original_space.clone(),
            full_space: original_space,
            num_colors,
            num_pins,
        }
    }
    fn get_current_space(&self) -> Vec<Row> {
        self.current_space.clone()
    }
    fn get_full_space(&self) -> Vec<Row> {
        self.full_space.clone()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn responses_make_sense() {
        let row_1 = Row::new(vec![1, 2, 2, 3, 5], 6);
        let row_2 = Row::new(vec![1, 6, 5, 3, 6], 6);
        let response = row_1.get_response(row_2);
        assert_eq!(response.right_color, 1);
        assert_eq!(response.right_place, 2);
    }
    #[test]
    fn space_is_properly_created() {
        let num_pins: u8 = 4;
        let num_colors: u8 = 3;
        assert!(
            Space::build(num_colors, num_pins)
                .get_current_space()
                .iter()
                .len()
                == (num_colors.pow(num_pins.into()).into())
        );
    }
}
