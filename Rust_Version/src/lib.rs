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
