use itertools::Itertools;
use rand;
use rand::seq::{IteratorRandom, SliceRandom};
use rayon::prelude::*;
use std::collections::HashMap;

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
    fn get_tuple(&self) -> (u8, u8) {
        (self.right_color, self.right_place)
    }
}

#[derive(Clone)]
pub struct Row {
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
    pub fn get_vals(&self) -> Vec<u8> {
        self.vals.clone()
    }
    fn get_response(&self, password: &Row) -> Response {
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

pub struct Space {
    current_space: Vec<Row>,
    full_space: Vec<Row>,
    num_colors: u8,
    num_pins: u8,
}
impl Space {
    pub fn build(num_colors: u8, num_pins: u8) -> Space {
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
    pub fn check_for_final_guess(&self, current_guess: &Row) -> Row {
        let space_size = self.get_space_size();
        if space_size == 1 {
            self.current_space[0].clone()
        } else if space_size > 1 {
            current_guess.clone()
        } else {
            panic!()
        }
    }
    pub fn choose_password(&self) -> Row {
        let mut rng = rand::rng();
        self.full_space.iter().choose(&mut rng).unwrap().clone()
    }
    fn trim_space(&mut self, guess: &Row, response: &Response) {
        //self.current_space = vec![Row::new(vec![1, 1, 1], 3)]
        self.current_space = self
            .current_space
            .iter()
            .cloned()
            .filter(|x| guess.get_response(x).get_tuple() == response.get_tuple())
            .collect()
    }
    fn info_of_guess(&self, guess: &Row) -> f32 {
        let responses: Vec<(u8, u8)> = self
            .current_space
            .iter()
            .cloned()
            .map(|x| guess.get_response(&x).get_tuple())
            .collect();
        let mut frequencies: HashMap<(u8, u8), u32> = HashMap::new();
        for &item in &responses {
            *frequencies.entry(item).or_insert(0) += 1;
        }
        let length = responses.iter().len() as f32;
        frequencies
            .iter()
            .map(|(_, v)| *v as f32 / length as f32)
            .map(|y| -y * y.log2())
            .sum()
    }
    pub fn select_best_guess(&self) -> Row {
        let info_of_guesses: Vec<(&Row, f32)> = self
            .full_space
            .par_iter()
            .map(|x| (x, self.info_of_guess(x)))
            .collect();
        let max_guess = info_of_guesses
            .iter()
            .map(|&(_, v)| v)
            .max_by(f32::total_cmp)
            .unwrap();
        let best_guesses: Vec<Row> = info_of_guesses
            .iter()
            .cloned()
            .filter(|&(_, b)| b == max_guess)
            .map(|(a, _)| a.clone())
            .collect();
        let mut rng = rand::rng();
        best_guesses.iter().choose(&mut rng).unwrap().clone()
    }
    pub fn get_space_size(&self) -> usize {
        self.current_space.iter().len()
    }
    pub fn run_turn(&mut self, password: &Row) -> Row {
        let guess = self.select_best_guess();
        self.trim_space(&guess, &guess.get_response(&password));
        self.check_for_final_guess(&guess)
    }
}

struct Combo {
    vals: Vec<u8>,
    num_colors: u8,
    num_pins: u8,
}
impl Combo {
    fn new(color_vector: Vec<u8>, num_colors: u8, num_pins: u8) -> Combo {
        Combo {
            vals: color_vector,
            num_colors,
            num_pins,
        }
    }
    fn combo_to_guess(&self) -> Row {
        let mut rng = rand::rng();
        let mut colors: Vec<u8> = (1..=self.num_colors).collect();
        let num_needed_colors = self.vals.iter().len();
        colors.shuffle(&mut rng);
        let colors = &colors[0..num_needed_colors];
        let mut guess: Vec<u8> = self
            .vals
            .iter()
            .zip(colors.iter())
            .flat_map(|(rep, col)| std::iter::repeat_n(col, *rep as usize))
            .cloned()
            .collect();
        guess.shuffle(&mut rng);
        Row::new(guess, self.num_colors)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn responses_make_sense() {
        let row_1 = Row::new(vec![1, 2, 2, 3, 5], 6);
        let row_2 = Row::new(vec![1, 6, 5, 3, 6], 6);
        let response = row_1.get_response(&row_2);
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
