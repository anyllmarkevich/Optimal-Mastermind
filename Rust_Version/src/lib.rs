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

#[derive(Clone, Debug)]
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
        let length = responses.len() as f32;
        frequencies
            .iter()
            .map(|((_, _), v)| {
                //println!("{:?}", v);
                *v as f32 / length as f32
            })
            .map(|y| {
                //println!("{:?}", y);
                -y * y.log2()
            })
            .sum()
    }
    pub fn select_best_guess(&self) -> Row {
        let info_of_guesses: Vec<(&Row, f32)> = self
            .full_space
            .par_iter()
            .map(|x| (x, self.info_of_guess(x)))
            .collect();
        println!("{:?}", info_of_guesses);
        let max_guess = info_of_guesses
            .iter()
            .map(|&(_, v)| v)
            .max_by(f32::total_cmp)
            .unwrap();
        println!("Max guess: {:?}", max_guess);
        let best_guesses: Vec<Row> = info_of_guesses
            .iter()
            .cloned()
            .filter(|&(_, b)| b == max_guess)
            .map(|(a, _)| a.clone())
            .collect();
        let mut rng = rand::rng();
        //println!("{:?}", best_guesses);
        best_guesses.iter().choose(&mut rng).unwrap().clone()
    }
    pub fn get_space_size(&self) -> usize {
        self.current_space.iter().len()
    }
    pub fn run_turn(&mut self, password: &Row, first_turn_optimization: &bool) -> Row {
        if *first_turn_optimization {
            let combos = Combo::find_possible_starting_combos(self.num_colors, self.num_pins);
            let options = combos.iter().map(|x| x.combo_to_guess()).collect();
            let temp_space = Space {
                current_space: self.current_space.clone(),
                full_space: options,
                num_colors: self.num_colors,
                num_pins: self.num_pins,
            };
            let guess = temp_space.select_best_guess();
            self.trim_space(&guess, &guess.get_response(&password));
            self.check_for_final_guess(&guess)
        } else {
            let guess = self.select_best_guess();
            self.trim_space(&guess, &guess.get_response(&password));
            self.check_for_final_guess(&guess)
        }
    }
}

#[derive(Clone, Debug)]
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
    fn get_vals(&self) -> Vec<u8> {
        self.vals.clone()
    }
    fn get_num_used_colors(&self) -> u8 {
        self.vals.len() as u8
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
    fn next_num_search(
        current: &mut Vec<u8>,
        n_left: &u8,
        possible_combos: &mut Vec<Combo>,
        num_colors: &u8,
        num_pins: &u8,
    ) {
        if *n_left == 0 && current.iter().len() as u8 <= *num_colors {
            possible_combos.push(Combo::new(current.to_vec(), *num_colors, *num_pins));
        } else if *n_left != 0 {
            for i in 1..=*n_left {
                if current.iter().len() == 0 {
                    let mut temp_current = current.clone();
                    temp_current.push(i);
                    Self::next_num_search(
                        &mut temp_current,
                        &(n_left - i),
                        possible_combos,
                        &num_colors,
                        &num_pins,
                    )
                } else if i >= current[current.len() - 1] {
                    let mut temp_current = current.clone();
                    temp_current.push(i);
                    Self::next_num_search(
                        &mut temp_current,
                        &(n_left - i),
                        possible_combos,
                        &num_colors,
                        &num_pins,
                    )
                }
            }
        }
    }
    pub fn find_possible_starting_combos(num_colors: u8, num_pins: u8) -> Vec<Combo> {
        let mut possible_combos: Vec<Combo> = Vec::new();
        Self::next_num_search(
            &mut Vec::new(),
            &num_pins,
            &mut possible_combos,
            &num_pins,
            &num_colors,
        );
        possible_combos
            .iter()
            .cloned()
            .filter(|x| x.get_num_used_colors() <= num_colors)
            .collect()
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
    #[test]
    fn possible_combos_correct() {
        let combos = Combo::find_possible_starting_combos(4, 6);
        let correct_output: Vec<Vec<u8>> = vec![
            vec![1, 1, 1, 3],
            vec![1, 1, 2, 2],
            vec![1, 1, 4],
            vec![1, 2, 3],
            vec![1, 5],
            vec![2, 2, 2],
            vec![2, 4],
            vec![3, 3],
            vec![6],
        ];
        for i in 0..combos.len() {
            assert_eq!(combos[i].get_vals(), correct_output[i]);
        }
    }
    #[test]
    fn possible_starting_combos_checking_ground() {
        let combos = Combo::find_possible_starting_combos(3, 4);
        for i in combos {
            println!("{:?}", i.get_vals());
        }
    }
    #[test]
    fn check_on_guess_conversion() {
        let test = Combo::new(vec![2, 2], 3, 4);
        println!("Vals: {:?}", test.get_vals());
        println!("G: {:?}", test.combo_to_guess().get_vals());
    }
}
