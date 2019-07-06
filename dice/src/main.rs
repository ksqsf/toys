use std;
use std::sync::Arc;
use std::cell::RefCell;
use gtk::prelude::*;

mod rolls;
use rolls::roll_expression;
mod state;
use state::State;
mod main_window;
use main_window::MainWindow;

fn main() {
    gtk::init().expect("Unable to start GTK3.");
    let gui = Arc::new(MainWindow::new());
    let state = Arc::new(RefCell::new(State::new()));
    for sides in &[4, 6, 8, 10, 12, 20, 100] {
        let spec = format!("1d{}", sides);
        let button = gui.button(&format!("rollD{}", sides));
        let gui = gui.clone();
        let state = state.clone();
        button.connect_clicked(move |_| {
            let mut state = state.borrow_mut();
            state.update_from_roll_result(roll_expression(&spec));
            gui.update_from(&state);
        });
    }

    {
        let button = gui.button("clearResult");
        let gui = Arc::clone(&gui);
        let state = Arc::clone(&state);
        button.connect_clicked(move |_| {
            let mut state = state.borrow_mut();
            state.value = 0;
            gui.update_from(&state);
        });
    }
    {
        let button = gui.button("halveDownResult");
        let gui = Arc::clone(&gui);
        let state = Arc::clone(&state);
        button.connect_clicked(move |_| {
            let mut state = state.borrow_mut();
            let prev_value = state.value;
            state.value = (f64::from(prev_value) / 2.0).floor() as i32;
            gui.update_from(&state);
        });
    }
    {
        let button = gui.button("halveUpResult");
        let gui = Arc::clone(&gui);
        let state = Arc::clone(&state);
        button.connect_clicked(move |_| {
            let mut state = state.borrow_mut();
            let prev_value = state.value;
            state.value = (f64::from(prev_value) / 2.0).ceil() as i32;
            gui.update_from(&state);
        });
    }

    {
        let button = gui.button("rollUser");
        let gui = Arc::clone(&gui);
        let state = Arc::clone(&state);
        button.connect_clicked(move |_| {
            let spec = gui.user_spec_entry().get_text().unwrap().to_string();

            let mut state = state.borrow_mut();
            state.update_from_roll_result(roll_expression(&spec));
            gui.update_from(&state);
        });
    }

    {
        let user_spec_entry = gui.user_spec_entry();
        let gui = Arc::clone(&gui);
        let state = Arc::clone(&state);
        user_spec_entry.connect_activate(move |entry| {
            let spec = entry.get_text().unwrap().to_string();

            let mut state = state.borrow_mut();
            state.update_from_roll_result(roll_expression(&spec));
            gui.update_from(&state);
        });
    }

    gui.start();
    gtk::main();
}
