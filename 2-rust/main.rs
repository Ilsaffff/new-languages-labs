use std::sync::mpsc;
use std::thread;

fn main() {
    let numbers = vec![1, 2, 3, 4, 5, 6, 7, 8, 9, 10];
    let chunk_size = 2;

    let (tx, rx) = mpsc::channel();

    let mut handles = vec![];

    for chunk in numbers.chunks(chunk_size) {
        let tx_clone = tx.clone();
        let chunk = chunk.to_vec();
        let handle = thread::spawn(move || {
            let sum_of_squares: i32 = chunk.iter().map(|&x| x * x).sum();
            tx_clone.send(sum_of_squares).unwrap(); 
        });

        handles.push(handle);
    }

    drop(tx);

    let mut total_sum = 0;
    for result in rx {
        total_sum += result;
    }

    for handle in handles {
        handle.join().unwrap();
    }

    println!("Сумма квадратов чисел: {}", total_sum);
}