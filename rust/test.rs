fn calculate_count_unique(input_data: &str) -> Vec<i32> {
    // Split the input data by lines and filter out any empty lines
    let lines: Vec<&str> = input_data.lines().collect();

    // Calculate the cumulative sum with a reset at each non-numeric line
    let mut count = vec![0];
    let mut sum = 0;
    for &num in &lines {
        if num != "" {
            let tmp: i32 = num.parse().ok().unwrap();
            sum += tmp;
        } else {
            count.push(sum);
            sum = 0;
        }
    }
    println!("{:?}", lines);
    count
}

fn main() {
    let input_data = "5104\n6131\n\n3553\n4496\n\n5847\n3253\n2361\n5143\n\n";
    let result = calculate_count_unique(input_data);
    println!("{:?}", result);
}