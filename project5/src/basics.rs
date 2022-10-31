/**
    Returns the sum 1 + 2 + ... + n
    If n is less than 0, return -1
**/

pub fn gauss(n: i32) -> i32 {
    if n < 0 { -1 }
    else if n == 0 { 0 }
    else{
        let x = gauss(n - 1);
        x + n
    }
}

/**
    Returns the number of elements in the list that 
    are in the range [s,e]
**/
pub fn in_range(ls: &[i32], s: i32, e: i32) -> i32 {
    let mut n = 0;
    for i in ls.iter(){
        if i == &e { 
            n = n + 1;
            break;
        }
        else if i >= &s && i < &e{
            n = n + 1;
        }
    }
    return n;
}

/**
    Returns true if target is a subset of set, false otherwise

    Ex: [1,3,2] is a subset of [1,2,3,4,5]
**/
pub fn subset<T: PartialEq>(set: &[T], target: &[T]) -> bool {
    let mut boolean = false;

    if target == []{
        return true;
    }

    for item in target.iter(){
        boolean = false;
        for i in set.iter(){
            if item == i{
                boolean = true;
            }
        }

        if boolean == false{
            break;
        }



    }

    return boolean;


    
}



/**
    Returns the mean of elements in ls. If the list is empty, return None
    It might be helpful to use the fold method of the Iterator trait
**/
pub fn mean(ls: &[f64]) -> Option<f64> {
    let n: f64 = ls.len() as f64;
    let sum: f64 = ls.iter().sum();
    if n == 0.0{
        None
    }
    else{
        Some(sum/n)
    }
}

/**
    Converts a binary number to decimal, where each bit is stored in order in the array
    pub fn to_decimal(ls: &[i32]) -> i32 {
    let ls2: Vec<i32> = ls.iter().copied().rev().collect();
    let mut sum = 0;
    let mut i = 0;
    while i < ls2.len() {
        if ls2.get(i) == Some(&1) {
            sum += 2_i32.pow(i as u32);
        }
        i += 1;
    }
    sum
}
    
    Ex: to_decimal of [1,0,1,0] returns 10
**/
pub fn to_decimal(ls: &[i32]) -> i32 {
    let length: u32 =  ls.len() as u32;
    let mut ma = i32::pow(2,length - 1);
    let mut n = 0;

    for i in ls.iter(){
        if i == &1{
            n = n + ma;
            ma = ma/2;
        }
        else{
            ma = ma/2;
        }
    }
    return n;
}

/**
    Decomposes an integer into its prime factors and returns them in a vector
    You can assume factorize will never be passed anything less than 2

    Ex: factorize of 36 should return [2,2,3,3] since 36 = 2 * 2 * 3 * 3
**/
// fn firstfac(x: u32) -> u32 {
//     if x % 2 == 0 {
//         return 2;
//     };
//     for n in (1..).map(|m| 2 * m + 1).take_while(|m| m * m <= x) {
//         if x % n == 0 {
//             return n;
//         };
//     }

//     return x;
// }


pub fn factorize(n: u32) -> Vec<u32> {
    let mut v:Vec<u32> = Vec::new();
    let mut m = n;


    while m % 2 == 0 {
        v.push(2);
        m /= 2;
    }
    
    let mut i:u32 = 3;
    while i < (n as f64).sqrt() as u32 {
        while m % i == 0 {
            v.push(i as u32);
            m /= i;
        }
        i += 2;
    }
    if m > 2 {
        v.push(m);
    }
    v
    // if n <= 1 {
    //     return vec![];
    // };

    // let mut lst: Vec<u32> = Vec::new();
    // let mut hooplha = n;


    // loop {
    //     let m = firstfac(hooplha);
    //     lst.push(m);
    //     if hooplha == m {
    //         break;
    //     }
    //     while hooplha % m == 0 {
    //         hooplha /= m;
    //     }
    //     if hooplha == 1 {
    //         break;
    //     }
    // }
    // return lst;
}

/** 
    Takes all of the elements of the given slice and creates a new vector.
    The new vector takes all the elements of the original and rotates them, 
    so the first becomes the last, the second becomes first, and so on.
    
    EX: rotate [1,2,3,4] returns [2,3,4,1]
**/
pub fn rotate(lst: &[i32]) -> Vec<i32> {
    let mut vec = Vec::new();
    if lst.len() == 0{
        return vec;
    }
    let save = lst[0];

    for x in lst.iter().skip(1){
        vec.push(*x)
        // let index  = lst.iter().position(|&r| r == *x).unwrap();

        // vec[index - 1] = *x;
    }
    vec.push(save);
    return vec;
}

/**
    Returns true if target is a subtring of s, false otherwise
    You should not use the contains function of the string library in your implementation
    
    Ex: "ace" is a substring of "rustacean"
**/
pub fn substr(s: &String, target: &str) -> bool {
    if target.len() == 0{
        return true;
    }
    if s.len() <= target.len() && s != target{
        return false;
    }
    if target == &s[0..target.len()]{
        return true;
    }
    substr(&String::from(&s[1..s.chars().count()]), target)
    // if target == &s[0..target.len()]{
    //     return true;
    // }
    // else{
    //     let string = String::from(&s[1..s.chars().count()]);
    //     substr(&string, target);
    // }
    // return false;
}



/**
    Takes a string and returns the first longest substring of consecutive equal characters

    EX: longest_sequence of "ababbba" is Some("bbb")
    EX: longest_sequence of "aaabbb" is Some("aaa")
    EX: longest_sequence of "xyz" is Some("x")
    EX: longest_sequence of "" is None
**/
pub fn longest_sequence(s: &str) -> Option<&str> {
    if s.len() ==  0 {
        return None
    } else if s.len() == 1 {
        return Some(s)
    } else {
        let mut str_list: Vec<Vec<char>> = Vec::new();
        let mut lst: Vec<char> = Vec::new();

        let mut character = s.chars().nth(0).unwrap();

        for c in s.chars(){

            if character != c{
                character = c;
                str_list.push(lst);
                lst = Vec::new();
                lst.push(c);
            }
            else{
                lst.push(c);
            }

        }
        let mut max = 0;
        let mut index = 0;
        let mut end_index = 0;
        for row in str_list.iter(){
            if row.len() > max{
                max = row.len();
            }
        }

        for row in str_list.iter(){
            if row.len() == max{
                end_index = index + row.len();
                break;
            }
            else{
                index = index + row.len();
            }
        }
        

        return Some(&s[index..end_index]);



    }
    
}

fn add_elems(arr: &[i32; 6]) -> i32 {
    let mut sum = 0;
    for i in arr.iter(){
        sum += i;
    }
    return sum;
}