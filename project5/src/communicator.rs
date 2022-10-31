#[derive(Debug)]
#[derive(PartialEq)]
pub enum Command
{
    Power(bool,i32),    // [Increase/Decrease] power by [number].
    Missiles(bool,i32), // [Increase/Decrease] missiles by [number].
    Shield(bool),       // Turn [On/Off] the shield.
    Try,                // Try calling pepper.
    Invalid             // [anything else]
}


/**
    Adds functionality to Command enums
    Commands can be converted to strings with the as_str method
    
    Command     |     String format
    ---------------------------------------------------------
    Power       |  /Power (increased|decreased) by [0-9]+%/
    Missiles    |  /Missiles (increased|decreased) by [0-9]+/
    Shield      |  /Shield turned (on|off)/
    Try         |  /Call attempt failed/
    Invalid     |  /Not a command/
**/
impl Command {
    pub fn as_str (&self) -> String {
        match self {
            Command::Power(b, i) =>  if *b == true {format!("Power increased by {}%", i)} else {format!("Power decreased by {}%", i)},
            Command::Missiles(b, i) => if *b == true {format!("Missiles increased by {}%", i)} else {format!("Missiles decreased by {}%", i)},
            Command::Shield(b) => if *b == true {format!("Shield turned on")} else {format!("Shield turned off")},
            Command::Try => "Call attempt failed".to_string(),
            Command::Invalid => "Not a command".to_string(),
        }
    }
}

/**
    Complete this method that converts a string to a command 
    We list the format of the input strings below

    Command     |     String format
    ---------------------------------------------
    Power       |  /power (inc|dec) [0-9]+/
    Missiles    |  /(fire|add) [0-9]+ missiles/
    Shield      |  /shield (on|off)/
    Try         |  /try calling Miss Potts/
    Invalid     |  Anything else
**/
pub fn to_command(s: &str) -> Command {
    if s.len() < 9 {
        return Command::Invalid;
    }

    if s == "shield on" {
        return Command::Shield(true);
    }

    if s.len() == 10 {
        if s == "shield off" {
            return Command::Shield(false);
        }
    }

    if s.len() >= 11 {
        let cmd_power = s.get(0..10);
        if cmd_power == Some("power inc ") {
            let vy_value = s.get(10..).unwrap();
            let ans: i32 = vy_value.parse().unwrap();
            return Command::Power(true, ans);
        } else if cmd_power == Some("power dec ") {
            let vy_value = s.get(10..).unwrap();
            let ans: i32 = vy_value.parse().unwrap();
            return Command::Power(false, ans);
        }
    }

    if s.len() >= 14 {
        let cmd_add = s.get(0..4);
        if cmd_add == Some("add ") {
            let vy_value = s.get(4..s.len() - 9).unwrap();
            let ans: i32 = vy_value.parse().unwrap();
            if s.get(s.len() - 9..) == Some(" missiles") {
                return Command::Missiles(true, ans);
            }
        }
    }
    
    if s.len() >= 15 {
        let cmd_fire = s.get(0..5);
        if cmd_fire == Some("fire ") {
            let vy_value = s.get(5..s.len() - 9).unwrap();
            let ans: i32 = vy_value.parse().unwrap();
            if s.get(s.len() - 9..) == Some(" missiles") {
                return Command::Missiles(false, ans);
            } 
        }
    }
    


    if s.len() == 22 {
        if s == "try calling Miss Potts" {
            return Command::Try;
        }
    }
    return Command::Invalid;
}
