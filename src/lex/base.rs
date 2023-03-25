use std::collections::HashSet;

pub enum NumberBase {
    Hexadecimal,
    Decimal,
    Octal,
    Binary,
}

impl NumberBase {
    pub fn new(chars: &str) -> Self {
        if chars.starts_with("0x") {
            NumberBase::Hexadecimal
        } else if chars.starts_with("0o") {
            NumberBase::Octal
        } else if chars.starts_with("0b") {
            NumberBase::Binary
        } else {
            NumberBase::Decimal
        }
    }

    pub fn digits(&self) -> HashSet<char> {
        match self {
            NumberBase::Hexadecimal => HashSet::from_iter([
                '0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'A', 'B', 'C', 'D', 'E', 'F',
                'a', 'b', 'c', 'd', 'e', 'f',
            ]),
            NumberBase::Decimal => {
                HashSet::from_iter(['0', '1', '2', '3', '4', '5', '6', '7', '8', '9'])
            }
            NumberBase::Octal => HashSet::from_iter(['0', '1', '2', '3', '4', '5', '6', '7']),
            NumberBase::Binary => HashSet::from_iter(['0', '1']),
        }
    }

    pub fn radix(&self) -> u32 {
        match self {
            NumberBase::Hexadecimal => 16,
            NumberBase::Decimal => 10,
            NumberBase::Octal => 8,
            NumberBase::Binary => 2,
        }
    }
}
