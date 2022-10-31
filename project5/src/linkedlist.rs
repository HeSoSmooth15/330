use std::{
    borrow::BorrowMut,
    ops::{Deref, DerefMut},
    sync::{Arc, RwLock},
};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Component {
    Helmet(bool),              //is damaged?
    LeftThrusters(bool, i32),  //is damaged? How much power left?
    RightThrusters(bool, i32), //is damaged? How much power left?
    LeftRepulsor(bool, i32),   //is damaged? How much power left?
    RightRepulsor(bool, i32),  //is damaged? How much power left?
    ChestPiece(bool, i32),     //is damaged? How much power left?
    Missiles(i32),             //how many missiles left?
    ArcReactor(i32),           // How much power left?
    Wifi(bool),                // connected to wifi?
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Armor {
    pub component: Component,
    pub version: i32,
}

// Part 2

// Students should fill in the Link type themselves. The Node and List types are given as is.
type Link = (Option<Arc<RwLock<Node>>>);

struct Node {
    data: Armor,
    rest: Link,
}

#[derive(Clone)]
pub struct List {
    head_link: Link,
    size: usize,
}

impl List {
    pub fn new() -> Self {
        return List{head_link:None, size: 0};

        
    }

    pub fn size(&self) -> usize {
        self.size
    }

    pub fn peek(&self) -> Option<Armor> {
        match &self.head_link {
            None => None,
            Some(x) => {
                let mut copy = x.clone();
                match Arc::get_mut(&mut copy) {
                    None => None,
                    Some(v) => {
                        return Some(v.get_mut().unwrap().data.clone());
                    }
                }
            }
        }
    }

    pub fn push(&mut self, component: Armor) {
        match &self.head_link {
            None => {
                self.head_link = Some(Arc::new(RwLock::new(Node {data: component, rest: None})));
                self.size += 1;
            }
            Some(x) => {
                let copy = Arc::clone(&x);
                self.head_link = Some(Arc::new(RwLock::new(Node {data: component, rest: Some(copy)})));
                self.size += 1;
            }
        }
    }

    pub fn pop(&mut self) -> Option<Armor> {
        match &self.head_link{
            None => None,
            Some(x) => {
                let data2 = Arc::clone(&x);
                let node1 = data2.deref().read().unwrap();
                self.head_link = node1.rest.clone();
                self.size -= 1;
                return Some(node1.data);
            }
        }
    }
}

// Part 3



#[derive(Clone)]
pub struct Suit {
    pub armor: List,
    pub version: i32,
}

impl Suit {
    pub fn is_compatible(&self) -> bool {
        let mut pointer = self.armor.head_link.clone();
        let mut temp = 0;

        while temp != self.armor.size{
            match pointer{
                None => {
                    return true;
                }
                Some(x) =>{
                    let node1 = Arc::clone(&x);
                    let variable = node1.deref().read().unwrap();
                    if self.version != variable.data.version{
                        return false;
                    }
                    match variable.rest.clone(){
                        None =>{
                            return true;
                        }
                        Some(v) =>{
                            let t = Arc::clone(&v);
                            let var = t.deref().read().unwrap();
                            pointer = var.rest.clone();
                            temp += 1;
                        }
                    }

                }
            }
        }
        return true;
    }

    pub fn repair(&mut self) {
        unimplemented!()
    }
}
