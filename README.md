# ocalf
OCaml "inspired" interpreter written in Rust. Supports type inference and matchable destructuring.

"cargo run" to build and start the REPL. The parser uses lalrpop, so it might take some time.

Enter expressions or variants, over multiple lines if necessary, and then use ";" to evaluate entered expressions.

Quit the repl by entering "q".

Define variants like "type 'a list = Nil of unit | Cons of ('a * 'a list)" or "type 'a option = Some of 'a | None of unit".

Some example expresisons:

"let make_list = fun n -> 
     if n > 0 then Cons(n, make_list (n - 1)) 
     	      else Nil () in 
make_list 10"

"let sum_n = fun n -> 
     if n > 0 then n + sum_n (n - 1) else 0 in 
sum_n 20"

"let map = fun f -> fun l -> match l with | Nil () -> Nil () | Cons (hd, tl) -> Cons (f hd, map f tl)."

A period following a let binding tells the repl to create the binding in the environment, but not to evaluate anything.

Alternatively, you can evaluate bindings immediately by dropping the period and add attaching an "in" clause:

"let map = fun f -> fun l -> 
     match l with 
     | Nil () -> Nil () 
     | Cons (hd, tl) -> Cons (f hd, map f tl) in 
let l1 = Cons(1, Cons(2, Nil ())) in 
let fn = fun x -> (x + 1) in 
map fn l1"

Debug info and error statements are admittedly wanting. I am sorry.