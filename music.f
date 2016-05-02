/* Examples for music testing */
let x = {#"chord"|"test2"|"test3"|3#} in x;
{#"brokenchord"|"test2"|"test3"|3#};
{#"melody"|"test2"|"test3"|3#};

Noteset {#"melody"|"test2"|"test3"|3#} {#"melody"|"test2"|"test3"|3#};

let x = (Noteset 
			{#"melody"|"test2"|"test3"|3#} 
			{#"melody"|"test2"|"test3"|3#}) in 
let y = (Noteset 
			{#"melody"|"test2"|"test3"|3#} 
			{#"melody"|"test2"|"test3"|3#}) in
		(Noteset x y);

T = Note -> Note;	
lambda f:T. lambda x:Note. f (f x);