/* Examples for music testing */
let x = {#"chord"|"test2"|"test3"|3#} in x;
{#"brokenchord"|"test2"|"test3"|3#};
{#"melody"|"test2"|"test3"|3#};

MakeNoteset {#"melody"|"test2"|"test3"|3#} {#"melody"|"test2"|"test3"|3#};

let x = (MakeNoteset
			{#"melody"|"test2"|"test3"|3#} 
			{#"melody"|"test2"|"test3"|3#}) in 
let y = (MakeNoteset
			{#"melody"|"test2"|"test3"|3#} 
			{#"melody"|"test2"|"test3"|3#}) in
		(MakeNoteset x y);

T = Note@0 -> Note@0;	
lambda f:T. lambda x:Note@0. f (f x);