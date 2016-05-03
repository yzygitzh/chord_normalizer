/* Examples for music testing */
let x = {#"chord"|"135"|"135"|3#} in x;
{#"brokenchord"|"135"|"135"|3#};
{#"melody"|"135"|"135"|3#};

{#"chord"|"461"|"135"|3#};

MakeNoteset {#"melody"|"246"|"135"|3#} {#"melody"|"246"|"135"|3#};

let x = (MakeNoteset
			{#"melody"|"135"|"135"|3#} 
			{#"melody"|"135"|"135"|3#}) in 
let y = (MakeNoteset
			{#"melody"|"135"|"135"|3#} 
			{#"melody"|"135"|"135"|3#}) in
		(MakeNoteset x y);

T = Note@0 -> Note@0;	
lambda f:T. lambda x:Note@0. f (f x);