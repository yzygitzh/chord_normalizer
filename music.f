/* Examples for music testing */
x = {#"chord"|"641"|"135"|3#};
y = {#"brokenchord"|"641"|"135"|3#};
z = {#"melody"|"135"|"135"|3#};

MakeSegment (MakePhrase z (MakePhrase (MakeNoteset x y) z))(3,"harmony","major");

let x = (MakeNoteset
			{#"chord"|"135"|"135"|3#} 
			{#"brokenchord"|"135"|"135"|3#}) in 
let y = (MakeNoteset
			{#"chord"|"531"|"135"|3#} 
			{#"melody"|"135"|"135"|3#}) in
		(MakeNoteset x y);

T = Note@0 -> Note@0;	
lambda f:T. lambda x:Note@0. f (f x);