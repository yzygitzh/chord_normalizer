/* Examples for music testing */
x = {#"chord"|"641"|"135"|3#};
y = {#"brokenchord"|"641"|"135"|3#};
z = {#"melody"|"135"|"135"|3#};

s1 = MakeSegment (MakePhrase z (MakePhrase (MakeNoteset x y) z))(1,"major");
s2 = MakeSegment (MakePhrase z (MakePhrase (MakeNoteset x y) z))(3,"minor");
p1 = MakePassage s1 s1;
p2 = MakePassage s2 s2;
MakePassage p1 p2;

let x = (MakeNoteset
			{#"chord"|"135"|"135"|3#} 
			{#"brokenchord"|"135"|"135"|3#}) in 
let y = (MakeNoteset
			{#"chord"|"531"|"135"|3#} 
			{#"melody"|"135"|"135"|3#}) in
		(MakeNoteset x y);

T = Note@0 -> Note@0;	
lambda f:T. lambda x:Note@0. f (f x);