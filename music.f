/* Examples for music testing */
x = {#"chord"|"641"|"135"|3#};
y = {#"brokenchord"|"641"|"135"|3#};
z = {#"melody"|"135"|"135"|3#};

s1 = MakeSegment (MakePhrase z (MakePhrase (MakeNoteset x y) z))(1,"major");
s2 = MakeSegment (MakePhrase z (MakePhrase (MakeNoteset x y) z))(3,"minor");
p1 = MakePassage s1 s2;
p2 = MakePassage s2 s1;
MakePassage p1 p2;

filename = "girigiri_eye";

let x = (MakeNoteset
			{#"chord"|"135"|"135"|3#} 
			{#"brokenchord"|"135"|"135"|3#}) in 
let y = (MakeNoteset
			{#"chord"|"531"|"135"|3#} 
			{#"melody"|"135"|"135"|3#}) in
		(MakeNoteset x y);

T = Note@0 -> Note@0;	
lambda f:T. lambda x:Note@0. f (f x);

ExportPsg p1 => "girigiri_eye";

bc1 = {#"brokenchord"|"13513513"|"44455455"|3#};
bc2 = {#"brokenchord"|"14614614"|"44455455"|3#};
bc3 = {#"brokenchord"|"72525525"|"34455455"|3#};
bc4 = bc1;

ph1 = MakePhrase bc1 bc1;
ph2 = MakePhrase (MakePhrase ph1 bc2) bc2;
ph3 = MakePhrase (MakePhrase ph2 bc3) bc3;
ph4 = MakePhrase (MakePhrase ph3 bc4) bc4;

sg = MakeSegment ph4 (1, "major");

psg = MakePassage sg sg;

ExportPsg psg => "1201";