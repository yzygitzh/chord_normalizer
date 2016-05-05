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

/* 1201 */

bc1 = {#"brokenchord"|"13513513"|"44455455"|8#};
bc2 = {#"brokenchord"|"14614614"|"44455455"|8#};
bc3 = {#"brokenchord"|"72525525"|"34455455"|8#};
bc4 = bc1;

ph1 = MakePhrase (MakeNoteset bc1 {#"chord"|"135"|"060"|8#}) bc1;
ph2 = MakePhrase (MakePhrase ph1 (MakeNoteset bc2 {#"chord"|"146"|"060"|8#})) bc2;
ph3 = MakePhrase (MakePhrase ph2 (MakeNoteset bc3 {#"chord"|"257"|"060"|8#})) (MakeNoteset bc3 {#"brokenchord"|"52"|"06"|8#});
ph4 = MakePhrase (MakePhrase ph3 (MakeNoteset bc1 {#"chord"|"135"|"060"|8#})) bc4;

sg = MakeSegment ph4 (1, "major");

psg = MakePassage sg sg;

ExportPsg psg => "1201";

/* hana */

m1 = {#"melody"|"1230"|"5550"|8#};
m2 = {#"melody"|"4230"|"5550"|8#};
m31 = {#"melody"|"123217"|"555554"|12#};
m32 = {#"melody"|"123235"|"555555"|12#};
m41 = {#"melody"|"5"|"4"|4#};
m42 = {#"melody"|"5"|"5"|4#};
mf1 = {#"melody"|"1340"|"4440"|8#};
mf2 = {#"melody"|"4571"|"4445"|8#};
mf3 = {#"melody"|"5721"|"4455"|8#};
mf4 = {#"melody"|"1"|"5"|8#};

ph1 = MakePhrase (MakePhrase (MakePhrase m1 m2) m31) m41;
ph2 = MakePhrase (MakePhrase (MakePhrase m1 m2) m32) m42;
ph3 = MakePhrase (MakePhrase (MakePhrase mf1 mf2) mf3) mf4;

finalph = MakePhrase (MakePhrase (MakePhrase ph1 ph2) ph1) ph3;

sg1 = MakeSegment finalph (1, "minor");
sg2 = MakeSegment finalph (6, "minor");

psg = MakePassage sg1 sg2;
ExportPsg psg => "hana";

/* feature test */

c1 = {#"chord"|"135"|"555"|1#};

rep = fix (lambda rep_origin: Phrase@(1,1) -> Nat -> Phrase@(1,1). lambda x:Phrase@(1,1). lambda y:Nat. if (iszero y) then x else (rep_origin (MakePhrase x x) (pred y)));

ph = rep (MakePhrase c1 c1) 8;

sg1 = MakeSegment ph (1, "major");
sg2 = MakeSegment ph (3, "minor");

ExportPsg (MakePassage sg1 sg2) => "feature";