open Format
open Syntax
open Support.Error
open Support.Pervasive

(* ------------------------   EVALUATION  ------------------------ *)

exception NoRuleApplies

let rec isnumericval ctx t = match t with
    TmZero(_) -> true
  | TmSucc(_,t1) -> isnumericval ctx t1
  | _ -> false

let rec isval ctx t = match t with
    TmTrue(_)  -> true
  | TmFalse(_) -> true
  | TmTag(_,l,t1,_) -> isval ctx t1
  | TmString _  -> true
  | TmUnit(_)  -> true
  | TmFloat _  -> true
  | t when isnumericval ctx t  -> true
  | TmAbs(_,_,_,_) -> true
  | TmRecord(_,fields) -> List.for_all (fun (l,ti) -> isval ctx ti) fields
  (* @yzy for chord normalizer *)
  | TmNote(_,_,_,_,_) -> true
  | TmNoteset(_,_,_) -> true
  | TmPhrase(_,_,_) -> true
  | TmSegment(_,_,_,_,_) -> true
  | TmPassage(_,_,_) -> true
  | _ -> false

let rec eval1 ctx t = match t with
    TmIf(_,TmTrue(_),t2,t3) ->
      t2
  | TmIf(_,TmFalse(_),t2,t3) ->
      t3
  | TmIf(fi,t1,t2,t3) ->
      let t1' = eval1 ctx t1 in
      TmIf(fi, t1', t2, t3)
  | TmTag(fi,l,t1,tyT) ->
      let t1' = eval1 ctx t1 in
      TmTag(fi, l, t1',tyT)
  | TmCase(fi,TmTag(_,li,v11,_),branches) when isval ctx v11->
      (try 
         let (x,body) = List.assoc li branches in
         termSubstTop v11 body
       with Not_found -> raise NoRuleApplies)
  | TmCase(fi,t1,branches) ->
      let t1' = eval1 ctx t1 in
      TmCase(fi, t1', branches)
  | TmApp(fi,TmAbs(_,x,tyT11,t12),v2) when isval ctx v2 ->
      termSubstTop v2 t12
  | TmApp(fi,v1,t2) when isval ctx v1 ->
      let t2' = eval1 ctx t2 in
      TmApp(fi, v1, t2')
  | TmApp(fi,t1,t2) ->
      let t1' = eval1 ctx t1 in
      TmApp(fi, t1', t2)
  | TmLet(fi,x,v1,t2) when isval ctx v1 ->
      termSubstTop v1 t2 
  | TmLet(fi,x,t1,t2) ->
      let t1' = eval1 ctx t1 in
      TmLet(fi, x, t1', t2) 
  | TmFix(fi,v1) as t when isval ctx v1 ->
      (match v1 with
         TmAbs(_,_,_,t12) -> termSubstTop t t12
       | _ -> raise NoRuleApplies)
  | TmFix(fi,t1) ->
      let t1' = eval1 ctx t1
      in TmFix(fi,t1')
  | TmVar(fi,n,_) ->
      (match getbinding fi ctx n with
          TmAbbBind(t,_) -> t 
        | _ -> raise NoRuleApplies)
  | TmAscribe(fi,v1,tyT) when isval ctx v1 ->
      v1
  | TmAscribe(fi,t1,tyT) ->
      let t1' = eval1 ctx t1 in
      TmAscribe(fi,t1',tyT)
  | TmRecord(fi,fields) ->
      let rec evalafield l = match l with 
        [] -> raise NoRuleApplies
      | (l,vi)::rest when isval ctx vi -> 
          let rest' = evalafield rest in
          (l,vi)::rest'
      | (l,ti)::rest -> 
          let ti' = eval1 ctx ti in
          (l, ti')::rest
      in let fields' = evalafield fields in
      TmRecord(fi, fields')
  | TmProj(fi, (TmRecord(_, fields) as v1), l) when isval ctx v1 ->
      (try List.assoc l fields
       with Not_found -> raise NoRuleApplies)
  | TmProj(fi, t1, l) ->
      let t1' = eval1 ctx t1 in
      TmProj(fi, t1', l)
  | TmTimesfloat(fi,TmFloat(_,f1),TmFloat(_,f2)) ->
      TmFloat(fi, f1 *. f2)
  | TmTimesfloat(fi,(TmFloat(_,f1) as t1),t2) ->
      let t2' = eval1 ctx t2 in
      TmTimesfloat(fi,t1,t2') 
  | TmTimesfloat(fi,t1,t2) ->
      let t1' = eval1 ctx t1 in
      TmTimesfloat(fi,t1',t2) 
  | TmSucc(fi,t1) ->
      let t1' = eval1 ctx t1 in
      TmSucc(fi, t1')
  | TmPred(_,TmZero(_)) ->
      TmZero(dummyinfo)
  | TmPred(_,TmSucc(_,nv1)) when (isnumericval ctx nv1) ->
      nv1
  | TmPred(fi,t1) ->
      let t1' = eval1 ctx t1 in
      TmPred(fi, t1')
  | TmIsZero(_,TmZero(_)) ->
      TmTrue(dummyinfo)
  | TmIsZero(_,TmSucc(_,nv1)) when (isnumericval ctx nv1) ->
      TmFalse(dummyinfo)
  | TmIsZero(fi,t1) ->
      let t1' = eval1 ctx t1 in
      TmIsZero(fi, t1')
  | _ -> 
      raise NoRuleApplies

let rec eval ctx t =
  try let t' = eval1 ctx t
      in eval ctx t'
  with NoRuleApplies -> t

let evalbinding ctx b = match b with
    TmAbbBind(t,tyT) ->
      let t' = eval ctx t in 
      TmAbbBind(t',tyT)
  | bind -> bind

let istyabb ctx i = 
  match getbinding dummyinfo ctx i with
    TyAbbBind(tyT) -> true
  | _ -> false

let gettyabb ctx i = 
  match getbinding dummyinfo ctx i with
    TyAbbBind(tyT) -> tyT
  | _ -> raise NoRuleApplies

let rec computety ctx tyT = match tyT with
    TyVar(i,_) when istyabb ctx i -> gettyabb ctx i
  | _ -> raise NoRuleApplies

let rec simplifyty ctx tyT =
  try
    let tyT' = computety ctx tyT in
    simplifyty ctx tyT' 
  with NoRuleApplies -> tyT

let rec tyeqv ctx tyS tyT =
  let tyS = simplifyty ctx tyS in
  let tyT = simplifyty ctx tyT in
  match (tyS,tyT) with
    (TyString,TyString) -> true
  | (TyUnit,TyUnit) -> true
  | (TyId(b1),TyId(b2)) -> b1=b2
  | (TyFloat,TyFloat) -> true
  | (TyVar(i,_), _) when istyabb ctx i ->
      tyeqv ctx (gettyabb ctx i) tyT
  | (_, TyVar(i,_)) when istyabb ctx i ->
      tyeqv ctx tyS (gettyabb ctx i)
  | (TyVar(i,_),TyVar(j,_)) -> i=j
  | (TyArr(tyS1,tyS2),TyArr(tyT1,tyT2)) ->
       (tyeqv ctx tyS1 tyT1) && (tyeqv ctx tyS2 tyT2)
  | (TyBool,TyBool) -> true
  | (TyNat,TyNat) -> true
  | (TyRecord(fields1),TyRecord(fields2)) -> 
       List.length fields1 = List.length fields2
       &&                                         
       List.for_all 
         (fun (li2,tyTi2) ->
            try let (tyTi1) = List.assoc li2 fields1 in
                tyeqv ctx tyTi1 tyTi2
            with Not_found -> false)
         fields2
  | (TyVariant(fields1),TyVariant(fields2)) ->
       (List.length fields1 = List.length fields2)
       && List.for_all2
            (fun (li1,tyTi1) (li2,tyTi2) ->
               (li1=li2) && tyeqv ctx tyTi1 tyTi2)
            fields1 fields2
  (* @yzy for chord normalizer *)
  | (TyNote(rank1),TyNote(rank2)) -> 
        if (rank1 == rank2) then true else false
  | (TyNoteset(rank1),TyNoteset(rank2)) -> 
        if (rank1 == rank2) then true else false
  | (TyPhrase(begin_rank1,end_rank1),TyPhrase(begin_rank2,end_rank2)) -> 
        if ((begin_rank1 == end_rank1) && (begin_rank2 == end_rank2)) then true else false
  | (TySegment(p1,d1,c1),TySegment(p2,d2,c2)) -> 
        if ((p1 == p2) && (0 == String.compare d1 d2) && (0 == String.compare c1 c2))
        then true else false
  | (TyPassage,TyPassage) -> true
  | _ -> false

(* ------------------------   TYPING  ------------------------ *)

let rec typeof ctx t =
  match t with
    TmInert(fi,tyT) ->
      tyT
  | TmTrue(fi) -> 
      TyBool
  | TmFalse(fi) -> 
      TyBool
  | TmIf(fi,t1,t2,t3) ->
     if tyeqv ctx (typeof ctx t1) TyBool then
       let tyT2 = typeof ctx t2 in
       if tyeqv ctx tyT2 (typeof ctx t3) then tyT2
       else error fi "arms of conditional have different types"
     else error fi "guard of conditional not a boolean"
  | TmCase(fi, t, cases) ->
      (match simplifyty ctx (typeof ctx t) with
         TyVariant(fieldtys) ->
           List.iter
             (fun (li,(xi,ti)) ->
                try let _ = List.assoc li fieldtys in ()
                with Not_found -> error fi ("label "^li^" not in type"))
             cases;
           let casetypes =
             List.map (fun (li,(xi,ti)) ->
                         let tyTi =
                           try List.assoc li fieldtys
                           with Not_found ->
                             error fi ("label "^li^" not found") in
                         let ctx' = addbinding ctx xi (VarBind(tyTi)) in
                         typeShift (-1) (typeof ctx' ti))
                      cases in
           let tyT1 = List.hd casetypes in
           let restTy = List.tl casetypes in
           List.iter
             (fun tyTi -> 
                if not (tyeqv ctx tyTi tyT1)
                then error fi "fields do not have the same type")
             restTy;
           tyT1
        | _ -> error fi "Expected variant type")
  | TmTag(fi, li, ti, tyT) ->
      (match simplifyty ctx tyT with
          TyVariant(fieldtys) ->
            (try
               let tyTiExpected = List.assoc li fieldtys in
               let tyTi = typeof ctx ti in
               if tyeqv ctx tyTi tyTiExpected
                 then tyT
                 else error fi "field does not have expected type"
             with Not_found -> error fi ("label "^li^" not found"))
        | _ -> error fi "Annotation is not a variant type")
  | TmVar(fi,i,_) -> getTypeFromContext fi ctx i
  | TmAbs(fi,x,tyT1,t2) ->
      let ctx' = addbinding ctx x (VarBind(tyT1)) in
      let tyT2 = typeof ctx' t2 in
      TyArr(tyT1, typeShift (-1) tyT2)
  | TmApp(fi,t1,t2) ->
      let tyT1 = typeof ctx t1 in
      let tyT2 = typeof ctx t2 in
      (match simplifyty ctx tyT1 with
          TyArr(tyT11,tyT12) ->
            if tyeqv ctx tyT2 tyT11 then tyT12
            else error fi "parameter type mismatch"
        | _ -> error fi "arrow type expected")
  | TmLet(fi,x,t1,t2) ->
     let tyT1 = typeof ctx t1 in
     let ctx' = addbinding ctx x (VarBind(tyT1)) in         
     typeShift (-1) (typeof ctx' t2)
  | TmFix(fi, t1) ->
      let tyT1 = typeof ctx t1 in
      (match simplifyty ctx tyT1 with
           TyArr(tyT11,tyT12) ->
             if tyeqv ctx tyT12 tyT11 then tyT12
             else error fi "result of body not compatible with domain"
         | _ -> error fi "arrow type expected")
  | TmString _ -> TyString
  | TmUnit(fi) -> TyUnit
  | TmAscribe(fi,t1,tyT) ->
     if tyeqv ctx (typeof ctx t1) tyT then
       tyT
     else
       error fi "body of as-term does not have the expected type"
  | TmRecord(fi, fields) ->
      let fieldtys = 
        List.map (fun (li,ti) -> (li, typeof ctx ti)) fields in
      TyRecord(fieldtys)
  | TmProj(fi, t1, l) ->
      (match simplifyty ctx (typeof ctx t1) with
          TyRecord(fieldtys) ->
            (try List.assoc l fieldtys
             with Not_found -> error fi ("label "^l^" not found"))
        | _ -> error fi "Expected record type")
  | TmFloat _ -> TyFloat
  | TmTimesfloat(fi,t1,t2) ->
      if tyeqv ctx (typeof ctx t1) TyFloat
      && tyeqv ctx (typeof ctx t2) TyFloat then TyFloat
      else error fi "argument of timesfloat is not a number"
  | TmZero(fi) ->
      TyNat
  | TmSucc(fi,t1) ->
      if tyeqv ctx (typeof ctx t1) TyNat then TyNat
      else error fi "argument of succ is not a number"
  | TmPred(fi,t1) ->
      if tyeqv ctx (typeof ctx t1) TyNat then TyNat
      else error fi "argument of pred is not a number"
  | TmIsZero(fi,t1) ->
      if tyeqv ctx (typeof ctx t1) TyNat then TyBool
      else error fi "argument of iszero is not a number"
  (* @yzy for chord normalizer *)
  | TmNote(fi,ty,seq,height,len) ->
      let regex_numseq = Str.regexp "[0-9]+" in
      let seq_len = String.length seq in 
      let height_len = String.length height in
      if ((Str.string_match regex_numseq seq 0) && 
          (Str.string_match regex_numseq height 0) &&
          (seq_len == height_len)) then (
            match ty with
              "chord" | "brokenchord" -> (
                let regex_p1 = Str.regexp "[135]+" in 
                let regex_p2 = Str.regexp "[246]+" in 
                let regex_p3 = Str.regexp "[357]+" in 
                let regex_p4 = Str.regexp "[146]+" in 
                let regex_p5 = Str.regexp "[257]+" in 
                let regex_p6 = Str.regexp "[136]+" in 
                let regex_p7 = Str.regexp "[247]+" in (
                  if ((Str.string_match regex_p1 seq 0) && 
                      (seq_len == String.length (Str.matched_string seq))) then TyNote(1)
                  else if ((Str.string_match regex_p2 seq 0) && 
                      (seq_len == String.length (Str.matched_string seq))) then TyNote(2)
                  else if ((Str.string_match regex_p3 seq 0) && 
                      (seq_len == String.length (Str.matched_string seq))) then TyNote(3)
                  else if ((Str.string_match regex_p4 seq 0) && 
                      (seq_len == String.length (Str.matched_string seq))) then TyNote(4)
                  else if ((Str.string_match regex_p5 seq 0) && 
                      (seq_len == String.length (Str.matched_string seq))) then TyNote(5)
                  else if ((Str.string_match regex_p6 seq 0) && 
                      (seq_len == String.length (Str.matched_string seq))) then TyNote(6)
                  else if ((Str.string_match regex_p7 seq 0) && 
                      (seq_len == String.length (Str.matched_string seq))) then TyNote(7)
                  else error fi "invalid chord / brokenchord sequence"
                )
              )
            | "melody" -> (
                let first_tone = String.get seq 0 in (
                  if (first_tone == '1') then TyNote(1)
                  else if (first_tone == '2') then TyNote(2)
                  else if (first_tone == '3') then TyNote(3)
                  else if (first_tone == '4') then TyNote(4)
                  else if (first_tone == '5') then TyNote(5)
                  else if (first_tone == '6') then TyNote(6)
                  else if (first_tone == '7') then TyNote(7)
                  else error fi "invalid melody sequence"
                )
              )
            | _ -> error fi "invalid note type; a note must be a chord / brokenchord / melody"
          )
      else error fi "invalid sequence / height"
  | TmNoteset(fi,t1,t2) ->
      let typet1 = typeof ctx t1 in 
      let typet2 = typeof ctx t2 in (
        match typet1 with
            TyNote(rank1) | TyNoteset(rank1) -> (
              match typet2 with
                  TyNote(rank2) | TyNoteset(rank2) -> (
                    if (rank1 == rank2) then TyNoteset(rank1)
                    else error fi "Noteset constructors' rank mismatch"
                  )
                | _ -> error fi "invalid noteset constructor #2"
            )
          | _ -> error fi "invalid noteset constructor #1"
      )
  | TmPhrase(fi,t1,t2) -> 
      let typet1 = typeof ctx t1 in 
      let typet2 = typeof ctx t2 in (
        match typet1 with
            TyNote(nr1) | TyNoteset(nr1) -> (
              match typet2 with
                  TyNote(nr2) | TyNoteset(nr2) -> (
                    if ((nr1 == 1 && nr2 == 4) || 
                        (nr1 == 1 && nr2 == 5) || 
                        (nr1 == 4 && nr2 == 1) ||
                        (nr1 == 5 && nr2 == 1) || 
                        (nr1 == 4 && nr2 == 5) ||
                        (nr1 == nr2))
                    then TyPhrase(nr1,nr2)
                    else error fi "Phrase constructors' rank mismatch"
                  )
                | TyPhrase(pr21,pr22) -> (
                    if ((nr1 == 1 && pr21 == 4) || 
                        (nr1 == 1 && pr21 == 5) || 
                        (nr1 == 4 && pr21 == 1) ||
                        (nr1 == 5 && pr21 == 1) || 
                        (nr1 == 4 && pr21 == 5) ||
                        (nr1 == pr21))
                    then TyPhrase(nr1,pr22)
                    else error fi "Phrase constructors' rank mismatch"
                  )
                | _ -> error fi "invalid phrase constructor #2"
            )
          | TyPhrase(pr11,pr12) -> (
              match typet2 with
                  TyNote(nr2) | TyNoteset(nr2) -> (
                    if ((pr12 == 1 && nr2 == 4) || 
                        (pr12 == 1 && nr2 == 5) || 
                        (pr12 == 4 && nr2 == 1) ||
                        (pr12 == 5 && nr2 == 1) || 
                        (pr12 == 4 && nr2 == 5) ||
                        (pr12 == nr2))
                    then TyPhrase(pr11,nr2)
                    else error fi "Phrase constructors' rank mismatch"
                  )
                | TyPhrase(pr21,pr22) -> (
                    if ((pr12 == 1 && pr21 == 4) || 
                        (pr12 == 1 && pr21 == 5) || 
                        (pr12 == 4 && pr21 == 1) ||
                        (pr12 == 5 && pr21 == 1) || 
                        (pr12 == 4 && pr21 == 5) ||
                        (pr12 == pr21))
                    then TyPhrase(pr11,pr22)
                    else error fi "Phrase constructors' rank mismatch"
                  )
                | _ -> error fi "invalid phrase constructor #2"
            )
          | _ -> error fi "invalid phrase constructor #1"
      )
  | TmSegment(fi,t1,mode_pitch,mode_descriptor,mode_class) -> 
      let typet1 = typeof ctx t1 in (
        match typet1 with
            TyPhrase(1,1) ->
              if (mode_pitch >= 1 && mode_pitch <= 12) then (
                match mode_descriptor with 
                    "natural" | "harmony" | "melody" -> (
                      match mode_class with
                          "major" | "minor" -> TySegment(mode_pitch,mode_descriptor,mode_class)
                        | _ -> error fi "invalid segment mode class"
                    )
                  | _ -> error fi "invalid segment mode descriptor"
              )
              else error fi "invalid segment mode pitch"
          | _ -> error fi "invalid segment phrase"
      )
  | TmPassage(fi,t1,t2) ->
      let typet1 = typeof ctx t1 in 
      let typet2 = typeof ctx t2 in (
        match typet1 with 
          TySegment(_) -> (
            match typet2 with
              TySegment(_) -> TyPassage
            | _ -> error fi "invalid passage constructor #2"
          )
        | _ -> error fi "invalid passage constructor #1"
      )
