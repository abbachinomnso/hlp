namespace ARMSim
module Tokeniser = 
    open ARMSim.Types
    open System
    /// convert string into char list
    let explode (str : string) = str |> List.ofSeq
    /// convert char list into string
    let implode (x : char list) = x |> System.String.Concat
    /// charater is new line
    let isNewLine (c : char) = List.contains c [ '\n'; '\f'; '\r' ]
    /// the comment indicator 
    let isComment (c : char) = List.contains c [';']

    /// remove comments which start with ; gives out a list of each line of code
    let removeComment (src: string)=
        let srb = 
            ///deals with null exception
            match src with
            |null -> ";"
            |a ->a  
        let rec removeComment1 code_=
            match code_ with
            |a::b when isComment a -> removeComment1 (List.skipWhile (isNewLine >> not) b)
            |a::b ->a :: removeComment1 b
            |[] ->  []
            |_ -> failwithf "not working" 
         // split input string into array of lines
        srb.Split[| '\n'|] |>Array.toList |> List.map explode |>List.map (removeComment1 >>implode)
        

    ///returns first part of Instruction
    let CompareInst (stactual:string) =
        let lst = ["MOV";"ADD";"ADC";"SUB";"SBC"]
        let stcaps = stactual.ToUpper()
        let rec checkerinst (stcaps:string) (lst: string list) =
            match lst with
            |[] -> failwithf "not a command"
            |a::b when stcaps.Contains a -> a
            |a::b -> checkerinst stcaps b
        checkerinst stcaps lst
    
    
    let CompareCond (stactual: string) =
        let lst = [EQ,"EQ" ;NE,"NE" ;CS,"CS" ;HI,"HI" ;LS,"LS" ;GE,"GE" ;LT,"LT" ;GT,"GT" ;LE,"LE" ]
        let stcaps = stactual.ToUpper()
        let rec checkerCond (stcaps:string) (lst: (Cond*string) list) =
            match lst with
            |(a,c)::b -> match stcaps.Contains c with
                         |true -> Some a
                         |false -> checkerCond stcaps b
            |[] -> None
        checkerCond stcaps lst

    let CompareS (stactual: string)=
        let stcaps: string = stactual.ToUpper()
        let lst1 = ["B"; "BL"; "FILL"]
        let rec checkerS (stcaps:string) (lst1: string list) =
            match lst1 with
            |[] -> false
            |a::b when stcaps.Contains (a.ToString()) -> true
            |a::b -> checkerS stcaps b
        let aa = checkerS stcaps lst1
        let checkotherS = stcaps.Contains ("CS") |> not
        let checks (stcaps : string) = stcaps.Contains "S"
        let fourthelementcheck (st:string)= 
            if st.Length >3 then
                match st.[3] = 'S' with
                |true -> true
                |false -> false
            else false

        if aa && checkotherS then checks stcaps
        else fourthelementcheck stcaps
    let CompareDes (stactual : string)=
        let stcaps:string = stactual.ToUpper()
        //let reg = [R1 ;R2 ; R3 ;R4 ; R5 ;R6 ; R7 ;R8 ; R9 ;R10 ; R11 ;R12; R13 ;PC ;LR]
        match stcaps with
        |"R0" -> R0
        |"R1" -> R1
        |"R2" -> R2
        |"R3" -> R3
        |"R4" -> R4
        |"R5" -> R5
        |"R6" -> R6
        |"R7" -> R7
        |"R8" -> R8
        |"R9" -> R9
        |"R10" -> R10
        |"R11" -> R11
        |"R12" -> R12
        |"R13" -> R13
        |"PC" -> PC
        |"LR" -> LR
        |a -> WRONG (a)

    let CompareOperand (stactual : string) =
        let stcaps:string = stactual.ToUpper()
        let strip chars = String.collect (fun c -> if Seq.exists((=)c) chars then "" else string c)
        if stcaps.StartsWith "#" then Number(int32(strip "#" stcaps)) 
        else if stcaps.StartsWith "0B" then Number(Convert.ToInt32((strip "B" stcaps), 2))
            else if stcaps.StartsWith "0X" then Number(Convert.ToInt32((strip "X" stcaps), 16))
                else match stcaps with
                     |"R0" -> RName(R0)
                     |"R1" -> RName(R1)
                     |"R2" -> RName(R2)
                     |"R3" -> RName(R3)
                     |"R4" -> RName(R4)
                     |"R5" -> RName(R5)
                     |"R6" -> RName(R6)
                     |"R7" -> RName(R7)
                     |"R8" -> RName(R8)
                     |"R9" -> RName(R9)
                     |"R10" -> RName(R10)
                     |"R11" -> RName(R11)
                     |"R12" -> RName(R12)
                     |"R13" -> RName(R13)
                     |"PC" -> RName(PC)
                     |"LR" -> RName(LR)
                     |a -> RName (WRONG (a))
                     //|c ->Branch (c)