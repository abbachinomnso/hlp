namespace ARMSim
module Parser = 
    open System
    open Tokeniser
    open Types
    //type givemetext = |Instruct of Inst |ROL of RegorLiteral
    let getElements (str: string) =
        str.Split[|'\t'; ' ';'\n';'\r';'f'; ','; |] |> Array.toList
    
    

    ///to remove empty lines when parsing
    let checkempty lst =
        let rec checkempty' lst = 
            match lst with 
            |a::b when a = [] ->checkempty' b
            |a:: b -> a :: checkempty' b
            |[] -> []
        checkempty' lst
    
    ///to remove blanks when parsing
    let remove lst =
        let rec remove' n lst =
            match lst with
            //|[] -> []
            | h::tl when h = n -> remove' n tl
            | h::tl -> h :: (remove' n tl)
            |[] -> []
        remove' "" lst

    //gives full text with lines as different elements of list
    let putinList (strr:string) = strr |> removeComment |> List.map getElements |> List.map remove |> checkempty
    

    let getparsing (workingtext:string list) =
        let inst = workingtext.[0]
        let dest = workingtext.[1]
        //let op1 = workingtext.[2]
        //let op2 = workingtext.[3]
        match CompareInst inst with
        |"MOV" -> MOV(CompareS inst , CompareCond inst, CompareDes dest, CompareOperand workingtext.[2])
        |"ADD" -> ADD(CompareS inst, CompareCond inst, CompareDes dest, CompareDes workingtext.[2], CompareOperand workingtext.[3] )
        |"ADC" -> ADC(CompareS inst, CompareCond inst, CompareDes dest, CompareDes workingtext.[2], CompareOperand workingtext.[3] )
        |"SUB" -> SUB(CompareS inst, CompareCond inst, CompareDes dest, CompareDes workingtext.[2], CompareOperand workingtext.[3] )
        |"SBC" -> SBC(CompareS inst, CompareCond inst, CompareDes dest, CompareDes workingtext.[2], CompareOperand workingtext.[3] )
        |_ -> failwithf "invalid instruction"


    let parse (stringinput :string) =
        let workingtext = putinList stringinput
        workingtext |> List.map getparsing