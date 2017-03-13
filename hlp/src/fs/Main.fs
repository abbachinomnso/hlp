module App.Main
open Fable.Import
open Fable.Core
open App.CodeMirrorInterface
open App.Renderer
open App.CodeMirrorImports




let getText (s: string)=
    let test=getById<Fable.Import.Browser.HTMLTextAreaElement> "test"
    let text:string= s
    text


let main () =
    printfn "Starting..."
    let editId = getById<Fable.Import.Browser.HTMLTextAreaElement> "code"
    printfn "Creating editor"
    let initOptions : CodeMirrorOptions= [LineNumbers;Mode("text/x-gas")]
    let cmEditor = App.CodeMirrorImports.CodeMirror.fromTextArea(editId, initOptions)
    printfn "Setting editor value"
    cmEditor.setValue "Enter Code hello"
    printfn "Line tokens: %A" (cmEditor.getLineTokens 0)


    let test=getById<Fable.Import.Browser.HTMLTextAreaElement> "test"
    test.defaultValue<-"Printing value here"


    let runButton = getById<Fable.Import.Browser.HTMLButtonElement> ("compile")
    runButton.textContent<-"Compile"
    runButton.addEventListener_click(fun _ -> getText(cmEditor.getValue()) ;null)


    render()
    printfn "Main code finished"


main()
