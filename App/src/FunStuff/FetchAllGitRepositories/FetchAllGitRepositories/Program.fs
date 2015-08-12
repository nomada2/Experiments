#if INTERACTIVE
#r "../../../bin/FSharp.Data.dll"
#endif

open System
open System.IO
open System.Net
open System.Reflection
open FSharp.Data

let username = "rikace"
let gitUrl = sprintf "https://api.github.com/users/%s/repos" username


let UseUnsafeHeaderParsing() =
    let assembly : Assembly = Assembly.GetAssembly(typeof<System.Net.Configuration.SettingsSection>)    
    let myType : Type = assembly.GetType("System.Net.Configuration.SettingsSectionInternal")
    let obj : Object = myType.InvokeMember("Section", BindingFlags.Static ||| BindingFlags.GetProperty ||| BindingFlags.NonPublic, null, null, Array.zeroCreate 0)
    let fi : FieldInfo = myType.GetField("useUnsafeHeaderParsing", BindingFlags.NonPublic ||| BindingFlags.Instance)
    if (not(Convert.ToBoolean(fi.GetValue(obj)))) then
        fi.SetValue(obj, true)


let getHtml url = 
    UseUnsafeHeaderParsing()    
    let wreq = WebRequest.Create("https://api.github.com/users/rikace/repos") :?> HttpWebRequest
    wreq.Headers.Add(HttpRequestHeader.Authorization, "Basic " + Convert.ToBase64String(System.Text.ASCIIEncoding.ASCII.GetBytes("rikace:Jocker74!")));
    wreq.Credentials <- new System.Net.NetworkCredential("rikace", "Jocker74!")
    wreq.KeepAlive <- false
    wreq.UserAgent <- "Dummy"
    wreq.Accept <- "application/json"
    wreq.Method <- "GET"
    let wresp = wreq.GetResponse()
    let stream = wresp.GetResponseStream()
    let reader = new StreamReader(stream)
    reader.ReadToEnd()

let [<LiteralAttribute>] fileJson = __SOURCE_DIRECTORY__ + @"\\gitwebpage.json"
let data = getHtml gitUrl
File.WriteAllText(fileJson, data)

type GitSample = JsonProvider<fileJson>
let data' = GitSample.Parse(data)

let prgPath = Environment.GetFolderPath(Environment.SpecialFolder.ProgramFilesX86);

data'
|> Seq.map(fun i -> i.FullName)
|> Seq.take 1
|> Seq.iter( fun i -> 
    printfn "Cloning Repository %s..." i
    printfn @"%s\Git\bin\git.exe clone https://github.com/%s" prgPath i
    System.Diagnostics.Process.Start((sprintf @"%s\Git\bin\git.exe clone https://github.com/%s" prgPath i)) |> ignore)




[<EntryPoint>]
let main argv = 
    printfn "%A" (getHtml gitUrl)

    0 // return an integer exit code
