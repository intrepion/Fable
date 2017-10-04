module Fable.CLI.StateUtil

open Fable
open Fable.AST
open Fable.State
open System
open System.Reflection
open System.Collections.Concurrent
open System.Collections.Generic
open Microsoft.FSharp.Compiler
open Microsoft.FSharp.Compiler.SourceCodeServices
open Newtonsoft.Json
open ProjectCracker

module private Cache =
    let plugins = Dictionary<string,PluginInfo list>()
    let add (cache: Dictionary<'K,'V>) key value =
        cache.Add(key, value)
        value

type Command =
    | JSClient of jsonMsg:string * replyChannel:(string -> unit)
    | FileRequest of filePath:string * project:Project * replyChannel:(FSharpImplementationFileContents option -> unit)

let createRequester (agent: MailboxProcessor<Command>) =
    fun (filePath, project) ->
        Async.FromContinuations(fun (cb,_,_) ->
            FileRequest(filePath, project, cb) |> agent.Post)

let loadPlugins pluginPaths =
    pluginPaths
    |> Seq.collect (fun path ->
        let path = Path.normalizeFullPath path
        match Cache.plugins.TryGetValue(path) with
        | true, pluginInfos -> pluginInfos
        | false, _ ->
            try
                Reflection.loadAssembly path
                |> Reflection.getTypes
                |> Seq.filter typeof<IPlugin>.IsAssignableFrom
                |> Seq.map (fun x ->
                    { path = path
                    ; plugin = Activator.CreateInstance x :?> IPlugin })
                |> Seq.toList
                |> Cache.add Cache.plugins path
            with
            | ex -> failwithf "Cannot load plugin %s: %s" path ex.Message)
    |> Seq.toList

let getRelativePath path =
    Path.getRelativePath (IO.Directory.GetCurrentDirectory()) path

let hasFlag flagName (opts: IDictionary<string, string>) =
    match opts.TryGetValue(flagName) with
    | true, value ->
        match bool.TryParse(value) with
        | true, value -> value
        | _ -> false
    | _ -> false

let tryGetOption name (opts: IDictionary<string, string>) =
    match opts.TryGetValue(name) with
    | true, value -> Some value
    | false, _ -> None

let parseFile (checker: FSharpChecker) (project: Project) (timestamp: DateTime) filePath =
    // Log.logAlways(sprintf "Parsing %s..." (getRelativePath msg.path))
    let source = IO.File.ReadAllText(filePath)
    let results, answer =
        // About version see https://github.com/fsharp/FSharp.Compiler.Service/issues/796#issuecomment-333094956
        checker.ParseAndCheckFileInProject(filePath, int timestamp.Ticks, source, project.ProjectOptions)
        |> Async.RunSynchronously
    match answer with
    | FSharpCheckFileAnswer.Aborted ->
        failwith "ParseAndCheckFileInProject with Aborted"
    | FSharpCheckFileAnswer.Succeeded res ->
        match res.ImplementationFiles with
        | None -> failwith "ParseAndCheckFileInProject generated no Implementation files"
        // TODO: Check DependencyFiles property
        | Some implFiles -> implFiles, res.Errors

let updateProject (checker: FSharpChecker) requester (project: Project) (timestamp: DateTime) filePath =
    let implFiles, errors = parseFile checker project timestamp filePath
    // TODO: Handle errors
    project.UpdateImplementationFiles(implFiles)
    project

let recreateProject (checker: FSharpChecker) requester (project: Project) (timestamp: DateTime) filePath =
    printfn "RECREATE PROJECT"
    let implFiles, errors = parseFile checker project timestamp filePath
    let implFiles =
        (project.ImplementationFiles, implFiles) ||> Seq.fold (fun acc file ->
            Map.add (Path.normalizePath file.FileName) { AST=file; TimeStamp=timestamp } acc)
    Project(project.ProjectOptions, implFiles |> Seq.map (fun kv -> kv.Value.AST), errors, project.FableCore, requester)

let createProject checker requester (msg: Parser.Message) projFile =
    let projectOptions, fableCore =
        getFullProjectOpts checker msg projFile
    Log.logVerbose(lazy
        let proj = getRelativePath projectOptions.ProjectFileName
        let opts = projectOptions.OtherOptions |> String.concat "\n   "
        let files = projectOptions.SourceFiles |> String.concat "\n   "
        sprintf "F# PROJECT: %s\n   %s\n   %s" proj opts files)
    Log.logAlways(sprintf "Parsing %s..." (getRelativePath projectOptions.ProjectFileName))
    // When parsing the whole project, LoadTime must be reset
    let projectOptions = { projectOptions with LoadTime = DateTime.Now }
    let checkedProject =
        projectOptions
        |> checker.ParseAndCheckProject
        |> Async.RunSynchronously
    tryGetOption "saveAst" msg.extra |> Option.iter (fun dir ->
        Printers.printAst dir checkedProject)
    Project(projectOptions, checkedProject.AssemblyContents.ImplementationFiles, checkedProject.Errors, fableCore, requester)

let toJson =
    let jsonSettings =
        JsonSerializerSettings(
            Converters=[|Json.ErasedUnionConverter()|],
            NullValueHandling=NullValueHandling.Ignore)
            // StringEscapeHandling=StringEscapeHandling.EscapeNonAscii)
    fun (value: obj) ->
        JsonConvert.SerializeObject(value, jsonSettings)

let sendError replyChannel (ex: Exception) =
    let rec innerStack (ex: Exception) =
        if isNull ex.InnerException then ex.StackTrace else innerStack ex.InnerException
    let stack = innerStack ex
    Log.logAlways(sprintf "ERROR: %s\n%s" ex.Message stack)
    ["error", ex.Message] |> dict |> toJson |> replyChannel

let updateState (checker: FSharpChecker) (state: State) requester (msg: Parser.Message) =
    let addOrUpdateProject state (project: Project) =
        let state = Map.add project.ProjectFile project state
        state, project
    let checkWatchCompilation (project: Project) file filePath =
        let now = DateTime.Now
        if DateTime.Now - project.TimeStamp > TimeSpan.FromSeconds(5.) then
            recreateProject checker requester project now msg.path
            |> addOrUpdateProject state |> Some
        else
            project.TimeStamp <- now
            let currentTimestamp = IO.File.GetLastWriteTime(filePath)
            if currentTimestamp > file.TimeStamp then
                updateProject checker requester project currentTimestamp msg.path
                |> addOrUpdateProject state |> Some
            else Some(state, project)
    let tryFindAndUpdateProject state ext sourceFile =
        match msg.extra.TryGetValue("projectFile") with
        | true, projFile ->
            failwith "TODO: extra.projectFile"
            // let projFile = Path.normalizeFullPath projFile
            // match Map.tryFind projFile state with
            // | Some project -> checkWatchCompilation project sourceFile
            // | None -> createProject checker requester msg projFile
            //           |> addOrUpdateProject state |> Some
        | false, _ ->
            state |> Map.tryPick (fun _ (project: Project) ->
                match Map.tryFind sourceFile project.ImplementationFiles with
                | Some file -> checkWatchCompilation project file sourceFile
                | None -> None)
    match IO.Path.GetExtension(msg.path).ToLower() with
    | ".fsproj" | ".fsx" ->
        createProject checker requester msg msg.path
        |> addOrUpdateProject state
    | ".fs" as ext ->
        match tryFindAndUpdateProject state ext msg.path with
        | Some stateAndProject -> stateAndProject
        | None ->
            state |> Map.map (fun _ p -> p.ProjectFile) |> Seq.toList
            |> failwithf "%s doesn't belong to any of loaded projects %A" msg.path
    | ".fsi" -> failwithf "Signature files cannot be compiled to JS: %s" msg.path
    | _ -> failwithf "Not an F# source file: %s" msg.path

let addFSharpErrorLogs (com: ICompiler) (errors: FSharpErrorInfo array) (fileFilter: string) =
    for er in errors do
        if Path.normalizePath er.FileName = fileFilter then
            let severity =
                match er.Severity with
                | FSharpErrorSeverity.Warning -> Severity.Warning
                | FSharpErrorSeverity.Error -> Severity.Error
            let range =
                { start={ line=er.StartLineAlternate; column=er.StartColumn}
                  ``end``={ line=er.EndLineAlternate; column=er.EndColumn} }
            com.AddLog(er.Message, severity, range, er.FileName, "FSHARP")

let compile (com: Compiler) (project: Project) (filePath: string) =
    let babel =
        if filePath.EndsWith(".fsproj") then
            Fable2Babel.Compiler.createFacade project.ProjectOptions.SourceFiles filePath
        else
            let implFile =
                match Map.tryFind filePath project.ImplementationFiles with
                | Some file -> file.AST
                | None -> failwithf "File %s doesn't belong to parsed project %s" filePath project.ProjectFile
            FSharp2Fable.Compiler.transformFile com project implFile filePath
            |> Fable2Babel.Compiler.transformFile com project
    // Add logs and convert to JSON
    addFSharpErrorLogs com project.Errors filePath
    let loc = defaultArg babel.loc SourceLocation.Empty
    Babel.Program(babel.fileName, loc, babel.body, babel.directives, com.ReadAllLogs(), babel.dependencies)
    |> toJson

let startAgent () =
  MailboxProcessor<Command>.Start(fun agent ->
    let requester = createRequester agent
    let rec loop (checker: FSharpChecker) (oldState: State) = async {
        let! msg = agent.Receive()
        match msg with
        | JSClient (jsonMsg, replyChannel) ->
            try
                let msg = Parser.parse jsonMsg
                // lazy sprintf "Received message %A" msg |> Log.logVerbose
                let newState, activeProject = updateState checker oldState requester msg
                async {
                    try
                        let comOptions =
                            { fableCore =
                                match activeProject.FableCore with
                                | FilePath p -> (Path.getRelativePath msg.path p).TrimEnd('/')
                                | NonFilePath p -> p.TrimEnd('/')
                              emitReplacements = Map.empty // TODO: Parse from message
                              typedArrays = msg.typedArrays
                              clampByteArrays = msg.clampByteArrays
                              declaration = msg.declaration }
                        let com = Compiler(options=comOptions, plugins=loadPlugins msg.plugins)
                        compile com activeProject msg.path |> replyChannel
                    with ex ->
                        sendError replyChannel ex
                } |> Async.Start
                return! loop checker newState
            with ex ->
                sendError replyChannel ex
                return! loop checker oldState
        | FileRequest (filePath, project, replyChannel) ->
            match Map.tryFind filePath project.ImplementationFiles with
            | Some file ->
                let currentTimestamp = IO.File.GetLastWriteTime(filePath)
                if currentTimestamp > file.TimeStamp then
                    printfn "RECOMPILING REQUESTED FILE %s" filePath
                    let project = updateProject checker requester project currentTimestamp filePath
                    Map.tryFind filePath project.ImplementationFiles
                    |> Option.map (fun f -> f.AST) |> replyChannel
                else
                    replyChannel (Some file.AST)
            | None ->
                replyChannel None
            return! loop checker oldState
    }
    let checker = FSharpChecker.Create(keepAssemblyContents=true)
    loop checker Map.empty
  )
