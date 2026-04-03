module ElmOffline exposing (run)

import BackendTask exposing (BackendTask)
import BackendTask.Http as Http
import Build
import BuildTask exposing (BuildTask, FileOrDirectory)
import BuildTask.Do as Do
import BuildTask.Unsafe as Unsafe
import Cli.Option as Option
import Cli.OptionsParser as OptionsParser
import Cli.Program as Program
import Dict exposing (Dict)
import FatalError exposing (FatalError)
import Json.Decode
import Pages.Script as Script exposing (Script)
import Path exposing (Path)
import Result.Extra
import Url.Builder


run : Script
run =
    Script.withCliOptions programConfig Build.toTask


programConfig : Program.Config (Build.Config Inputs)
programConfig =
    Program.config
        |> Program.add
            (OptionsParser.build
                (Build.Config getInputs buildAction)
                |> OptionsParser.with
                    (Option.requiredKeywordArg "build"
                        |> Option.map Path.path
                        |> Option.withDisplayName "dir"
                        |> Option.withDescription "Build directory - contains the intermediate files"
                    )
                |> OptionsParser.with
                    (Option.requiredKeywordArg "output"
                        |> Option.map Path.path
                        |> Option.withDisplayName "dir"
                        |> Option.withDescription "Output directory"
                    )
                |> OptionsParser.with
                    (Option.flag "remove-stale"
                        |> Option.withDescription "Remove unused files from the build directory"
                    )
                |> OptionsParser.with
                    (Option.optionalKeywordArg "jobs"
                        |> Option.withDescription "Number of parallel jobs to run"
                        |> Option.withDisplayName "n"
                        |> Option.validateMapIfPresent
                            (\j ->
                                case String.toInt j of
                                    Nothing ->
                                        Err ("Invalid number of jobs: " ++ j)

                                    Just i ->
                                        Ok i
                            )
                    )
                |> OptionsParser.with
                    (Option.flag "debug"
                        |> Option.withDescription "Output debug info"
                    )
                |> OptionsParser.with
                    (Option.optionalKeywordArg "hash-kind"
                        |> Option.withDescription "Kind of hash to use. Choose fast for FNV1a, secure for sha256."
                        |> Option.withDefault "fast"
                        |> Option.oneOf [ ( "fast", Build.fastHash ), ( "secure", Build.secureHash ) ]
                    )
            )


type alias Inputs =
    List ( String, String )


getInputs : BackendTask FatalError Inputs
getInputs =
    -- 6557 is the 0.19 cutoff
    Http.getJson "https://package.elm-lang.org/all-packages/since/6557"
        (Json.Decode.list Json.Decode.string)
        |> BackendTask.allowFatal
        |> BackendTask.andThen
            (\strings ->
                strings
                    |> Result.Extra.combineMap
                        (\string ->
                            case String.split "@" string of
                                [ packageName, packageVersion ] ->
                                    Ok ( packageName, packageVersion )

                                _ ->
                                    Err (FatalError.fromString ("Invalid package name@version: " ++ string))
                        )
                    |> BackendTask.fromResult
            )


buildAction : Inputs -> BuildTask FileOrDirectory
buildAction packages =
    Do.jobs <| \parallelism ->
    BuildTask.do
        (packages
            |> List.map
                (\( packageName, packageVersion ) ->
                    BuildTask.do
                        (Unsafe.downloadImmutable
                            (Url.Builder.crossOrigin
                                "https://package.elm-lang.org"
                                [ "packages"
                                , packageName
                                , packageVersion
                                , "endpoint.json"
                                ]
                                []
                            )
                        )
                    <| \hash ->
                    BuildTask.succeed
                        { filename = Path.path (packageName ++ "/" ++ packageVersion ++ "/endpoint.json")
                        , hash = hash
                        }
                )
            |> BuildTask.combineBy parallelism
        )
    <| \files ->
    BuildTask.combine files
