namespace CommonResourceFormats.AssetStore.Operations

open Freql.Sqlite
open CommonResourceFormats.AssetStore.Store.Persistence

module Initialization =


    let run (ctx: SqliteContext) =
        Initialization.run true ctx
        
        // Currently this gets missed in the auto generated bindings because it's recursive nature.
        Records.SceneObject.InitializationSql true
        |> List.iter (ctx.ExecuteSqlNonQuery >> ignore)

        // Do what ever else is needed

        ()
