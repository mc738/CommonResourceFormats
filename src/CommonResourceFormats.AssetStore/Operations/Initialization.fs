namespace CommonResourceFormats.AssetStore.Operations

open Freql.Sqlite
open CommonResourceFormats.AssetStore.Store.Persistence

module Initialization =


    let run (ctx: SqliteContext) =
        Initialization.run true ctx

        // Do what ever else is needed

        ()
