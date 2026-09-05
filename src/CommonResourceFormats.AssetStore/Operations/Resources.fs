namespace CommonResourceFormats.AssetStore.Operations

open CommonResourceFormats.AssetStore.Core.Domain
open Freql.Sqlite
open CommonResourceFormats.AssetStore.Store.Persistence

[<RequireQualifiedAccess>]
module Resources =
    
    module private Internal =
        
        let entityTypeName = "resource"
    
    
    let add (ctx: SqliteContext) =
        ()
    
    let getMetadata (ctx: SqliteContext) (resourceId: EntityId) =
        Generic.getMetadataForEntity ctx Internal.entityTypeName resourceId

    let setMetadataValue (ctx: SqliteContext) (resourceId: EntityId) (key: string) (value: string) =
        match
            Operations.selectResourceMetadataItemRecord ctx [ "WHERE resource_id = @0" ] [ resourceId.Serialize() ]
        with
        | None ->
            ({ ResourceId = resourceId.Serialize()
               ItemKey = key
               ItemValue = value }
            : Parameters.NewResourceMetadataItem)
            |> Operations.insertResourceMetadataItem ctx
        | Some value ->
            ctx.ExecuteVerbatimNonQueryAnon(
                "UPDATE resource_metadata SET item_value = @0 WHERE resource_id = @1 AND item_key = @2",
                [ value; resourceId.Serialize(); key ]
            )
            |> ignore
    