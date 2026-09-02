namespace CommonResourceFormats.AssetStore.Operations

open System
open CommonResourceFormats.AssetStore.Core.Domain
open CommonResourceFormats.AssetStore.Core.Exceptions
open Freql.Sqlite
open CommonResourceFormats.AssetStore.Store.Persistence


[<RequireQualifiedAccess>]
module Assets =

    module Internal =

        let entityTypeName = "asset"

        let getAssetAndVersion (ctx: SqliteContext) (assetId: EntityId) (version: Version) =
            Generic.getEntityAndVersion
                Operations.selectAssetRecord
                Operations.selectAssetVersionRecord
                entityTypeName
                ctx
                assetId
                version

        let getMetadata (ctx: SqliteContext) (assetId: EntityId) =
            Generic.getMetadataForEntity ctx entityTypeName assetId

        let getVersionMetadata (ctx: SqliteContext) (assetVersionId: EntityId) =
            Generic.getMetadataForEntity ctx $"{entityTypeName}_version" assetVersionId

        let setMetadataValue (ctx: SqliteContext) (assetId: EntityId) (key: string) (value: string) =
            match Operations.selectAssetMetadataItemRecord ctx [ "WHERE asset_id = @0" ] [ assetId.Serialize() ] with
            | None ->
                ({ AssetId = assetId.Serialize()
                   ItemKey = key
                   ItemValue = value }
                : Parameters.NewAssetMetadataItem)
                |> Operations.insertAssetMetadataItem ctx
            | Some value ->
                ctx.ExecuteVerbatimNonQueryAnon(
                    "UPDATE asset_metadata SET item_value = @0 WHERE asset_id = @1 AND item_key = @2",
                    [ value; assetId.Serialize(); key ]
                )
                |> ignore

        let setVersionMetadataValue (ctx: SqliteContext) (assetVersionId: EntityId) (key: string) (value: string) =
            match
                Operations.selectAssetVersionMetadataItemRecord
                    ctx
                    [ "WHERE asset_version_id = @0" ]
                    [ assetVersionId.Serialize() ]
            with
            | None ->
                ({ AssetId = assetVersionId.Serialize()
                   ItemKey = key
                   ItemValue = value }
                : Parameters.NewAssetMetadataItem)
                |> Operations.insertAssetMetadataItem ctx
            | Some value ->
                ctx.ExecuteVerbatimNonQueryAnon(
                    "UPDATE asset_metadata SET item_value = @0 WHERE asset_id = @1 AND item_key = @2",
                    [ value; assetVersionId.Serialize(); key ]
                )
                |> ignore


        let buildModel (ctx: SqliteContext) (er: Records.Asset) (evr: Records.AssetVersion) =
            let entityId = EntityId.Deserialize er.Id

            { Id = entityId
              VersionId = EntityId.Deserialize evr.Id
              Version = evr.Version
              AssetType = er.AssetType
              IsPrototype = evr.IsPrototype
              Metadata = getMetadata ctx entityId
              VersionMetadata = getVersionMetadata ctx entityId
              Path = evr.AssetPath |> EntityPath.Deserialize
              Resources =
                Operations.selectAssetVersionResourceRecords ctx [ "WHERE asset_version_id = @0" ] [ evr.Id ]
                |> List.map (fun avr ->
                    let rr =
                        Operations.selectResourceRecord ctx [ "WHERE id = @0" ] [ avr.ResourceId ]
                        |> Option.defaultWith (fun () -> raise (IllegalDatabaseState()))

                    ({ Id = EntityId.Deserialize rr.Id
                       Name = failwith "todo"
                       Description = failwith "todo"
                       FileType = failwith "todo"
                       Hash = failwith "todo"
                       Metadata = failwith "todo" }
                    : AssetResource))
                |> ResizeArray }


    let addNew (ctx: SqliteContext) (asset: NewAsset) =
        let createdOn = DateTime.UtcNow

        ({ Id = asset.Id.Serialize()
           Name = failwith "todo"
           AssetType = asset.AssetType
           CreatedOn = createdOn }
        : Parameters.NewAsset)
        |> Operations.insertAsset ctx


        ({ Id = asset.VersionId.Serialize()
           AssetId = asset.Id.Serialize()
           Version = 1
           IsPrototype = asset.IsPrototype
           CreatedOn = createdOn
           Active = true
           AssetPath = asset.Path.Serialize() }
        : Parameters.NewAssetVersion)
        |> Operations.insertAssetVersion ctx


        // TODO add metadata

        ()

    let add (ctx: SqliteContext) (assetType: string) (name: string) =
        let id = EntityId.Create()

        ({ Id = id.Serialize()
           Name = name
           AssetType = assetType
           CreatedOn = DateTime.UtcNow }
        : Parameters.NewAsset)
        |> Operations.insertAsset ctx

    [<RequireQualifiedAccess>]
    type AddVersionFailure =
        | AssetNotFound of EntityId
        | AssetVersionAlreadyExists of EntityId * int

    let addVersion (ctx: SqliteContext) (asset: EntityId) (version: Version) (isPrototype: bool) (path: EntityPath) =
        let id = EntityId.Create()

        match Operations.selectAssetRecord ctx [ "WHERE id = @0" ] [ asset.Serialize() ] with
        | None -> Error(AddVersionFailure.AssetNotFound asset)
        | Some ar ->
            match version with
            | Version.Latest ->

                let (previous, versionNumber) =
                    match
                        Operations.selectAssetVersionRecord
                            ctx
                            [ "WHERE asset_id = @0 ORDER BY version DESC LIMIT 1" ]
                            [ asset.Serialize() ]
                    with
                    | None -> (None, 1) // This is the first.
                    | Some value -> (Some value, value.Version + 1)

                // TODO handle cloning previous version.

                ({ Id = id.Serialize()
                   AssetId = ar.Id
                   Version = versionNumber
                   IsPrototype = isPrototype
                   CreatedOn = DateTime.UtcNow
                   Active = true
                   AssetPath = failwith "todo" }
                : Parameters.NewAssetVersion)
                |> Operations.insertAssetVersion ctx

                Ok id
            | Version.Specific i ->
                match
                    Operations.selectAssetVersionRecord ctx [ "WHERE asset_id = @0 AND version = @1" ] [ ar.Id; i ]
                with
                | Some _ -> failwith ""
                | None ->
                    ({ Id = id.Serialize()
                       AssetId = ar.Id
                       Version = i
                       IsPrototype = isPrototype
                       CreatedOn = DateTime.UtcNow
                       Active = true
                       AssetPath = path.Serialize() }
                    : Parameters.NewAssetVersion)
                    |> Operations.insertAssetVersion ctx

                Ok id

    let getMetadata (ctx: SqliteContext) (assetId: EntityId) = Internal.getMetadata ctx assetId

    let getVersionMetadata (ctx: SqliteContext) (assetVersionId: EntityId) =
        Internal.getVersionMetadata ctx assetVersionId

    let setMetadataValue (ctx: SqliteContext) (assetId: EntityId) (key: string) (value: string) =
        Internal.setMetadataValue ctx assetId key value

    let setVersionMetadataValue (ctx: SqliteContext) (assetVersionId: EntityId) (key: string) (value: string) =
        Internal.setVersionMetadataValue ctx assetVersionId key value

    [<RequireQualifiedAccess>]
    type GetFailure =
        | AssetNotFound of EntityId
        | AssetVersionNotFound of EntityId * Version

    [<RequireQualifiedAccess>]
    type GetVersionFailure = VersionNotFound of EntityId

    let getVersion (ctx: SqliteContext) (assetVersionId: EntityId) =
        match Operations.selectAssetVersionRecord ctx [ "WHERE id = @0" ] [ assetVersionId.Serialize() ] with
        | None -> Error(GetVersionFailure.VersionNotFound assetVersionId)
        | Some evr ->
            let er =
                Operations.selectAssetRecord ctx [ "WHERE id = @0" ] [ evr.AssetId ]
                |> Option.defaultWith (fun () -> failwith "Asset missing, this represents a illegal database state")

            Internal.buildModel ctx er evr |> Ok

    // ************ Get ************
    let get (ctx: SqliteContext) (assetId: EntityId) (version: Version) =
        match Internal.getAssetAndVersion ctx assetId version with
        | Error errorValue ->
            match errorValue with
            | Generic.EntityNotFound(_, id) -> Error <| GetFailure.AssetNotFound id
            | Generic.EntityVersionNotFound(_, id, version) -> Error <| GetFailure.AssetVersionNotFound(id, version)
        | Ok(er, evr) -> Internal.buildModel ctx er evr |> Ok
