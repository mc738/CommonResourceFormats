namespace CommonResourceFormats.AssetStore.Core.Domain

open System
open System.Collections.Generic
open System.IO
open System.Text.RegularExpressions
open FsToolbox.GameDevelopment.Core

module private Cfg =
    let serializationFormation = "n"

[<RequireQualifiedAccess>]
type EntityId =
    | Guid of Guid

    static member Deserialize(str: string) =
        Guid.ParseExact(str, Cfg.serializationFormation) |> EntityId.Guid

    static member Create() = EntityId.Guid <| Guid.NewGuid()

    member eid.Serialize() =
        match eid with
        | Guid guid -> guid.ToString(Cfg.serializationFormation)

[<RequireQualifiedAccess>]
type EntityKey =
    | Namespace of Namespace: string * Key: string
    | Literal of string

    member ek.Create(str: string) =
        match str.Contains(":") with
        | true ->
            let (h, t) = str.Split([| ':' |]) |> fun r -> r[0], r[1]
            EntityKey.Namespace(h, t)
        | false -> EntityKey.Literal str

    member ek.Serialize() =
        match ek with
        | Namespace(ns, key) -> $"{ns}:{key}"
        | Literal s -> s

type EntityMetadata =
    { Raw: Map<string, string> }

    static member Empty = { Raw = Map.empty }

    static member Create(kvs: (string * string) seq) = { Raw = kvs |> Map.ofSeq }

    static member FromMap(map: Map<string, string>) = { Raw = map }

    member emd.TryGet(key: EntityKey) = emd.Raw.TryFind <| key.Serialize()
    
    member emd.TryGetBool(key: EntityKey) =
        emd.TryGet key
        |> Option.bind (fun v ->
            match v.ToLowerInvariant() with
            | "1"
            | "y"
            | "t"
            | "true"
            | "yes"
            | "ok" -> Some true
            | "0"
            | "n"
            | "f"
            | "false" -> Some false
            | _ -> None)


[<RequireQualifiedAccess>]
type Version =
    | Latest
    | Specific of int

[<RequireQualifiedAccess>]
type EntityPath =
    | Relative of RelativePathType * string
    | Absolute of string

    static member Deserialize(str: string) =
        match str with
        | _ when str.StartsWith("$") -> Relative(RelativePathType.Root, str.Substring(2))
        | _ when str.StartsWith("%") ->
            let name = Regex.Match(str, "^%(.*)%").Value.Replace("%", "")
            Relative(RelativePathType.Named(name.Replace("%", "")), str.Substring(name.Length + 1))
        | _ -> EntityPath.Absolute str

    member ep.Serialize() =
        match ep with
        | Relative(relativePathType, s) ->
            Path.Combine(
                (match relativePathType with
                 | RelativePathType.Root -> "$"
                 | RelativePathType.Named name -> $"%%{name}%%"),
                s
            )
        | Absolute s -> s

and [<RequireQualifiedAccess>] RelativePathType =
    | Root
    | Named of Name: string

type Asset =
    { Id: EntityId
      VersionId: EntityId
      AssetType: string
      Version: int
      IsPrototype: bool
      Metadata: EntityMetadata
      VersionMetadata: EntityMetadata
      Path: EntityPath
      Resources: AssetResource ResizeArray }

and AssetResource =
    { Id: EntityId
      Name: string
      Description: string
      FileType: string
      Hash: string
      Metadata: EntityMetadata }

type NewAsset =
    { Id: EntityId
      VersionId: EntityId
      Name: string
      AssetType: string
      IsPrototype: bool
      Metadata: EntityMetadata
      VersionMetadata: EntityMetadata
      Path: EntityPath }

type ComponentAsset =
    { Id: EntityId
      Asset: Asset
      Metadata: EntityMetadata }

type ComponentAssetId = EntityId

type Component =
    { Id: EntityId
      VersionId: EntityId
      Version: int
      Name: string
      ComponentType: string

      Metadata: EntityMetadata
      VersionMetadata: EntityMetadata

      SerializedData: string

      Assets: Dictionary<ComponentAssetId, ComponentAsset> }

type NewComponent =
    { Id: EntityId
      VersionId: EntityId
      Name: string
      ComponentType: string
      Metadata: EntityMetadata
      VersionMetadata: EntityMetadata
      SerializedData: string }

type SceneObject =
    { Id: EntityId
      Name: string
      Children: SceneObject ResizeArray
      Components: SceneObjectComponent ResizeArray
      Transform: Transform
      Metadata: EntityMetadata }

and SceneObjectComponent =
    {
        Id: EntityId
        /// <summary>
        /// The component data is stored as a json string.
        /// It is up to the consumer to deserialize it.
        /// This is not 100% ideal but is flexible.
        /// </summary>
        OverrideComponentData: string option
        Component: Component
        Metadata: EntityMetadata
    }

type Scene =
    { Id: EntityId
      VersionId: EntityId
      Version: int
      Name: string

      // TODO metadata

      Objects: SceneObject ResizeArray }

type EntityListings = { Entities: EntitiesListingItem list }

and EntitiesListingItem =
    { Id: EntityId
      Name: string
      Versions: EntityVersionListingItem list }

and EntityVersionListingItem = { Id: EntityId; Version: int }
