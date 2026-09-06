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
    // Format: $scope:key
    // This used the $scope as the name space.
    // It is internal to the metadata and not shared
    | ScopeDefinition of Scope: string * Key: string

    // Format: @[scope]__[index]__[namespace]:[key]
    | RepeatNamespace of Scope: string * Index: int * Namespace: string * Key: string
    // Format: [namespace]:[key]
    | Namespace of Namespace: string * Key: string
    | Literal of string

    static member Deserialize(str: string) =
        match str.Contains(":") with
        | true ->

            let (h, t) = str.Split([| ':' |]) |> fun r -> r[0], r[1]

            match h.[0] with
            | '@' ->
                let rSplit = h.Split("__")

                EntityKey.RepeatNamespace(rSplit[0].Substring(1), Int32.Parse(rSplit[1]), rSplit[2], t)
            | '$' -> EntityKey.ScopeDefinition(h.Substring(1), t)
            | _ -> EntityKey.Namespace(h, t)

        | false -> EntityKey.Literal str

    member ek.Serialize() =
        match ek with
        | ScopeDefinition(scope, key) -> $"${scope}:{key}"
        | RepeatNamespace(scope, index, ns, key) -> $"@{scope}__{index}__{ns}:{key}"
        | Namespace(ns, key) -> $"{ns}:{key}"
        | Literal s -> s

    member ek.NamespaceEquals(ons: string) =
        match ek with
        | ScopeDefinition(scope, key) -> false
        | RepeatNamespace(scope, index, ns, key) -> ns.Equals(ons, StringComparison.OrdinalIgnoreCase)
        | Namespace(ns, key) -> ns.Equals(ons, StringComparison.OrdinalIgnoreCase)
        | Literal s -> false


type EntityMetadata =
    { Raw: Map<string, string> }

    static member Empty = { Raw = Map.empty }

    static member Create(kvs: (string * string) seq) = { Raw = kvs |> Map.ofSeq }

    static member Create(kvs: (EntityKey * string) seq) =
        { Raw = kvs |> Seq.map (fun (f, s) -> f.Serialize(), s) |> Map.ofSeq }

    static member FromMap(map: Map<string, string>) = { Raw = map }


    static member ``count-key`` = "count"

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

    member emd.TryGetInt(key: EntityKey) =
        emd.TryGet key
        |> Option.bind (fun v ->
            match Int32.TryParse v with
            | false, _ -> None
            | true, r -> Some r)


    member emd.TryGetEntityId(key: EntityKey) =
        emd.TryGet key |> Option.map EntityId.Deserialize

    member emd.GetScopedCollection(scope: string) =
        // Get
        let scopeCountKey = EntityKey.ScopeDefinition(scope, EntityMetadata.``count-key``)

        let count = emd.TryGetInt scopeCountKey |> Option.defaultValue 0

        { Scope = scope
          Items =
            [ for i in 0 .. count - 1 do
                  { Index = i
                    Metadata =
                      emd.Raw
                      |> Map.filter (fun k v -> k.StartsWith($@"{scope}__{i}__"))
                      |> Map.toList
                      |> List.map (fun (k, v) ->
                          // Convert repeat name spaces to normal ones.
                          // This is for convenience but might change.
                          match EntityKey.Deserialize k with
                          | EntityKey.ScopeDefinition _ as k -> k, v
                          | EntityKey.RepeatNamespace(_, _, ns, key) -> EntityKey.Namespace(ns, key), v
                          | EntityKey.Namespace _ as k -> k, v
                          | EntityKey.Literal _ as k -> k, v)
                      |> EntityMetadata.Create } ] }

and EntityMetadataScopedCollection =
    { Scope: string
      Items: EntityMetadataScopedCollectionItem list }

and EntityMetadataScopedCollectionItem =
    { Index: int
      Metadata: EntityMetadata }

    member eci.GetRealKey(scope: string, key: EntityKey) =
        match key with
        | EntityKey.ScopeDefinition _ -> key
        | EntityKey.RepeatNamespace _ -> key
        | EntityKey.Namespace(ns, key) -> EntityKey.RepeatNamespace(scope, eci.Index, ns, key)
        | EntityKey.Literal s -> key

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

type AssetId = EntityId

type Asset =
    { Id: EntityId
      VersionId: EntityId
      AssetType: EntityKey
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
      AssetType: EntityKey
      IsPrototype: bool
      Metadata: EntityMetadata
      VersionMetadata: EntityMetadata
      Path: EntityPath }

type ComponentId = EntityId

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
      ComponentType: EntityKey

      Metadata: EntityMetadata
      VersionMetadata: EntityMetadata

      SerializedData: string

      Assets: Dictionary<ComponentAssetId, ComponentAsset> }

type NewComponent =
    { Id: EntityId
      VersionId: EntityId
      Name: string
      ComponentType: EntityKey
      Metadata: EntityMetadata
      VersionMetadata: EntityMetadata
      SerializedData: string }

type NewComponentAsset =
    { Id: EntityId
      ComponentVersionId: EntityId
      AssetVersionId: EntityId
      Metadata: EntityMetadata }

type SceneObjectId = EntityId

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
