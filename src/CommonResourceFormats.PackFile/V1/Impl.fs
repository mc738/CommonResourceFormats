namespace CommonResourceFormats.PackFile.V1

open System
open System.IO
open System.Text


type PackFileIndexItem =
    { Id: Guid // 16 bytes
      StartOffset: int64 // 8 bytes
      Length: int64 } // 8bytes

type PackFileBlobReference =
    { Id: Guid; Source: PackFileBlobSource }

and [<RequireQualifiedAccess>] PackFileBlobSource =
    | AssetStore
    | File of Path: string

type PackFileWriter(fs: FileStream) =

    member _.WriteFloat32(value: float32) = fs.Write(BitConverter.GetBytes(value))

    member _.WriteInt(value: int) =
        fs.Write(BitConverter.GetBytes(value).AsSpan())

    member _.WriteInt64(value: int64) =
        fs.Write(BitConverter.GetBytes(value).AsSpan())

    member _.WriteBool(value: bool) =
        fs.Write(BitConverter.GetBytes(value).AsSpan())

    member _.WriteString(value: string) = fs.Write(Encoding.UTF8.GetBytes(value))

    member _.WriteGuid(guid: Guid) = fs.Write(guid.ToByteArray())

type PackFileBuilder() as this =
    let references = ResizeArray<PackFileBlobReference>()
    let mutable versionSuffix = ""

    member _.WithReference(reference) =
        references.Add(reference)
        this

    member _.WithVersionSuffix(vs) =
        versionSuffix <- vs
        this

    member _.Build(outputPath: string, fileName: string, overwrite: bool) =

        use fs = File.Create(Path.Combine(outputPath, $"{fileName}.pck"))
        let writer = PackFileWriter(fs)

        let mutable startOffset = 0L

        let indexes =
            references
            |> Seq.map (fun br ->
                match br.Source with
                | PackFileBlobSource.AssetStore ->
                    // TODO
                    failwith "todo"
                | PackFileBlobSource.File path ->
                    let fi = FileInfo(path)

                    let result =
                        { Id = br.Id
                          StartOffset = startOffset
                          Length = fi.Length }

                    startOffset <- startOffset + fi.Length

                    result)
            |> Seq.toList

        let indexOffset = 8 // 4 for PACK, 4 for version

        // TODO this could change
        let indexesSize = 32 * indexes.Length

        // Magic string
        writer.WriteString("PACK")

        // Version
        writer.WriteInt(1)

        // Pad to 16?

        // write item count
        writer.WriteInt(indexes.Length)

        // Index offset
        writer.WriteInt(indexOffset)

        // Blobs offset
        writer.WriteInt(indexOffset + indexesSize)

        // Write indexes
        for index in indexes do
            writer.WriteGuid(index.Id)
            writer.WriteInt64(index.StartOffset)
            writer.WriteInt64(index.Length)

        // Write blobs
        for reference in references do
            match reference.Source with
            | PackFileBlobSource.AssetStore -> failwith "todo"
            | PackFileBlobSource.File path ->
                use nfs = File.OpenRead(path)

                nfs.CopyTo(fs)

type PackFileReader(fs: FileStream) as this =

    member _.ReadBytes(count: int) =
        let buffer = Array.create count 0uy
        fs.Read(buffer) |> ignore
        buffer

    member _.ReadInt() =
        let buffer = Array.create sizeof<int> 0uy
        fs.Read(buffer) |> ignore
        BitConverter.ToInt32(buffer)

    member _.ReadInt64() =
        let buffer = Array.create sizeof<int64> 0uy
        fs.Read(buffer) |> ignore
        BitConverter.ToInt64(buffer)

    member _.ReadGuid() =
        let buffer = Array.create sizeof<Guid> 0uy
        fs.Read(buffer) |> ignore
        Guid(buffer)

    member _.ReadFloat32() =
        let buffer = Array.create sizeof<float32> 0uy
        fs.Read(buffer) |> ignore
        BitConverter.ToSingle(buffer)

    member _.ReadRange(start: int64, length: int64) =
        let buffer = Array.create (int length) 0uy
        
        fs.Seek(start, SeekOrigin.Begin) |> ignore
        
        fs.Read(buffer) |> ignore
        buffer
        
    member _.Test() = ()

type PackFile(indexes: Map<Guid, PackFileIndexItem>, reader: PackFileReader, blobsStartOffset: int64) =
    //let indexes = Map<Guid, PackFileIndexItem>

    static member Load(path: string) =
        let fs = File.OpenRead(path)

        let reader = PackFileReader(fs)

        match reader.ReadBytes(4) = [| 80uy; 65uy; 67uy; 75uy |] with
        | false -> failwith "Magic bytes do not match."
        | true ->
            let version = reader.ReadInt()

            if version <> 1 then
                failwith "Incompatible version for reader"

            let itemCount = reader.ReadInt()
            let indexesOffset = reader.ReadInt()
            let blobsOffset = reader.ReadInt()

            let indexes =
                [ for i in 0 .. itemCount - 1 do
                      let id = reader.ReadGuid()
                      let startOffset = reader.ReadInt64()
                      let length = reader.ReadInt64()

                      id,
                      { Id = id
                        StartOffset = startOffset
                        Length = length } ]
                |> Map.ofList

            PackFile(indexes, reader, blobsOffset)

    member _.GetBlob(id: Guid) =
        match indexes.TryFind id with
        | None -> failwith "Not found"
        | Some index ->
            reader.ReadRange(blobsStartOffset + index.StartOffset, index.Length)
            