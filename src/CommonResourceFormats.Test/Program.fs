open System
open System.IO
open System.Text
open CommonResourceFormats.PackFile.V1

let file1 = File.ReadAllBytes("C:\\Users\\mclif\\Projects\\project_boom\\asset_pipeline\\textures\\prototypes\\generic\\brick_1\\DefaultMaterial_Base_color.png")
let file2  = File.ReadAllBytes("C:\\Users\\mclif\\Projects\\project_boom\\asset_pipeline\\textures\\prototypes\\generic\\brick_1\\DefaultMaterial_Height.png")

let id1 = Guid.NewGuid()
let id2 = Guid.NewGuid()

PackFileBuilder()
    .WithReference({ Id = id1; Source = PackFileBlobSource.File "C:\\Users\\mclif\\Projects\\project_boom\\asset_pipeline\\textures\\prototypes\\generic\\brick_1\\DefaultMaterial_Base_color.png" })
    .WithReference({ Id = id2; Source = PackFileBlobSource.File "C:\\Users\\mclif\\Projects\\project_boom\\asset_pipeline\\textures\\prototypes\\generic\\brick_1\\DefaultMaterial_Height.png" })
    .Build("C:\\Users\\mclif\\Projects", "test", true)
    
let file = PackFile.Load("C:\\Users\\mclif\\Projects\\test.pck")

let loadedBlob1 = file.GetBlob(id1)
let loadedBlob2 = file.GetBlob(id2)

// For more information see https://aka.ms/fsharp-console-apps
printfn "Hello from F#"