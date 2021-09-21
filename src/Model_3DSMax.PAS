Unit Model_3DSMax;
{
-----------------------------------------------------------------------------
This source file is part of OGRE
    (Object-oriented Graphics Rendering Engine)
For the latest info, see http://www.stevestreeting.com/ogre/

Copyright © 2000-2001 Steven J. Streeting
Also see acknowledgements in Readme.html

This program is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free Software
Foundation; either version 2 of the License, or (at your option) any later
version.

This program is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.

You should have received a copy of the GNU General Public License along with
this program; if not, write to the Free Software Foundation, Inc., 59 Temple
Place - Suite 330, Boston, MA 02111-1307, USA, or go to
http://www.gnu.org/copyleft/gpl.html.
-----------------------------------------------------------------------------
}



// --------------------------------------------------------------------------
// Redesigned and converted to Delphi by Windcatcher.  Also added ability to
// save to .3DS file.
// --------------------------------------------------------------------------



// 3DS Model file modified by Steve Streeting 2001 for OGRE
// Original credits shown below
// Modifications clearly marked, delimited by:
//    ...

//////////////////////////////////////////////////////////////////////
//
// 3D Studio Model Class
// by: Matthew Fairfax
//
// Model_3DS.h: interface for the Model_3DS class.
// This is a simple class for loading and viewing
// 3D Studio model files (.3ds). It supports models
// with multiple objects. It also supports multiple
// textures per object. It does not support the animation
// for 3D Studio models b/c there are simply too many
// ways for an artist to animate a 3D Studio model and
// I didn't want to impose huge limitations on the artists.
// However, I have imposed a limitation on how the models are
// textured:
// 1) Every faces must be assigned a material
// 2) If you want the face to be textured assign the
//    texture to the Diffuse Color map
// 3) The texture must be supported by the GLTexture class
//    which only supports bitmap and targa right now
// 4) The texture must be located in the same directory as
//    the model
//
// Support for non-textured faces is done by reading the color
// from the material's diffuse color.
//
// Some models have problems loading even if you follow all of
// the restrictions I have stated and I don't know why. If you
// can import the 3D Studio file into Milkshape 3D
// (http://www.swissquake.ch/chumbalum-soft) and then export it
// to a new 3D Studio file. This seems to fix many of the problems
// but there is a limit on the number of faces and vertices Milkshape 3D
// can read.
//
// Usage:
// Model_3DS m;
//
// m.Load("model.3ds"); // Load the model
// m.Draw();            // Renders the model to the screen
//
// // If you want to show the model's normals
// m.shownormals = true;
//
// // If the model is not going to be lit then set the lit
// // variable to false. It defaults to true.
// m.lit = false;
//
// // You can disable the rendering of the model
// m.visible = false;
//
// // You can move and rotate the model like this:
// m.rot.x = 90.0;
// m.rot.y = 30.0;
// m.rot.z = 0.0;
//
// m.pos.x = 10.0;
// m.pos.y = 0.0;
// m.pos.z = 0.0;
//
// // If you want to move or rotate individual objects
// m.Objects[0].rot.x = 90.0;
// m.Objects[0].rot.y = 30.0;
// m.Objects[0].rot.z = 0.0;
//
// m.Objects[0].pos.x = 10.0;
// m.Objects[0].pos.y = 0.0;
// m.Objects[0].pos.z = 0.0;
//
//////////////////////////////////////////////////////////////////////

Interface

Uses Classes;

Type
  // A VERY simple vector struct
  // I could have included a complex class but I wanted the model class to stand alone
  Vector_3DS = Packed Record
    X : Single;
    Y : Single;
    Z : Single;
  End;

  TexCoord_3DS = Packed Record
    TX : Single;
    TZ : Single;
  End;

  Face_3DS = Packed Record
    Index    : Array[0..2] Of Word;
    MatIndex : Integer;
  End;

  // Color struct holds the diffuse color of the material
  Color4i_3DS = Packed Record
    R,G,B,A: Byte;
  End;

  // Holds the material info
  // TODO: add color support for non textured polys
  Material_3DS = Packed Record
    Name              : String;     // The material's name
    TexName           : String;     // Texture file name
    OpacityMapName    : String;     // Opacity map file name
    Textured          : Boolean;    // Whether or not it is textured
    OpacityMap        : Boolean;    // Whether or not it has an opacity map
    Diffuse           : Color4i_3DS;
    Ambient           : Color4i_3DS;
    Specular          : Color4i_3DS;
    Shininess         : Word;       // Percentage shininess ratio    (0=matte, 100=shiny)
    ShininessStrength : Word;       // Percentage shininess strength (0=matte, 100=shiny)
    Transparency      : Word;       // Percentage transparency       (0=opaque, 100=transparent)
    DiffuseSet        : Boolean;
    AmbientSet        : Boolean;
    SpecularSet       : Boolean;
  End;

  Light_3DS = Packed Record
    Name       : String;
    Location   : Vector_3DS;
    Color      : Color4i_3DS;
    Off        : Boolean;
    InnerRange : Single;
    OuterRange : Single;
    Multiplier : Single;   // Intensity multiplier.  Should never be 0.
  End;

  // Every chunk in the 3ds file starts with this struct
  ChunkHeader = Packed Record
    ID  : Word;        // The chunk's id
    Len : LongInt;     // The length of the chunk
  End;

  // I sort the mesh by material so that I won't have to switch textures a great deal
  MaterialFaces = Packed Record
    SubFaces         : Packed Array Of Face_3DS; // Index to our vertex array of all the faces that use this material
    NumSubFaces      : Integer;                  // The number of faces
    MatIndex         : Integer;                  // An index to our materials
    UseOwnVertexList : Boolean;
    NumOwnVertices   : Integer;
    OwnVertices      : Packed Array Of Vector_3DS;
    OwnNormals       : Packed Array Of Vector_3DS;
    OwnTexCoords     : Packed Array Of TexCoord_3DS;
  End;

  // The 3ds file can be made up of several objects
  Object_3DS = Packed Record
    Name                  : String;                          // The object name
    Vertexes              : Packed Array Of Vector_3DS;      // The array of vertices
    Normals               : Packed Array Of Vector_3DS;      // The array of the normals for the vertices
    TexCoords             : Packed Array Of TexCoord_3DS;    // The array of texture coordinates for the vertices
    Faces                 : Packed Array Of Face_3DS;        // The array of face indices
    NumFaces              : Integer;                         // The number of faces
    NumMatFaces           : Integer;                         // The number of differnet material faces
    NumVerts              : Integer;                         // The number of vertices
    NumTexCoords          : Integer;                         // The number of vertices
    Textured              : Boolean;                         // True: the object has textures
    MatFaces              : Packed Array Of MaterialFaces;   // The faces are divided by materials
    Light                 : Light_3DS;                       // A light source
    HasLight              : Boolean;                         // Whether the data in Light is valid
    Pos                   : Vector_3DS;                      // The position to move the object to
    Rot                   : Vector_3DS;                      // The angles to rotate the object
    FaceNormals           : Packed Array Of Vector_3DS;
    FaceSmoothGroups      : Packed Array Of LongWord;
    LocalMatrix           : Packed Array [0..3,0..2] Of Single;
    IncludeSharedVertices : Boolean;
  End;

  Model_3DS = Class
  Public
    ModelName    : String;           // The name of the model
    Path         : String;           // The path of the model
    NumObjects   : Integer;          // Total number of objects in the model
    NumMaterials : Integer;          // Total number of materials in the model
    TotalVerts   : Integer;          // Total number of vertices in the model
    TotalFaces   : Integer;          // Total number of faces in the model
    ShowNormals  : Boolean;          // True: show the normals
    Materials    : Packed Array Of Material_3DS;         // The array of materials
    Objects      : Packed Array Of Object_3DS;           // The array of objects in the model
    Pos          : Vector_3DS;       // The position to move the model to
    Rot          : Vector_3DS;       // The angles to rotate the model
    Scale        : Single;           // The size you want the model scaled to
    Lit          : Boolean;          // True: the model is lit
    Visible      : Boolean;          // True: the model gets rendered
    Bin3DS       : File;             // The binary 3ds file
    Constructor Create;              // Constructor
    Destructor  Destroy; Override;   // Destructor
    Procedure   Load(Name: String);  // Loads a model
    Procedure   Save(Name: String);  // Saves a model
  Private
    Procedure IntColorChunkProcessor(Len,FIndex: LongInt; MatIndex: Integer; ColType: Word);
    Procedure FloatColorChunkProcessor(Len,FIndex: LongInt; MatIndex: Integer; ColType: Word);
    // Processes the Main Chunk that all the other chunks exist is
    Procedure MainChunkProcessor(Len,FIndex: LongInt);
    // Processes the model's info
    Procedure EditChunkProcessor(Len,FIndex: LongInt);

    // Processes the model's materials
    Procedure MaterialChunkProcessor(Len,FIndex: LongInt; MatIndex: Integer);
    // Processes the names of the materials
    Procedure MaterialNameChunkProcessor(Len,FIndex: LongInt; MatIndex: Integer);
    // Processes the material's diffuse color
    Procedure ColorChunkProcessor(Len,FIndex: LongInt; MatIndex: Integer; ColType: Word);
    // Processes material shininess
    Procedure ShininessChunkProcessor(Len,FIndex: LongInt; MatIndex: Integer);
    // Processes material shininess strength
    Procedure ShininessStrengthChunkProcessor(Len,FIndex: LongInt; MatIndex: Integer);
    // Processes material transparency
    Procedure TransparencyChunkProcessor(Len,FIndex: LongInt; MatIndex: Integer);
    // Processes the material's texture maps
    Procedure TextureMapChunkProcessor(Len,FIndex: LongInt; MatIndex: Integer);
    // Processes the material's opacity maps
    Procedure OpacityMapChunkProcessor(Len,FIndex: LongInt; MatIndex: Integer);
    // Processes the names of the textures and load the textures
    Procedure TextureMapNameChunkProcessor(Len,FIndex: LongInt; MatIndex: Integer);
    // Processes the names of the opacity maps and load them
    Procedure OpacityMapNameChunkProcessor(Len,FIndex: LongInt; MatIndex: Integer);

    // Processes the model's geometry
    Procedure ObjectChunkProcessor(Len,FIndex: LongInt; ObjIndex: Integer);
    // Processes the triangles of the model
    Procedure TriangularMeshChunkProcessor(Len,FIndex: LongInt; ObjIndex: Integer);
    // Processes the vertices of the model and loads them
    Procedure VertexListChunkProcessor(Len,FIndex: LongInt; ObjIndex: Integer);
    // Processes the texture cordiantes of the vertices and loads them
    Procedure TexCoordsChunkProcessor(Len,FIndex: LongInt; ObjIndex: Integer);
    // Processes smoothing groups
    Procedure SmoothGroupChunkProcessor(Len,FIndex: LongInt; ObjIndex: Integer);
    // Processes the faces of the model and loads the faces
    Procedure FacesDescriptionChunkProcessor(Len,FIndex: LongInt; ObjIndex: Integer);
    // Processes the materials of the faces and splits them up by material
    Procedure FacesMaterialsListChunkProcessor(Len,FIndex: LongInt; ObjIndex,SubFacesIndex: Integer);
    // Processes local matrix for verts
    Procedure LocalCoordinatesChunkProcessor(Len,FIndex: LongInt; ObjIndex: Integer);
    // Calculates the normals of the vertices by averaging
    // the normals of the faces that use that vertex
    Procedure CalculateNormals;
    // Updates the vertex data based on local transform
    Procedure TransformLocalCoords;
    // Processes the light source
    Procedure LightChunkProcessor(Len,FIndex: LongInt; ObjIndex: Integer);
    // Processes the light inner attenuation range
    Procedure LightInnerRangeChunkProcessor(Len,FIndex: LongInt; ObjIndex: Integer);
    // Processes the light outer attenuation range
    Procedure LightOuterRangeChunkProcessor(Len,FIndex: LongInt; ObjIndex: Integer);
    // Processes the light intensity multiplier
    Procedure LightMultiplierChunkProcessor(Len,FIndex: LongInt; ObjIndex: Integer);

    Procedure WriteVersionChunk(Stream: TStream);

    Procedure WriteEditChunk(Stream: TStream);
    Procedure WriteMeshVersionChunk(Version: Integer; Stream: TStream);
    Procedure WriteObjectChunk(Index: Integer; Stream: TStream);
    Procedure WriteTriangularMeshChunk(ObjectIndex: Integer; Stream: TStream);
    Procedure WriteVertexListChunk(ObjectIndex: Integer; Stream: TStream);
    Procedure WriteLocalCoordinatesChunk(ObjectIndex: Integer; Stream: TStream);
    Procedure WriteTexCoordsChunk(ObjectIndex: Integer; Stream: TStream);
    Procedure WriteFacesDescriptionChunk(ObjectIndex: Integer; Stream: TStream);
    Procedure WriteFacesMaterialsListChunk(ObjectIndex,SubIndex: Integer; Stream: TStream);
    Procedure WriteSmoothGroupChunk(ObjectIndex: Integer; Stream: TStream);

    Procedure WriteMaterialChunk(MaterialIndex: Integer; Stream: TStream);
    Procedure WriteMaterialNameChunk(MaterialIndex: Integer; Stream: TStream);
    Procedure WriteMatIntColorChunk(MaterialIndex,ID: Integer; Var Color: Color4i_3DS; Stream: TStream);
    Procedure WriteIntColorChunk(Var Color: Color4i_3DS; Stream: TStream);
    Procedure WriteTextureMapChunk(MaterialIndex: Integer; Stream: TStream);
    Procedure WriteOpacityMapChunk(MaterialIndex: Integer; Stream: TStream);
    Procedure WriteMapNameChunk(MaterialIndex: Integer; Stream: TStream; MapName: String);
    Procedure WriteShininessChunk(MaterialIndex: Integer; Stream: TStream);
    Procedure WriteShininessStrengthChunk(MaterialIndex: Integer; Stream: TStream);
    Procedure WriteTransparencyChunk(MaterialIndex: Integer; Stream: TStream);
    Procedure WritePercentIntChunk(W: Word; Stream: TStream);
    Procedure WriteMasterScaleChunk(Scale: Single; Stream: TStream);
  End;

Procedure AddVector_3DS(Var V: Vector_3DS; V1: Vector_3DS);
Procedure SubtractVector_3DS(Var V: Vector_3DS; V1: Vector_3DS);
Procedure ClearVector_3DS(Var V: Vector_3DS);
Procedure RotateVector_3DS(Var V: Vector_3DS; Rot: Vector_3DS);

Implementation

Uses SysUtils,MyHashes;

{ 3DS Model file loader modified by Steve Streeting 2001 for OGRE
   Original credits are shown below.
   My changes include:
        - removed the display functions, generalised to be a loader only
        - removed OpenGL dependencies
        - added proper vertex normal support using smooth groups
        - added full vertex colour support (diffuse, ambient, specular & shininess)
        - a few bugfixes
}

// The chunk's id numbers

Const
  MAIN3DS          = $4D4D;  // 19789
  MAIN_VERS        = $0002;  // 2
  EDIT3DS          = $3D3D;  // 15677
  MESH_VERS        = $3D3E;  // 15678
  _OBJECT          = $4000;  // 16384
  TRIG_MESH        = $4100;  // 16640
  VERT_LIST        = $4110;  // 16656
  FACE_DESC        = $4120;  // 16672
  FACE_MAT         = $4130;  // 16688
  TEX_VERTS        = $4140;  // 16704
  SMOOTH_GROUP     = $4150;  // 16720
  LOCAL_COORDS     = $4160;  // 16736
  N_DIRECT_LIGHT   = $4600;  // 17920
  DL_SPLOTLIGHT    = $4610;  // 17936
  DL_OFF           = $4620;  // 17952
  DL_ATTENUATE     = $4625;  // 17957
  DL_LOCAL_SHADOW2 = $4641;  // 17985
  DL_EXCLUDE       = $4654;  // 18004
  DL_SPOT_ROLL     = $4656;  // 18006
  DL_RAY_BIAS      = $4658;  // 18008
  DL_INNER_RANGE   = $4659;  // 18009
  DL_OUTER_RANGE   = $465A;  // 18010
  DL_MULTIPLIER    = $465B;  // 18011
  MATERIAL         = $AFFF;  // 45055
  MAT_NAME         = $A000;  // 40960
  MAT_AMBIENT      = $A010;  // 40976
  MAT_DIFFUSE      = $A020;  // 40992
  MAT_SPECULAR     = $A030;  // 41008
  SHINY_PERC       = $A040;  // 41024
  SHINY_STR_PERC   = $A041;  // 41025
  TRANS_PERC       = $A050;  // 41040
  TRANS_FOFF_PERC  = $A052;  // 41042
  REF_BLUR_PERC    = $A053;  // 41043
  RENDER_TYPE      = $A100;  // 41216
  SELF_ILLUM       = $A084;  // 41092
  MAT_SELF_ILPCT   = $A08A;  // 41098
  WIRE_THICKNESS   = $A087;  // 41095
  MAT_TEXMAP       = $A200;  // 41472
  MAT_OPACMAP      = $A210;  // 41488
  MAT_MAPNAME      = $A300;  // 41728
  ONE_UNIT         = $0100;  // 256
  KEYF3DS          = $B000;  // 45056
  FRAMES           = $B008;  // 45064
  MESH_INFO        = $B002;  // 45058
  HIER_POS         = $B030;  // 45104
  HIER_FATHER      = $B010;  // 45072
  PIVOT_PT         = $B013;  // 45075
  TRACK00          = $B020;  // 45088
  TRACK01          = $B021;  // 45089
  TRACK02          = $B022;  // 45090
  COLOR_RGB        = $0010;  // 16
  COLOR_TRU        = $0011;  // 17
  COLOR_TRUG       = $0012;  // 18
  COLOR_RGBG       = $0013;  // 19
  PERC_INT         = $0030;  // 48
  PERC_FLOAT       = $0031;  // 49
  MASTER_SCALE     = $0100;  // 256

Procedure AddVector_3DS(Var V: Vector_3DS; V1: Vector_3DS);
Begin
  V.X := V.X + V1.X;
  V.Y := V.Y + V1.Y;
  V.Z := V.Z + V1.Z;
End; // AddVector_3DS

Procedure SubtractVector_3DS(Var V: Vector_3DS; V1: Vector_3DS);
Begin
  V.X := V.X - V1.X;
  V.Y := V.Y - V1.Y;
  V.Z := V.Z - V1.Z;
End; // SubtractVector_3DS

Procedure ClearVector_3DS(Var V: Vector_3DS);
Begin
  V.X := 0;
  V.Y := 0;
  V.Z := 0;
End; // ClearVector_3DS

Procedure RotateVector_3DS(Var V: Vector_3DS; Rot: Vector_3DS);
Var V1: Vector_3DS;
Begin
  // Rotate the model around the X axis

  V1.X := V.X;
  V1.Y := V.Y * Cos(Rot.X) - V.Z * Sin(Rot.X);
  V1.Z := V.Z * Cos(Rot.X) + V.Y * Sin(Rot.X);
  V    := V1;

  // Rotate the model around the Y axis

  V1.X := V.X * Cos(Rot.Y) - V.Z * Sin(Rot.Y);
  V1.Y := V.Y;
  V1.Z := V.Z * Cos(Rot.Y) + V.X * Sin(Rot.Y);
  V    := V1;

  // Rotate the model around the Z axis

  V1.X := V.X * Cos(Rot.Z) - V.Y * Sin(Rot.Z);
  V1.Y := V.Y * Cos(Rot.Z) + V.X * Sin(Rot.Z);
  V1.Z := V.Z;
  V    := V1;
End; // RotateVector_3DS

Function ReadNameFromFile(Var F: File): String;
Var
  St   : Packed Array[0..79] Of Char;
  I    : Integer;
  Done : Boolean;

Begin
  I    := 0;
  Done := False;
  While (I < 80) And Not Done Do
  Begin
    BlockRead(F,St[I],1);
    If St[I] = #0 Then Done := True Else Inc(I);
  End; // While
  Result := StrPas(St);
End; // ReadNameFromFile

Procedure WriteNameToStream(St: String; Stream: TStream);
Var
  S : PChar;
  I : Integer;

Begin
  I := Length(St);
  If I > 79 Then I := 79;
  St := Copy(St,1,I);
  S  := StrAlloc(I + 1);
  StrPCopy(S,St);
  Stream.Write(S^,I + 1);
  StrDispose(S);
End; // WriteNameToStream

//////////////////////////////////////////////////////////////////////
// Construction/Destruction
//////////////////////////////////////////////////////////////////////

Constructor Model_3DS.Create;
Begin
  // Initialization

  // Don't show the normals by default

  ShowNormals := False;

  // The model is lit by default

  Lit := True;

  // The model is visible by default

  Visible := True;

  // Set up the default position

  ClearVector_3DS(Pos);

  // Set up the default rotation

  ClearVector_3DS(Rot);

  // Set up the path

  Path := '';

  // Zero out our counters for MFC

  NumObjects   := 0;
  NumMaterials := 0;

  // Set the scale to one

  Scale := 1;
End; // Model_3DS.Create

Destructor Model_3DS.Destroy;
Var
  I,J : Integer;
  Obj : Object_3DS;

Begin
  For I := 0 To NumObjects - 1 Do
  Begin
    Obj := Objects[I];
    SetLength(Obj.Vertexes,0);
    SetLength(Obj.Normals,0);
    SetLength(Obj.TexCoords,0);
    SetLength(Obj.Faces,0);
    SetLength(Obj.FaceNormals,0);
    SetLength(Obj.FaceSmoothGroups,0);
    For J := 0 To Obj.NumMatFaces - 1 Do
    Begin
      SetLength(Obj.MatFaces[J].SubFaces,0);
      SetLength(Obj.MatFaces[J].OwnVertices,0);
      SetLength(Obj.MatFaces[J].OwnNormals,0);
      SetLength(Obj.MatFaces[J].OwnTexCoords,0);
    End; // For J
    SetLength(Obj.MatFaces,0);
  End; // For I
  SetLength(Objects,0);
  SetLength(Materials,0);
End; // Model_3DS.Destroy

Procedure Model_3DS.Load(Name: String);
Var
  Main : ChunkHeader; // holds the main chunk header
  I,J  : Integer;

Begin
  // Strip double quotes

  Name := Trim(Name);
  If Copy(Name,1,1) = '"' Then Name := Copy(Name,2,Length(Name) - 1);
  If Copy(Name,Length(Name),1) = '"' Then Name := Copy(Name,1,Length(Name) - 1);

  If FileExists(Name) Then
  Begin
    // Find the path

    Path := ExtractFilePath(Name);

    // Load the file

    AssignFile(Bin3DS,Name);
    Reset(Bin3DS,1);

    // Load the Main Chunk's header

    BlockRead(Bin3DS,Main.ID,SizeOf(Main.ID));
    BlockRead(Bin3DS,Main.Len,SizeOf(Main.Len));

    // Start Processing

    MainChunkProcessor(Main.Len, FilePos(Bin3DS));

    // Don't need the file anymore so close it

    CloseFile(Bin3DS);

    // Transform by local coords

    TransformLocalCoords;

    // Calculate the vertex normals

    CalculateNormals;

    // For future reference

    Modelname := Name;

    // Find the total number of faces and vertices

    TotalFaces := 0;
    TotalVerts := 0;

    For I := 0 To NumObjects - 1 Do
    Begin
      Inc(TotalFaces,Objects[I].NumFaces);
      Inc(TotalVerts,Objects[I].NumVerts);
    End; // For I

    // If the object doesn't have any texcoords generate some

    For I := 0 To NumObjects - 1 Do
    Begin
      If Objects[I].NumTexCoords = 0 Then
      Begin
        // Set the number of texture coords

        Objects[I].NumTexCoords := Objects[I].NumVerts;

        // Allocate an array to hold the texture coordinates

        SetLength(Objects[I].TexCoords,Objects[I].NumTexCoords);

        // Make some texture coords

        For J := 0 To Objects[I].NumTexCoords - 1 Do
        Begin
          Objects[I].TexCoords[J].TX := Objects[I].Vertexes[J].X;
          Objects[I].TexCoords[J].TZ := Objects[I].Vertexes[J].Y;
        End; // For J
      End;
    End; // For K
  End;
End; // Model_3DS.Load

Procedure Model_3DS.CalculateNormals;
Type LPtr = ^LongWord;
Var
  ObjIdx     : Integer;
  Face1      : Integer;
  Face2      : Integer;
  Vert1      : Integer;
  Vert2      : Integer;
  Obj        : Object_3DS;
  VertIndex1 : Integer;
  VertIndex2 : Integer;
  Vertex1    : Vector_3DS;
  Vertex2    : Vector_3DS;
  I,J        : Integer;
  Len        : Single;
  VUnit      : Vector_3DS;
  FSG        : TIntegerObjectHash; // Contains TLists
  List       : TList;
  FSGs       : TEnumeration;
  VertList   : TStringList;         // Contains TIntegerIntegerHashes
  VHash      : TIntegerIntegerHash;
  Verts      : TEnumeration;
  St         : String;

Begin
    // Go through each face, and look for faces in the same smooth
    // group. For these, look for vertices with the same position
    // on each face (but not the same vertex index) - these are vertices
    // whose normals won't have already been averaged by the addition
    // of face normals to their vertices. This happens along a 'seam'
    // where different vertices in the same position are required (e.g.
    // for different texture coords) but where averaged normals are still
    // If we don't do this we get a hard edge where we don't want one.

    For ObjIdx := 0 To NumObjects - 1 Do
    Begin
      Obj := Objects[ObjIdx];

      // Skip this object if there are no smooth groups

      If High(Obj.FaceSmoothGroups) >= 0 Then
      Begin
        // Form a hash of TLists where the hash is indexed by face smooth group.  We are
        // going to form lists of face indices for each unique smooth group.  This will
        // allow us to know exactly how many smooth groups there are and exactly those
        // faces in them.

        FSG := TIntegerObjectHash.Create(False,True);
        For I := 0 To High(Obj.FaceSmoothGroups) Do
        Begin
          List := TList(FSG.Get(Obj.FaceSmoothGroups[I]));
          If List = Nil Then
          Begin
            List := TList.Create;
            FSG.Put(Obj.FaceSmoothGroups[I],List);
          End;
          List.Add(Pointer(I));
        End; // For I

        // Iterate through each smooth group

        FSG.GetKeysAndValues(FSGs);
        For I := 0 To High(FSGs) Do
        Begin
          // Iterate through all of the faces in the smooth group (and all of their vertices),
          // building a master list that is indexed by unique vertex value.  Each element of
          // the list will contain an integer-integer hash of vertex indices as keys.  The
          // values will contain the face index + 1, since writing 0 won't save the value to
          // the hash.

          VertList := TStringList.Create;
          VertList.Sorted := True;
          List     := TList(FSGs[I].Value.AsObject);
          For Face1 := 0 To List.Count - 1 Do
          Begin
            For Vert1 := 0 To 2 Do
            Begin
              VertIndex1 := Obj.Faces[Integer(List.Items[Face1])].Index[Vert1];
              Vertex1    := Obj.Vertexes[VertIndex1];
              St         := IntToHex(LPtr(@(Vertex1.X))^,8) + IntToHex(LPtr(@(Vertex1.Y))^,8) + IntToHex(LPtr(@(Vertex1.Z))^,8);
              J := VertList.IndexOf(St);
              If J < 0 Then
              Begin
                VHash := TIntegerIntegerHash.Create(False);
                VertList.AddObject(St,VHash);
              End
              Else VHash := TIntegerIntegerHash(VertList.Objects[J]);
              VHash.Put(VertIndex1,Face1 + 1);
            End; // For Vert1
          End; // For Face1

          // For each hash in VertList, we now have a list of vertices that need to have their normals averaged together

          For J := 0 To VertList.Count - 1 Do
          Begin
            VHash := TIntegerIntegerHash(VertList.Objects[J]);
            VHash.GetKeysAndValues(Verts);
            For VertIndex1 := 0 To High(Verts) - 1 Do
            Begin
              For VertIndex2 := VertIndex1 + 1 To High(Verts) Do
              Begin

                // Both vertex normals must be averaged
                // with the other face normal to apply smooth group

                AddVector_3DS(Obj.Normals[Verts[VertIndex1].Key.Int],Obj.FaceNormals[Verts[VertIndex2].Value.AsInteger - 1]);
                AddVector_3DS(Obj.Normals[Verts[VertIndex2].Key.Int],Obj.FaceNormals[Verts[VertIndex1].Value.AsInteger - 1]);
              End; // For Vert2
            End; // For Vert1

            // Cleanup

            SetLength(Verts,0);
          End; // For J

          // Cleanup

          For J := 0 To VertList.Count - 1 Do VertList.Objects[J].Free;
          VertList.Clear;
        End; // For I
        VertList.Free;

        // Cleanup

//        FSG.GetKeysAndValues(FSGs);
//        For I := 0 To High(FSGs) Do TList(FSGs[I].Value.Ptr).Free;
//        SetLength(FSGs,0);
        FSG.Free;



{
        For Face1 := 0 To Obj.NumFaces - 1 Do
        Begin
          For Face2 := Face1 + 1 To Obj.NumFaces - 1 Do
          Begin
            If Obj.FaceSmoothGroups[Face1] = Obj.FaceSmoothGroups[Face2] Then
            Begin
              // Same smooth group - check matching vertices

              For Vert1 := 0 To 2 Do
              Begin
                For Vert2 := 0 To 2 Do
                Begin
                  // Check if indexes are not the same
                  //   but position is

                  VertIndex1 := Obj.Faces[Face1].Index[Vert1];
                  VertIndex2 := Obj.Faces[Face2].Index[Vert2];

                  Vertex1    := Obj.Vertexes[VertIndex1];
                  Vertex2    := Obj.Vertexes[VertIndex2];

                  If (VertIndex1 <> VertIndex2) And
                     (Vertex1.X = Vertex2.X)    And
                     (Vertex1.Y = Vertex2.Y)    And
                     (Vertex1.Z = Vertex2.Z)    Then
                  Begin
                    // Both vertex normals must be averaged
                    // with the other face normal to apply smooth group

                    AddVector_3DS(Obj.Normals[VertIndex1],Obj.FaceNormals[Face2]);
                    AddVector_3DS(Obj.Normals[VertIndex2],Obj.FaceNormals[Face1]);
                  End; // matched position but not index, same smooth group
                End; // For Vert2
              End; // For Vert1
            End; // smoothgroup match for faces
          End; // For Face2
        End; // For Face1
}
      End;
    End; // For ObjIdx

    // Normalise all vertex normals

    For I := 0 To NumObjects - 1 Do
    Begin
      For J := 0 To Objects[I].NumVerts - 1 Do
      Begin
        // Reduce each vert's normal to unit

        VUnit := Objects[I].Normals[J];
        Len   := Sqrt(Sqr(VUnit.X)  + Sqr(VUnit.Y) + Sqr(VUnit.Z));

        If Len = 0 Then Len := 1;

        VUnit.X := VUnit.X / Len;
        VUnit.Y := VUnit.Y / Len;
        VUnit.Z := VUnit.Z / Len;

        Objects[I].Normals[J] := VUnit;
      End; // For J
    End; // For I
End; // Model_3DS.CalculateNormals

Procedure Model_3DS.MainChunkProcessor(Len,FIndex: LongInt);
Var H: ChunkHeader;
Begin
  // move the file pointer to the beginning of the main
  // chunk's data FIndex + the size of the header

  Seek(Bin3DS, FIndex);

  While FilePos(Bin3DS) < FIndex + Len - 6 Do
  Begin
    BlockRead(Bin3DS,H,SizeOf(H));
    Case H.ID Of
      EDIT3DS: EditChunkProcessor(H.Len, FilePos(Bin3DS)); // This is the mesh information like vertices, faces, and materials
//      case KEYF3DS: KeyFrameChunkProcessor(H.Len, ftell(Bin3DS));  // I left this in case anyone gets very ambitious
    End; // Case

    Seek(Bin3DS,FilePos(Bin3DS) + (H.Len - 6));
  End; // While

  // move the file pointer back to where we got it so
  // that the ProcessChunk() which we interrupted will read
  // from the right place

  Seek(Bin3DS, FIndex);
End; // Model_3DS.MainChunkProcessor

Procedure Model_3DS.EditChunkProcessor(Len,FIndex: LongInt);
Var
  H : ChunkHeader;
  I : Integer;

Begin
  // move the file pointer to the beginning of the main
  // chunk's data FIndex + the size of the header

  Seek(Bin3DS, FIndex);

  // First count the number of Objects and Materials
  
  While FilePos(Bin3DS) < FIndex + Len - 6 Do
  Begin
    BlockRead(Bin3DS,H,SizeOf(H));
    Case H.ID Of
      _OBJECT: Inc(numObjects);
      MATERIAL: Inc(numMaterials);
    End; // Case
    Seek(Bin3DS, FilePos(Bin3DS) + (H.Len - 6));
  End; // While

  // Now load the materials

  If NumMaterials > 0 Then
  Begin
    SetLength(Materials,NumMaterials);

    // Material is set to untextured until we find otherwise

    For I := 0 To NumMaterials - 1 Do
    Begin
      Materials[I].Textured    := False;
      Materials[I].OpacityMap  := False;
      Materials[I].DiffuseSet  := False;
      Materials[I].AmbientSet  := False;
      Materials[I].SpecularSet := False;
    End; // For I
    Seek(Bin3DS, FIndex);
    
    I := 0;
    While FilePos(Bin3DS) < FIndex + Len - 6 Do
    Begin
      BlockRead(Bin3DS,H,SizeOf(H));
      Case H.ID Of
        MATERIAL:
        Begin
          MaterialChunkProcessor(H.Len, FilePos(Bin3DS), I);
          Inc(I);
        End;
      End; // Case
      Seek(Bin3DS,FilePos(Bin3DS) +  (H.Len - 6));
    End; // While
  End;

  // Load the Objects (individual meshes in the whole model)
  
  If NumObjects > 0 Then
  Begin
    SetLength(Objects,NumObjects);
    
    For I := 0 To NumObjects - 1 Do
    Begin
      // Set the textured variable to false until we find a texture

      Objects[I].Textured := False;

      // Zero the objects position and rotation

      ClearVector_3DS(Objects[I].Pos);
      ClearVector_3DS(Objects[I].Rot);

      // Zero out the number of texture coords

      Objects[I].NumTexCoords := 0;

      // Start out without a light source

      Objects[I].HasLight := False;
    End; // For I

    Seek(Bin3DS, FIndex);

    I := 0;
    While FilePos(Bin3DS) < FIndex + Len - 6 Do
    Begin
      BlockRead(Bin3DS,H,SizeOf(H));
      Case H.ID Of
        _OBJECT:
        Begin
          ObjectChunkProcessor(H.Len, FilePos(Bin3DS), I);
          Inc(I);
        End;
      End; // Case
      Seek(Bin3DS,FilePos(Bin3DS) + (H.Len - 6));
    End; // While
  End;

  // move the file pointer back to where we got it so
  // that the ProcessChunk() which we interrupted will read
  // from the right place
  
  Seek(Bin3DS, FIndex);
End; // Model_3DS.EditChunkProcessor

Procedure Model_3DS.MaterialChunkProcessor(Len,FIndex: LongInt; MatIndex: Integer);
Var H: ChunkHeader;
Begin
  // move the file pointer to the beginning of the main
  // chunk's data FIndex + the size of the header

  Seek(Bin3DS, FIndex);

  While FilePos(Bin3DS) < FIndex + Len - 6 Do
  Begin
    BlockRead(Bin3DS,H,SizeOf(H));
    Case H.ID Of
      MAT_NAME:       MaterialNameChunkProcessor(H.Len, FilePos(Bin3DS), MatIndex);   // Loads the material's names
      MAT_AMBIENT,
      MAT_DIFFUSE,
      MAT_SPECULAR:   ColorChunkProcessor(H.Len, FilePos(Bin3DS), MatIndex, H.ID);
      MAT_TEXMAP:     TextureMapChunkProcessor(H.Len, FilePos(Bin3DS), MatIndex);     // Finds the names of the textures of the material and loads them
      MAT_OPACMAP:    OpacityMapChunkProcessor(H.Len, FilePos(Bin3DS), MatIndex);     // Finds the names of the opacity maps of the material and loads them
      SHINY_PERC:     ShininessChunkProcessor(H.Len, FilePos(Bin3DS), MatIndex);
      SHINY_STR_PERC: ShininessStrengthChunkProcessor(H.Len, FilePos(Bin3DS), MatIndex);
      TRANS_PERC:     TransparencyChunkProcessor(H.Len, FilePos(Bin3DS), MatIndex);
    End; // Case
    Seek(Bin3DS,FilePos(Bin3DS) + (H.Len - 6));
  End; // While

  // move the file pointer back to where we got it so
  // that the ProcessChunk() which we interrupted will read
  // from the right place

  Seek(Bin3DS, FIndex);
End; // Model_3DS.MaterialChunkProcessor

Procedure Model_3DS.MaterialNameChunkProcessor(Len,FIndex: LongInt; MatIndex: Integer);
Begin
  // move the file pointer to the beginning of the main
  // chunk's data FIndex + the size of the header

  Seek(Bin3DS, FIndex);

  // Read the material's name

  Materials[MatIndex].Name := ReadNameFromFile(Bin3DS);

  // move the file pointer back to where we got it so
  // that the ProcessChunk() which we interrupted will read
  // from the right place

  Seek(Bin3DS, FIndex);
End; // Model_3DS.MaterialNameChunkProcessor

Procedure Model_3DS.ColorChunkProcessor(Len,FIndex: LongInt; MatIndex: Integer; ColType: Word);
Var H: ChunkHeader;
Begin
  // move the file pointer to the beginning of the main
  // chunk's data FIndex + the size of the header

  Seek(Bin3DS, FIndex);

  While FilePos(Bin3DS) < FIndex + Len - 6 Do
  Begin
    BlockRead(Bin3DS,H,SizeOf(H));

    // Determine the format of the color and load it

    Case H.ID Of
      COLOR_RGB:  FloatColorChunkProcessor(H.Len, FilePos(Bin3DS), MatIndex, ColType);   // A rgb float color chunk
      COLOR_TRU:  IntColorChunkProcessor(H.Len, FilePos(Bin3DS), MatIndex, ColType);     // A rgb int color chunk
      COLOR_RGBG: FloatColorChunkProcessor(H.Len, FilePos(Bin3DS), MatIndex, ColType);   // A rgb gamma corrected float color chunk
      COLOR_TRUG: IntColorChunkProcessor(H.Len, FilePos(Bin3DS), MatIndex, ColType);     // A rgb gamma corrected int color chunk
    End; // Case

    Seek(Bin3DS,FilePos(Bin3DS) + (H.Len - 6));
  End; // While

  // move the file pointer back to where we got it so
  // that the ProcessChunk() which we interrupted will read
  // from the right place

  Seek(Bin3DS, FIndex);
End; // Model_3DS.ColorChunkProcessor

Procedure Model_3DS.ShininessChunkProcessor(Len,FIndex: LongInt; MatIndex: Integer);
Var
  H         : ChunkHeader;
  FloatPerc : Single;

Begin
  // move the file pointer to the beginning of the main
  // chunk's data FIndex + the size of the header

  Seek(Bin3DS, FIndex);

  BlockRead(Bin3DS,H,SizeOf(H));

  // Determine type of percentage

  If H.ID = PERC_INT Then BlockRead(Bin3DS,Materials[MatIndex].Shininess,SizeOf(Materials[MatIndex].Shininess))
  Else If H.ID = PERC_FLOAT Then
  Begin
    BlockRead(Bin3DS,FloatPerc,SizeOf(FloatPerc));
    Materials[MatIndex].shininess := Round(FloatPerc);
  End;

  // move the file pointer back to where we got it so
  // that the ProcessChunk() which we interrupted will read
  // from the right place

  Seek(Bin3DS, FIndex);
End; // Model_3DS.ShininessChunkProcessor

Procedure Model_3DS.ShininessStrengthChunkProcessor(Len,FIndex: LongInt; MatIndex: Integer);
Var
  H         : ChunkHeader;
  FloatPerc : Single;

Begin
  // move the file pointer to the beginning of the main
  // chunk's data FIndex + the size of the header

  Seek(Bin3DS, FIndex);

  BlockRead(Bin3DS,H,SizeOf(H));

  // Determine type of percentage

  If H.ID = PERC_INT Then BlockRead(Bin3DS,Materials[MatIndex].ShininessStrength,SizeOf(Materials[MatIndex].ShininessStrength))
  Else If H.ID = PERC_FLOAT Then
  Begin
    BlockRead(Bin3DS,FloatPerc,SizeOf(FloatPerc));
    Materials[MatIndex].ShininessStrength := Round(FloatPerc);
  End;

  // move the file pointer back to where we got it so
  // that the ProcessChunk() which we interrupted will read
  // from the right place

  Seek(Bin3DS, FIndex);
End; // Model_3DS.ShininessStrengthChunkProcessor

Procedure Model_3DS.TransparencyChunkProcessor(Len,FIndex: LongInt; MatIndex: Integer);
Var
  H         : ChunkHeader;
  FloatPerc : Single;

Begin
  // move the file pointer to the beginning of the main
  // chunk's data FIndex + the size of the header

  Seek(Bin3DS, FIndex);

  BlockRead(Bin3DS,H,SizeOf(H));

  // Determine type of percentage

  If H.ID = PERC_INT Then BlockRead(Bin3DS,Materials[MatIndex].Transparency,SizeOf(Materials[MatIndex].Transparency))
  Else If H.ID = PERC_FLOAT Then
  Begin
    BlockRead(Bin3DS,FloatPerc,SizeOf(FloatPerc));
    Materials[MatIndex].Transparency := Round(FloatPerc);
  End;

  // move the file pointer back to where we got it so
  // that the ProcessChunk() which we interrupted will read
  // from the right place

  Seek(Bin3DS, FIndex);
End; // Model_3DS.TransparencyChunkProcessor

// Generalised to handle diffuse, abient and specular
// Also bugfixed - original was using red for all components!
Procedure Model_3DS.FloatColorChunkProcessor(Len,FIndex: LongInt; MatIndex: Integer; ColType: Word);
Var R,G,B: Single;
Begin
  // move the file pointer to the beginning of the main
  // chunk's data FIndex + the size of the header
  
  Seek(Bin3DS, FIndex);

  BlockRead(Bin3DS, R,SizeOf(R));
  BlockRead(Bin3DS, G,SizeOf(G));
  BlockRead(Bin3DS, B,SizeOf(B));

  If ColType = MAT_AMBIENT Then
  Begin
    Materials[MatIndex].Ambient.R  := Round(R * 255);
    Materials[MatIndex].Ambient.G  := Round(G * 255);
    Materials[MatIndex].Ambient.B  := Round(B * 255);
    Materials[MatIndex].Ambient.A  := 255;
    Materials[MatIndex].AmbientSet := True;
  End
  Else If ColType = MAT_DIFFUSE Then
  Begin
    Materials[MatIndex].Diffuse.R  := Round(R * 255);
    Materials[MatIndex].Diffuse.G  := Round(G * 255);
    Materials[MatIndex].Diffuse.B  := Round(B * 255);
    Materials[MatIndex].Diffuse.A  := 255;
    Materials[MatIndex].DiffuseSet := True;
  End
  Else If ColType = MAT_SPECULAR Then
  Begin
    Materials[MatIndex].Specular.R  := Round(R * 255);
    Materials[MatIndex].Specular.G  := Round(G * 255);
    Materials[MatIndex].Specular.B  := Round(B * 255);
    Materials[MatIndex].Specular.A  := 255;
    Materials[MatIndex].SpecularSet := True;
  End
  Else If ColType = N_DIRECT_LIGHT Then
  Begin
    Objects[MatIndex].Light.Color.R := Round(R * 255);
    Objects[MatIndex].Light.Color.G := Round(G * 255);
    Objects[MatIndex].Light.Color.B := Round(B * 255);
    Objects[MatIndex].Light.Color.A := 255;
  End;

  // move the file pointer back to where we got it so
  // that the ProcessChunk() which we interrupted will read
  // from the right place

  Seek(Bin3DS, FIndex);
End; // Model_3DS.FloatColorChunkProcessor

// Generalised to handle diffuse, abient and specular
Procedure Model_3DS.IntColorChunkProcessor(Len,FIndex: LongInt; MatIndex: Integer; ColType: Word);
Var R,G,B: Byte;
Begin
  // move the file pointer to the beginning of the main
  // chunk's data FIndex + the size of the header
  
  Seek(Bin3DS, FIndex);

  BlockRead(Bin3DS, R,SizeOf(R));
  BlockRead(Bin3DS, G,SizeOf(G));
  BlockRead(Bin3DS, B,SizeOf(B));

  If ColType = MAT_AMBIENT Then
  Begin
    Materials[MatIndex].Ambient.R  := R;
    Materials[MatIndex].Ambient.G  := G;
    Materials[MatIndex].Ambient.B  := B;
    Materials[MatIndex].Ambient.A  := 255;
    Materials[MatIndex].AmbientSet := True;
  End
  Else If ColType = MAT_DIFFUSE Then
  Begin
    Materials[MatIndex].Diffuse.R  := R;
    Materials[MatIndex].Diffuse.G  := G;
    Materials[MatIndex].Diffuse.B  := B;
    Materials[MatIndex].Diffuse.A  := 255;
    Materials[MatIndex].DiffuseSet := True;
  End
  Else If ColType = MAT_SPECULAR Then
  Begin
    Materials[MatIndex].Specular.R  := R;
    Materials[MatIndex].Specular.G  := G;
    Materials[MatIndex].Specular.B  := B;
    Materials[MatIndex].Specular.A  := 255;
    Materials[MatIndex].SpecularSet := True;
  End;

  // move the file pointer back to where we got it so
  // that the ProcessChunk() which we interrupted will read
  // from the right place

  Seek(Bin3DS, FIndex);
End; // Model_3DS.IntColorChunkProcessor

Procedure Model_3DS.TextureMapChunkProcessor(Len,FIndex: LongInt; MatIndex: Integer);
Var H: ChunkHeader;
Begin
  // move the file pointer to the beginning of the main
  // chunk's data FIndex + the size of the header

  Seek(Bin3DS, FIndex);

  While FilePos(Bin3DS) < FIndex + Len - 6 Do
  Begin
    BlockRead(Bin3DS,H,SizeOf(H));
    Case H.ID Of
      MAT_MAPNAME: TextureMapNameChunkProcessor(H.Len, FilePos(Bin3DS), MatIndex); // Read the name of texture in the Diffuse Color map
    End; // Case
    Seek(Bin3DS,FilePos(Bin3DS) + (H.Len - 6));
  End; // While

  // move the file pointer back to where we got it so
  // that the ProcessChunk() which we interrupted will read
  // from the right place

  Seek(Bin3DS, FIndex);
End; // Model_3DS.TextureMapChunkProcessor

Procedure Model_3DS.OpacityMapChunkProcessor(Len,FIndex: LongInt; MatIndex: Integer);
Var H: ChunkHeader;
Begin
  // move the file pointer to the beginning of the main
  // chunk's data FIndex + the size of the header

  Seek(Bin3DS, FIndex);

  While FilePos(Bin3DS) < FIndex + Len - 6 Do
  Begin
    BlockRead(Bin3DS,H,SizeOf(H));
    Case H.ID Of
      MAT_MAPNAME: OpacityMapNameChunkProcessor(H.Len, FilePos(Bin3DS), MatIndex); // Read the name of texture in the Diffuse Color map
    End; // Case
    Seek(Bin3DS,FilePos(Bin3DS) + (H.Len - 6));
  End; // While

  // move the file pointer back to where we got it so
  // that the ProcessChunk() which we interrupted will read
  // from the right place

  Seek(Bin3DS, FIndex);
End; // Model_3DS.OpacityMapChunkProcessor

Procedure Model_3DS.TextureMapNameChunkProcessor(Len,FIndex: LongInt; MatIndex: Integer);
Begin
  // move the file pointer to the beginning of the main
  // chunk's data FIndex + the size of the header

  Seek(Bin3DS, FIndex);

  // Read the name of the texture

  Materials[MatIndex].TexName  := ReadNameFromFile(Bin3DS);
  Materials[MatIndex].Textured := True;

  // move the file pointer back to where we got it so
  // that the ProcessChunk() which we interrupted will read
  // from the right place

  Seek(Bin3DS, FIndex);
End; // Model_3DS.TextureMapNameChunkProcessor

Procedure Model_3DS.OpacityMapNameChunkProcessor(Len,FIndex: LongInt; MatIndex: Integer);
Begin
  // move the file pointer to the beginning of the main
  // chunk's data FIndex + the size of the header

  Seek(Bin3DS, FIndex);

  // Read the name of the opacity map

  Materials[MatIndex].OpacityMapName := ReadNameFromFile(Bin3DS);
  Materials[MatIndex].OpacityMap     := True;

  // move the file pointer back to where we got it so
  // that the ProcessChunk() which we interrupted will read
  // from the right place

  Seek(Bin3DS, FIndex);
End; // Model_3DS.OpacityMapNameChunkProcessor

Procedure Model_3DS.ObjectChunkProcessor(Len,FIndex: LongInt; ObjIndex: Integer);
Var
  H    : ChunkHeader;
  Row  : Integer;
  Col  : Integer;

Begin
  // move the file pointer to the beginning of the main
  // chunk's data FIndex + the size of the header

  Seek(Bin3DS, FIndex);

  // Load the object's name

  Objects[ObjIndex].Name := ReadNameFromFile(Bin3DS);

  // Init
  
  Objects[ObjIndex].NumVerts     := 0;
  Objects[ObjIndex].NumFaces     := 0;
  Objects[ObjIndex].NumMatFaces  := 0;
  Objects[ObjIndex].NumTexCoords := 0;
  SetLength(Objects[ObjIndex].FaceSmoothGroups,0);

  // Init local matrix

  For Row := 0 To 3 Do
   For Col := 0 To 2 Do Objects[ObjIndex].LocalMatrix[Row,Col] := 0;
  Objects[ObjIndex].LocalMatrix[0,0] := 1;
  Objects[ObjIndex].LocalMatrix[1,1] := 1;
  Objects[ObjIndex].LocalMatrix[2,2] := 1;

  While FilePos(Bin3DS) < FIndex + Len - 6 Do
  Begin
    BlockRead(Bin3DS,H,SizeOf(H));
    Case H.ID Of
      TRIG_MESH:      TriangularMeshChunkProcessor(H.Len, FilePos(Bin3DS), ObjIndex); // Process the triangles of the object
      N_DIRECT_LIGHT: LightChunkProcessor(H.Len, FilePos(Bin3DS), ObjIndex);          // Process the light source
    End; // Case
    Seek(Bin3DS,FilePos(Bin3DS) + (H.Len - 6));
  End; // While

  // move the file pointer back to where we got it so
  // that the ProcessChunk() which we interrupted will read
  // from the right place

  Seek(Bin3DS, FIndex);
End; // Model_3DS.ObjectChunkProcessor

Procedure Model_3DS.LightChunkProcessor(Len,FIndex: LongInt; ObjIndex: Integer);
Var H: ChunkHeader;
Begin
  // move the file pointer to the beginning of the main
  // chunk's data FIndex + the size of the header


  Seek(Bin3DS, FIndex);
  Objects[ObjIndex].HasLight         := True;
  Objects[ObjIndex].Light.Off        := False;
  Objects[ObjIndex].Light.InnerRange := 0;
  Objects[ObjIndex].Light.OuterRange := 1E10;
  BlockRead(Bin3DS,Objects[ObjIndex].Light.Location.X,SizeOf(Single));
  BlockRead(Bin3DS,Objects[ObjIndex].Light.Location.Y,SizeOf(Single));
  BlockRead(Bin3DS,Objects[ObjIndex].Light.Location.Z,SizeOf(Single));
  While FilePos(Bin3DS) < FIndex + Len - 6 Do
  Begin
    BlockRead(Bin3DS,H,SizeOf(H));
    Case H.ID Of
      COLOR_RGB:      FloatColorChunkProcessor(H.Len, FilePos(Bin3DS), ObjIndex, N_DIRECT_LIGHT);   // A rgb float color chunk
      DL_OFF:         Objects[ObjIndex].Light.Off := True;
      DL_INNER_RANGE: LightInnerRangeChunkProcessor(H.Len, FilePos(Bin3DS), ObjIndex);
      DL_OUTER_RANGE: LightOuterRangeChunkProcessor(H.Len, FilePos(Bin3DS), ObjIndex);
      DL_MULTIPLIER:  LightMultiplierChunkProcessor(H.Len, FilePos(Bin3DS), ObjIndex);
    End; // Case
    Seek(Bin3DS,FilePos(Bin3DS) + (H.Len - 6));
  End; // While

  // move the file pointer back to where we got it so
  // that the ProcessChunk() which we interrupted will read
  // from the right place

  Seek(Bin3DS, FIndex);
End; // Model_3DS.LightChunkProcessor

Procedure Model_3DS.LightInnerRangeChunkProcessor(Len,FIndex: LongInt; ObjIndex: Integer);
Begin
  // move the file pointer to the beginning of the main
  // chunk's data FIndex + the size of the header

  Seek(Bin3DS, FIndex);
  BlockRead(Bin3DS,Objects[ObjIndex].Light.InnerRange,SizeOf(Single));

  // move the file pointer back to where we got it so
  // that the ProcessChunk() which we interrupted will read
  // from the right place

  Seek(Bin3DS, FIndex);
End; // Model_3DS.LightInnerRangeChunkProcessor

Procedure Model_3DS.LightOuterRangeChunkProcessor(Len,FIndex: LongInt; ObjIndex: Integer);
Begin
  // move the file pointer to the beginning of the main
  // chunk's data FIndex + the size of the header

  Seek(Bin3DS, FIndex);
  BlockRead(Bin3DS,Objects[ObjIndex].Light.OuterRange,SizeOf(Single));

  // move the file pointer back to where we got it so
  // that the ProcessChunk() which we interrupted will read
  // from the right place

  Seek(Bin3DS, FIndex);
End; // Model_3DS.LightOuterRangeChunkProcessor

Procedure Model_3DS.LightMultiplierChunkProcessor(Len,FIndex: LongInt; ObjIndex: Integer);
Begin
  // move the file pointer to the beginning of the main
  // chunk's data FIndex + the size of the header

  Seek(Bin3DS, FIndex);
  BlockRead(Bin3DS,Objects[ObjIndex].Light.Multiplier,SizeOf(Single));

  // move the file pointer back to where we got it so
  // that the ProcessChunk() which we interrupted will read
  // from the right place

  Seek(Bin3DS, FIndex);
End; // Model_3DS.LightMultiplierChunkProcessor

Procedure Model_3DS.TriangularMeshChunkProcessor(Len,FIndex: LongInt; ObjIndex: Integer);
Var H: ChunkHeader;
Begin
  // move the file pointer to the beginning of the main
  // chunk's data FIndex + the size of the header

  Seek(Bin3DS, FIndex);

  While FilePos(Bin3DS) < FIndex + Len - 6 Do
  Begin
    BlockRead(Bin3DS,H,SizeOf(H));
    Case H.ID Of
      VERT_LIST:    VertexListChunkProcessor(H.Len, FilePos(Bin3DS), ObjIndex); // Load the vertices of the onject
      LOCAL_COORDS: LocalCoordinatesChunkProcessor(H.Len, FilePos(Bin3DS), ObjIndex);
      TEX_VERTS:
      Begin
        // Load the texture coordinates for the vertices

        TexCoordsChunkProcessor(H.Len, FilePos(Bin3DS), ObjIndex);
        Objects[ObjIndex].Textured := True;
      End;
    End; // Case
    Seek(Bin3DS,FilePos(Bin3DS) + (H.Len - 6));
  End; // While

  // After we have loaded the vertices we can load the faces

  Seek(Bin3DS, FIndex);

  While FilePos(Bin3DS) < FIndex + Len - 6 Do
  Begin
    BlockRead(Bin3DS,H,SizeOf(H));
    Case H.ID Of
      FACE_DESC: FacesDescriptionChunkProcessor(H.Len, FilePos(Bin3DS), ObjIndex); // Load the faces of the object
    End; // Case
    Seek(Bin3DS,FilePos(Bin3DS) + (H.Len - 6));
  End; // While

  // move the file pointer back to where we got it so
  // that the ProcessChunk() which we interrupted will read
  // from the right place

  Seek(Bin3DS, FIndex);
End; // Model_3DS.TriangularMeshChunkProcessor

Procedure Model_3DS.VertexListChunkProcessor(Len,FIndex: LongInt; ObjIndex: Integer);
Var
  NumVerts : Word;
  I        : Integer;

Begin
  // move the file pointer to the beginning of the main
  // chunk's data FIndex + the size of the header

  Seek(Bin3DS, FIndex);

  // Read the number of vertices of the object

  BlockRead(Bin3DS,NumVerts,SizeOf(NumVerts));

  // Allocate arrays for the vertices and normals

  SetLength(Objects[ObjIndex].Vertexes,NumVerts);
  SetLength(Objects[ObjIndex].Normals,NumVerts);

  // Assign the number of vertices for future use

  Objects[ObjIndex].NumVerts := NumVerts;

  // Zero out the normals array

  For I := 0 To NumVerts - 1 Do ClearVector_3DS(Objects[ObjIndex].Normals[I]);
  For I := 0 To NumVerts - 1 Do
  Begin
    BlockRead(Bin3DS,Objects[ObjIndex].Vertexes[I].X,SizeOf(Single));
    BlockRead(Bin3DS,Objects[ObjIndex].Vertexes[I].Y,SizeOf(Single));
    BlockRead(Bin3DS,Objects[ObjIndex].Vertexes[I].Z,SizeOf(Single));
  End; // For I

  // move the file pointer back to where we got it so
  // that the ProcessChunk() which we interrupted will read
  // from the right place
  
  Seek(Bin3DS, FIndex);
End; // Model_3DS.VertexListChunkProcessor

Procedure Model_3DS.TexCoordsChunkProcessor(Len,FIndex: LongInt; ObjIndex: Integer);
Var
  NumCoords : Word;     // The number of texture coordinates
  I         : Integer;

Begin
  // move the file pointer to the beginning of the main
  // chunk's data FIndex + the size of the header

  Seek(Bin3DS, FIndex);

  // Read the number of coordinates

  BlockRead(Bin3DS,NumCoords,SizeOf(NumCoords));

  // Allocate an array to hold the texture coordinates

  SetLength(Objects[ObjIndex].TexCoords,NumCoords);

  // Set the number of texture coords

  Objects[ObjIndex].NumTexCoords := NumCoords;

  // Read the texture coordiantes into the array

  For I := 0 To NumCoords - 1 Do
  Begin
    BlockRead(Bin3DS,Objects[ObjIndex].TexCoords[I].TX,SizeOf(Single));
    BlockRead(Bin3DS,Objects[ObjIndex].TexCoords[I].TZ,SizeOf(Single));
  End; // For I

  // move the file pointer back to where we got it so
  // that the ProcessChunk() which we interrupted will read
  // from the right place

  Seek(Bin3DS, FIndex);
End; // Model_3DS.TexCoordsChunkProcessor

Procedure Model_3DS.FacesDescriptionChunkProcessor(Len,FIndex: LongInt; ObjIndex: Integer);
Var
  H           : ChunkHeader;
  NumFaces    : Word;            // The number of faces in the object
  VertA       : Word;            // The first vertex of the face
  VertB       : Word;            // The second vertex of the face
  VertC       : Word;            // The third vertex of the face
  Flags       : Word;            // The winding order flags
  Subs        : LongInt;         // Holds our place in the file
  NumMatFaces : Integer;         // The number of different materials
  I           : Integer;
  N           : Vector_3DS;
  V1          : Vector_3DS;
  V2          : Vector_3DS;
  V3          : Vector_3DS;
  U,V         : Packed Array[0..2] Of Single;

Begin
  NumMatFaces := 0;

  // move the file pointer to the beginning of the main
  // chunk's data FIndex + the size of the header

  Seek(Bin3DS, FIndex);

  // Read the number of faces

  BlockRead(Bin3DS,NumFaces,SizeOf(NumFaces));

  // Allocate an array to hold the faces

  SetLength(Objects[ObjIndex].Faces,NumFaces);

  // Allocate memory for face normals

  SetLength(Objects[ObjIndex].FaceNormals,NumFaces);

  // Store the number of faces

  Objects[ObjIndex].NumFaces := NumFaces;

  // Read the faces into the array

  For I := 0 To NumFaces - 1 Do
  Begin
    // Read the vertices of the face
    
    BlockRead(Bin3DS,VertA,SizeOf(VertA));
    BlockRead(Bin3DS,VertB,SizeOf(VertB));
    BlockRead(Bin3DS,VertC,SizeOf(VertC));
    BlockRead(Bin3DS,Flags,SizeOf(Flags));

    // Place them in the array

    Objects[ObjIndex].Faces[I].Index[0] := VertA;
    Objects[ObjIndex].Faces[I].Index[1] := VertB;
    Objects[ObjIndex].Faces[I].Index[2] := VertC;
    Objects[ObjIndex].Faces[I].MatIndex := -1;

    // Calculate the face's normal

    V1 := Objects[ObjIndex].Vertexes[VertA];
    V2 := Objects[ObjIndex].Vertexes[VertB];
    V3 := Objects[ObjIndex].Vertexes[VertC];

    // calculate the normal

    // V2 - V3;

    U[0] := V2.X - V3.X;
    U[1] := V2.Y - V3.Y;
    U[2] := V2.Z - V3.Z;

    // V2 - V1;

    V[0] := V2.X - V1.X;
    V[1] := V2.Y - V1.Y;
    V[2] := V2.Z - V1.Z;

    N.X := U[1] * V[2] - U[2] * V[1];
    N.Y := U[2] * V[0] - U[0] * V[2];
    N.Z := U[0] * V[1] - U[1] * V[0];

    //  Store the face normal for this purpose for later use in the
    //  CalclateNormals() routine
    //  Note this is not normalised yet - this is OK

    Objects[ObjIndex].FaceNormals[I] := N;

    // Add this normal to its verts' normals

    AddVector_3DS(Objects[ObjIndex].Normals[VertA],N);
    AddVector_3DS(Objects[ObjIndex].Normals[VertB],N);
    AddVector_3DS(Objects[ObjIndex].Normals[VertC],N);
  End; // While

  // Store our current file position

  Subs := FilePos(Bin3DS);

  // Check to see how many materials the faces are split into

  While FilePos(Bin3DS) < FIndex + Len - 6 Do
  Begin
    BlockRead(Bin3DS, H,SizeOf(H));
    Case H.ID Of
      FACE_MAT:
      Begin
        //FacesMaterialsListChunkProcessor(H.Len, FilePos(Bin3DS), ObjIndex);
        Inc(NumMatFaces);
      End;
      SMOOTH_GROUP: SmoothGroupChunkProcessor(H.Len, FilePos(Bin3DS),ObjIndex); // Deal with smoothing groups
    End; // Case
    Seek(Bin3DS,FilePos(Bin3DS) + (H.Len - 6));
  End; // While

  // Split the faces up according to their materials

  If NumMatFaces > 0 Then
  Begin
    // Allocate an array to hold the lists of faces divided by material

    SetLength(Objects[ObjIndex].MatFaces,NumMatFaces);

    // Store the number of material faces

    Objects[ObjIndex].NumMatFaces := NumMatFaces;

    Seek(Bin3DS, Subs);

    // Split the faces up

    I := 0;
    While FilePos(Bin3DS) < FIndex + Len - 6 Do
    Begin
      BlockRead(Bin3DS, H,SizeOf(H));
      Case H.ID Of
        FACE_MAT:
        Begin
          // Process the faces and split them up

          FacesMaterialsListChunkProcessor(H.Len, FilePos(Bin3DS), ObjIndex, I);
          Inc(I);
        End;
      End; // Case
      Seek(Bin3DS,FilePos(Bin3DS) + (H.Len - 6));
    End; // While
  End;

  // move the file pointer back to where we got it so
  // that the ProcessChunk() which we interrupted will read
  // from the right place
  
  Seek(Bin3DS, FIndex);
End; // Model_3DS.FacesDescriptionChunkProcessor

Procedure Model_3DS.FacesMaterialsListChunkProcessor(Len,FIndex: LongInt; ObjIndex,SubFacesIndex: Integer);
Var
  Name       : String;
  NumEntries : Word;                               // The number of faces associated with this material
  Face       : Word;                               // Holds the faces as they are read
  Material   : Integer;                            // An index to the Materials array for this material
  I          : Integer;
  Done       : Boolean;

Begin
  // move the file pointer to the beginning of the main
  // chunk's data FIndex + the size of the header

  Seek(Bin3DS, FIndex);

  // Read the material's name

  Name := ReadNameFromFile(Bin3DS);

  // Find the material's index in the Materials array

  Material := 0;
  Done     := False;
  While (Material < NumMaterials) And Not Done Do
  Begin
    If Name = Materials[Material].Name Then Done := True Else Inc(Material);
  End; // While

  // Store this value for later so that we can find the material

  Objects[ObjIndex].MatFaces[SubFacesIndex].MatIndex := Material;

  // Read the number of faces associated with this material

  BlockRead(Bin3DS,NumEntries,SizeOf(NumEntries));

  // Allocate an array to hold the list of faces associated with this material

  SetLength(Objects[ObjIndex].MatFaces[SubFacesIndex].SubFaces,NumEntries);

  // Store this number for later use

  Objects[ObjIndex].MatFaces[SubFacesIndex].NumSubFaces := NumEntries;

  // Read the faces into the array

  For I := 0 To NumEntries - 1 Do
  Begin
    // read the face

    BlockRead(Bin3DS,Face,SizeOf(Face));

    // Add the face's vertices to the list

    Objects[ObjIndex].MatFaces[SubFacesIndex].SubFaces[I] := Objects[ObjIndex].Faces[Face];
    Objects[ObjIndex].Faces[Face].MatIndex := Material;
  End; // For I

  // move the file pointer back to where we got it so
  // that the ProcessChunk() which we interrupted will read
  // from the right place

  Seek(Bin3DS, FIndex);
End; // Model_3DS.FacesMaterialsListChunkProcessor

Procedure Model_3DS.SmoothGroupChunkProcessor(Len,FIndex: LongInt; ObjIndex: Integer);
Begin
  // move the file pointer to the beginning of the main
  // chunk's data FIndex + the size of the header

  Seek(Bin3DS, FIndex);

  // For this object, read in the smooth group for each face
  // Reserve memory first

  SetLength(Objects[ObjIndex].FaceSmoothGroups,Objects[ObjIndex].NumFaces);

  BlockRead(Bin3DS,Objects[ObjIndex].FaceSmoothGroups[0],Objects[ObjIndex].NumFaces * SizeOf(LongWord));

  // move the file pointer back to where we got it so
  // that the ProcessChunk() which we interrupted will read
  // from the right place

  Seek(Bin3DS, FIndex);
End; // Model_3DS.SmoothGroupChunkProcessor

Procedure Model_3DS.LocalCoordinatesChunkProcessor(Len,FIndex: LongInt; ObjIndex: Integer);
Begin
  // move the file pointer to the beginning of the main
  // chunk's data FIndex + the size of the header

  Seek(Bin3DS, FIndex);

  BlockRead(Bin3DS,Objects[ObjIndex].LocalMatrix[0],12 * SizeOf(Single));

  // move the file pointer back to where we got it so
  // that the ProcessChunk() which we interrupted will read
  // from the right place

  Seek(Bin3DS, FIndex);
End; // Model_3DS.LocalCoordinatesChunkProcessor

Procedure Model_3DS.TransformLocalCoords;
Var
  ObjIdx : Integer;
  Vert   : Integer;
  Obj    : Object_3DS;
  Orig   : Vector_3DS;

Begin
  // Transform by matrix
  // Note that 3DS matrix is COLUMN major not row major

  For ObjIdx := 0 To NumObjects - 1 Do
  Begin
    Obj  := Objects[ObjIdx];
    For Vert := 0 To Obj.NumVerts - 1 Do
    Begin
      // Assumed w value of vector is 1.0
      // w result is discarded
      // Copy current value

      Orig := Obj.Vertexes[Vert];
      Obj.Vertexes[Vert].X := Obj.LocalMatrix[0,0] * Orig.X +
                              Obj.LocalMatrix[1,0] * Orig.Y +
                              Obj.LocalMatrix[2,0] * Orig.Z +
                              Obj.LocalMatrix[3,0] * 1.0;

      Obj.Vertexes[Vert].Y := Obj.LocalMatrix[0,1] * Orig.X +
                              Obj.LocalMatrix[1,1] * Orig.Y +
                              Obj.LocalMatrix[2,1] * Orig.Z +
                              Obj.LocalMatrix[3,1] * 1.0;

      Obj.Vertexes[Vert].Z := Obj.LocalMatrix[0,2] * Orig.X +
                              Obj.LocalMatrix[1,2] * Orig.Y +
                              Obj.LocalMatrix[2,2] * Orig.Z +
                              Obj.LocalMatrix[3,2] * 1.0;
    End; // For Vert
  End; // For ObjIdx
End; // Model_3DS.TransformLocalCoords

Procedure Model_3DS.Save(Name: String);
Var
  H      : ChunkHeader;
  Stream : TMemoryStream;

Begin
  H.ID   := MAIN3DS;
  H.Len  := 0;
  Stream := TMemoryStream.Create;
  Stream.Write(H,SizeOf(H));
  WriteVersionChunk(Stream);
  WriteEditChunk(Stream);
  H.Len  := Stream.Position;
  Stream.Position := 0;
  Stream.Write(H,SizeOf(H));
  Stream.Position := 0;
  Stream.SaveToFile(Name);
  Stream.Free;
End; // Model_3DS.Save

Procedure Model_3DS.WriteVersionChunk(Stream: TStream);
Var
  H       : ChunkHeader;
  OldPos  : Integer;
  NewPos  : Integer;
  Version : LongWord;

Begin
  H.ID   := MAIN_VERS;
  H.Len  := 0;
  OldPos := Stream.Position;
  Stream.Write(H,SizeOf(H));

  Version := 3;
  Stream.Write(Version,SizeOf(Version));

  NewPos := Stream.Position;
  Stream.Position := OldPos;
  H.Len := NewPos - OldPos;
  Stream.Write(H,SizeOf(H));
  Stream.Position := NewPos;
End; // Model_3DS.WriteVersionChunk

Procedure Model_3DS.WriteEditChunk(Stream: TStream);
Var
  H      : ChunkHeader;
  OldPos : Integer;
  NewPos : Integer;
  I      : Integer;

Begin
  H.ID   := EDIT3DS;
  H.Len  := 0;
  OldPos := Stream.Position;
  Stream.Write(H,SizeOf(H));

  WriteMeshVersionChunk(3,Stream);
  For I := 0 To NumMaterials - 1 Do WriteMaterialChunk(I,Stream);
  WriteMasterScaleChunk(1,Stream);
  For I := 0 To NumObjects   - 1 Do WriteObjectChunk(I,Stream);

  NewPos := Stream.Position;
  Stream.Position := OldPos;
  H.Len := NewPos - OldPos;
  Stream.Write(H,SizeOf(H));
  Stream.Position := NewPos;
End; // Model_3DS.WriteEditChunk

Procedure Model_3DS.WriteMeshVersionChunk(Version: Integer; Stream: TStream);
Var
  H      : ChunkHeader;
  OldPos : Integer;
  NewPos : Integer;

Begin
  H.ID   := MESH_VERS;
  H.Len  := 0;
  OldPos := Stream.Position;
  Stream.Write(H,SizeOf(H));

  Stream.Write(Version,4);

  NewPos := Stream.Position;
  Stream.Position := OldPos;
  H.Len := NewPos - OldPos;
  Stream.Write(H,SizeOf(H));
  Stream.Position := NewPos;
End; // Model_3DS.WriteMeshVersionChunk

Procedure Model_3DS.WriteObjectChunk(Index: Integer; Stream: TStream);
Var
  H      : ChunkHeader;
  OldPos : Integer;
  NewPos : Integer;

Begin
  H.ID   := _OBJECT;
  H.Len  := 0;
  OldPos := Stream.Position;
  Stream.Write(H,SizeOf(H));

  WriteNameToStream(Objects[Index].Name,Stream);
  WriteTriangularMeshChunk(Index,Stream);

  NewPos := Stream.Position;
  Stream.Position := OldPos;
  H.Len := NewPos - OldPos;
  Stream.Write(H,SizeOf(H));
  Stream.Position := NewPos;
End; // Model_3DS.WriteObjectChunk

Procedure Model_3DS.WriteTriangularMeshChunk(ObjectIndex: Integer; Stream: TStream);
Var
  H      : ChunkHeader;
  OldPos : Integer;
  NewPos : Integer;

Begin
  H.ID   := TRIG_MESH;
  H.Len  := 0;
  OldPos := Stream.Position;
  Stream.Write(H,SizeOf(H));

  WriteVertexListChunk(ObjectIndex,Stream);
  WriteLocalCoordinatesChunk(ObjectIndex,Stream);
  WriteTexCoordsChunk(ObjectIndex,Stream);
  WriteFacesDescriptionChunk(ObjectIndex,Stream);

  NewPos := Stream.Position;
  Stream.Position := OldPos;
  H.Len := NewPos - OldPos;
  Stream.Write(H,SizeOf(H));
  Stream.Position := NewPos;
End; // Model_3DS.WriteTriangularMeshChunk

Procedure Model_3DS.WriteVertexListChunk(ObjectIndex: Integer; Stream: TStream);
Var
  H        : ChunkHeader;
  OldPos   : Integer;
  NewPos   : Integer;
  NumVerts : Word;
  I        : Integer;

Begin
  H.ID   := VERT_LIST;
  H.Len  := 0;
  OldPos := Stream.Position;
  Stream.Write(H,SizeOf(H));

  NumVerts := Objects[ObjectIndex].NumVerts;
  Stream.Write(NumVerts,SizeOf(NumVerts));
  For I := 0 To NumVerts - 1 Do
  Begin
    Stream.Write(Objects[ObjectIndex].Vertexes[I].X,SizeOf(Single));
    Stream.Write(Objects[ObjectIndex].Vertexes[I].Y,SizeOf(Single));
    Stream.Write(Objects[ObjectIndex].Vertexes[I].Z,SizeOf(Single));
  End; // For I

  NewPos := Stream.Position;
  Stream.Position := OldPos;
  H.Len := NewPos - OldPos;
  Stream.Write(H,SizeOf(H));
  Stream.Position := NewPos;
End; // Model_3DS.WriteVertexListChunk

Procedure Model_3DS.WriteLocalCoordinatesChunk(ObjectIndex: Integer; Stream: TStream);
Var
  H      : ChunkHeader;
  OldPos : Integer;
  NewPos : Integer;

Begin
  H.ID   := LOCAL_COORDS;
  H.Len  := 0;
  OldPos := Stream.Position;
  Stream.Write(H,SizeOf(H));

  Stream.Write(Objects[ObjectIndex].LocalMatrix,12 * SizeOf(Single));

  NewPos := Stream.Position;
  Stream.Position := OldPos;
  H.Len := NewPos - OldPos;
  Stream.Write(H,SizeOf(H));
  Stream.Position := NewPos;
End; // Model_3DS.WriteLocalCoordinatesChunk

Procedure Model_3DS.WriteTexCoordsChunk(ObjectIndex: Integer; Stream: TStream);
Var
  H            : ChunkHeader;
  OldPos       : Integer;
  NewPos       : Integer;
  NumTexCoords : Word;
  I            : Integer;

Begin
  H.ID   := TEX_VERTS;
  H.Len  := 0;
  OldPos := Stream.Position;
  Stream.Write(H,SizeOf(H));

  NumTexCoords := Objects[ObjectIndex].NumTexCoords;
  Stream.Write(NumTexCoords,SizeOf(NumTexCoords));
  For I := 0 To NumTexCoords - 1 Do
  Begin
    Stream.Write(Objects[ObjectIndex].TexCoords[I].TX,SizeOf(Single));
    Stream.Write(Objects[ObjectIndex].TexCoords[I].TZ,SizeOf(Single));
  End; // For I

  NewPos := Stream.Position;
  Stream.Position := OldPos;
  H.Len := NewPos - OldPos;
  Stream.Write(H,SizeOf(H));
  Stream.Position := NewPos;
End; // Model_3DS.WriteTexCoordsChunk

Procedure Model_3DS.WriteFacesDescriptionChunk(ObjectIndex: Integer; Stream: TStream);
Var
  H        : ChunkHeader;
  OldPos   : Integer;
  NewPos   : Integer;
  NumFaces : Word;
  VertA    : Word;
  VertB    : Word;
  VertC    : Word;
  Flags    : Word;
  I        : Integer;

Begin
  H.ID   := FACE_DESC;
  H.Len  := 0;
  OldPos := Stream.Position;
  Stream.Write(H,SizeOf(H));

  NumFaces := Objects[ObjectIndex].NumFaces;
  Stream.Write(NumFaces,SizeOf(NumFaces));
  For I := 0 To NumFaces - 1 Do
  Begin
    VertA := Objects[ObjectIndex].Faces[I].Index[0];
    VertB := Objects[ObjectIndex].Faces[I].Index[1];
    VertC := Objects[ObjectIndex].Faces[I].Index[2];
    Flags := 0;
    Stream.Write(VertA,SizeOf(VertA));
    Stream.Write(VertB,SizeOf(VertB));
    Stream.Write(VertC,SizeOf(VertC));
    Stream.Write(Flags,SizeOf(Flags));
  End; // For I

  For I := 0 To Objects[ObjectIndex].NumMatFaces - 1 Do WriteFacesMaterialsListChunk(ObjectIndex,I,Stream);

  If High(Objects[ObjectIndex].FaceSmoothGroups) >= 0 Then WriteSmoothGroupChunk(ObjectIndex,Stream);

  NewPos := Stream.Position;
  Stream.Position := OldPos;
  H.Len := NewPos - OldPos;
  Stream.Write(H,SizeOf(H));
  Stream.Position := NewPos;
End; // Model_3DS.WriteFacesDescriptionChunk

Procedure Model_3DS.WriteFacesMaterialsListChunk(ObjectIndex,SubIndex: Integer; Stream: TStream);
Var
  H          : ChunkHeader;
  OldPos     : Integer;
  NewPos     : Integer;
  NumEntries : Word;
  I,J        : Integer;
  Found      : Boolean;
  Face       : Word;

  Function EqualFaces(Var F1,F2: Face_3DS): Boolean;
  Begin
    Result := (F1.Index[0] = F2.Index[0]) And
              (F1.Index[1] = F2.Index[1]) And
              (F1.Index[2] = F2.Index[2]) And
              (F1.MatIndex = F2.MatIndex);
  End; // EqualFaces

Begin
  H.ID   := FACE_MAT;
  H.Len  := 0;
  OldPos := Stream.Position;
  Stream.Write(H,SizeOf(H));

  If Objects[ObjectIndex].MatFaces[SubIndex].MatIndex >= 0 Then WriteNameToStream(Materials[Objects[ObjectIndex].MatFaces[SubIndex].MatIndex].Name,Stream);
  NumEntries := Objects[ObjectIndex].MatFaces[SubIndex].NumSubFaces;
  Stream.Write(NumEntries,SizeOf(NumEntries));

  For I := 0 To NumEntries - 1 Do
  Begin
    J     := 0;
    Found := False;
    While (J < Objects[ObjectIndex].NumFaces) And Not Found Do
    Begin
      If EqualFaces(Objects[ObjectIndex].MatFaces[SubIndex].SubFaces[I],Objects[ObjectIndex].Faces[J])
       Then Found := True
       Else Inc(J);
    End; // While
    If Found Then
    Begin
      Face := J;
      Stream.Write(Face,SizeOf(Face));
    End;
  End; // For I

  NewPos := Stream.Position;
  Stream.Position := OldPos;
  H.Len := NewPos - OldPos;
  Stream.Write(H,SizeOf(H));
  Stream.Position := NewPos;
End; // Model_3DS.WriteFacesMaterialsListChunk

Procedure Model_3DS.WriteSmoothGroupChunk(ObjectIndex: Integer; Stream: TStream);
Var
  H      : ChunkHeader;
  OldPos : Integer;
  NewPos : Integer;

Begin
  H.ID   := SMOOTH_GROUP;
  H.Len  := 0;
  OldPos := Stream.Position;
  Stream.Write(H,SizeOf(H));

  Stream.Write(Objects[ObjectIndex].FaceSmoothGroups[0],(High(Objects[ObjectIndex].FaceSmoothGroups) + 1) * SizeOf(LongWord));

  NewPos := Stream.Position;
  Stream.Position := OldPos;
  H.Len := NewPos - OldPos;
  Stream.Write(H,SizeOf(H));
  Stream.Position := NewPos;
End; // Model_3DS.WriteSmoothGroupChunk

Procedure Model_3DS.WriteMaterialChunk(MaterialIndex: Integer; Stream: TStream);
Var
  H      : ChunkHeader;
  OldPos : Integer;
  NewPos : Integer;

Begin
  H.ID   := MATERIAL;
  H.Len  := 0;
  OldPos := Stream.Position;
  Stream.Write(H,SizeOf(H));

  WriteMaterialNameChunk(MaterialIndex,Stream);
  If Materials[MaterialIndex].AmbientSet  Then WriteMatIntColorChunk(MaterialIndex,MAT_AMBIENT, Materials[MaterialIndex].Ambient, Stream);
  If Materials[MaterialIndex].DiffuseSet  Then WriteMatIntColorChunk(MaterialIndex,MAT_DIFFUSE, Materials[MaterialIndex].Diffuse, Stream);
  If Materials[MaterialIndex].SpecularSet Then WriteMatIntColorChunk(MaterialIndex,MAT_SPECULAR,Materials[MaterialIndex].Specular,Stream);
  If Materials[MaterialIndex].Textured    Then WriteTextureMapChunk(MaterialIndex,Stream);
  If Materials[MaterialIndex].OpacityMap  Then WriteOpacityMapChunk(MaterialIndex,Stream);
  WriteShininessChunk(MaterialIndex,Stream);
  WriteShininessStrengthChunk(MaterialIndex,Stream);
  WriteTransparencyChunk(MaterialIndex,Stream);

  NewPos := Stream.Position;
  Stream.Position := OldPos;
  H.Len := NewPos - OldPos;
  Stream.Write(H,SizeOf(H));
  Stream.Position := NewPos;
End; // Model_3DS.WriteMaterialChunk

Procedure Model_3DS.WriteMaterialNameChunk(MaterialIndex: Integer; Stream: TStream);
Var
  H      : ChunkHeader;
  OldPos : Integer;
  NewPos : Integer;

Begin
  H.ID   := MAT_NAME;
  H.Len  := 0;
  OldPos := Stream.Position;
  Stream.Write(H,SizeOf(H));

  WriteNameToStream(Materials[MaterialIndex].Name,Stream);

  NewPos := Stream.Position;
  Stream.Position := OldPos;
  H.Len := NewPos - OldPos;
  Stream.Write(H,SizeOf(H));
  Stream.Position := NewPos;
End; // Model_3DS.WriteMaterialChunk

Procedure Model_3DS.WriteMatIntColorChunk(MaterialIndex,ID: Integer; Var Color: Color4i_3DS; Stream: TStream);
Var
  H      : ChunkHeader;
  OldPos : Integer;
  NewPos : Integer;

Begin
  H.ID   := ID;
  H.Len  := 0;
  OldPos := Stream.Position;
  Stream.Write(H,SizeOf(H));

  WriteIntColorChunk(Color,Stream);

  NewPos := Stream.Position;
  Stream.Position := OldPos;
  H.Len := NewPos - OldPos;
  Stream.Write(H,SizeOf(H));
  Stream.Position := NewPos;
End; // Model_3DS.WriteMatIntColorChunk

Procedure Model_3DS.WriteIntColorChunk(Var Color: Color4i_3DS; Stream: TStream);
Var
  H      : ChunkHeader;
  OldPos : Integer;
  NewPos : Integer;

Begin
  H.ID   := COLOR_TRU;
  H.Len  := 0;
  OldPos := Stream.Position;
  Stream.Write(H,SizeOf(H));

  Stream.Write(Color.R,SizeOf(Color.R));
  Stream.Write(Color.G,SizeOf(Color.G));
  Stream.Write(Color.B,SizeOf(Color.B));

  NewPos := Stream.Position;
  Stream.Position := OldPos;
  H.Len := NewPos - OldPos;
  Stream.Write(H,SizeOf(H));
  Stream.Position := NewPos;
End; // Model_3DS.WriteIntColorChunk

Procedure Model_3DS.WriteTextureMapChunk(MaterialIndex: Integer; Stream: TStream);
Var
  H      : ChunkHeader;
  OldPos : Integer;
  NewPos : Integer;

Begin
  H.ID   := MAT_TEXMAP;
  H.Len  := 0;
  OldPos := Stream.Position;
  Stream.Write(H,SizeOf(H));

  If Materials[MaterialIndex].Textured Then WriteMapNameChunk(MaterialIndex,Stream,Materials[MaterialIndex].TexName);

  NewPos := Stream.Position;
  Stream.Position := OldPos;
  H.Len := NewPos - OldPos;
  Stream.Write(H,SizeOf(H));
  Stream.Position := NewPos;
End; // Model_3DS.WriteTextureMapChunk

Procedure Model_3DS.WriteOpacityMapChunk(MaterialIndex: Integer; Stream: TStream);
Var
  H      : ChunkHeader;
  OldPos : Integer;
  NewPos : Integer;

Begin
  H.ID   := MAT_TEXMAP;
  H.Len  := 0;
  OldPos := Stream.Position;
  Stream.Write(H,SizeOf(H));

  If Materials[MaterialIndex].OpacityMap Then WriteMapNameChunk(MaterialIndex,Stream,Materials[MaterialIndex].OpacityMapName);

  NewPos := Stream.Position;
  Stream.Position := OldPos;
  H.Len := NewPos - OldPos;
  Stream.Write(H,SizeOf(H));
  Stream.Position := NewPos;
End; // Model_3DS.WriteOpacityMapChunk

Procedure Model_3DS.WriteMapNameChunk(MaterialIndex: Integer; Stream: TStream; MapName: String);
Var
  H      : ChunkHeader;
  OldPos : Integer;
  NewPos : Integer;

Begin
  H.ID   := MAT_MAPNAME;
  H.Len  := 0;
  OldPos := Stream.Position;
  Stream.Write(H,SizeOf(H));

  WriteNameToStream(MapName,Stream);

  NewPos := Stream.Position;
  Stream.Position := OldPos;
  H.Len := NewPos - OldPos;
  Stream.Write(H,SizeOf(H));
  Stream.Position := NewPos;
End; // Model_3DS.WriteMapNameChunk

Procedure Model_3DS.WriteShininessChunk(MaterialIndex: Integer; Stream: TStream);
Var
  H      : ChunkHeader;
  OldPos : Integer;
  NewPos : Integer;

Begin
  H.ID   := SHINY_PERC;
  H.Len  := 0;
  OldPos := Stream.Position;
  Stream.Write(H,SizeOf(H));

  WritePercentIntChunk(Materials[MaterialIndex].Shininess,Stream);

  NewPos := Stream.Position;
  Stream.Position := OldPos;
  H.Len := NewPos - OldPos;
  Stream.Write(H,SizeOf(H));
  Stream.Position := NewPos;
End; // Model_3DS.WriteShininessChunk

Procedure Model_3DS.WriteShininessStrengthChunk(MaterialIndex: Integer; Stream: TStream);
Var
  H      : ChunkHeader;
  OldPos : Integer;
  NewPos : Integer;

Begin
  H.ID   := SHINY_STR_PERC;
  H.Len  := 0;
  OldPos := Stream.Position;
  Stream.Write(H,SizeOf(H));

  WritePercentIntChunk(Materials[MaterialIndex].ShininessStrength,Stream);

  NewPos := Stream.Position;
  Stream.Position := OldPos;
  H.Len := NewPos - OldPos;
  Stream.Write(H,SizeOf(H));
  Stream.Position := NewPos;
End; // Model_3DS.WriteShininessStrengthChunk

Procedure Model_3DS.WriteTransparencyChunk(MaterialIndex: Integer; Stream: TStream);
Var
  H      : ChunkHeader;
  OldPos : Integer;
  NewPos : Integer;

Begin
  H.ID   := TRANS_PERC;
  H.Len  := 0;
  OldPos := Stream.Position;
  Stream.Write(H,SizeOf(H));

  WritePercentIntChunk(Materials[MaterialIndex].Transparency,Stream);

  NewPos := Stream.Position;
  Stream.Position := OldPos;
  H.Len := NewPos - OldPos;
  Stream.Write(H,SizeOf(H));
  Stream.Position := NewPos;
End; // Model_3DS.WriteTransparencyChunk

Procedure Model_3DS.WritePercentIntChunk(W: Word; Stream: TStream);
Var
  H      : ChunkHeader;
  OldPos : Integer;
  NewPos : Integer;

Begin
  H.ID   := PERC_INT;
  H.Len  := 0;
  OldPos := Stream.Position;
  Stream.Write(H,SizeOf(H));

  Stream.Write(W,SizeOf(W));

  NewPos := Stream.Position;
  Stream.Position := OldPos;
  H.Len := NewPos - OldPos;
  Stream.Write(H,SizeOf(H));
  Stream.Position := NewPos;
End; // Model_3DS.WritePercentIntChunk

Procedure Model_3DS.WriteMasterScaleChunk(Scale: Single; Stream: TStream);
Var
  H      : ChunkHeader;
  OldPos : Integer;
  NewPos : Integer;

Begin
  H.ID   := MASTER_SCALE;
  H.Len  := 0;
  OldPos := Stream.Position;
  Stream.Write(H,SizeOf(H));

  Stream.Write(Scale,SizeOf(Scale));

  NewPos := Stream.Position;
  Stream.Position := OldPos;
  H.Len := NewPos - OldPos;
  Stream.Write(H,SizeOf(H));
  Stream.Position := NewPos;
End; // Model_3DS.WriteMasterScaleChunk

End.

