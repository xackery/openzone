unit S3D;
// --------------------------------------------
// Generic S3D file format handler
// --------------------------------------------

interface

Uses Classes;

Type
  TS3DFileHeader = Packed Record
    DirectoryOffset        : LongWord;                   // File position of the S3D directory
    MagicCookie            : Packed Array[0..3] Of Char; // Always "PFS ".  It identifies the file type
    Unknown_Always_131072  : LongWord;                   // As the spec says, it always contains 131072 (128k)
  End;
  TS3DDirectoryEntry = Packed Record
    CRC                    : LongWord;                   // Filename CRC.  Calculated with the standard IEEE 802.3 Ethernet CRC-32 algorithm.
    DataOffset             : LongWord;                   // Position in the archive of the compressed data
    DataLengthInflated     : LongWord;                   // The file size once inflated
  End;
  TS3DFileFooter = Packed Record
    SteveCookie5           : Packed Array[0..4] Of Char; // Always "STEVE".
    Date                   : LongWord;                   // I think the patcher uses this to check a file's version
  End;
  TS3DDataBlockHeader = Packed Record
    DeflatedLength         : LongWord;                   // Compressed size
    InflatedLength         : LongWord;                   // Uncompressed size
  End;
  TS3DFileData = Class
  Protected
    FCompressedSize : Integer;
    FSize           : Integer;
    FData           : Pointer;
    FUncompressed   : Boolean;
    FBlocks         : Array Of TS3DDataBlockHeader;
    Procedure   SetSize(I: Integer);
    Function    GetDataPointer: Pointer;
    Function    GetNumBlocks: Integer;
    Function    GetBlock(Index: Integer): TS3DDataBlockHeader;
    Procedure   SetBlock(Index: Integer; Block: TS3DDataBlockHeader);
  Public
    Constructor Create;
    Destructor  Destroy; Override;
    Procedure   AddBlock(Block: TS3DDataBlockHeader);
    Property    Size           : Integer Read FSize Write SetSize;
    Property    CompressedSize : Integer Read FCompressedSize;
    Property    DataPointer    : Pointer Read GetDataPointer;
    Property    RawDataPointer : Pointer Read FData;
    Property    Uncompressed   : Boolean Read FUncompressed Write FUncompressed;
    Property    NumBlocks      : Integer Read GetNumBlocks;
    Property    Blocks[Index: Integer]: TS3DDataBlockHeader Read GetBlock Write SetBlock;
  End;
  TS3DFile = Class
  Protected
    FDirectory  : Array Of TS3DDirectoryEntry;            // Stores all the directory entries
    FFileData   : TStringList;                            // Stores the uncompressed data
    FFileNames  : Array Of String;                        // Stores the list of file names
    FFooter     : TS3DFileFooter;
    FFooterRead : Boolean;
    FFileRead   : Boolean;
    Function    GetFileCount: Integer;
    Procedure   SetFileCount(NewCount: Integer);
    Function    GetFileName(Index: Integer): String;
    Procedure   SetFileName(Index: Integer; St: String);
    Function    GetFileSize(Index: Integer): LongInt;
    Procedure   SetFileSize(Index,NewSize: Integer);
    Function    GetCompressedFileSize(Index: Integer): LongInt;
    Function    GetDataPointer(Index: Integer): Pointer;
    Function    GetRawDataPointer(Index: Integer): Pointer;
  Public
    Constructor Create;
    Destructor  Destroy; Override;
    Procedure   Clear;
    Procedure   LoadFromFile(FileName: String);
    Procedure   SaveToFile(FileName: String);
    Procedure   LoadFromStream(Stream: TStream);
    Procedure   SaveToStream(Stream: TStream);
    Procedure   AddFile(NewFileName: String);
    Procedure   DeleteFile(Index: Integer);
    Function    ContainsFile(FileName: String): Boolean;
    Function    GetFileIndex(FileName: String): Integer;
    Property    FileCount                          : Integer Read GetFileCount Write SetFileCount;
    Property    FileName[Index: Integer]           : String  Read GetFileName  Write SetFileName;
    Property    FileSize[Index: Integer]           : LongInt Read GetFileSize  Write SetFileSize;
    Property    CompressedFileSize[Index: Integer] : LongInt Read GetCompressedFileSize;
    Property    DataPointer[Index: Integer]        : Pointer Read GetDataPointer;
    Property    RawDataPointer[Index: Integer]     : Pointer Read GetRawDataPointer;
  End;

implementation

Uses Ethernet,SysUtils,zUtil,zUnCompr,ZCompres,Dialogs,Math,ZLib;

// TS3DFileData

Constructor TS3DFileData.Create;
Begin
  FSize           := 0;
  FCompressedSize := 0;
  FData           := Nil;
  FUncompressed   := False;
  SetLength(FBlocks,0);
End; // TS3DFileData.Create

Destructor TS3DFileData.Destroy;
Begin
  If (FSize > 0) And (FData <> Nil) Then
  Begin
    FreeMem(FData,FSize);
    FData           := Nil;
    FSize           := 0;
    FCompressedSize := 0;
    FUncompressed   := False;
  End;
  SetLength(FBlocks,0);
End; // TS3DFileData.Destroy

Procedure TS3DFileData.AddBlock(Block: TS3DDataBlockHeader);
Begin
  SetLength(FBlocks, High(FBlocks) + 2);
  FBlocks[High(FBlocks)] := Block;
  Inc(FCompressedSize,Block.DeflatedLength);
End; // TS3DFileData.AddBlock

Function TS3DFileData.GetNumBlocks: Integer;
Begin
  Result := High(FBlocks) + 1;
End; // TS3DFileData.GetNumBlocks

Function TS3DFileData.GetBlock(Index: Integer): TS3DDataBlockHeader;
Begin
  Result := FBlocks[Index];
End; // TS3DFileData.GetBlock

Procedure TS3DFileData.SetBlock(Index: Integer; Block: TS3DDataBlockHeader);
Begin
  FBlocks[Index] := Block;
End; // TS3DFileData.SetBlock

Procedure TS3DFileData.SetSize(I: Integer);
Var
  P : Pointer;
  J : Integer;

Begin
  If I > 0 Then
  Begin
    GetMem(P,I);
    FillChar(P^,I,0);
    If (FSize > 0) And (FData <> Nil) Then
    Begin
      If FSize < I Then J := FSize Else J := I;
      Move(FData^,P^,J);
      FreeMem(FData,FSize);
    End;
    FData := P;
  End
  Else
  Begin
    If (FSize > 0) And (FData <> Nil) Then
    Begin
      FreeMem(FData,FSize);
      FData := Nil;
    End;
  End;
  FSize         := I;
  FUncompressed := True;
  SetLength(FBlocks,0);
End; // TS3DFileData.SetSize

Function TS3DFileData.GetDataPointer: Pointer;
Var
  I,J,K       : Integer;
  InBuf       : TUncomprSource;
  DestLen     : uLong;
  Buffer      : Pointer;

Begin
  If Not FUncompressed Then
  Begin
    J := 0;
    K := 0;
    GetMem(Buffer,FSize);
    For I := 0 To High(FBlocks) Do
    Begin
      SetLength(InBuf,FBlocks[I].DeflatedLength);
      Move(Pointer(LongInt(FData) + J)^,InBuf[0],FBlocks[I].DeflatedLength);
      DestLen := FBlocks[I].InflatedLength;
      If Uncompress(Pointer(LongInt(Buffer) + K),DestLen,InBuf,FBlocks[I].DeflatedLength) <> Z_OK Then
      Begin
        SetLength(InBuf,0);
        FreeMem(Buffer);
        Result := Nil;
        Exit;
      End;
      Inc(J,FBlocks[I].DeflatedLength);
      Inc(K,FBlocks[I].InflatedLength);
    End; // For I
    SetLength(InBuf,0);
    Size := K;
    Move(Buffer^,FData^,K);
    FreeMem(Buffer);
    FUncompressed := True;
  End;
  Result := FData;
End; // TS3DFileData.GetDataPointer

// TS3DFile

Constructor TS3DFile.Create;
Begin
  FFileData := TStringList.Create;
  Clear;
End; // TS3DFile.Create

Destructor TS3DFile.Destroy;
Begin
  Clear;
  FFileData.Free;
End; // TS3DFile.Destroy

Procedure TS3DFile.Clear;
Var I: Integer;
Begin
  SetLength(FDirectory,0);
  For I := 0 To FFileData.Count - 1 Do FFileData.Objects[I].Free;
  FFileData.Clear;
  SetLength(FFileNames,0);
  FFooterRead := False;
  FFileRead   := False;
End; // TS3DFile.Clear

Procedure TS3DFile.LoadFromFile(FileName: String);
Var Stream: TFileStream;
Begin
  If FileExists(FileName) Then
  Begin
    Stream := TFileStream.Create(FileName,fmOpenRead);
    LoadFromStream(Stream);
    Stream.Free;
  End;
End; // TS3DFile.LoadFromFile

Procedure TS3DFile.LoadFromStream(Stream: TStream);
Var
  Header      : TS3DFileHeader;
  St          : String;
  NumEntries  : Integer;
  I,J,K,L     : Integer;
  FileListPos : LongInt;
  Buffer      : TUncomprSource;
  BlockHeader : TS3DDataBlockHeader;
  FileData    : TS3DFileData;
  FileData1   : TS3DFileData;
  P           : Pointer;

  Procedure QuickSortByOffset(P,R: Integer);
  Var Q: Integer;

    Function Partition(P,R: Integer): Integer;
    Var
      I,J : Integer;
      K   : LongWord;
      Dir : TS3DDirectoryEntry;

    Begin
      K := FDirectory[P].DataOffset;
      I := P - 1;
      J := R + 1;
      While True Do
      Begin
        Repeat
          Dec(J);
        Until FDirectory[J].DataOffset <= K;
        Repeat
          Inc(I);
        Until FDirectory[I].DataOffset >= K;
        If I < J Then
        Begin
          Dir           := FDirectory[I];
          FDirectory[I] := FDirectory[J];
          FDirectory[J] := Dir;
          FFileData.Exchange(I,J);
        End
        Else
        Begin
          Partition := J;
          Exit;
        End;
      End; { While }
    End; // Partition

  Begin
    If P < R Then
    Begin
      Q := Partition(P,R);
      QuickSortByOffset(P,Q);
      QuickSortByOffset(Q + 1,R);
    End;
  End; // QuickSortByOffset

Begin
  If Assigned(Stream) Then
  Begin
    Clear;
    FFileRead := True;
    St        := '1234';

    // Load the header

    If Stream.Size >= SizeOf(Header) Then Stream.ReadBuffer(Header,SizeOf(Header));

    // Make sure it's the right file type

    Move(Header.MagicCookie[0],St[1],4);
    If St = 'PFS ' Then
    Begin
      Stream.Position := Header.DirectoryOffset;

      // Get the number of entries

      Stream.ReadBuffer(NumEntries,SizeOf(NumEntries));

      // Load the directory entries

      SetLength(FDirectory,NumEntries);
      For I := 0 To NumEntries - 1 Do Stream.ReadBuffer(FDirectory[I],SizeOf(FDirectory[I]));

      // Read the footer, if it's there (sometimes it isn't)

      If (Stream.Size - Stream.Position >= SizeOf(FFooter)) Then
      Begin
        Stream.ReadBuffer(FFooter,SizeOf(FFooter));
        FFooterRead := True;
      End;

      // Read the data

      For I := 0 To NumEntries - 1 Do
      Begin
        FileData  := TS3DFileData.Create;
        FileData1 := TS3DFileData.Create;
        FFileData.AddObject('',FileData);

        // Move to the data block

        Stream.Position := FDirectory[I].DataOffset;

        // We're going to read the data and keep in compressed form, but allocate
        // enough memory to store it in uncompressed form if we have to.  Asking
        // for the data pointer will cause it to be uncompressed on the fly.

        FileData.Size := Round(FDirectory[I].DataLengthInflated * 1.5 + 16);
        FileData.Uncompressed := False;

        J := 0;
        K := 0;
        SetLength(FileData.FBlocks,0);

        // Keep reading data blocks until we load the entire file

        While K < Int64(FDirectory[I].DataLengthInflated) Do // Typecast to a signed type
        Begin
          // Read the data block header and save it in a separate location

          Stream.ReadBuffer(BlockHeader,SizeOf(BlockHeader));
          FileData1.AddBlock(BlockHeader);

          // Store the data in compressed form

          Stream.ReadBuffer(Pointer(LongInt(FileData.FData) + J)^,BlockHeader.DeflatedLength);
          Inc(J,BlockHeader.DeflatedLength);
          Inc(K,BlockHeader.InflatedLength);
        End; // While

        // Set the buffer size to an accurate value, but leave the data compressed.  Setting
        // the size clears the block list, but we've saved it elsewhere so we can copy it in.

        FileData.Size := Max(J,K);
        FileData.Uncompressed := False;
        For K := 0 To FileData1.NumBlocks - 1 Do FileData.AddBlock(FileData1.Blocks[K]);
        FileData1.Free;
      End; // For I

      SetLength(Buffer,0);

      // The file listing is at the bottom, in terms of file offset

      J           := -1;
      FileListPos := -1;
      For I := 0 To NumEntries - 1 Do
      Begin
        If Int64(FDirectory[I].DataOffset) > FileListPos Then // Typecast to a signed type
        Begin
          FileListPos := FDirectory[I].DataOffset;
          J           := I;
        End;
      End; // For I

      // Process the directory

      If J >= 0 Then
      Begin
        // Get the number of filenames

        FileData := FFileData.Objects[J] As TS3DFileData;
        P := FileData.DataPointer;
        Move(P^,I,SizeOf(I));
        SetLength(FFileNames,I);
        Inc(LongInt(P),4);
        For I := 0 To High(FFileNames) Do
        Begin
          Move(P^,L,SizeOf(L));
          Inc(LongInt(P),4);
          If L > 0 Then
          Begin
            FFileNames[I] := '';

            // Copy the file name, omitting the null terminator

            While Length(FFileNames[I]) < L - 1 Do FFileNames[I] := FFileNames[I] + ' ';
            Move(P^,FFileNames[I][1],L - 1);
          End;
          Inc(LongInt(P),L);
        End; // For I

        // Get rid of the directory data area and entry

        FileData.Free;
        FFileData.Delete(J);
        While J < NumEntries - 1 Do
        Begin
          FDirectory[J] := FDirectory[J + 1];
          Inc(J);
        End; // While
        SetLength(FDirectory,High(FDirectory));
      End;

      // The directory is in filename CRC order, not file offset order.
      // We need to reorder the directory into file offset order

      QuickSortByOffset(0,High(FDirectory));
    End;
  End;
End; // TS3DFile.LoadFromStream

Function TS3DFile.GetFileCount: Integer;
Begin
  Result := High(FFileNames) + 1;
End; // TS3DFile.GetFileCount

Procedure TS3DFile.SetFileCount(NewCount: Integer);
Begin
  If NewCount >= 0 Then
  Begin
    While NewCount > FileCount Do AddFile('');
    While NewCount < FileCount Do DeleteFile(FileCount - 1);
  End;
End; // TS3DFile.SetFileCount

Function TS3DFile.GetFileName(Index: Integer): String;
Begin
  If (Index >= 0) And (Index <= High(FFileNames))
   Then Result := FFileNames[Index]
   Else Result := '';
End; // TS3DFile.GetFileName

Procedure TS3DFile.SetFileName(Index: Integer; St: String);
Begin
  If (Index >= 0) And (Index <= High(FFileNames)) Then
  Begin
    St                    := Trim(St);
    FFileNames[Index]     := St;
    FDirectory[Index].CRC := GetCRCOfString(LowerCase(St));
  End;
End; // TS3DFile.SetFileName

Procedure TS3DFile.AddFile(NewFileName: String);
Begin
  SetLength(FDirectory,High(FDirectory) + 2);
  FFileData.AddObject('',TS3DFileData.Create);
  SetLength(FFileNames,High(FFileNames) + 2);
  FileName[FileCount - 1] := NewFileName;
End; // TS3DFile.AddFile

Procedure TS3DFile.DeleteFile(Index: Integer);
Var I: Integer;
Begin
  If (Index >= 0) And (Index <= High(FFileNames)) Then
  Begin
    FFileData.Objects[Index].Free;
    FFileData.Delete(Index);
    For I := Index To High(FDirectory) - 1 Do FDirectory[I] := FDirectory[I + 1];
    For I := Index To High(FFileNames) - 1 Do FFileNames[I] := FFileNames[I + 1];
    SetLength(FDirectory,High(FDirectory));
    SetLength(FFileNames,High(FFileNames));
  End;
End; // TS3DFile.DeleteFile

Function TS3DFile.GetFileSize(Index: Integer): LongInt;
Begin
  If (Index >= 0) And (Index < FFileData.Count)
   Then Result := (FFileData.Objects[Index] As TS3DFileData).Size
   Else Result := -1;
End; // TS3DFile.GetFileSize

Procedure TS3DFile.SetFileSize(Index,NewSize: Integer);
Begin
  If (Index >= 0) And (Index < FFileData.Count) And (NewSize >= 0) Then
   (FFileData.Objects[Index] As TS3DFileData).Size := NewSize;
End; // TS3DFile.SetFileSize

Function TS3DFile.GetCompressedFileSize(Index: Integer): LongInt;
Begin
  If (Index >= 0) And (Index < FFileData.Count)
   Then Result := (FFileData.Objects[Index] As TS3DFileData).CompressedSize
   Else Result := -1;
End; // TS3DFile.GetCompressedFileSize

Function TS3DFile.GetDataPointer(Index: Integer): Pointer;
Begin
  If (Index >= 0) And (Index < FFileData.Count)
   Then Result := (FFileData.Objects[Index] As TS3DFileData).DataPointer
   Else Result := Nil;
End; // TS3DFile.GetDataPointer

Function TS3DFile.GetRawDataPointer(Index: Integer): Pointer;
Begin
  If (Index >= 0) And (Index < FFileData.Count)
   Then Result := (FFileData.Objects[Index] As TS3DFileData).RawDataPointer
   Else Result := Nil;
End; // TS3DFile.GetRawDataPointer

Function TS3DFile.ContainsFile(FileName: String): Boolean;
Var
  I     : Integer;
  Found : Boolean;

Begin
  FileName := Trim(FileName);
  I        := 0;
  Found    := False;
  While (I <= High(FFileNames)) And Not Found Do
  Begin
    If FFileNames[I] = FileName Then Found := True Else Inc(I);
  End; // While
  Result := Found;
End; // TS3DFile.ContainsFile

Function TS3DFile.GetFileIndex(FileName: String): Integer;
Var
  I     : Integer;
  Found : Boolean;

Begin
  FileName := Trim(FileName);
  I        := 0;
  Found    := False;
  While (I <= High(FFileNames)) And Not Found Do
  Begin
    If FFileNames[I] = FileName Then Found := True Else Inc(I);
  End; // While
  If Found
   Then Result := I
   Else Result := -1;
End; // TS3DFile.GetFileIndex

Procedure TS3DFile.SaveToFile(FileName: String);
Var
  F      : File;
  Stream : TFileStream;

Begin
  IOResult;
  If FileExists(FileName) Then
  Begin
    AssignFile(F,FileName);
    IOResult;
    Erase(F);
  End;
  Stream := TFileStream.Create(FileName,fmCreate);
  SaveToStream(Stream);
  Stream.Free;
End; // TS3DFile.SaveToFile

Procedure TS3DFile.SaveToStream(Stream: TStream);
Var
  Header       : TS3DFileHeader;
  BlockHeader  : Array Of Array Of TS3DDataBlockHeader;
  DestBufSize  : Array Of Array Of uLong;
  DestBufSize1 : Array Of Array Of uLong;
  DestBuf      : Array Of Array Of pBytef;
  FileIndex    : Array Of Integer;
  FileIndex1   : Array Of Integer;
  I,J,K,L      : Integer;
  FileNames    : Packed Array Of Byte;
  FileDir      : Array Of TS3DDirectoryEntry;
  Blocks       : Integer;
  BlockSize    : Integer;
  FileData     : TS3DFileData;

  Procedure QuickSortFileNames(P,R: Integer);
  Var Q: Integer;

    Function Partition(P,R: Integer): Integer;
    Var
      I,J,L : Integer;
      K     : String;

    Begin
      K := LowerCase(FFileNames[FileIndex1[P]]);
      I := P - 1;
      J := R + 1;
      While True Do
      Begin
        Repeat
          Dec(J);
        Until LowerCase(FFileNames[FileIndex1[J]]) <= K;
        Repeat
          Inc(I);
        Until LowerCase(FFileNames[FileIndex1[I]]) >= K;
        If I < J Then
        Begin
          L             := FileIndex1[I];
          FileIndex1[I] := FileIndex1[J];
          FileIndex1[J] := L;
        End
        Else
        Begin
          Partition := J;
          Exit;
        End;
      End; { While }
    End; // Partition

  Begin
    If P < R Then
    Begin
      Q := Partition(P,R);
      QuickSortFileNames(P,Q);
      QuickSortFileNames(Q + 1,R);
    End;
  End; // QuickSortFileNames

  Procedure QuickSortCRCs(P,R: Integer);
  Var Q: Integer;

    Function Partition(P,R: Integer): Integer;
    Var
      I,J,L : Integer;
      K     : LongWord;

    Begin
      K := FileDir[FileIndex[P]].CRC;
      I := P - 1;
      J := R + 1;
      While True Do
      Begin
        Repeat
          Dec(J);
        Until FileDir[FileIndex[J]].CRC <= K;
        Repeat
          Inc(I);
        Until FileDir[FileIndex[I]].CRC >= K;
        If I < J Then
        Begin
          L            := FileIndex[I];
          FileIndex[I] := FileIndex[J];
          FileIndex[J] := L;
        End
        Else
        Begin
          Partition := J;
          Exit;
        End;
      End; { While }
    End; // Partition

  Begin
    If P < R Then
    Begin
      Q := Partition(P,R);
      QuickSortCRCs(P,Q);
      QuickSortCRCs(Q + 1,R);
    End;
  End; // QuickSortCRCs

Begin
  // Allocate data

  SetLength(FileIndex,FileCount + 1);
  SetLength(FileIndex1,FileCount + 1);
  SetLength(FileDir,FileCount + 1);
  SetLength(BlockHeader,FileCount + 1); // An additional set for the file list
  SetLength(DestBuf,FileCount + 1);
  SetLength(DestBufSize,FileCount + 1);
  SetLength(DestBufSize1,FileCount + 1);

  // Sort by filename

  For I := 0 To High(FileIndex1) Do FileIndex1[I] := I;
  QuickSortFileNames(0,High(FileIndex1) - 1);  // Don't change the last one

  // Initialize the header

  Header.MagicCookie[0]        := 'P';
  Header.MagicCookie[1]        := 'F';
  Header.MagicCookie[2]        := 'S';
  Header.MagicCookie[3]        := ' ';
  Header.Unknown_Always_131072 := 131072;

  // Figure out how long the uncompressed filename data is, and build the filename data

  J := 4; // Filename count
  For I := 0 To High(FFileNames) Do Inc(J,Length(FFileNames[I]) + 5); // Null terminator and length dword
  SetLength(FileNames,J);
  FillChar(FileNames[0],J,0); // Initialize to zeros so we get all our null terminators
  I := FileCount;
  Move(I,FileNames[0],4);
  K := 4;
  For I := 0 To High(FFileNames) Do
  Begin
    // Skip the reference to the filename data

    If FileIndex1[I] <= High(FFileNames) Then
    Begin
      J := Length(FFileNames[FileIndex1[I]]) + 1;
      Move(J,FileNames[K],4);
      Move(FFileNames[FileIndex1[I]][1],FileNames[K + 4],J - 1); // Pascal strings aren't null-terminated, so only copy the characters
      Inc(K,J + 4);
    End;
  End; // For I

  // Each file has to be broken up into 8k blocks.  Find out how many blocks we need
  // and allocate enough block headers.  Then compress the data.

  For I := 0 To FileCount Do
  Begin
    If I < FileCount Then J := FileSize[I] Else J := High(FileNames) + 1;

    // If the data is already compressed, all we have to do is copy the blocks

    If I < FileCount
     Then FileData := FFileData.Objects[I] As TS3DFileData
     Else FileData := Nil;
    If (I < FileCount) And Not (FFileData.Objects[I] As TS3DFileData).Uncompressed Then
    Begin
      Blocks   := FileData.NumBlocks;
      SetLength(BlockHeader[I],Blocks);
      SetLength(DestBuf[I],Blocks);
      SetLength(DestBufSize[I],Blocks);
      SetLength(DestBufSize1[I],Blocks);
      FileData.FCompressedSize := 0;
      L := 0;
      For K := 0 To Blocks - 1 Do
      Begin
        DestBufSize[I][K]  := FileData.Blocks[K].DeflatedLength;
        DestBufSize1[I][K] := FileData.Blocks[K].DeflatedLength;
        GetMem(DestBuf[I][K],DestBufSize[I][K]);
        Move(Pointer(LongInt(RawDataPointer[I]) + L)^,DestBuf[I][K]^,DestBufSize[I][K]);
        Inc(L,DestBufSize[I][K]);
        BlockHeader[I][K] := FileData.Blocks[K];
        Inc(FileData.FCompressedSize,DestBufSize[I][K]);
      End; // For K
    End
    Else
    Begin
      Blocks := J Div 8192;
      If Blocks * 8192 < J Then Inc(Blocks);
      SetLength(BlockHeader[I],Blocks);
      SetLength(DestBuf[I],Blocks);
      SetLength(DestBufSize[I],Blocks);
      SetLength(DestBufSize1[I],Blocks);
      If FileData <> Nil Then FileData.FCompressedSize := 0;

      // Compress the data

      For K := 0 To Blocks - 1 Do
      Begin
        If K < Blocks - 1
         Then BlockSize := 8192
         Else BlockSize := J - K * 8192;
        DestBufSize[I][K]  := Round((BlockSize + 12) * 1.25); // Have to allow for growth
        DestBufSize1[I][K] := DestBufSize[I][K];
        GetMem(DestBuf[I][K],DestBufSize[I][K]);
        If I < FileCount
         Then Compress4(DestBuf[I][K],DestBufSize1[I][K],Pointer(LongInt(DataPointer[I]) + K * 8192),BlockSize)
         Else Compress4(DestBuf[I][K],DestBufSize1[I][K],@(FileNames[K * 8192]),BlockSize);
        If FileData <> Nil Then Inc(FileData.FCompressedSize,DestBufSize1[I][K]);

        // Fill in the block header data

        BlockHeader[I][K].DeflatedLength := DestBufSize1[I][K];
        BlockHeader[I][K].InflatedLength := BlockSize;
      End; // For K
    End;
  End; // For I

  // Start the directory entries

  For I := 0 To FileCount - 1 Do
  Begin
    FileDir[I].CRC                := FDirectory[I].CRC;
    FileDir[I].DataLengthInflated := FileSize[I];
  End; // For I
  FileDir[High(FileDir)].DataLengthInflated := High(FileNames) + 1;
  FileDir[High(FileDir)].CRC := $61580AC9; // Always the same

  // Find out where the directory will be and set the data positions in the directory entries

  J := SizeOf(Header);
  For I := 0 To High(BlockHeader) Do
  Begin
    FileDir[FileIndex1[I]].DataOffset := J;
    For K := 0 To High(BlockHeader[FileIndex1[I]]) Do
     Inc(J,SizeOf(TS3DDataBlockHeader) + BlockHeader[FileIndex1[I]][K].DeflatedLength);
  End; // For I

  // Set the directory position

  Header.DirectoryOffset := J;

  // Set the footer

  FFooter.SteveCookie5[0] := 'S';
  FFooter.SteveCookie5[1] := 'T';
  FFooter.SteveCookie5[2] := 'E';
  FFooter.SteveCookie5[3] := 'V';
  FFooter.SteveCookie5[4] := 'E';
  If Not FFooterRead Then FFooter.Date := 0;    // If this is only used to check the version, it isn't important

  // Write the data to the file

  Stream.WriteBuffer(Header,SizeOf(Header));
  For I := 0 To High(BlockHeader) Do
  Begin
    For J := 0 To High(BlockHeader[FileIndex1[I]]) Do
    Begin
      Stream.WriteBuffer(BlockHeader[FileIndex1[I]][J],SizeOf(TS3DDataBlockHeader));
      Stream.WriteBuffer(DestBuf[FileIndex1[I]][J]^,DestBufSize1[FileIndex1[I]][J]);
    End; // For J
  End; // For I

  // Now that we know the CRC's, sort in increasing CRC value.  Everything from here on out
  // will have to take the sort order into account

  For I := 0 To High(FileIndex) Do FileIndex[I] := I;
  QuickSortCRCs(0,High(FileDir));

  // Write the directory entries

  I := High(FileDir) + 1;
  Stream.WriteBuffer(I,4);
  For I := 0 To High(FileDir) Do Stream.WriteBuffer(FileDir[FileIndex[I]],SizeOf(TS3DDirectoryEntry)); // Write in sorted order
  If FFooterRead Or Not FFileRead Then Stream.WriteBuffer(FFooter,SizeOf(FFooter));

  // Cleanup

  SetLength(FileIndex,0);
  SetLength(FileIndex1,0);
  SetLength(FileDir,0);
  SetLength(FileNames,0);
  For I := 0 To High(BlockHeader) Do SetLength(BlockHeader[I],0);
  For I := 0 To High(DestBuf) Do
   For J := 0 To High(DestBuf[I]) Do FreeMem(DestBuf[I][J],DestBufSize[I][J]);
  For I := 0 To High(DestBuf) Do SetLength(DestBuf[I],0);
  For I := 0 To High(DestBufSize) Do SetLength(DestBufSize[I],0);
  For I := 0 To High(DestBufSize1) Do SetLength(DestBufSize1[I],0);
  SetLength(BlockHeader,0);
  SetLength(DestBuf,0);
  SetLength(DestBufSize,0);
  SetLength(DestBufSize1,0);
End; // TS3DFile.SaveToStream

end.
