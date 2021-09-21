unit frmDeleteAN8ObjectsUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ClearTypeText, IAeverButton, Anim8or, ComCtrls, Grids,
  GLVisir, ExtCtrls, ActnList, GLPanel, Buttons;

type
  TfrmDeleteAN8Objects = class(TGLForm)
    ClearTypeLabel1: TClearTypeLabel;
    ClearTypeLabel2: TClearTypeLabel;
    edtInputFile: TClearTypeEdit;
    edtOutputFile: TClearTypeEdit;
    btnBrowseInput: TIAEverButton;
    btnBrowseOutput: TIAEverButton;
    lbObjects: TClearTypeListBox;
    btnLoad: TIAEverButton;
    btnSave: TIAEverButton;
    dlgOpen: TOpenDialog;
    dlgSave: TSaveDialog;
    btnDeleteObject: TIAEverButton;
    pcMain: TPageControl;
    tsFile: TTabSheet;
    tsObject: TTabSheet;
    tsSequenceAdjust: TTabSheet;
    tsMaterials: TTabSheet;
    btnDeleteMaterial: TIAEverButton;
    sgGlobalMaterials: TClearTypeStringGrid;
    btnRenameGlobalMaterial: TIAEverButton;
    tsFigures: TTabSheet;
    sgFigures: TClearTypeStringGrid;
    btnDeleteNamedObject: TIAEverButton;
    ClearTypeLabel3: TClearTypeLabel;
    btnRenameObjectMaterial: TIAEverButton;
    sgObjectMaterials: TClearTypeStringGrid;
    tsTextures: TTabSheet;
    sgTextures: TClearTypeStringGrid;
    btnRenameTexture: TIAEverButton;
    btnDeleteTexture: TIAEverButton;
    btnFixupCyclic: TIAEverButton;
    Splitter1: TSplitter;
    glvMesh: TGLVisir;
    tsSequenceClone: TTabSheet;
    ClearTypeLabel4: TClearTypeLabel;
    lbSourceSequence: TClearTypeListBox;
    ClearTypeLabel5: TClearTypeLabel;
    lbDestinationSequence: TClearTypeListBox;
    btnCloneEndpointStance: TIAEverButton;
    ActionList1: TActionList;
    acShowSourceSequence: TAction;
    acShowDestinationSequence: TAction;
    btnCloneSequence: TIAEverButton;
    btnClearSequence: TIAEverButton;
    btnCopyWeights: TIAEverButton;
    btnMoveLegsApart: TIAEverButton;
    ClearTypeLabel6: TClearTypeLabel;
    edtMoveLegsApart: TClearTypeEdit;
    lbAdjustSequence: TClearTypeListBox;
    acShowAdjustSequence: TAction;
    lblSequenceFrames: TClearTypeLabel;
    imgFrames: TImage;
    btnMoveFrameUp: TIAEverButton;
    btnMoveFrameDown: TIAEverButton;
    acMoveFrameUp: TAction;
    acMoveFrameDown: TAction;
    ClearTypeLabel7: TClearTypeLabel;
    edtMoveUpDownAmount: TClearTypeEdit;
    udMoveUpDownAmount: TUpDown;
    Panel1: TPanel;
    Panel2: TPanel;
    Splitter2: TSplitter;
    Panel3: TPanel;
    ClearTypeLabel8: TClearTypeLabel;
    lbObjectMaterials: TClearTypeListBox;
    btnChangeMaterials: TIAEverButton;
    ClearTypeLabel9: TClearTypeLabel;
    lbAvailableMaterials: TClearTypeListBox;
    btnCopySequence: TIAEverButton;
    IAEverButton2: TIAEverButton;
    IAEverButton3: TIAEverButton;
    btnRemoveUnusedMaterials: TIAEverButton;
    btnMoveMaterialUp: TSpeedButton;
    btnMoveMaterialDown: TSpeedButton;
    btnScaleFigure: TIAEverButton;
    procedure btnBrowseInputClick(Sender: TObject);
    procedure btnBrowseOutputClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnLoadClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure btnSaveClick(Sender: TObject);
    procedure btnDeleteNamedObjectClick(Sender: TObject);
    procedure btnDeleteObjectClick(Sender: TObject);
    procedure btnDeleteMaterialClick(Sender: TObject);
    procedure btnRenameGlobalMaterialClick(Sender: TObject);
    procedure lbObjectsClick(Sender: TObject);
    procedure lbObjectsKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure btnRenameObjectMaterialClick(Sender: TObject);
    procedure btnDeleteTextureClick(Sender: TObject);
    procedure btnRenameTextureClick(Sender: TObject);
    procedure btnFixupCyclicClick(Sender: TObject);
    procedure acShowSourceSequenceExecute(Sender: TObject);
    procedure lbSourceSequenceClick(Sender: TObject);
    procedure lbSourceSequenceKeyPress(Sender: TObject; var Key: Char);
    procedure acShowDestinationSequenceExecute(Sender: TObject);
    procedure lbDestinationSequenceClick(Sender: TObject);
    procedure lbDestinationSequenceKeyPress(Sender: TObject;
      var Key: Char);
    procedure btnCloneEndpointStanceClick(Sender: TObject);
    procedure btnCloneSequenceClick(Sender: TObject);
    procedure btnClearSequenceClick(Sender: TObject);
    procedure btnCopyWeightsClick(Sender: TObject);
    procedure btnMoveLegsApartClick(Sender: TObject);
    procedure acShowAdjustSequenceExecute(Sender: TObject);
    procedure lbAdjustSequenceClick(Sender: TObject);
    procedure lbAdjustSequenceKeyPress(Sender: TObject; var Key: Char);
    procedure imgFramesMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure acMoveFrameUpExecute(Sender: TObject);
    procedure acMoveFrameDownExecute(Sender: TObject);
    procedure btnChangeMaterialsClick(Sender: TObject);
    procedure btnCopySequenceClick(Sender: TObject);
    procedure IAEverButton2Click(Sender: TObject);
    procedure IAEverButton3Click(Sender: TObject);
    procedure btnRemoveUnusedMaterialsClick(Sender: TObject);
    procedure btnMoveMaterialUpClick(Sender: TObject);
    procedure btnMoveMaterialDownClick(Sender: TObject);
    procedure btnScaleFigureClick(Sender: TObject);
  private
    { Private declarations }
    An8File           : TAn8File;
    LoadedFile        : String;
    LoadedRenderables : TStringList;
    HasAnimations     : Boolean;
    AdjustedSequence  : TAn8Sequence;
    SelectedFrame     : Integer;
    Procedure LoadObjectList;
    Procedure LoadTextureList;
    Procedure LoadMaterialList;
    Procedure LoadNamedObjectList;
    Procedure LoadObjectMaterials;
    Procedure LoadSequenceLists;
    Procedure LoadInfo;
    Procedure LoadObjectIntoView(Obj: TAn8Object);
    Procedure RenameMaterial(Material: TAn8Material);
    Function  LoadSequence(LB: TListBox; AnimSpeed: Single): TAn8Sequence;
    Procedure PaintFrameSelection;
  public
    { Public declarations }
  end;

var
  frmDeleteAN8Objects: TfrmDeleteAN8Objects;

implementation

Uses Math,ZoneClasses,URickGL,U3DPolys,Points3D,frmMainUnit;

Const BoxWidth = 16;

{$R *.dfm}

Procedure TfrmDeleteAN8Objects.PaintFrameSelection;
Var
  I,J    : Integer;
  Jack1X : TAn8JointAngle;
  Jack2X : TAn8JointAngle;
  Jack3X : TAn8JointAngle;
  Key1   : TAn8FloatKey;
  Key2   : TAn8FloatKey;
  Key3   : TAn8FloatKey;
  Enable : Boolean;
  Entity : TEntity;

Begin
  If AdjustedSequence <> Nil Then
  Begin
    // Set the frame selection image dimensions

    lblSequenceFrames.Caption := 'Frames: ' + IntToStr(AdjustedSequence.Frames);
    If imgFrames.Width < BoxWidth * AdjustedSequence.Frames + 1 Then
    Begin
      I := imgFrames.Height;
      imgFrames.Width  := BoxWidth * AdjustedSequence.Frames + 1;
      imgFrames.Height := I;
      imgFrames.Picture.Bitmap.Width  := 16 * AdjustedSequence.Frames + 1;
      imgFrames.Picture.Bitmap.Height := I;
    End;

    // Clear the image

    imgFrames.Canvas.Brush.Color := Color;
    imgFrames.Canvas.Pen.Color   := Color;
    imgFrames.Canvas.Brush.Style := bsSolid;
    imgFrames.Canvas.Rectangle(0,0,imgFrames.Width,imgFrames.Height);

    // Highlight the selected frame

    If (SelectedFrame >= 0) And (SelectedFrame < AdjustedSequence.Frames) Then
    Begin
      imgFrames.Canvas.Pen.Color   := clWhite;
      imgFrames.Canvas.Brush.Color := clWhite;
      imgFrames.Canvas.Rectangle(SelectedFrame * BoxWidth + 1,0,(SelectedFrame + 1) * BoxWidth,imgFrames.Height);
    End;

    // Draw the bottom and rightmost lines

    imgFrames.Canvas.Pen.Color := clBlack;
    imgFrames.Canvas.MoveTo(BoxWidth * AdjustedSequence.Frames,0);
    imgFrames.Canvas.LineTo(BoxWidth * AdjustedSequence.Frames,imgFrames.Height);
    imgFrames.Canvas.MoveTo(0,imgFrames.Height - 1);
    imgFrames.Canvas.LineTo(BoxWidth * AdjustedSequence.Frames + 1,imgFrames.Height - 1);

    // Draw the frame separators

    For I := 0 To AdjustedSequence.Frames - 1 Do
    Begin
      If (I Mod 5) = 0
       Then imgFrames.Canvas.MoveTo(I * BoxWidth,0)
       Else imgFrames.Canvas.MoveTo(I * BoxWidth,imgFrames.Height Div 2);
      imgFrames.Canvas.LineTo(I * BoxWidth,imgFrames.Height);
    End; // For I

    // Find the joint angles for jack1.x, jack2.x, and jack3.x

    Jack1X := AdjustedSequence.FindJointAngle('jack1',axX);
    Jack2X := AdjustedSequence.FindJointAngle('jack2',axX);
    Jack3X := AdjustedSequence.FindJointAngle('jack3',axX);
    If (Jack1X <> Nil) And (Jack2X <> Nil) And (Jack3X <> Nil) Then
    Begin
      // Draw dots where keys have been set for jack1, jack2, and jack3

      imgFrames.Canvas.Brush.Color := clBlack;
      J := imgFrames.Height Div 2;
      For I := 0 To AdjustedSequence.Frames - 1 Do
      Begin
        Key1 := Jack1X.Track.FindKeyForFrame(I);
        Key2 := Jack2X.Track.FindKeyForFrame(I);
        Key3 := Jack3X.Track.FindKeyForFrame(I);
        If (Key1 <> Nil) And (Key2 <> Nil) And (Key3 <> Nil) Then
        Begin
          imgFrames.Canvas.Ellipse(I * BoxWidth + (BoxWidth Div 2) - 4,J - 4,I * BoxWidth + (BoxWidth Div 2) + 4,J + 4);
        End;
      End; // For I
    End;

    // Repaint the image

    imgFrames.Repaint;

    // Enable/disable the move buttons depending on whether there is a key for the selected frame

    Enable := False;
    If (SelectedFrame >= 0) And (SelectedFrame < AdjustedSequence.Frames) And (Jack1X <> Nil) And (Jack2X <> Nil) And (Jack3X <> Nil) Then
    Begin
      Key1 := Jack1X.Track.FindKeyForFrame(SelectedFrame);
      Key2 := Jack2X.Track.FindKeyForFrame(SelectedFrame);
      Key3 := Jack3X.Track.FindKeyForFrame(SelectedFrame);
      If (Key1 <> Nil) And (Key2 <> Nil) And (Key3 <> Nil) Then Enable := True;
    End;
    acMoveFrameUp.Enabled   := Enable;
    acMoveFrameDown.Enabled := Enable;

    // Set the entity frame based on the selected frame

    If (lbAdjustSequence.Items.Count > 0) And (lbAdjustSequence.ItemIndex >= 0) Then
    Begin
      If HasAnimations Then I := 0 Else I := 1;
      If (LoadedRenderables.Count = An8File.Sequences.Count + I) And (glvMesh.Scene3D.Scene.Entities.Count > FirstEntity) Then
      Begin
        glvMesh.Scene3D.Scene.LockBSPTree('TfrmDeleteAN8Objects.PaintFrameSelection');
        Entity := TEntity(glvMesh.Scene3D.Scene.Entities.Items[FirstEntity]);
        If (Entity <> Nil) And (Entity.Renderable <> Nil) Then Entity.AnimFrame := TRenderable(Entity.Renderable).GetFramePos(SelectedFrame);
        glvMesh.Scene3D.Scene.UnlockBSPTree;
        While glvMesh.Scene3D.Scene.Redrawing Do Sleep(1);
        glvMesh.Scene3D.Redraw;
      End;
    End;
  End
  Else
  Begin
    // Clear the image

    imgFrames.Canvas.Brush.Color := Color;
    imgFrames.Canvas.Pen.Color   := Color;
    imgFrames.Canvas.Brush.Style := bsSolid;
    imgFrames.Canvas.Rectangle(0,0,imgFrames.Width,imgFrames.Height);
  End;
End; // TfrmDeleteAN8Objects.PaintFrameSelection

Function TfrmDeleteAN8Objects.LoadSequence(LB: TListBox; AnimSpeed: Single): TAn8Sequence;
Var
  Entity : TEntity;
  I      : Integer;

begin
  Result := Nil;
  If (LB.Items.Count > 0) And (LB.ItemIndex >= 0) Then
  Begin
    glvMesh.Caption := LB.Items.Strings[LB.ItemIndex];
    If HasAnimations Then I := 0 Else I := 1;
    If (LoadedRenderables.Count = An8File.Sequences.Count + I) And (glvMesh.Scene3D.Scene.Entities.Count > FirstEntity) Then
    Begin
      Result := TAn8Sequence(An8File.Sequences.Objects[LB.ItemIndex]);
      glvMesh.Scene3D.Scene.LockBSPTree('TfrmDeleteAN8Objects.LoadSequence');
      Entity                       := TEntity(glvMesh.Scene3D.Scene.Entities.Items[FirstEntity]);
      Entity.Renderable            := TRenderable(LoadedRenderables.Objects[LB.ItemIndex + I]);
      Entity.Gravity               := 0;
      Entity.NewlyVisibleInFrustum := True;
      Entity.AnimFrame             := 0;
      Entity.AnimSpeed             := AnimSpeed;
      Entity.Visible               := True;
      Entity.CanChangeTreeNodes    := True;
      Entity.Update;
      Entity.CollisionAvoidance    := True;
      glvMesh.Scene3D.Scene.UnlockBSPTree;
      glvMesh.Scene3D.Redraw;
    End;
  End;
End; // TfrmDeleteAN8Objects.LoadSequence

Procedure TfrmDeleteAN8Objects.LoadObjectIntoView(Obj: TAn8Object);
Var
  I,J,K          : Integer;
  Light          : TLight;
  Entity         : TEntity;
  Model          : TModel;
  Zone           : TZone;
  List           : TStringList;
  Mesh           : TMeshObject;
  Tree           : TTree;
  ZO             : TZoneObject;
  Sequence       : TAn8Sequence;
  SR             : TSkeletonRenderable;
  Frames         : TStringList;
  BoneMatrix     : T4x4Matrix;
  BoneQuaternion : TQuaternion;
  SeqName        : String;
  SeqPrefix      : String;
  DropLastFrame  : Boolean;
  MSPerFrame     : Integer;
  RootBone       : TAn8Bone;
  Bones          : TStringList;
  V1             : T3DPoint;
  V              : T3DPoint; // Anim8or class
  NO             : TAn8NamedObject;
  Bone           : TAn8Bone;
  Bone1          : TAn8Bone;
  Q              : TQuaternion;
  Q1             : TQuaternion;

  Procedure ProcessBone(FrameCount: Integer; FrameTime: Single; Frame: TSkeletonFrame; Bone: TAn8Bone; Sequence: TAn8Sequence; M: T4x4Matrix; Q: TQuaternion);
  Var
    I,J,K       : Integer;
    Q1,Q0,Q2    : TQuaternion;
    M1          : T3x3Matrix;
    MA1         : T4x4Matrix;
    Angle       : Single;
    AX,AY,AZ    : Single;
    Angle0      : Single;
    AX0,AY0,AZ0 : Single;
    ParentIndex : Integer;
    BX,BY,BZ    : Single;

      JointAngleX : TAn8JointAngle;
      JointAngleY : TAn8JointAngle;
      JointAngleZ : TAn8JointAngle;
      L           : Integer;
      V           : T3DPoint;

  Begin
    If Bone <> Nil Then
    Begin
      Q1  := TQuaternion.Create(Bone.Orientation);
      Q1.Normalize; // Important!!


      If Bone.Parent <> Nil Then
      Begin
        BX := 0;
        BY := 0;
        BZ := Bone.Parent.Len;
        If Bone.Index = 1 Then BZ := 0;
      End
      Else
      Begin
        BX := 0;
        BY := 0;
        BZ := 0;
      End;








        V := T3DPoint.Create;

        // Find the joint angles

        L := Sequence.JointAngles.IndexOf(Bone.Name + '_X');
        If L >= 0
         Then JointAngleX := TAn8JointAngle(Sequence.JointAngles.Objects[L])
         Else JointAngleX := Nil;

        L := Sequence.JointAngles.IndexOf(Bone.Name + '_Y');
        If L >= 0
         Then JointAngleY := TAn8JointAngle(Sequence.JointAngles.Objects[L])
         Else JointAngleY := Nil;

        L := Sequence.JointAngles.IndexOf(Bone.Name + '_Z');
        If L >= 0
         Then JointAngleZ := TAn8JointAngle(Sequence.JointAngles.Objects[L])
         Else JointAngleZ := Nil;

        Q0 := TQuaternion.Create(Bone.Orientation);

        If JointAngleX <> Nil Then
        Begin
          V.Copy(1,0,0);
          Q0.Transform(V);
          Q1.FromAngleAxis(JointAngleX.Track.GetValueForFrame(FrameCount) * Pi / 180,V);
          Q1.Multiply(Q0);
          Q0.Copy(Q1);
        End;

        If JointAngleZ <> Nil Then
        Begin
          V.Copy(0,0,1);
          Q0.Transform(V);
          Q1.FromAngleAxis(JointAngleZ.Track.GetValueForFrame(FrameCount) * Pi / 180,V);
          Q1.Multiply(Q0);
          Q0.Copy(Q1);
        End;

        If JointAngleY <> Nil Then
        Begin
          V.Copy(0,1,0);
          Q0.Transform(V);
          Q1.FromAngleAxis(JointAngleY.Track.GetValueForFrame(FrameCount) * Pi / 180,V);
          Q1.Multiply(Q0);
          Q0.Copy(Q1);
        End;

        Q1.Copy(Q0.W,Q0.Z,Q0.X,Q0.Y);
        Q1.Normalize;
        Q0.Free;
        V.Free;











//      Q1.Transform(BX,BY,BZ);

      M1  := Q1.ToRotationMatrix;
      MA1 := T4x4Matrix.Create;
      MA1.LoadIdentity;
      MA1.LoadRotationMatrix(M1);
      M1.Free;
      MA1.M[1,4] := BX;
      MA1.M[2,4] := BY;
      MA1.M[3,4] := BZ;

      MA1.Multiply(M);



      Q0 := TQuaternion.Create(Q);
      Q0.Normalize; // Important!!
      Q0.Multiply(Q1);
      Q2 := TQuaternion.Create(Q0);
//      Q1.Copy(Q0);
      Q0.Free;

      Q2.ToAngleAxis(Angle,AX,AY,AZ);      // Cumulative
      Q1.ToAngleAxis(Angle0,AX0,AY0,AZ0);  // Relative to last bone


      Angle  := Angle  * 180 / Pi; // Convert to degrees because OpenGL uses degrees
      Angle0 := Angle0 * 180 / Pi; // Convert to degrees because OpenGL uses degrees

      If Bone.Parent <> Nil
       Then ParentIndex := Bone.Parent.Index
       Else ParentIndex := -1;






      Frame.SetPiece(Bone.Index,Bone.Name,
                     MA1.M[1,4],MA1.M[2,4],MA1.M[3,4],
                     MA1.M[1,1],MA1.M[1,2],MA1.M[1,3],
                     MA1.M[2,1],MA1.M[2,2],MA1.M[2,3],
                     MA1.M[3,1],MA1.M[3,2],MA1.M[3,3],
                     Q2.W,Q2.X,Q2.Y,Q2.Z,
                     Q1.W,Q1.X,Q1.Y,Q1.Z,
                     Angle,AX,AY,AZ,
                     MA1.M[1,4],MA1.M[2,4],MA1.M[3,4],
                     Angle0,AX0,AY0,AZ0,
                     BX,
                     BY,
                     BZ,
                     ParentIndex);

      For J := 0 To Bone.Children.Count - 1 Do
      Begin
        ProcessBone(FrameCount,FrameTime,Frame,TAn8Bone(Bone.Children.Objects[J]),Sequence,MA1,Q2);
      End; // For J
      Q1.Free;
      Q2.Free;
      MA1.Free;
    End;
  End; // ProcessBone

Begin
  glvMesh.Scene3D.Scene.LockBSPTree('TfrmDeleteAN8Objects.LoadObjectIntoView');

  glvMesh.Scene3D.Scene.bTranslucentMode := True;

  // Get rid of all entities

  glvMesh.Scene3D.Scene.ClearScene(True,True);
  glvMesh.Clear;

  // Load the object into the scene

  Zone := TZone.Create;
  List := TStringList.Create;
  List.AddObject('',Obj);
  ZO := Zone.ImportAn8Objects('an8',An8File,List,True,False);
  List.Clear;
  HasAnimations := False;
  If ZO <> Nil Then
  Begin
    ZO.AddToPolygonList(List,False,True,True,True);
    Mesh := Zone.CoalescePolygonList(List);
    For I := 0 To List.Count - 1 Do List.Objects[I].Free;

    // Subtract the bone origin from all points

    V := T3DPoint.Create;
    If An8File.Sequences.Count > 0 Then
    Begin
      Sequence := TAn8Sequence(An8File.Sequences.Objects[0]);
      If (Sequence <> Nil) And (Sequence.Figure <> Nil) And (Sequence.Figure.RootBone <> Nil) Then
      Begin
        HasAnimations := True;
        NO := TAn8NamedObject(Sequence.Figure.RootBone.FindNamedObjects.Objects[0]);

        Bones := TStringList.Create;
        Sequence.Figure.RootBone.GetBones(Bones);
        Q  := TQuaternion.Create;
        Q1 := TQuaternion.Create;
        For I := 0 To Mesh.Vertices.Count - 1 Do
        Begin
          V1 := T3DPoint(Mesh.Vertices.Objects[I]);
          If (High(Mesh.BoneIndices) >= I) And (Mesh.BoneIndices[I] >= 0) And (Mesh.BoneIndices[I] < Bones.Count) Then
          Begin
            Bone := TAn8Bone(Bones.Objects[Mesh.BoneIndices[I]]);
            V.Copy(V1.Y,V1.Z,V1.X);
            V.Subtract(Bone.Origin);

            If NO <> Nil Then V.Add(NO.Base);
            Q.Copy(Bone.Orientation);
            Bone1 := Bone.Parent;
            While Bone1 <> Nil Do
            Begin
              Q1.Copy(Bone1.Orientation);
              Q1.Multiply(Q);
              Q.Copy(Q1);
              Bone1 := Bone1.Parent;
            End; // While
            Q.Invert;
            Q.Transform(V);

            V1.Copy(V.Z,V.X,V.Y);
          End;
        End; // For I
        Q.Free;
        Q1.Free;
        Bones.Free;
      End;
    End;
    V.Free;

    List.Clear;
    List.AddObject('',Mesh);
    Tree := Nil;
    frmMain.ReloadMesh(List,0,0,False,False,False,glvMesh,Tree,False,ExtractFilePath(LoadedFile));
    Tree.Free;
    Mesh.Free;
    ZO.Free;
  End;
  List.Free;
  Zone.Free;

  // Make a list of all renderables so we can switch between them

  LoadedRenderables.Clear;
  AdjustedSequence := Nil;
  SelectedFrame := -1;
  Model := Nil;
  If glvMesh.Scene3D.Scene.Entities.Count > FirstEntity Then
  Begin
    Entity := TEntity(glvMesh.Scene3D.Scene.Entities.Items[FirstEntity]);
    Entity.Name := 'Loaded An8 object';
    If Entity.Renderable <> Nil Then
    Begin
      If Not HasAnimations Then LoadedRenderables.AddObject('',Entity.Renderable);
      If TRenderable(Entity.Renderable).Count > 0 Then Model := TRenderable(Entity.Renderable).Models[0];
    End;
  End;

  // Load the other renderables (animations)

  For I := 0 To An8File.Sequences.Count - 1 Do
  Begin
    Sequence := TAn8Sequence(An8File.Sequences.Objects[I]);
    If (Sequence <> Nil) And (Sequence.Figure <> Nil) And TokenizeAn8SequenceName(Sequence.Name,SeqName,SeqPrefix,DropLastFrame,MSPerFrame) Then
    Begin
      If Sequence.Figure.ReferencesObject(Obj) Then
      Begin
        SR := TSkeletonRenderable(glvMesh.Scene3D.Scene.Renderables.GetNew(1));
        If Model <> Nil Then SR.AddModel('body0',Model);
        Frames := TStringList.Create;
        For J := 0 To Sequence.Frames - 1 Do
        Begin
          RootBone := Sequence.Figure.RootBone;
          //RootBone := Sequence.GetRootBone(0);//J / Sequence.Frames);
          K := 0;
          RootBone.AssignIndex(K);
          Frames.AddObject('',TSkeletonFrame.Create(SR));
          SR.AddFrame(TSkeletonFrame(Frames.Objects[Frames.Count - 1]));
          BoneMatrix := T4x4Matrix.Create;
          BoneMatrix.LoadIdentity;
          BoneQuaternion := TQuaternion.Create;

          ProcessBone(J,J / Sequence.Frames,TSkeletonFrame(Frames.Objects[Frames.Count - 1]),RootBone,Sequence,BoneMatrix,BoneQuaternion);

          BoneQuaternion.Free;
          BoneMatrix.Free;
          //RootBone.Free;
        End; // For J
        If Sequence.Frames > 1 Then SR.AnimSpeed := 1 / (Sequence.Frames * MSPerFrame / 1000);
        //For J := 0 To Frames.Count - 1 Do SR.AddFrame(TSkeletonFrame(Frames.Objects[J]));
        Frames.Free;
        LoadedRenderables.AddObject('',SR);
        If I = 0 Then Entity.Renderable := SR;
      End;
    End;
  End; // For I

  // Set the view parameters

  glvMesh.Scene3D.Scene.DefaultCamera.LookAt(-100,0,0);
  glvMesh.Scene3D.Scene.DefaultCamera.SetVectorUp(0,0,1);
  glvMesh.Scene3D.Scene.DefaultCamera.SetPosition(8,0,0);

  // Set up the scene's light

  If glvMesh.Scene3D.Scene.Lights.Count > 0 Then
  Begin
    Light := TLight(glvMesh.Scene3D.Scene.Lights.Items[0]);
    Light.Position.Copy(0,0,100000);
    Light.SetOrientation(1,1,0);
    Light.Attenuation := 0;//.001;
  End;

  // The frustum culling and occlusion culling code don't play well with LocalScale

  glvMesh.Scene3D.Scene.FrustumCulling := False;
  glvMesh.Scene3D.Scene.OcclusionManager.Enabled := False;
  glvMesh.Fit(True,True,True);
  For I := 0 To glvMesh.Scene3D.Scene.Entities.Count - 1 Do
  Begin
    Entity := TEntity(glvMesh.Scene3D.Scene.Entities.Items[I]);
    Entity.BSPTreeNode.CheckEntity(Entity);
  End; // For I

  glvMesh.Scene3D.Scene.DefaultCamera.Position.Dirty := True;

  glvMesh.Scene3D.Scene.UnlockBSPTree;

  glvMesh.Scene3D.Redraw;
End; // TfrmDeleteAN8Objects.LoadObjectIntoView

Procedure TfrmDeleteAN8Objects.LoadObjectList;
Var I: Integer;
Begin
  lbObjects.Clear;
  For I := 0 To An8File.Objects.Count - 1 Do lbObjects.Items.Add(An8File.Objects.Strings[I]);
End; // TfrmDeleteAN8Objects.LoadObjectList

Procedure TfrmDeleteAN8Objects.LoadTextureList;
Var
  I,J,K        : Integer;
  Count        : Integer;
  Texture      : TAn8Texture;
  Obj          : TAn8Object;
  Figure       : TAn8Figure;
  NamedObjects : TStringList;
  NamedObject  : TAn8NamedObject;
  Material     : TAn8Material;

  Procedure ProcessComponent(Texture: TAn8Texture; C: TAn8Component; Var Count: Integer);
  Var
    I        : Integer;
    Mesh     : TAn8Mesh;
    Group    : TAn8Group;
    Material : TAn8Material;

  Begin
    If C <> Nil Then
    Begin
      If C Is TAn8Mesh Then
      Begin
        Mesh := TAn8Mesh(C);
        For I := 0 To Mesh.MaterialList.Count - 1 Do
        Begin
          Material := TAn8Material(Mesh.MaterialList.Objects[I]);
          If (Material <> Nil) And Material.ReferencesTexture(Texture) Then Inc(Count);
        End; // For I
      End
      Else If C Is TAn8Group Then
      Begin
        Group := TAn8Group(C);
        For I := 0 To Group.Components.Count - 1 Do ProcessComponent(Texture,TAn8Component(Group.Components.Objects[I]),Count);
      End;
    End;
  End; // ProcessComponent

Begin
  sgTextures.RowCount := Max(1,An8File.Textures.Count + 1);
  sgTextures.Cells[0,0] := 'Texture name';
  sgTextures.Cells[1,0] := 'File name';
  sgTextures.Cells[2,0] := 'References';
  For I := 0 To An8File.Textures.Count - 1 Do
  Begin
    Texture := TAn8Texture(An8File.Textures.Objects[I]);
    sgTextures.Cells[0,I + 1] := Texture.Name;
    sgTextures.Cells[1,I + 1] := ExtractFileName(Texture.FileName);

    // Count the meshes that reference the texture

    Count := 0;
    For J := 0 To An8File.Objects.Count - 1 Do
    Begin
      Obj := TAn8Object(An8File.Objects.Objects[J]);
      For K := 0 To Obj.Components.Count - 1 Do ProcessComponent(Texture,TAn8Component(Obj.Components.Objects[K]),Count);
      For K := 0 To Obj.Materials.Count - 1 Do
      Begin
        Material := TAn8Material(Obj.Materials.Objects[K]);
        If (Material <> Nil) And Material.ReferencesTexture(Texture) Then Inc(Count);
      End; // For K
    End; // For J

    // Count the figures that reference the texture

    For J := 0 To An8File.Figures.Count - 1 Do
    Begin
      Figure := TAn8Figure(An8File.Figures.Objects[J]);
      For K := 0 To Figure.Materials.Count - 1 Do
      Begin
        If Figure.Materials.Objects[K] <> Nil Then
        Begin
          Material := TAn8Material(Figure.Materials.Objects[K]);
          If (Material <> Nil) And Material.ReferencesTexture(Texture) Then Inc(Count);
        End;
      End; // For K
    End; // For J

    // Count the named objects that reference the material

    For J := 0 To An8File.Figures.Count - 1 Do
    Begin
      Figure := TAn8Figure(An8File.Figures.Objects[J]);
      If Figure.RootBone <> Nil Then
      Begin
        NamedObjects := Figure.RootBone.FindNamedObjects;
        If NamedObjects <> Nil Then
        Begin
          For K := 0 To NamedObjects.Count - 1 Do
          Begin
            NamedObject := TAn8NamedObject(NamedObjects.Objects[K]);
            If (NamedObject.Material <> Nil) And NamedObject.Material.ReferencesTexture(Texture) Then Inc(Count);
          End; // For K
        End;
      End;
    End; // For J

    // Count the global materials that reference the texture

    For J := 0 To An8File.Materials.Count - 1 Do
    Begin
      Material := TAn8Material(An8File.Materials.Objects[J]);
      If (Material <> Nil) And Material.ReferencesTexture(Texture) Then Inc(Count);
    End; // For J

    // Display the number of references to the material

    sgTextures.Cells[2,I + 1] := IntToStr(Count);
  End; // For I
End; // TfrmDeleteAN8Objects.LoadTextureList

Procedure TfrmDeleteAN8Objects.LoadMaterialList;
Var
  I,J,K        : Integer;
  Count        : Integer;
  Material     : TAn8Material;
  Obj          : TAn8Object;
  Figure       : TAn8Figure;
  NamedObjects : TStringList;
  NamedObject  : TAn8NamedObject;

  Procedure ProcessComponent(Material: TAn8Material; C: TAn8Component; Var Count: Integer);
  Var
    I     : Integer;
    Mesh  : TAn8Mesh;
    Group : TAn8Group;

  Begin
    If C <> Nil Then
    Begin
      If C Is TAn8Mesh Then
      Begin
        Mesh := TAn8Mesh(C);
        If Mesh.MaterialList.IndexOfObject(Material) >= 0 Then Inc(Count);
      End
      Else If C Is TAn8Group Then
      Begin
        Group := TAn8Group(C);
        For I := 0 To Group.Components.Count - 1 Do ProcessComponent(Material,TAn8Component(Group.Components.Objects[I]),Count);
      End;
    End;
  End; // ProcessComponent

Begin
  sgGlobalMaterials.RowCount := Max(1,An8File.Materials.Count + 1);
  sgGlobalMaterials.Cells[0,0] := 'Material name';
  sgGlobalMaterials.Cells[1,0] := 'Diffuse texture';
  sgGlobalMaterials.Cells[2,0] := 'References';
  For I := 0 To An8File.Materials.Count - 1 Do
  Begin
    Material := TAn8Material(An8File.Materials.Objects[I]);
    sgGlobalMaterials.Cells[0,I + 1] := Material.Name;
    If (Material.Surface <> Nil) And (Material.Surface.Diffuse <> Nil) And (Material.Surface.Diffuse.Texture <> Nil)
     Then sgGlobalMaterials.Cells[1,I + 1] := ExtractFileName(Material.Surface.Diffuse.Texture.FileName)
     Else sgGlobalMaterials.Cells[1,I + 1] := '(none)';

    // Count the meshes that reference the material

    Count := 0;
    For J := 0 To An8File.Objects.Count - 1 Do
    Begin
      Obj := TAn8Object(An8File.Objects.Objects[J]);
      For K := 0 To Obj.Components.Count - 1 Do ProcessComponent(Material,TAn8Component(Obj.Components.Objects[K]),Count);
    End; // For J

    // Count the figures that reference the material

    For J := 0 To An8File.Figures.Count - 1 Do
    Begin
      Figure := TAn8Figure(An8File.Figures.Objects[J]);
      For K := 0 To Figure.Materials.Count - 1 Do
      Begin
        If Figure.Materials.Objects[K] = Material Then Inc(Count);
      End; // For K
    End; // For J

    // Count the named objects that reference the material

    For J := 0 To An8File.Figures.Count - 1 Do
    Begin
      Figure := TAn8Figure(An8File.Figures.Objects[J]);
      If Figure.RootBone <> Nil Then
      Begin
        NamedObjects := Figure.RootBone.FindNamedObjects;
        If NamedObjects <> Nil Then
        Begin
          For K := 0 To NamedObjects.Count - 1 Do
          Begin
            NamedObject := TAn8NamedObject(NamedObjects.Objects[K]);
            If NamedObject.Material = Material Then Inc(Count);
          End; // For K
        End;
      End;
    End; // For J

    // Display the number of references to the material

    sgGlobalMaterials.Cells[2,I + 1] := IntToStr(Count);
  End; // For I
End; // TfrmDeleteAN8Objects.LoadMaterialList

Procedure TfrmDeleteAN8Objects.LoadNamedObjectList;
Var
  I,J,K        : Integer;
  Figure       : TAn8Figure;
  NamedObjects : TStringList;
  NamedObject  : TAn8NamedObject;

Begin
  sgFigures.Cells[0,0] := 'Figure';
  sgFigures.Cells[1,0] := 'Named object';
  sgFigures.Cells[2,0] := 'Referenced object';
  sgFigures.Cells[3,0] := 'Object is present';
  J := 0;
  For I := 0 To An8File.Figures.Count - 1 Do
  Begin
    Figure := TAn8Figure(An8File.Figures.Objects[I]);
    If Figure.RootBone <> Nil Then
    Begin
      NamedObjects := Figure.RootBone.FindNamedObjects;
      If NamedObjects <> Nil Then Inc(J,NamedObjects.Count);
    End;
  End; // For I
  sgFigures.RowCount := Max(1,J + 1);
  J := 1;
  For I := 0 To An8File.Figures.Count - 1 Do
  Begin
    Figure := TAn8Figure(An8File.Figures.Objects[I]);
    If Figure.RootBone <> Nil Then
    Begin
      NamedObjects := Figure.RootBone.FindNamedObjects;
      If NamedObjects <> Nil Then
      Begin
        For K := 0 To NamedObjects.Count - 1 Do
        Begin
          NamedObject := TAn8NamedObject(NamedObjects.Objects[K]);
          sgFigures.Cells[0,J] := Figure.Name;
          sgFigures.Cells[1,J] := NamedObject.Name;
          sgFigures.Cells[2,J] := NamedObject.ObjName;
          If NamedObject.Obj = Nil
           Then sgFigures.Cells[3,J] := 'No'
           Else sgFigures.Cells[3,J] := 'Yes';
          Inc(J);
        End; // For K
      End;
    End;
  End; // For I
End; // TfrmDeleteAN8Objects.LoadNamedObjectList

Procedure TfrmDeleteAN8Objects.LoadObjectMaterials;
Var
  Obj      : TAn8Object;
  I        : Integer;
  Material : TAn8Material;

  Procedure ProcessComponent(C: TAn8Component);
  Var
    I        : Integer;
    Mesh     : TAn8Mesh;
    Group    : TAn8Group;
    Material : TAn8Material;

  Begin
    If C <> Nil Then
    Begin
      If C Is TAn8Mesh Then
      Begin
        Mesh := TAn8Mesh(C);
        For I := 0 To Mesh.MaterialList.Count - 1 Do
        Begin
          Material := TAn8Material(Mesh.MaterialList.Objects[I]);
          If Material <> Nil Then
          Begin
            If lbObjectMaterials.Items.IndexOf(Material.Name) < 0 Then lbObjectMaterials.Items.Add(Material.Name);
          End;
        End;
      End
      Else If C Is TAn8Group Then
      Begin
        Group := TAn8Group(C);
        For I := 0 To Group.Components.Count - 1 Do ProcessComponent(TAn8Component(Group.Components.Objects[I]));
      End;
    End;
  End; // ProcessComponent

Begin
  // Populate the string grid with the materials contained in the selected object

  I   := 0;
  Obj := Nil;
  If (lbObjects.ItemIndex >= 0) And (lbObjects.ItemIndex < lbObjects.Items.Count) Then
  Begin
    Obj := TAn8Object(An8File.Objects.Objects[lbObjects.ItemIndex]);
    If Obj <> Nil Then I := Obj.Materials.Count;
  End;
  sgObjectMaterials.RowCount := Max(1,I + 1);
  sgObjectMaterials.Cells[0,0] := 'Material name';
  sgObjectMaterials.Cells[1,0] := 'Diffuse texture';
  If Obj <> Nil Then
  Begin
    For I := 0 To Obj.Materials.Count - 1 Do
    Begin
      Material := TAn8Material(Obj.Materials.Objects[I]);
      sgObjectMaterials.Cells[0,I + 1] := Material.Name;
      If (Material.Surface <> Nil) And (Material.Surface.Diffuse <> Nil) And (Material.Surface.Diffuse.Texture <> Nil)
       Then sgObjectMaterials.Cells[1,I + 1] := ExtractFileName(Material.Surface.Diffuse.Texture.FileName)
       Else sgObjectMaterials.Cells[1,I + 1] := '(none)';
    End; // For I
    LoadObjectIntoView(Obj);
    glvMesh.Caption := Obj.Name;
  End;

  // Populate the list box with the materials that the object uses (some might be global)

  lbObjectMaterials.Clear;
  If Obj <> Nil Then
  Begin
    For I := 0 To Obj.Components.Count - 1 Do ProcessComponent(TAn8Component(Obj.Components.Objects[I]));
  End;

  // Populate the list box with all global materials and all materials contained in this object

  lbAvailableMaterials.Clear;
  For I := 0 To An8File.Materials.Count - 1 Do lbAvailableMaterials.Items.Add(TAn8Material(An8File.Materials.Objects[I]).Name);
  If Obj <> Nil Then
  Begin
    For I := 0 To Obj.Materials.Count - 1 Do lbAvailableMaterials.Items.Add(TAn8Material(Obj.Materials.Objects[I]).Name);
  End;
End; // TfrmDeleteAN8Objects.LoadObjectMaterials

Procedure TfrmDeleteAN8Objects.LoadSequenceLists;
Var
  I             : Integer;
  Sequence      : TAn8Sequence;
  SeqName       : String;
  SeqPrefix     : String;
  DropLastFrame : Boolean;
  MSPerFrame    : Integer;

Begin
  lbAdjustSequence.Clear;
  lbSourceSequence.Clear;
  lbDestinationSequence.Clear;
  For I := 0 To An8File.Sequences.Count - 1 Do
  Begin
    Sequence := TAn8Sequence(An8File.Sequences.Objects[I]);
    If TokenizeAn8SequenceName(Sequence.Name,SeqName,SeqPrefix,DropLastFrame,MSPerFrame) Then
    Begin
      lbAdjustSequence.Items.Add(Sequence.Name);
      lbSourceSequence.Items.Add(Sequence.Name);
      lbDestinationSequence.Items.Add(Sequence.Name);
    End;
  End; // For I
End; // TfrmDeleteAN8Objects.LoadSequenceLists

Procedure TfrmDeleteAN8Objects.LoadInfo;
Begin
  LoadObjectList;
  LoadTextureList;
  LoadMaterialList;
  LoadNamedObjectList;
  LoadObjectMaterials;
  LoadSequenceLists;
End; // TfrmDeleteAN8Objects.LoadInfo

Procedure TfrmDeleteAN8Objects.RenameMaterial(Material: TAn8Material);
Const BufSize = 65000;
Var
  St      : String;
  OldFile : File;
  NewFile : File;
  Buffer  : Pointer;
  Loaded  : Integer;

Begin
  If (Material.Surface <> Nil) And
     (Material.Surface.Diffuse <> Nil) And
     (Material.Surface.Diffuse.Texture <> Nil) And
     FileExists(Material.Surface.Diffuse.Texture.FileName) Then
  Begin
    St := Material.Name;
    If InputQuery('Rename material','Enter new material name:',St) Then
    Begin
      Material.Name := St;
      If St <> ExtractFileNameNoExt(Material.Surface.Diffuse.Texture.FileName) Then
      Begin
        GetMem(Buffer,BufSize);
        AssignFile(OldFile,Material.Surface.Diffuse.Texture.FileName);
        Reset(OldFile,1);
        AssignFile(NewFile,ExtractFilePath(Material.Surface.Diffuse.Texture.FileName) + St + '.bmp');
        ReWrite(NewFile,1);
        While Not Eof(OldFile) Do
        Begin
          If FileSize(OldFile) - FilePos(OldFile) > BufSize
           Then Loaded := BufSize
           Else Loaded := FileSize(OldFile) - FilePos(OldFile);
          BlockRead(OldFile,Buffer^,Loaded);
          BlockWrite(NewFile,Buffer^,Loaded);
          Material.Surface.Diffuse.Texture.FileName := ExtractFilePath(Material.Surface.Diffuse.Texture.FileName) + St + '.bmp';
        End; // While
        FreeMem(Buffer);
        CloseFile(NewFile);
        CloseFile(OldFile);
      End;
    End;
  End;
End; // TfrmDeleteAN8Objects.RenameMaterial

procedure TfrmDeleteAN8Objects.btnBrowseInputClick(Sender: TObject);
begin
  dlgOpen.InitialDir := ExtractFilePath(Application.ExeName) + 'LIBRARY\CREATURES';
  If dlgOpen.Execute Then edtInputFile.Text := dlgOpen.FileName;
end;

procedure TfrmDeleteAN8Objects.btnBrowseOutputClick(Sender: TObject);
begin
  dlgSave.InitialDir := ExtractFilePath(Application.ExeName) + 'LIBRARY\CREATURES';
  If dlgSave.Execute Then edtOutputFile.Text := dlgSave.FileName;
end;

procedure TfrmDeleteAN8Objects.FormCreate(Sender: TObject);
begin
  glvMesh.Scene3D.Scene.DistFar := 10000;
  An8File := Nil;
  AdjustedSequence := Nil;
  SelectedFrame := -1;
  LoadedRenderables := TStringList.Create;
end;

procedure TfrmDeleteAN8Objects.FormDestroy(Sender: TObject);
begin
  If An8File <> Nil Then An8File.Free;
  LoadedRenderables.Free;
end;

procedure TfrmDeleteAN8Objects.btnLoadClick(Sender: TObject);
Var
  I,J : Integer;
  Obj : TAn8Object;

begin
  If (edtInputFile.Text <> '') And FileExists(edtInputFile.Text) Then
  Begin
    LoadedFile := edtInputFile.Text;
    If An8File <> Nil Then An8File.Free;
    An8File := TAn8File.Create;
    An8File.LoadFromFile(edtInputFile.Text);
    For I := 0 To An8File.Objects.Count - 1 Do
    Begin
      Obj := TAn8Object(An8File.Objects.Objects[I]);
      For J := 0 To Obj.Components.Count - 1 Do TAn8Component(Obj.Components.Objects[J]).ConvertToTriangles;
      Obj.MergeVeryClosePoints;
    End; // For I
    For I := 0 To An8File.Figures.Count - 1 Do TAn8Figure(An8File.Figures.Objects[I]).DeterminePrimaryBones;
    LoadInfo;
    ShowMessage('File ' + ExtractFileName(edtInputFile.Text) + ' loaded.');
  End;
end;

procedure TfrmDeleteAN8Objects.FormShow(Sender: TObject);
begin
  pcMain.ActivePage := tsFile;
  glvMesh.Scene3D.Scene.LockBSPTree('TfrmDeleteAN8Objects.FormShow');
  glvMesh.Scene3D.Scene.ClearScene(True,True);
  glvMesh.Update;//.Scene3D.Update(glvMesh.Width,glvMesh.Height);
  glvMesh.Scene3D.Scene.UnlockBSPTree;

  While Not glvMesh.Initialized Do
  Begin
    Application.ProcessMessages;
    Sleep(1);
  End; // While
end;

procedure TfrmDeleteAN8Objects.btnSaveClick(Sender: TObject);
Var
  St       : String;
  St1      : PChar;
  Continue : Boolean;

begin
  If edtOutputFile.Text <> '' Then
  Begin
    If An8File <> Nil Then
    Begin
      If FileExists(edtOutputFile.Text) Then
      Begin
        St  := 'Overwrite file ' + edtOutputFile.Text + '?';
        St1 := StrAlloc(Length(St) + 1);
        StrPCopy(St1,St);
        Continue := (Application.MessageBox(St1,'File Exists',MB_OKCANCEL) = IDOK);
        StrDispose(St1);
      End
      Else Continue := True;
      If Continue Then
      Begin
        An8File.SaveToFile(edtOutputFile.Text);
        ShowMessage('File ' + ExtractFileName(edtOutputFile.Text) + ' saved.');
      End;
    End
    Else ShowMessage('You cannot save an Anim8or file. You have not loaded one.');
  End
  Else ShowMessage('You must specify an output file name.');
end;

procedure TfrmDeleteAN8Objects.btnDeleteNamedObjectClick(Sender: TObject);
Var
  I,J,K        : Integer;
  Figure       : TAn8Figure;
  NamedObjects : TStringList;
  NamedObject  : TAn8NamedObject;
  Done         : Boolean;

begin
  If (sgFigures.Row > 0) And (sgFigures.Row < sgFigures.RowCount) Then
  Begin
    // Only delete a named object if it references an object that isn't there

    If sgFigures.Cells[3,sgFigures.Row] = 'No' Then
    Begin
      J    := 0;
      I    := 0;
      Done := False;
      While (I < An8File.Figures.Count) And Not Done Do
      Begin
        Figure := TAn8Figure(An8File.Figures.Objects[I]);
        If Figure.RootBone <> Nil Then
        Begin
          NamedObjects := Figure.RootBone.FindNamedObjects;
          If NamedObjects <> Nil Then
          Begin
            K := 0;
            While (K < NamedObjects.Count) And Not Done Do
            Begin
              If J = sgFigures.Row - 1 Then
              Begin
                NamedObject := TAn8NamedObject(NamedObjects.Objects[K]);
                If NamedObject.Obj = Nil Then
                Begin
                  NamedObject.Free;
                  NamedObjects.Delete(K);
                  Done := True;
                  LoadInfo;
                End
                Else ShowMessage('Error: Named object not an orphan');
              End;
              Inc(J);
              Inc(K);
            End; // While
          End;
        End;
        Inc(I);
      End; // While
    End
    Else ShowMessage('You can only delete an orphaned named object.');
  End;
end;

procedure TfrmDeleteAN8Objects.btnDeleteObjectClick(Sender: TObject);
Var
  Obj          : TAn8Object;
  I,J          : Integer;
  Figure       : TAn8Figure;
  NamedObjects : TStringList;
  NamedObject  : TAn8NamedObject;

begin
  If (lbObjects.ItemIndex >= 0) And (lbObjects.ItemIndex < lbObjects.Items.Count) Then
  Begin
    Obj := TAn8Object(An8File.Objects.Objects[lbObjects.ItemIndex]);
    If Obj <> Nil Then
    Begin
      // Disconnect any named objects that reference this object

      For I := 0 To An8File.Figures.Count - 1 Do
      Begin
        Figure := TAn8Figure(An8File.Figures.Objects[I]);
        If Figure.RootBone <> Nil Then
        Begin
          NamedObjects := Figure.RootBone.FindNamedObjects;
          If NamedObjects <> Nil Then
          Begin
            For J := 0 To NamedObjects.Count - 1 Do
            Begin
              NamedObject := TAn8NamedObject(NamedObjects.Objects[J]);
              If NamedObject.Obj = Obj Then NamedObject.Obj := Nil;
            End; // For J
          End;
        End;
      End; // For I
      Obj.Free;
      An8File.Objects.Delete(lbObjects.ItemIndex);
      LoadInfo;
    End;
  End;
end;

procedure TfrmDeleteAN8Objects.btnDeleteMaterialClick(Sender: TObject);
Var Material: TAn8Material;
begin
  If (sgGlobalMaterials.Row > 0) And (sgGlobalMaterials.Row < sgGlobalMaterials.RowCount) Then
  Begin
    // Only delete a material if nothing references it

    If sgGlobalMaterials.Cells[2,sgGlobalMaterials.Row] = '0' Then
    Begin
      Material := TAn8Material(An8File.Materials.Objects[sgGlobalMaterials.Row - 1]);
      Material.Free;
      An8File.Materials.Delete(sgGlobalMaterials.Row - 1);
      LoadInfo;
    End
    Else ShowMessage('You can only delete an orphaned material.');
  End;
end;

procedure TfrmDeleteAN8Objects.btnRenameGlobalMaterialClick(
  Sender: TObject);
Var Material: TAn8Material;
begin
  If (sgGlobalMaterials.Row > 0) And (sgGlobalMaterials.Row < sgGlobalMaterials.RowCount) Then
  Begin
    Material := TAn8Material(An8File.Materials.Objects[sgGlobalMaterials.Row - 1]);
    If (Material.Surface <> Nil) And
       (Material.Surface.Diffuse <> Nil) And
       (Material.Surface.Diffuse.Texture <> Nil) Then
    Begin
      If FileExists(Material.Surface.Diffuse.Texture.FileName) Then
      Begin
        RenameMaterial(Material);
        LoadInfo;
      End
      Else ShowMessage('Could not find the texture file');
    End
    Else ShowMessage('This material does not appear to contain a diffuse texture');
  End;
end;

procedure TfrmDeleteAN8Objects.lbObjectsClick(Sender: TObject);
begin
  LoadObjectMaterials;
end;

procedure TfrmDeleteAN8Objects.lbObjectsKeyUp(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
  LoadObjectMaterials;
end;

procedure TfrmDeleteAN8Objects.btnRenameObjectMaterialClick(
  Sender: TObject);
Var
  Obj      : TAn8Object;
  Material : TAn8Material;

begin
  If (lbObjects.ItemIndex >= 0) And (lbObjects.ItemIndex < lbObjects.Items.Count) Then
  Begin
    Obj := TAn8Object(An8File.Objects.Objects[lbObjects.ItemIndex]);
    If Obj <> Nil Then
    Begin
      If Obj.Materials.Count > 0 Then
      Begin
        If (sgObjectMaterials.Row > 0) And (sgObjectMaterials.Row < sgObjectMaterials.RowCount) Then
        Begin
          Material := TAn8Material(Obj.Materials.Objects[sgObjectMaterials.Row - 1]);
          If (Material.Surface <> Nil) And
             (Material.Surface.Diffuse <> Nil) And
             (Material.Surface.Diffuse.Texture <> Nil) Then
          Begin
            If FileExists(Material.Surface.Diffuse.Texture.FileName) Then
            Begin
              RenameMaterial(Material);
              LoadInfo;
            End
            Else ShowMessage('Could not find the texture file');
          End
          Else ShowMessage('This material does not appear to contain a diffuse texture');
        End
        Else ShowMessage('You must first select a material to rename');
      End
      Else ShowMessage('This object doesn''t appear to contain any materials');
    End;
  End
  Else ShowMessage('You must first select an object');
end;

procedure TfrmDeleteAN8Objects.btnDeleteTextureClick(Sender: TObject);
Var Texture: TAn8Texture;
begin
  If (sgTextures.Row > 0) And (sgTextures.Row < sgTextures.RowCount) Then
  Begin
    // Only delete a texture if nothing references it

    If sgTextures.Cells[2,sgTextures.Row] = '0' Then
    Begin
      Texture := TAn8Texture(An8File.Textures.Objects[sgTextures.Row - 1]);
      Texture.Free;
      An8File.Textures.Delete(sgTextures.Row - 1);
      LoadInfo;
    End
    Else ShowMessage('You can only delete an orphaned texture.');
  End;
end;

procedure TfrmDeleteAN8Objects.btnRenameTextureClick(Sender: TObject);
Var
  Texture : TAn8Texture;
  St      : String;
  
begin
  If (sgTextures.Row > 0) And (sgTextures.Row < sgTextures.RowCount) Then
  Begin
    Texture := TAn8Texture(An8File.Textures.Objects[sgTextures.Row - 1]);
    St      := Texture.Name;
    If InputQuery('Rename texture','Enter new texture name:',St) Then
    Begin
      Texture.Name := St;
      LoadInfo;
    End;
  End;
end;

procedure TfrmDeleteAN8Objects.btnFixupCyclicClick(Sender: TObject);
Var
  I,J           : Integer;
  Sequence      : TAn8Sequence;
  SeqName       : String;
  SeqPrefix     : String;
  DropLastFrame : Boolean;
  MSPerFrame    : Integer;
  JointAngle    : TAn8JointAngle;
  FirstKey      : TAn8FloatKey;
  LastKey       : TAn8FloatKey;

begin
  If Application.MessageBox('This will fix up all cyclic sequences in your file,'#13#10'creating extra keys where necessary.'#13#10#13#10'Continue?','Confirm',MB_YESNO) = IDYES Then
  Begin
    For I := 0 To An8File.Sequences.Count - 1 Do
    Begin
      Sequence := TAn8Sequence(An8File.Sequences.Objects[I]);
      If TokenizeAn8SequenceName(Sequence.Name,SeqName,SeqPrefix,DropLastFrame,MSPerFrame) And DropLastFrame Then
      Begin
        For J := 0 To Sequence.JointAngles.Count - 1 Do
        Begin
          JointAngle := TAn8JointAngle(Sequence.JointAngles.Objects[J]);

          // If there is only one key then we don't have to do anything

          If JointAngle.Track.Keys.Count > 1 Then
          Begin
            FirstKey := JointAngle.Track.FindKeyForFrame(0);
            If FirstKey <> Nil Then
            Begin
              // Get the last key

              LastKey := JointAngle.Track.FindKeyForFrame(Sequence.Frames - 1);

              // If the last key isn't at the last frame then we need to add a new key at the last frame,
              // otherwise just copy the first key's value

              If LastKey = Nil Then
              Begin
                LastKey       := TAn8FloatKey.Create;
                LastKey.Frame := Sequence.Frames - 1;
                LastKey.Key   := True;
                JointAngle.Track.Keys.AddObject('',LastKey);
              End;
              LastKey.Value    := FirstKey.Value;
              LastKey.Unknown1 := FirstKey.Unknown1;
              LastKey.Unknown2 := FirstKey.Unknown2;
            End;
          End;
        End; // For J
      End;
    End; // For I
    I := SelectedFrame;
    J := lbAdjustSequence.ItemIndex;
    LoadObjectIntoView(TAn8Object(An8File.Objects.Objects[lbObjects.ItemIndex]));
    lbAdjustSequence.ItemIndex := J;
    acShowAdjustSequence.Execute;
    SelectedFrame              := I;
    PaintFrameSelection;
    ShowMessage('Cyclic sequences have been fixed up.');
  End;
end;

procedure TfrmDeleteAN8Objects.acShowSourceSequenceExecute(
  Sender: TObject);
begin
  LoadSequence(lbSourceSequence,10);
end;

procedure TfrmDeleteAN8Objects.lbSourceSequenceClick(Sender: TObject);
begin
  acShowSourceSequence.Execute;
end;

procedure TfrmDeleteAN8Objects.lbSourceSequenceKeyPress(Sender: TObject;
  var Key: Char);
begin
  acShowSourceSequence.Execute;
end;

procedure TfrmDeleteAN8Objects.acShowDestinationSequenceExecute(
  Sender: TObject);
begin
  LoadSequence(lbDestinationSequence,10);
end;

procedure TfrmDeleteAN8Objects.lbDestinationSequenceClick(Sender: TObject);
begin
  acShowDestinationSequence.Execute;
end;

procedure TfrmDeleteAN8Objects.lbDestinationSequenceKeyPress(
  Sender: TObject; var Key: Char);
begin
  acShowDestinationSequence.Execute;
end;

procedure TfrmDeleteAN8Objects.btnCloneEndpointStanceClick(Sender: TObject);
Var
  I,J           : Integer;
  Src           : TAn8Sequence;
  Dst           : TAn8Sequence;
  SrcJA         : TAn8JointAngle;
  DstJA         : TAn8JointAngle;
  SeqName       : String;
  SeqPrefix     : String;
  DropLastFrame : Boolean;
  MSPerFrame    : Integer;
  SrcKey        : TAn8FloatKey;
  DstKey        : TAn8FloatKey;

begin
  If (lbSourceSequence.Items.Count > 0) And (lbSourceSequence.ItemIndex >= 0) Then
  Begin
    If (lbDestinationSequence.Items.Count > 0) And (lbDestinationSequence.ItemIndex >= 0) Then
    Begin
      If lbSourceSequence.ItemIndex <> lbDestinationSequence.ItemIndex Then
      Begin
        Src := TAn8Sequence(An8File.Sequences.Objects[lbSourceSequence.ItemIndex]);
        Dst := TAn8Sequence(An8File.Sequences.Objects[lbDestinationSequence.ItemIndex]);
        For I := 0 To Src.JointAngles.Count - 1 Do
        Begin
          SrcJA := TAn8JointAngle(Src.JointAngles.Objects[I]);

          // Look for a matching joint angle in the destination sequence and add one if necessary

          J := Dst.JointAngles.IndexOf(Src.JointAngles.Strings[I]);
          If J < 0 Then
          Begin
            DstJA      := TAn8JointAngle.Create(Dst);
            DstJA.Bone := Src.Figure.FindBoneByName(SrcJA.Bone.Name);
            DstJA.Axis := SrcJA.Axis;
            Case DstJA.Axis Of
              axX: Dst.JointAngles.AddObject(DstJA.Bone.Name + '_X',DstJA);
              axY: Dst.JointAngles.AddObject(DstJA.Bone.Name + '_Y',DstJA);
              axZ: Dst.JointAngles.AddObject(DstJA.Bone.Name + '_Z',DstJA);
            End; // Case
            J := Dst.JointAngles.IndexOf(Src.JointAngles.Strings[I]);
          End;

          If J >= 0 Then
          Begin
            DstJA := TAn8JointAngle(Dst.JointAngles.Objects[J]);
            SrcKey := SrcJA.Track.FindKeyForFrame(0);
            If SrcKey <> Nil Then
            Begin
              // Copy the stance to the first frame, creating an appropriate frame if we have to

              DstKey := DstJA.Track.FindKeyForFrame(0);
              If DstKey = Nil Then
              Begin
                DstKey       := TAn8FloatKey.Create;
                DstKey.Frame := 0;
                DstKey.Key   := True;
                DstJA.Track.Keys.InsertObject(0,'',DstKey);
              End;
              DstKey.Value    := SrcKey.Value;
              DstKey.Unknown1 := SrcKey.Unknown1;
              DstKey.Unknown2 := SrcKey.Unknown2;
              DstKey.Key      := SrcKey.Key;

              // If the destination sequence is cyclic, copy the stance to the last frame as well, creating
              // an appropriate frame if we have to (but only if there is more than one frame to begin with)

              If (DstJA.Track.Keys.Count > 1) And TokenizeAn8SequenceName(Dst.Name,SeqName,SeqPrefix,DropLastFrame,MSPerFrame) And DropLastFrame Then
              Begin
                DstKey := DstJA.Track.FindKeyForFrame(Dst.Frames - 1);
                If DstKey = Nil Then
                Begin
                  DstKey       := TAn8FloatKey.Create;
                  DstKey.Frame := Dst.Frames - 1;
                  DstKey.Key   := True;
                  DstJA.Track.Keys.AddObject('',DstKey);
                End;
                DstKey.Value    := SrcKey.Value;
                DstKey.Unknown1 := SrcKey.Unknown1;
                DstKey.Unknown2 := SrcKey.Unknown2;
                DstKey.Key      := SrcKey.Key;
              End;
            End;
          End;
        End; // For I
        ShowMessage('Endpoint stance has been copied.');
        LoadInfo;
      End
      Else ShowMessage('The source and destination sequences must be different.');
    End
    Else ShowMessage('You must first select a destination sequence.');
  End
  Else ShowMessage('You must first select a source sequence.');
end;

procedure TfrmDeleteAN8Objects.btnCloneSequenceClick(Sender: TObject);
Var
  I,J    : Integer;
  Src    : TAn8Sequence;
  Dst    : TAn8Sequence;
  SrcJA  : TAn8JointAngle;
  DstJA  : TAn8JointAngle;
  SrcKey : TAn8FloatKey;
  DstKey : TAn8FloatKey;
  St     : String;

begin
  If Application.MessageBox('This will completely overwrite the destination sequence!'#13#10#13#10'Are you sure?','Confirm',MB_YESNO) = IDYES Then
  Begin
    If (lbSourceSequence.Items.Count > 0) And (lbSourceSequence.ItemIndex >= 0) Then
    Begin
      If (lbDestinationSequence.Items.Count > 0) And (lbDestinationSequence.ItemIndex >= 0) Then
      Begin
        If lbSourceSequence.ItemIndex <> lbDestinationSequence.ItemIndex Then
        Begin
          Src := TAn8Sequence(An8File.Sequences.Objects[lbSourceSequence.ItemIndex]);
          Dst := TAn8Sequence(An8File.Sequences.Objects[lbDestinationSequence.ItemIndex]);
          Dst.CopyFrom(Src);
          ShowMessage('Sequence has been copied.');
        End
        Else ShowMessage('The source and destination sequences must be different.');
      End
      Else ShowMessage('You must first select a destination sequence.');
    End
    Else ShowMessage('You must first select a source sequence.');
  End;
end;

procedure TfrmDeleteAN8Objects.btnClearSequenceClick(Sender: TObject);
Var
  I,J            : Integer;
  Sequence       : TAn8Sequence;
  JA             : TAn8JointAngle;
  SeqName        : String;
  SeqPrefix      : String;
  DropLastFrame  : Boolean;
  MSPerFrame     : Integer;
  Key            : TAn8FloatKey;

begin
  If Application.MessageBox('This will completely clear all but the endpoint keys in this sequence!'#13#10#13#10'Are you sure?','Confirm',MB_YESNO) = IDYES Then
  Begin
    If (lbAdjustSequence.Items.Count > 0) And (lbAdjustSequence.ItemIndex >= 0) Then
    Begin
      Sequence := TAn8Sequence(An8File.Sequences.Objects[lbAdjustSequence.ItemIndex]);
      If Sequence <> Nil Then
      Begin
        DropLastFrame := False;
        TokenizeAn8SequenceName(Sequence.Name,SeqName,SeqPrefix,DropLastFrame,MSPerFrame);
        For I := 0 To Sequence.JointAngles.Count - 1 Do
        Begin
          JA := TAn8JointAngle(Sequence.JointAngles.Objects[I]);
          J  := 0;
          While J < JA.Track.Keys.Count Do
          Begin
            Key := TAn8FloatKey(JA.Track.Keys.Objects[J]);
            If (Key.Frame > 0) And ((Key.Frame < Sequence.Frames - 1) Or Not DropLastFrame) Then
            Begin
              Key.Free;
              JA.Track.Keys.Delete(J);
            End
            Else Inc(J);
          End; // While
        End; // For I
        ShowMessage('Sequence has been cleaned of all but the endpoint keys.');
      End
      Else ShowMessage('There was an error retrieving the sequence.');
    End
    Else ShowMessage('You must first select a source sequence.');
  End;
end;

procedure TfrmDeleteAN8Objects.btnCopyWeightsClick(Sender: TObject);
Var
  I,J               : Integer;
  Figure            : TAn8Figure;
  FoundNamedObjects : TStringList;
  SrcNamedObject    : TAn8NamedObject;
  DstNamedObject    : TAn8NamedObject;
  Figures           : TStringList;
  NamedObjects      : TStringList;

begin
  If (sgFigures.Row > 0) And (sgFigures.Row < sgFigures.RowCount) Then
  Begin
    Figures      := TStringList.Create;
    NamedObjects := TStringList.Create;
    For I := 0 To An8File.Figures.Count - 1 Do
    Begin
      Figure := TAn8Figure(An8File.Figures.Objects[I]);
      If Figure.RootBone <> Nil Then
      Begin
        FoundNamedObjects := Figure.RootBone.FindNamedObjects;
        If FoundNamedObjects <> Nil Then
        Begin
          For J := 0 To FoundNamedObjects.Count - 1 Do
          Begin
            SrcNamedObject := TAn8NamedObject(FoundNamedObjects.Objects[J]);
            Figures.AddObject(Figure.Name,Figure);
            NamedObjects.AddObject(SrcNamedObject.Name,SrcNamedObject);
          End; // For J
        End;
      End;
    End; // For I
    SrcNamedObject := TAn8NamedObject(NamedObjects.Objects[sgFigures.Row - 1]);
    If SrcNamedObject <> Nil Then
    Begin
      If (SrcNamedObject.Weights.Count > 0) Or (SrcNamedObject.WeightedBy.Count > 0) Then
      Begin
        // Only copy weight information to named objects that have none

        Figure := TAn8Figure(Figures.Objects[sgFigures.Row - 1]);
        FoundNamedObjects := Figure.RootBone.FindNamedObjects;
        For I := 0 To FoundNamedObjects.Count - 1 Do
        Begin
          DstNamedObject := TAn8NamedObject(FoundNamedObjects.Objects[I]);
          If DstNamedObject.Weights.Count = 0 Then
          Begin
            For J := 0 To SrcNamedObject.Weights.Count - 1 Do DstNamedObject.Weights.AddObject(SrcNamedObject.Weights.Strings[J],TAn8PaintedWeights.Create(TAn8PaintedWeights(SrcNamedObject.Weights.Objects[J])));
          End;
          If DstNamedObject.WeightedBy.Count = 0 Then
          Begin
            For J := 0 To SrcNamedObject.WeightedBy.Count - 1 Do DstNamedObject.WeightedBy.AddObject(SrcNamedObject.WeightedBy.Strings[J],SrcNamedObject.WeightedBy.Objects[J]);
          End;
        End; // For I
        ShowMessage('Weight information copied.');
      End
      Else ShowMessage('This named object has no weights assigned to it.');
    End;
    Figures.Free;
    NamedObjects.Free;
  End;
end;

procedure TfrmDeleteAN8Objects.btnMoveLegsApartClick(Sender: TObject);
Var
  I,J,K      : Integer;
  Sequence   : TAn8Sequence;
  JointAngle : TAn8JointAngle;
  Key        : TAn8FloatKey;
  Amount     : Single;
  St         : String;

begin
  St := Trim(edtMoveLegsApart.Text);
  If St <> '' Then
  Begin
    Val(St,Amount,I);
    If I = 0 Then
    Begin
      If Application.MessageBox('This will attempt to move the legs apart for all sequences in your file.'#13#10#13#10'Continue?','Confirm',MB_YESNO) = IDYES Then
      Begin
        For I := 0 To An8File.Sequences.Count - 1 Do
        Begin
          Sequence := TAn8Sequence(An8File.Sequences.Objects[I]);
          For J := 0 To Sequence.JointAngles.Count - 1 Do
          Begin
            JointAngle := TAn8JointAngle(Sequence.JointAngles.Objects[J]);
            If (JointAngle.Axis = axX) And ((JointAngle.Bone.Name = 'th_r') Or (JointAngle.Bone.Name = 'th_l')) Then
            Begin
              For K := 0 To JointAngle.Track.Keys.Count - 1 Do
              Begin
                Key := TAn8FloatKey(JointAngle.Track.Keys.Objects[K]);
                Key.Value := Key.Value + Amount;
              End; // For K
            End;
          End; // For J
        End; // For I
      End;
      ShowMessage('Legs moved apart for all sequences.');
      LoadInfo;
      I := SelectedFrame;
      J := lbAdjustSequence.ItemIndex;
      LoadObjectIntoView(TAn8Object(An8File.Objects.Objects[lbObjects.ItemIndex]));
      lbAdjustSequence.ItemIndex := J;
      acShowAdjustSequence.Execute;
      SelectedFrame              := I;
      PaintFrameSelection;
    End
    Else ShowMessage('Invalid number entered in the degree edit field.');
  End
  Else ShowMessage('You must enter a degree angle by which to move the legs apart.');
end;

procedure TfrmDeleteAN8Objects.acShowAdjustSequenceExecute(
  Sender: TObject);
begin
  AdjustedSequence := LoadSequence(lbAdjustSequence,0);
  PaintFrameSelection;
end;

procedure TfrmDeleteAN8Objects.lbAdjustSequenceClick(Sender: TObject);
begin
  acShowAdjustSequence.Execute;
end;

procedure TfrmDeleteAN8Objects.lbAdjustSequenceKeyPress(Sender: TObject;
  var Key: Char);
begin
  acShowAdjustSequence.Execute;
end;

procedure TfrmDeleteAN8Objects.imgFramesMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  // Find out which frame was selected

  If AdjustedSequence <> Nil Then
  Begin
    If (Y >= 0) And (Y < imgFrames.Height) Then
    Begin
      If (X > 0) And ((X Mod BoxWidth) <> 0) And ((X Div BoxWidth) < AdjustedSequence.Frames) Then
      Begin
        SelectedFrame := X Div BoxWidth;
        PaintFrameSelection;
      End;
    End;
  End;
end;

procedure TfrmDeleteAN8Objects.acMoveFrameUpExecute(Sender: TObject);
Var
  Jack1X   : TAn8JointAngle;
  Jack2X   : TAn8JointAngle;
  Jack3X   : TAn8JointAngle;
  Key1     : TAn8FloatKey;
  Key2     : TAn8FloatKey;
  Key3     : TAn8FloatKey;
  I,J      : Integer;

begin
  If AdjustedSequence <> Nil Then
  Begin
    Jack1X := AdjustedSequence.FindJointAngle('jack1',axX);
    Jack2X := AdjustedSequence.FindJointAngle('jack2',axX);
    Jack3X := AdjustedSequence.FindJointAngle('jack3',axX);
    If (SelectedFrame >= 0) And (SelectedFrame < AdjustedSequence.Frames) And (Jack1X <> Nil) And (Jack2X <> Nil) And (Jack3X <> Nil) Then
    Begin
      Key1 := Jack1X.Track.FindKeyForFrame(SelectedFrame);
      Key2 := Jack2X.Track.FindKeyForFrame(SelectedFrame);
      Key3 := Jack3X.Track.FindKeyForFrame(SelectedFrame);
      If (Jack1X <> Nil) And (Jack2X <> Nil) And (Jack3X <> Nil) Then
      Begin
        Key1.Value := Key1.Value + udMoveUpDownAmount.Position;
        Key2.Value := Key2.Value - 2 * udMoveUpDownAmount.Position;
        Key3.Value := Key3.Value + udMoveUpDownAmount.Position;
        If (lbObjects.ItemIndex >= 0) And (lbObjects.ItemIndex < lbObjects.Items.Count) Then
        Begin
          I := SelectedFrame;
          J := lbAdjustSequence.ItemIndex;
          LoadObjectIntoView(TAn8Object(An8File.Objects.Objects[lbObjects.ItemIndex]));
          lbAdjustSequence.ItemIndex := J;
          acShowAdjustSequence.Execute;
          SelectedFrame              := I;
          PaintFrameSelection;
        End;
      End;
    End;
  End;
end;

procedure TfrmDeleteAN8Objects.acMoveFrameDownExecute(Sender: TObject);
Var
  Jack1X   : TAn8JointAngle;
  Jack2X   : TAn8JointAngle;
  Jack3X   : TAn8JointAngle;
  Key1     : TAn8FloatKey;
  Key2     : TAn8FloatKey;
  Key3     : TAn8FloatKey;
  I,J      : Integer;

begin
  If AdjustedSequence <> Nil Then
  Begin
    Jack1X := AdjustedSequence.FindJointAngle('jack1',axX);
    Jack2X := AdjustedSequence.FindJointAngle('jack2',axX);
    Jack3X := AdjustedSequence.FindJointAngle('jack3',axX);
    If (SelectedFrame >= 0) And (SelectedFrame < AdjustedSequence.Frames) And (Jack1X <> Nil) And (Jack2X <> Nil) And (Jack3X <> Nil) Then
    Begin
      Key1 := Jack1X.Track.FindKeyForFrame(SelectedFrame);
      Key2 := Jack2X.Track.FindKeyForFrame(SelectedFrame);
      Key3 := Jack3X.Track.FindKeyForFrame(SelectedFrame);
      If (Jack1X <> Nil) And (Jack2X <> Nil) And (Jack3X <> Nil) Then
      Begin
        Key1.Value := Key1.Value - udMoveUpDownAmount.Position;
        Key2.Value := Key2.Value + 2 * udMoveUpDownAmount.Position;
        Key3.Value := Key3.Value - udMoveUpDownAmount.Position;
        If (lbObjects.ItemIndex >= 0) And (lbObjects.ItemIndex < lbObjects.Items.Count) Then
        Begin
          I := SelectedFrame;
          J := lbAdjustSequence.ItemIndex;
          LoadObjectIntoView(TAn8Object(An8File.Objects.Objects[lbObjects.ItemIndex]));
          lbAdjustSequence.ItemIndex := J;
          acShowAdjustSequence.Execute;
          SelectedFrame              := I;
          PaintFrameSelection;
        End;
      End;
    End;
  End;
end;

procedure TfrmDeleteAN8Objects.btnChangeMaterialsClick(Sender: TObject);
Var
  I             : Integer;
  Obj           : TAn8Object;
  UsedMaterials : TStringList;
  MatSource     : TAn8Material;
  MatDest       : TAn8Material;

  Procedure ProcessComponent(C: TAn8Component);
  Var
    I        : Integer;
    Mesh     : TAn8Mesh;
    Group    : TAn8Group;
    Material : TAn8Material;

  Begin
    If C <> Nil Then
    Begin
      If C Is TAn8Mesh Then
      Begin
        Mesh := TAn8Mesh(C);
        For I := 0 To Mesh.MaterialList.Count - 1 Do
        Begin
          Material := TAn8Material(Mesh.MaterialList.Objects[I]);
          If UsedMaterials.IndexOf(Material.Name) < 0 Then UsedMaterials.AddObject(Material.Name,Material);
        End;
      End
      Else If C Is TAn8Group Then
      Begin
        Group := TAn8Group(C);
        For I := 0 To Group.Components.Count - 1 Do ProcessComponent(TAn8Component(Group.Components.Objects[I]));
      End;
    End;
  End; // ProcessComponent

  Procedure ChangeMaterials(C: TAn8Component; MatSource,MatDest: TAn8Material);
  Var
    I     : Integer;
    Mesh  : TAn8Mesh;
    Group : TAn8Group;
    Face  : TAn8Face;

  Begin
    If C <> Nil Then
    Begin
      If C Is TAn8Mesh Then
      Begin
        Mesh := TAn8Mesh(C);
        For I := 0 To Mesh.MaterialList.Count - 1 Do
        Begin
          If TAn8Material(Mesh.MaterialList.Objects[I]) = MatSource Then
          Begin
            Mesh.MaterialList.Strings[I] := MatDest.Name;
            Mesh.MaterialList.Objects[I] := MatDest;
          End;
        End; // For I
        For I := 0 To Mesh.Faces.Count - 1 Do
        Begin
          Face := TAn8Face(Mesh.Faces.Objects[I]);
          If Face.Material = MatSource Then Face.Material := MatDest;
        End; // For I
      End
      Else If C Is TAn8Group Then
      Begin
        Group := TAn8Group(C);
        For I := 0 To Group.Components.Count - 1 Do ChangeMaterials(TAn8Component(Group.Components.Objects[I]),MatSource,MatDest);
      End;
    End;
  End; // ChangeMaterials

begin
  Obj := Nil;
  If (lbObjects.ItemIndex >= 0) And (lbObjects.ItemIndex < lbObjects.Items.Count) Then
  Begin
    Obj := TAn8Object(An8File.Objects.Objects[lbObjects.ItemIndex]);
  End;
  If Obj <> Nil Then
  Begin
    If (lbObjectMaterials.ItemIndex >= 0) And (lbObjectMaterials.ItemIndex < lbObjectMaterials.Items.Count) Then
    Begin
      If (lbAvailableMaterials.ItemIndex >= 0) And (lbAvailableMaterials.ItemIndex < lbAvailableMaterials.Items.Count) Then
      Begin
        // Build a list of materials this object uses

        UsedMaterials := TStringList.Create;
        For I := 0 To Obj.Components.Count - 1 Do ProcessComponent(TAn8Component(Obj.Components.Objects[I]));

        // Get the source and destination materials

        MatSource := TAn8Material(UsedMaterials.Objects[lbObjectMaterials.ItemIndex]);
        If lbAvailableMaterials.ItemIndex < An8File.Materials.Count
         Then MatDest := TAn8Material(An8File.Materials.Objects[lbAvailableMaterials.ItemIndex])
         Else MatDest := TAn8Material(Obj.Materials.Objects[lbAvailableMaterials.ItemIndex - An8File.Materials.Count]);

        // Change the materials in the object

        If UsedMaterials.IndexOfObject(MatDest) < 0 Then
        Begin
          For I := 0 To Obj.Components.Count - 1 Do ChangeMaterials(TAn8Component(Obj.Components.Objects[I]),MatSource,MatDest);
        End
        Else ShowMessage('The object is already using this material.'); 

        // Cleanup

        UsedMaterials.Free;
        LoadInfo;
      End
      Else ShowMessage('You must first select a material to change to.');
    End
    Else ShowMessage('You must first select a material to change.');
  End
  Else ShowMessage('You must first select an object.');
end;

procedure TfrmDeleteAN8Objects.btnCopySequenceClick(Sender: TObject);
Var
  St  : String;
  Src : TAn8Sequence;
  Dst : TAn8Sequence;

begin
  If (lbSourceSequence.Items.Count > 0) And (lbSourceSequence.ItemIndex >= 0) Then
  Begin
    If InputQuery('Copy sequence','Enter new sequence name:',St) Then
    Begin
      St := Trim(St);
      If St <> '' Then
      Begin
        If lbDestinationSequence.Items.IndexOf(St) < 0 Then
        Begin
          Src := TAn8Sequence(An8File.Sequences.Objects[lbSourceSequence.ItemIndex]);
          Dst := TAn8Sequence.Create(An8File);
          Dst.Name := St;
          Dst.Figure := Src.Figure;
          Dst.CopyFrom(Src);
          An8File.Sequences.AddObject(Dst.Name,Dst);
          LoadInfo;
        End
        Else ShowMessage('You cannot choose the name of an existing sequence.');
      End
      Else ShowMessage('You must enter a name for the new sequence.');
    End;
  End
  Else ShowMessage('You must first select a source sequence.');
end;

procedure TfrmDeleteAN8Objects.IAEverButton2Click(Sender: TObject);
Var I: Integer;
begin
  If (lbAdjustSequence.Items.Count > 0) And (lbAdjustSequence.ItemIndex > 0) Then
  Begin
    I := lbAdjustSequence.ItemIndex;
    An8File.Sequences.Exchange(lbAdjustSequence.ItemIndex,lbAdjustSequence.ItemIndex - 1);
    LoadInfo;
    lbAdjustSequence.ItemIndex := I - 1;
    lbAdjustSequenceClick(Self);
  End;
end;

procedure TfrmDeleteAN8Objects.IAEverButton3Click(Sender: TObject);
Var I: Integer;
begin
  If (lbAdjustSequence.Items.Count > 0) And (lbAdjustSequence.ItemIndex < lbAdjustSequence.Items.Count - 1) Then
  Begin
    I := lbAdjustSequence.ItemIndex;
    An8File.Sequences.Exchange(lbAdjustSequence.ItemIndex,lbAdjustSequence.ItemIndex + 1);
    LoadInfo;
    lbAdjustSequence.ItemIndex := I + 1;
    lbAdjustSequenceClick(Self);
  End;
end;

procedure TfrmDeleteAN8Objects.btnRemoveUnusedMaterialsClick(
  Sender: TObject);
Var
  Obj : TAn8Object;
  I   : Integer;

  Procedure ProcessComponent(C: TAn8Component);
  Var
    I,J      : Integer;
    Mesh     : TAn8Mesh;
    Group    : TAn8Group;
    Material : TAn8Material;
    Face     : TAn8Face;
    Remove   : Array Of Boolean;
    Found    : Boolean;

  Begin
    If C <> Nil Then
    Begin
      If C Is TAn8Mesh Then
      Begin
        Mesh := TAn8Mesh(C);
        SetLength(Remove,Mesh.MaterialList.Count);
        For I := 0 To High(Remove) Do Remove[I]  := True;
        For I := 0 To Mesh.Faces.Count - 1 Do
        Begin
          Face := TAn8Face(Mesh.Faces.Objects[I]);
          If Face.Material <> Nil Then
          Begin
            J := Mesh.MaterialList.IndexOfObject(Face.Material);
            If J >= 0 Then Remove[J] := False;
          End;
        End; // For I
        For I := 0 To Mesh.MaterialList.Count - 1 Do
        Begin
          Material := TAn8Material(Mesh.MaterialList.Objects[I]);
          If Material = Nil Then Remove[I] := True;
        End; // For I

        // Get rid of any flagged materials

        I := Mesh.MaterialList.Count - 1;
        While I >= 0 Do
        Begin
          If Remove[I] Then Mesh.MaterialList.Delete(I);
          Dec(I);
        End; // While

        SetLength(Remove,0);
      End
      Else If C Is TAn8Group Then
      Begin
        Group := TAn8Group(C);
        For I := 0 To Group.Components.Count - 1 Do ProcessComponent(TAn8Component(Group.Components.Objects[I]));
      End;
    End;
  End; // ProcessComponent

begin
  Obj := Nil;
  If (lbObjects.ItemIndex >= 0) And (lbObjects.ItemIndex < lbObjects.Items.Count) Then
  Begin
    Obj := TAn8Object(An8File.Objects.Objects[lbObjects.ItemIndex]);
  End;
  If Obj <> Nil Then
  Begin
    For I := 0 To Obj.Components.Count - 1 Do ProcessComponent(TAn8Component(Obj.Components.Objects[I]));
    ShowMessage('Unused materials removed from object.');
    LoadObjectMaterials;
  End
  Else ShowMessage('You must select an object.');
end;

procedure TfrmDeleteAN8Objects.btnMoveMaterialUpClick(Sender: TObject);
Var
  Obj  : TAn8Object;
  Mesh : TAn8Mesh;
  I    : Integer;

begin
  Obj := Nil;
  If (lbObjects.ItemIndex >= 0) And (lbObjects.ItemIndex < lbObjects.Items.Count) Then
  Begin
    Obj := TAn8Object(An8File.Objects.Objects[lbObjects.ItemIndex]);
  End;
  If Obj <> Nil Then
  Begin
    If lbObjectMaterials.ItemIndex >= 0 Then
    Begin
      If (Obj.Components.Count = 1) And (Obj.Components.Objects[0] Is TAn8Mesh) Then
      Begin
        Mesh := TAn8Mesh(Obj.Components.Objects[0]);
        If lbObjectMaterials.Items.Count = Mesh.MaterialList.Count Then
        Begin
          I := lbObjectMaterials.ItemIndex;
          If I > 0 Then
          Begin
            Mesh.MaterialList.Exchange(I,I - 1);
            LoadObjectMaterials;
            lbObjectMaterials.ItemIndex := I - 1;
          End;
        End
        Else ShowMessage('You can only reorder materials if there are no unused materials.'#13#10'Click "Remove unused" to get rid of any unused materials.');
      End
      Else ShowMessage('You can only reorder materials if the object contains only one mesh and no groups.');
    End
    Else ShowMessage('You must select a material.');
  End
  Else ShowMessage('You must select an object.');
end;

procedure TfrmDeleteAN8Objects.btnMoveMaterialDownClick(Sender: TObject);
Var
  Obj  : TAn8Object;
  Mesh : TAn8Mesh;
  I    : Integer;

begin
  Obj := Nil;
  If (lbObjects.ItemIndex >= 0) And (lbObjects.ItemIndex < lbObjects.Items.Count) Then
  Begin
    Obj := TAn8Object(An8File.Objects.Objects[lbObjects.ItemIndex]);
  End;
  If Obj <> Nil Then
  Begin
    If lbObjectMaterials.ItemIndex >= 0 Then
    Begin
      If (Obj.Components.Count = 1) And (Obj.Components.Objects[0] Is TAn8Mesh) Then
      Begin
        Mesh := TAn8Mesh(Obj.Components.Objects[0]);
        If lbObjectMaterials.Items.Count = Mesh.MaterialList.Count Then
        Begin
          I := lbObjectMaterials.ItemIndex;
          If I < Mesh.MaterialList.Count - 1 Then
          Begin
            Mesh.MaterialList.Exchange(I,I + 1);
            LoadObjectMaterials;
            lbObjectMaterials.ItemIndex := I + 1;
          End;
        End
        Else ShowMessage('You can only reorder materials if there are no unused materials.'#13#10'Click "Remove unused" to get rid of any unused materials.');
      End
      Else ShowMessage('You can only reorder materials if the object contains only one mesh and no groups.');
    End
    Else ShowMessage('You must select a material.');
  End
  Else ShowMessage('You must select an object.');
end;

procedure TfrmDeleteAN8Objects.btnScaleFigureClick(Sender: TObject);
Var
  St           : String;
  I,J          : Integer;
  Scale        : Single;
  Figures      : TStringList;
  Figure       : TAn8Figure;
  NamedObjects : TStringList;

begin
  If (sgFigures.Row > 0) And (sgFigures.Row < sgFigures.RowCount) Then
  Begin
    If InputQuery('Scale figure','Enter scaling factor:',St) Then
    Begin
      St := Trim(St);
      If St <> '' Then
      Begin
        Val(St,Scale,I);
        If I = 0 Then
        Begin
          Figures := TStringList.Create;
          For I := 0 To An8File.Figures.Count - 1 Do
          Begin
            Figure := TAn8Figure(An8File.Figures.Objects[I]);
            If Figure.RootBone <> Nil Then
            Begin
              NamedObjects := Figure.RootBone.FindNamedObjects;
              If NamedObjects <> Nil Then
              Begin
                For J := 0 To NamedObjects.Count - 1 Do Figures.AddObject('',Figure);
              End;
            End;
          End; // For I
          If sgFigures.Row - 1 < Figures.Count Then
          Begin
            Figure := TAn8Figure(Figures.Objects[sgFigures.Row - 1]);
            If (Figure <> Nil) And (Figure.RootBone <> Nil) Then Figure.RootBone.ScaleLength(Scale,True);
            ShowMessage('All bones in the figure have been rescaled.');
          End
          Else ShowMessage('There was an error trying to find the figure you selected.');
          Figures.Free;
        End
        Else ShowMessage('You must enter a valid scaling factor.');
      End;
    End;
  End
  Else ShowMessage('You must select a figure.');  
end;

End.
