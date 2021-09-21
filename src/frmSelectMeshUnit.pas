unit frmSelectMeshUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls, Buttons, GLVisir;

type
  TfrmSelectMesh = class(TForm)
    pnlMeshes: TPanel;
    pnlButtons: TPanel;
    btnOk: TBitBtn;
    btnCancel: TBitBtn;
    Label1: TLabel;
    lbMeshes: TListBox;
    pnlClient: TPanel;
    Splitter1: TSplitter;
    glvMesh: TGLVisir;
    procedure FormShow(Sender: TObject);
    procedure lbMeshesClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    Function GetSelectedMeshObject: String;
  end;

var
  frmSelectMesh: TfrmSelectMesh;

implementation

{$R *.dfm}

Uses ZoneClasses,frmMainUnit,URickGL,U3DPolys;

procedure TfrmSelectMesh.FormShow(Sender: TObject);
Var I: Integer;
begin
  lbMeshes.Clear;
  lbMeshes.Items.AddStrings(MeshLibrary);

  glvMesh.Scene3D.Scene.SetActive(True);

//  For I := 0 To MasterTextures.Count - 1 Do (MasterTextures.Objects[I] As TTexture).LoadTextureIntoOpenGL;

  For I := glvMesh.Scene3D.Scene.Entities.Count - 1 DownTo 0 Do
  Begin
    TEntity(glvMesh.Scene3D.Scene.Entities.Items[I]).Free;
    glvMesh.Scene3D.Scene.Entities.Delete(I);
  End; // For I
  glvMesh.Update;//Scene3D.Update(glvMesh.Width,glvMesh.Height);
end;

procedure TfrmSelectMesh.lbMeshesClick(Sender: TObject);
Var
  I      : Integer;
  GO     : TGroupObject;
  Mesh   : TMeshObject;
  List   : TStringList;
  Light  : TLight;
  Entity : TEntity;
  Tree   : TTree;

begin
  I := lbMeshes.ItemIndex;
  If (I >= 0) And (I < MeshLibrary.Count) Then
  Begin
    GO := MeshLibrary.Objects[I] As TGroupObject;
    List := TStringList.Create;
    GO.AddToPolygonList(List,False,True,True,True);
    Mesh := List.Objects[0] As TMeshObject;

    glvMesh.scene3D.scene.bTranslucentMode := True;

    glvMesh.Clear;
    glvMesh.Scene3D.Scene.ClearScene(True,True);
{
    // Get rid of all entities

    For I := glvMesh.Scene3D.Scene.Entities.Count - 1 DownTo 0 Do
    Begin
      TEntity(glvMesh.Scene3D.Scene.Entities.Items[I]).Free;
      glvMesh.Scene3D.Scene.Entities.Delete(I);
    End; // For I
}
{
    // Add a dummy entity since ReloadMesh expects it

    Entity := TEntity.Create(glvMesh.Scene3D.Scene);
    glvMesh.Scene3D.Scene.Entities.Add(Entity);
}
    Tree := Nil;
    frmMain.ReloadMesh(List,0,0,False,False,False,glvMesh,Tree,True,ExtractFilePath(Application.ExeName) + 'library\textures\');
    List.Free;
    Tree.Free;
    Mesh.Free;

    glvMesh.scene3D.scene.DefaultCamera.LookAt(0,0,-100);
    glvMesh.scene3D.scene.DefaultCamera.SetVectorUp(1,0,0);
    glvMesh.scene3D.scene.DefaultCamera.SetPosition(0,0,800);

    Light := TLight(glvMesh.scene3D.scene.Lights.Items[0]);
    Light.Position.Copy(0,0,100000);
    Light.SetOrientation(1,1,0);
    Light.Attenuation := 0;//.001;

    glvMesh.Fit(False,True,True);
    glvMesh.Scene3D.Redraw;
  End;
end;

procedure TfrmSelectMesh.FormClose(Sender: TObject;
  var Action: TCloseAction);
Var I: Integer;
begin
  glvMesh.Scene3D.Scene.SetActive(False);
end;

Function TfrmSelectMesh.GetSelectedMeshObject: String;
Begin
  If (lbMeshes.ItemIndex >= 0) And (lbMeshes.ItemIndex < lbMeshes.Items.Count)
   Then Result := Trim(lbMeshes.Items.Strings[lbMeshes.ItemIndex])
   Else Result := '';
End; // TfrmSelectMesh.GetSelectedMeshObject

procedure TfrmSelectMesh.FormCreate(Sender: TObject);
begin
  glvMesh.Scene3D.Scene.DistFar := 10000;
end;

end.
