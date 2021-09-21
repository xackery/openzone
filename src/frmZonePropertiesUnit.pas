unit frmZonePropertiesUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons, ClearTypeText, OZDMUnit, IAeverButton;

type
  TfrmZoneProperties = class(TForm)
    Label1: TClearTypeLabel;
    Label2: TClearTypeLabel;
    edtLongName: TClearTypeEdit;
    edtShortName: TClearTypeEdit;
    GroupBox1: TClearTypeGroupBox;
    rbOutdoor: TClearTypeRadioButton;
    rbIndoor: TClearTypeRadioButton;
    btnCancel: TIAEverButton;
    btnOk: TIAEverButton;
    GroupBox2: TClearTypeGroupBox;
    lbMeshLibrary: TClearTypeListBox;
    lbZoneExtraMeshes: TClearTypeListBox;
    Label3: TClearTypeLabel;
    Label4: TClearTypeLabel;
    btnHelp: TIAEverButton;
    btnAdd: TIAEverButton;
    btnRemove: TIAEverButton;
    GroupBox3: TClearTypeGroupBox;
    Label5: TClearTypeLabel;
    Label6: TClearTypeLabel;
    lbCreatureLibrary: TClearTypeListBox;
    lbZoneExtraCreatures: TClearTypeListBox;
    btnAddCreature: TIAEverButton;
    btnRemoveCreature: TIAEverButton;
    procedure FormShow(Sender: TObject);
    procedure btnHelpClick(Sender: TObject);
    procedure btnAddClick(Sender: TObject);
    procedure btnRemoveClick(Sender: TObject);
    procedure btnAddCreatureClick(Sender: TObject);
    procedure btnRemoveCreatureClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    Procedure RemoveIncludedItemsFromMeshLibraryList;
  end;

var
  frmZoneProperties: TfrmZoneProperties;

implementation

Uses ZoneClasses;

{$R *.dfm}

Procedure TfrmZoneProperties.RemoveIncludedItemsFromMeshLibraryList;
Var
  I,J   : Integer;
  Found : Boolean;
  St    : String;
  St1   : String;

Begin
  I := 0;
  While I < lbMeshLibrary.Items.Count Do
  Begin
    J     := 0;
    Found := False;
    St    := UpperCase(lbMeshLibrary.Items.Strings[I]);
    While (J < lbZoneExtraMeshes.Items.Count) And Not Found Do
    Begin
      St1 := UpperCase(lbZoneExtraMeshes.Items.Strings[J]);
      If Copy(St1,1,Length(St) + 1) = St + '/' Then
      Begin
        Found := True;
        lbMeshLibrary.ItemIndex := I;
        lbMeshLibrary.DeleteSelected;
      End
      Else Inc(J);
    End; // While
    If Not Found Then Inc(I);
  End; // For I

  I := 0;
  While I < lbCreatureLibrary.Items.Count Do
  Begin
    J     := 0;
    Found := False;
    St    := UpperCase(lbCreatureLibrary.Items.Strings[I]);
    While (J < lbZoneExtraCreatures.Items.Count) And Not Found Do
    Begin
      St1 := UpperCase(lbZoneExtraCreatures.Items.Strings[J]);
      If Copy(St1,Length(St1) - Length(St) + 1,Length(St)) = St Then
      Begin
        Found := True;
        lbCreatureLibrary.ItemIndex := I;
        lbCreatureLibrary.DeleteSelected;
      End
      Else Inc(J);
    End; // While
    If Not Found Then Inc(I);
  End; // For I
End; // TfrmZoneProperties.RemoveIncludedItemsFromMeshLibraryList

procedure TfrmZoneProperties.FormShow(Sender: TObject);
begin
  RemoveIncludedItemsFromMeshLibraryList;
end;

procedure TfrmZoneProperties.btnHelpClick(Sender: TObject);
begin
  Application.HelpJump('Editing_overall_zone_properties');
end;

procedure TfrmZoneProperties.btnAddClick(Sender: TObject);
Var St,St1: String;

  Function FindMesh(St: String): Integer;
  Var
    I     : Integer;
    Found : Boolean;
    St1   : String;

  Begin
    I     := 0;
    Found := False;
    St    := UpperCase(St);
    While (I < lbZoneExtraMeshes.Items.Count) And Not Found Do
    Begin
      St1 := UpperCase(lbZoneExtraMeshes.Items.Strings[I]);
      If Copy(St1,1,Length(St) + 1) = St + '/' Then Found := True Else Inc(I);
    End; // While
    If Found Then Result := I Else Result := -1;
  End; // FindMesh

  Function FindExportName(St: String): Integer;
  Var
    I     : Integer;
    Found : Boolean;
    St1   : String;

  Begin
    I     := 0;
    Found := False;
    St    := UpperCase(St);
    While (I < lbZoneExtraMeshes.Items.Count) And Not Found Do
    Begin
      St1 := UpperCase(lbZoneExtraMeshes.Items.Strings[I]);
      If Copy(St1,Length(St1) - Length(St),Length(St) + 1) = '/' + St Then Found := True Else Inc(I);
    End; // While
    If Found Then Result := I Else Result := -1;
  End; // FindExportName

begin
  If lbMeshLibrary.ItemIndex >= 0 Then
  Begin
    St := lbMeshLibrary.Items.Strings[lbMeshLibrary.ItemIndex];
    If FindMesh(St) < 0 Then
    Begin
      St1 := St;
      If InputQuery('Add Mesh','Enter export name:',St1) Then
      Begin
        St1 := UpperCase(Trim(St1));
        If St1 <> '' Then
        Begin
          If FindExportName(St1) < 0 Then
          Begin
            lbZoneExtraMeshes.Items.Add(St + '/' + St1);
            lbMeshLibrary.DeleteSelected;
          End
          Else ShowMessage('That name is already listed.  Please choose another');
        End
        Else ShowMessage('You must enter a name or it cannot be exported.');
      End;
    End
    Else ShowMessage('You have already added that mesh.');
  End;
end;

procedure TfrmZoneProperties.btnRemoveClick(Sender: TObject);
Var
  I,J  : Integer;
  Done : Boolean;
  St,S : String;

begin
  If lbZoneExtraMeshes.ItemIndex >= 0 Then
  Begin
    I    := 0;
    Done := False;
    S    := lbZoneExtraMeshes.Items.Strings[lbZoneExtraMeshes.ItemIndex];
    J    := Pos('/',S);
    If J > 0 Then S := Copy(S,1,J - 1);
    J    := MeshLibrary.IndexOf(S);
    If J >= 0 Then
    Begin
      While (I < lbMeshLibrary.Count) And Not Done Do
      Begin
        St := lbMeshLibrary.Items.Strings[I];
        If MeshLibrary.IndexOf(St) > J Then Done := True Else Inc(I);
      End; // While
      If Done
       Then lbMeshLibrary.Items.Insert(I,S)
       Else lbMeshLibrary.Items.Add(S);
    End;
    lbZoneExtraMeshes.DeleteSelected;
  End;
end;

procedure TfrmZoneProperties.btnAddCreatureClick(Sender: TObject);
Var St,St1: String;

  Function FindCreature(St: String): Integer;
  Var
    I     : Integer;
    Found : Boolean;
    St1   : String;

  Begin
    I     := 0;
    Found := False;
    St    := UpperCase(St);
    While (I < lbZoneExtraCreatures.Items.Count) And Not Found Do
    Begin
      St1 := UpperCase(lbZoneExtraCreatures.Items.Strings[I]);
      If Copy(St1,6,Length(St1)) = St Then Found := True Else Inc(I);
    End; // While
    If Found Then Result := I Else Result := -1;
  End; // FindCreature

  Function FindPrefix(St: String): Integer;
  Var
    I     : Integer;
    Found : Boolean;
    St1   : String;

  Begin
    I     := 0;
    Found := False;
    St    := UpperCase(St);
    While (I < lbZoneExtraCreatures.Items.Count) And Not Found Do
    Begin
      St1 := UpperCase(lbZoneExtraCreatures.Items.Strings[I]);
      If Copy(St1,1,Length(St)) = St Then Found := True Else Inc(I);
    End; // While
    If Found Then Result := I Else Result := -1;
  End; // FindPrefix

begin
  If lbCreatureLibrary.ItemIndex >= 0 Then
  Begin
    St := lbCreatureLibrary.Items.Strings[lbCreatureLibrary.ItemIndex];
    If FindCreature(St) < 0 Then
    Begin
      St1 := '';
      If InputQuery('Add creature','Enter creature prefix (e.g. ELF, ELM, GOB, etc.):',St1) Then
      Begin
        St1 := UpperCase(Trim(St1));
        If St1 <> '' Then
        Begin
          If FindPrefix(St1) < 0 Then
          Begin
            lbZoneExtraCreatures.Items.Add(St1 + ', ' + St);
            lbCreatureLibrary.DeleteSelected;
          End
          Else ShowMessage('That prefix is already listed.  Please choose another');
        End
        Else ShowMessage('You must enter a creature prefix or it cannot be exported.');
      End;
    End;
  End;
end;

procedure TfrmZoneProperties.btnRemoveCreatureClick(Sender: TObject);
Var
  I,J  : Integer;
  Done : Boolean;
  St,S : String;

begin
  If lbZoneExtraCreatures.ItemIndex >= 0 Then
  Begin
    I    := 0;
    Done := False;
    S    := lbZoneExtraCreatures.Items.Strings[lbZoneExtraCreatures.ItemIndex];
    S    := Copy(S,6,Length(S));
    J    := CreatureLibrary.IndexOf(S);
    If J >= 0 Then
    Begin
      While (I < lbCreatureLibrary.Count) And Not Done Do
      Begin
        St := lbCreatureLibrary.Items.Strings[I];
        If CreatureLibrary.IndexOf(St) > J Then Done := True Else Inc(I);
      End; // While
      If Done
       Then lbCreatureLibrary.Items.Insert(I,S)
       Else lbCreatureLibrary.Items.Add(S);
    End;
    lbZoneExtraCreatures.DeleteSelected;
  End;
end;

end.
