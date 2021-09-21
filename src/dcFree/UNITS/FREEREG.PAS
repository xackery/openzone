unit freereg;

interface
{$I dc.inc}
uses
  classes, Aviplay, Diffmake, FormFill, TrCtrls;

procedure Register;

implementation


uses
  {$IFDEF D6}
  DesignIntf, DesignEditors, VCLEditors,
  {$ELSE}
  dsgnintf,
  {$ENDIF}
  dialogs
  ;

{$R free.dcr}

type
  { Property editor for filename property }
  TFileNameEdit = class(TPropertyEditor)
  public
    function  GetAttributes : TPropertyAttributes; override;
    function  GetValue : string; override;
    procedure SetValue(const val : string); override;
    procedure Edit; override;

    {ovveride these function to change filter or Default extension of
     TOpendDialog  which will be show when user clicks elipsis}
    function  GetDefaultExtension : string; virtual;
    function  GetFilter : string; virtual;
  end;

  { Property editor for filename property with AVI extension}
  TAVIFileNameEdit = class(TFileNameEdit)
    function  GetDefaultExtension : string; override;
    function  GetFilter : string; override;
  end;

  TFillTypeEditor=Class(TStringProperty)
  public
    function GetAttributes:TPropertyAttributes;override;
    procedure GetValues(Proc:TGetStrProc);override;
  end;


{------------------------------------------------------------------}

procedure TFileNameEdit.Edit;
begin
  with TOpenDialog.Create(nil) do
    try
      DefaultExt := GetDefaultExtension;
      Filter := GetFilter;
      if Execute then
        SetStrValue(FileName);
    finally
      Free;
    end;
end;

{------------------------------------------------------------------}

function TFileNameEdit.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog];
end;

{------------------------------------------------------------------}

function TFileNameEdit.GetValue : string;
begin
  result := GetStrValue;
end;

{------------------------------------------------------------------}

procedure TFileNameEdit.SetValue(const val : string);
begin
  SetStrValue(val);
end;

{------------------------------------------------------------------}

function  TFileNameEdit.GetDefaultExtension : string;
begin
  result := '';
end;

{------------------------------------------------------------------}

function  TFileNameEdit.GetFilter : string;
begin
  result := 'All files|*.*';
end;

{******************************************************************}

function  TAVIFileNameEdit.GetDefaultExtension : string;
begin
  result := 'avi';
end;

{------------------------------------------------------------------}

function  TAVIFileNameEdit.GetFilter : string;
begin
  result := 'AVI files|*.avi';
end;

{------------------------------------------------------------------}

function TFillTypeEditor.GetAttributes:TPropertyAttributes;
begin
  Result:=[paValueList];
end;

{--------------------------------------------------------------}

procedure TFillTypeEditor.GetValues(Proc:TGetStrProc);
Var
  i:Integer;
begin
  For i:=0 to FillTypeList.Count-1 do Proc(FillTypeList[i]);
end;

{---------------------------------------------------------------------}

procedure FormFillRegister;
begin
  RegisterComponents('Dream Company',[TDCFormFill]);
  RegisterPropertyEditor(TypeInfo(String),TDCFormFill,'FillType',TFillTypeEditor);
end;

{--------------------------------------------------------------}

procedure DiffMakerRegister;
begin
  RegisterComponents('Dream Company',[TDiffMaker]);
  RegisterPropertyEditor(TypeInfo(string), TDiffMaker, 'InFile', TFileNameEdit);
  RegisterPropertyEditor(TypeInfo(string), TDiffMaker, 'OutFile', TFileNameEdit);
  RegisterPropertyEditor(TypeInfo(string), TDiffMaker, 'UseFile', TFileNameEdit);
end;

{---------------------------------------------------------------------}


procedure AviRegister;
begin
  RegisterComponents('Dream Company',[TDCAVIPlay]);
  RegisterPropertyEditor(TypeInfo(string), TDCAVIPlay, 'FileName', TAVIFileNameEdit);
end;

{---------------------------------------------------------------------}

procedure Register;
begin
 AviRegister;
 DiffMakerRegister;
 FormFillRegister;
 registerComponents('Dream Company', [TTrRadioButton ,TTrGroupBox, TTrCheckBox, TTrRadioGroup]);
end;

end.

