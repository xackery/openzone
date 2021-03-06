unit TB2DsgnConverter;

{
  Toolbar2000
  Copyright (C) 1998-2002 by Jordan Russell
  All rights reserved.
  For conditions of distribution and use, see LICENSE.TXT.

  $jrsoftware: tb2k/Source/TB2DsgnConverter.pas,v 1.13 2002/11/15 00:15:07 jr Exp $
}

interface

{$I TB2Ver.inc}

uses
  Windows, SysUtils, Classes, Controls, Forms, Menus, StdCtrls,
  TB2Item;

type
  TTBConverterForm = class(TForm)
    MessageList: TListBox;
    CloseButton: TButton;
    CopyButton: TButton;
    procedure CloseButtonClick(Sender: TObject);
    procedure CopyButtonClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  end;

procedure DoConvert(const ParentItem: TTBCustomItem; const Owner: TComponent);

implementation

{$R *.DFM}

uses
  Clipbrd, TB2DsgnConvertOptions;

procedure DoConvert(const ParentItem: TTBCustomItem; const Owner: TComponent);
const
  SPropNotTransferred = 'Warning: %s property not transferred on ''%s''.';
var
  ConverterForm: TTBConverterForm;

  procedure Log(const S: String);
  begin
    ConverterForm.MessageList.Items.Add(S);
    ConverterForm.MessageList.TopIndex := ConverterForm.MessageList.Items.Count-1; 
    ConverterForm.Update;
  end;

  procedure Recurse(MenuItem: TMenuItem; TBItem: TTBCustomItem);
  var
    I: Integer;
    Src: TMenuItem;
    IsSep, IsSubmenu: Boolean;
    Dst: TTBCustomItem;
    N: String;
  begin
    for I := 0 to MenuItem.Count-1 do begin
      Src := MenuItem[I];
      IsSep := (Src.Caption = '-');
      IsSubmenu := False;
      if not IsSep then begin
        if Src.Count > 0 then
          IsSubmenu := True;
        if not IsSubmenu then
          Dst := TTBItem.Create(Owner)
        else
          Dst := TTBSubmenuItem.Create(Owner);
        Dst.Action := Src.Action;
        Dst.Caption := Src.Caption;
        Dst.Checked := Src.Checked;
        if Src.Default then
          Dst.Options := Dst.Options + [tboDefault];
        Dst.Enabled := Src.Enabled;
        Dst.HelpContext := Src.HelpContext;
        Dst.ImageIndex := Src.ImageIndex;
        Dst.ShortCut := Src.ShortCut;
        {$IFDEF JR_D5}
        Dst.SubMenuImages := Src.SubMenuImages;
        {$ENDIF}
        Dst.OnClick := Src.OnClick;
      end
      else begin
        Dst := TTBSeparatorItem.Create(Owner);
      end;
      Dst.Hint := Src.Hint;
      Dst.Tag := Src.Tag;
      Dst.Visible := Src.Visible;
      if not IsSep then
        { Temporarily clear the menu item's OnClick property, so that renaming
          the menu item doesn't cause the function name to change }
        Src.OnClick := nil;
      try
        N := Src.Name;
        Src.Name := N + '_OLD';
        Dst.Name := N;
      finally
        if not IsSep then
          Src.OnClick := Dst.OnClick;
      end;
      TBItem.Add(Dst);
      if Src.GroupIndex <> 0 then
        Log(Format(SPropNotTransferred, ['GroupIndex', Dst.Name]));
      if Src.RadioItem then
        Log(Format(SPropNotTransferred, ['RadioItem', Dst.Name]));
      {$IFDEF JR_D5}
      if @Src.OnAdvancedDrawItem <> nil then
        Log(Format(SPropNotTransferred, ['OnAdvancedDrawItem', Dst.Name]));
      {$ENDIF}
      if @Src.OnDrawItem <> nil then
        Log(Format(SPropNotTransferred, ['OnDrawItem', Dst.Name]));
      if @Src.OnMeasureItem <> nil then
        Log(Format(SPropNotTransferred, ['OnMeasureItem', Dst.Name]));
      if IsSubmenu then
        Recurse(Src, Dst);
    end;
  end;

var
  OptionsForm: TTBConvertOptionsForm;
  I: Integer;
  C: TComponent;
  Menu: TMenu;
begin
  Menu := nil;
  OptionsForm := TTBConvertOptionsForm.Create(Application);
  try
    for I := 0 to Owner.ComponentCount-1 do begin
      C := Owner.Components[I];
      if (C is TMenu) and not(C is TTBPopupMenu) then
        OptionsForm.MenuCombo.Items.AddObject(C.Name, C);
    end;
    if OptionsForm.MenuCombo.Items.Count = 0 then
      raise Exception.Create('Could not find any menus on the form to convert');
    OptionsForm.MenuCombo.ItemIndex := 0;
    if (OptionsForm.ShowModal <> mrOK) or (OptionsForm.MenuCombo.ItemIndex < 0) then
      Exit;
    Menu := TMenu(OptionsForm.MenuCombo.Items.Objects[OptionsForm.MenuCombo.ItemIndex]);
  finally
    OptionsForm.Free;
  end;
  ParentItem.SubMenuImages := Menu.Images;
  ConverterForm := TTBConverterForm.Create(Application);
  ConverterForm.Show;
  ConverterForm.Update;
  Log(Format('Converting ''%s'', please wait...', [Menu.Name]));
  ParentItem.ViewBeginUpdate;
  try
    Recurse(Menu.Items, ParentItem);
  finally
    ParentItem.ViewEndUpdate;
  end;
  Log('Done!');
  ConverterForm.CloseButton.Enabled := True;
  ConverterForm.CopyButton.Enabled := True;
end;


{ TTBConverterForm }

procedure TTBConverterForm.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  Action := caFree;
end;

procedure TTBConverterForm.CloseButtonClick(Sender: TObject);
begin
  Close;
end;

procedure TTBConverterForm.CopyButtonClick(Sender: TObject);
begin
  Clipboard.AsText := MessageList.Items.Text;
end;


procedure FreeConverterForms;
var
  I: Integer;
  Form: TCustomForm;
label Restart;
begin
  Restart:
  for I := 0 to Screen.CustomFormCount-1 do begin
    Form := Screen.CustomForms[I];
    if Form is TTBConverterForm then begin
      Form.Free;
      goto Restart;
    end;
  end;
end;

initialization
finalization
  FreeConverterForms;
end.
