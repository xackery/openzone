unit CPButton;

interface

uses
  Winprocs, Wintypes, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons;

type
  TColorEvent=procedure(sender:TObject; color:TColor) of object;

  TColorPickButton = class(TBitBtn)
  {Click displays a color dialog. The OnChangeColor event can be used to
  assign the picked color to a color property of an object. The button's
  glyph is colored with the picked color. I'm assuming these buttons have
  no captions }
  private
    fOnChangeColor:TColorEvent;
    fColorDialog:TColorDialog;
    fColor:TColor;
    fversion:string;
    procedure setversion(x:string);
    { Private declarations }
  protected

    { Protected declarations }
  public
    constructor create(AOwner:TComponent); override;
    destructor destroy; override;
    procedure Click; override;
    {Click bring up a color dialog, then calls setcolor}

    procedure setcolor(color:TColor);
    {Sets the color of the button's glyph to color.
    OnChangeColor is triggered if the color is new.}

    { Public declarations }
  published
  property version:string read fversion write setversion;
  property OnChangeColor:TColorEvent read fOnChangeColor write fOnChangeColor;
  {Use this event to assign color to some object's property of
  type TColor. It's a good idea to check whether the object exists
  before doing so.}
  property color:TColor read fColor write setcolor;
  {The color displayed by the button }
  

    { Published declarations }
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('Samples', [TColorPickButton]);
end;

constructor TColorPickButton.create;
begin
  inherited create(AOwner);
  fColorDialog:=TColorDialog.create(nil);
  fColorDialog.Options:=[cdFullOpen];
  fColor:=3;
  fColorDialog.color:=fcolor;
  fVersion:='0.7 August 99';
end;

destructor TColorPickButton.destroy;
begin
  fColorDialog.Free;
  inherited destroy;
end;

procedure TColorPickButton.click;
begin
  inherited click;
  If fColorDialog.execute then
    setcolor(fColorDialog.color);
end;

procedure TColorPickButton.setcolor;
var bitmap:TBitmap;
begin
  if (fcolor=3) or (color<>fcolor) then
  begin
    Bitmap:=TBitmap.Create;
    Try
      Bitmap.height:=height-4; Bitmap.width:=width-4;
      with Bitmap do
      begin
        {$IFDEF WIN32}
        if color=clwhite then
        TransparentColor:=clblack;
        {$ENDIF}
        canvas.brush.color:=color;
        canvas.fillrect(rect(0,0,width-1,height-1));
      end;
      glyph:=Bitmap;
      fcolor:=color;
      fcolordialog.color:=color;
      if assigned(fOnChangeColor) then fOnChangeColor(self,Color);
    finally
      Bitmap.free;
    end;
  end;
end;

procedure TColorPickButton.setversion;
begin
end;

end.
