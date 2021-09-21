unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, ComCtrls, AviPlay;

type
  TForm1 = class(TForm)
    OpenDialog1: TOpenDialog;
    Panel1: TPanel;
    Button1: TButton;
    Button5: TButton;
    Button2: TButton;
    Edit1: TEdit;
    Button6: TButton;
    CheckSound: TCheckBox;
    Image1: TImage;
    CheckTransparent: TCheckBox;
    CheckStretch: TCheckBox;
    CheckCenter: TCheckBox;
    Button4: TButton;
    OpenDialog2: TOpenDialog;
    Label1: TLabel;
    AVI: TDCAVIPlay;
    procedure Button2Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure CheckSoundClick(Sender: TObject);
    procedure CheckTransparentClick(Sender: TObject);
    procedure CheckStretchClick(Sender: TObject);
    procedure CheckCenterClick(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    avifile: PAVIFile;
    stream : PAVIStream;
    { Private declarations }
    function  OpenAVIFile : boolean;
    procedure CloseAVIFile;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

  pgetframe : pointer;
  avifilename : string;
implementation
  uses unit2;

{$R *.DFM}

function TForm1.OpenAVIFile : boolean;
begin
  result := false;
  if avifilename = '' then
    exit;
  AviFileInit;
  if (AVIFileOpen(@avifile, @(avifilename[1]), 0, nil) <> 0)
   or (AVIFileGetStream(avifile, @stream, 0, 0) <> 0) then
     begin
       result := false;
       exit;
     end;
  result := true;
end;

procedure TForm1.CloseAVIFile;
begin
  AVIStreamRelease(stream);
  AVIFileRelease(avifile);
  AviFileExit;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
 with AVI do
   Play(0, FrameCount - 1, 0);
end;


procedure TForm1.Button1Click(Sender: TObject);
begin
  if OpenDialog1.Execute then
    begin
      Caption := avifilename;
      avifilename := OpenDialog1.Filename;
      AVI.Filename := avifilename;
    end;
end;

procedure TForm1.Button5Click(Sender: TObject);
begin
  AVI.Stop;
end;

procedure TForm1.Button6Click(Sender: TObject);
begin
  try
    AVI.Seek(StrToInt(Edit1.Text));
  except
    exit;
  end;
end;

procedure TForm1.CheckSoundClick(Sender: TObject);
begin
  AVI.PlaySound := CheckSound.Checked;
end;

procedure TForm1.CheckTransparentClick(Sender: TObject);
begin
  AVI.Transparent := CheckTransparent.Checked;
end;

procedure TForm1.CheckStretchClick(Sender: TObject);
begin
  AVI.Stretch := CheckStretch.Checked;
end;

procedure TForm1.CheckCenterClick(Sender: TObject);
begin
  AVI.Center := CheckCenter.Checked;
end;

procedure TForm1.Button4Click(Sender: TObject);
var
  info        : TAVIFileInfo;
  sinfo       : TAVIStreamInfo;
  forminfo    : TForm2;
  videostream : PAVIStream;
  audiostream : PAVIStream;

  procedure _add;
  begin
    with forminfo.ListBox1.Items, sinfo do
      begin
        Add('fccType ' + IntToStr(fccType));
        Add('fccHandler ' + IntToStr(fccHandler));
        Add('dwFlags ' + IntToStr(dwFlags));
        Add('dwCaps ' + IntToStr(dwCaps));
        Add('wPriority ' + IntToStr(wPriority));
        Add('wLanguage ' + IntToStr(wLanguage));
        Add('dwScale ' + IntToStr(dwScale));
        Add('dwRate ' +IntToStr(dwRate));
        Add('dwStart ' + IntToStr(dwStart));
        Add('dwLength ' + IntToStr(dwLength));
        Add('dwInitialFrames ' + IntToStr(dwInitialFrames));
        Add('dwSuggestedBufferSize ' + IntToStr(dwSuggestedBufferSize));
        Add('dwQuality ' + IntToStr(dwQuality));
        Add('dwSampleSize ' + IntToStr(dwSampleSize));
        with rcFrame do
          Add('Frame : (' + IntToStr(left) + ', ' + IntToStr(Top) + ')-('+IntToStr(right) + ', ' + IntToStr(Bottom) + ')');

        Add('dwEditCount ' + IntToStr(dwEditCount));
        Add('dwFormatChangeCount ' + IntToStr(dwFormatChangeCount));
        Add(szName);
      end;
  end;

begin
  if not OpenAVIFile then
    exit;

  AVIFileInfo(avifile, @info, sizeof(info));
  forminfo := TForm2.Create(nil);
  try
    with forminfo.ListBox1.Items, info do
      begin
        Add('File information');
        Add('================');
        Add('dwMaxBytesPerSec ' + IntToStr(dwMaxBytesPerSec));
        Add('dwFlags ' + IntToStr(dwFlags));
        Add('dwCaps ' + IntToStr(dwCaps));
        Add('dwStreams ' + IntToStr(dwStreams));
        Add('dwSuggestedBufferSize ' + IntToStr(dwSuggestedBufferSize));
        Add('dwWidth ' + IntToStr(dwWidth));
        Add('dwHeight ' + IntToStr(dwHeight));
        Add('dwScale ' + IntToStr(dwScale));
        Add('dwRate ' + IntToStr(dwRate));
        Add('dwLength ' + IntToStr(dwLength));
        Add('dwEditCount ' + IntToStr(dwEditCount));
        Add(szFileType);

        if AVIFileGetStream(avifile, @videostream, streamtypeVIDEO, 0) = 0 then
          begin
            Add('');
            Add('Video Stream');
            Add('============');
            AVIStreamInfo(videostream, @sinfo, sizeof(sinfo));
            _add;
            AVIStreamRelease(videostream);
          end;

        if AVIFileGetStream(avifile, @audiostream, streamtypeAUDIO, 0) = 0 then
          begin
            Add('');
            Add('Audio Stream');
            Add('============');
            AVIStreamInfo(audiostream, @sinfo, sizeof(sinfo));
            _add;
            AVIStreamRelease(audiostream);
          end;

        forminfo.ShowModal;
      end;
  finally
    forminfo.Free;
  end;

  CloseAVIFile;
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
  if OpenDialog2.Execute then
    begin
      Image1.Picture.LoadFromFile(OpenDialog2.FileName);
    end;
end;


procedure TForm1.FormCreate(Sender: TObject);
begin
  OpenDialog1.Filter := 'AVI files|*.avi'; 
end;

end.

