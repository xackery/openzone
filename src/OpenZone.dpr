program OpenZone;

uses
  FastMM4,
  Forms,
  Windows,
  MemCheck,
  idGlobal,
  SysUtils,
  frmMainUnit in 'frmMainUnit.pas' {frmMain},
  ZoneClasses in 'ZoneClasses.pas',
  frmNewZoneUnit in 'frmNewZoneUnit.pas' {frmNewZone},
  ZoneFile in 'ZoneFile.pas',
  frmScriptLogUnit in 'frmScriptLogUnit.pas' {frmScriptLog},
  EQWldData in 'EQWldData.pas',
  Expressions in 'Expressions.pas',
  frmStatusUnit in 'frmStatusUnit.pas' {frmStatus},
  frmWaterPropertiesUnit in 'frmWaterPropertiesUnit.pas' {frmWaterProperties},
  frmEditTextureListUnit in 'frmEditTextureListUnit.pas' {frmEditTextureList},
  Model_3DSMax in 'Model_3DSMax.PAS',
  S3D in 'S3D.pas',
  Ethernet in 'Ethernet.pas',
  Targa in 'TARGA.PAS',
  frmTexPickerUnit in 'frmTexPickerUnit.pas' {frmTexPicker},
  frmTranslateUnit in 'frmTranslateUnit.pas' {frmTranslate},
  frmMountainizeUnit in 'frmMountainizeUnit.pas' {frmMountainize},
  OpenIL in 'openil.pas',
  OpenILU in 'openilu.pas',
  OpenILUT in 'openilut.pas',
  frmConvertBMPUnit in 'frmConvertBMPUnit.pas' {frmConvertBMP},
  frmExtendGroundUnit in 'frmExtendGroundUnit.pas' {frmExtendGround},
  frmGenerateMapUnit in 'frmGenerateMapUnit.pas' {frmGenerateMap},
  frmZonePropertiesUnit in 'frmZonePropertiesUnit.pas' {frmZoneProperties},
  frmPreferencesUnit in 'frmPreferencesUnit.pas' {frmPreferences},
  frmShiftZoneUnit in 'frmShiftZoneUnit.pas' {frmShiftZone},
  frmAddZoneExitUnit in 'frmAddZoneExitUnit.pas' {frmAddZoneExit},
  frmSplashUnit in 'frmSplashUnit.pas' {frmSplash},
  frmChooseTextureUnit in 'frmChooseTextureUnit.pas' {frmTextureChooser},
  frmEditPolygonPropertiesUnit in 'frmEditPolygonPropertiesUnit.pas' {frmEditPolygonProperties},
  XWFFiles in 'XWFFiles.pas',
  OZDMUnit in 'OZDMUnit.pas' {DataModule1: TDataModule},
  frmXWFExportOptionsUnit in 'frmXWFExportOptionsUnit.pas' {frmXWFExportOptions},
  Anim8or in 'Anim8or.pas',
  frmFlipTexCoordsUnit in 'frmFlipTexCoordsUnit.pas' {frmFlipTexCoords},
  frmMultiplyTexCoordsUnit in 'frmMultiplyTexCoordsUnit.pas' {frmMultiplyTexCoords},
  frmSelectHotspotUnit in 'frmSelectHotspotUnit.pas' {frmSelectHotSpot},
  frmDeleteAN8ObjectsUnit in 'frmDeleteAN8ObjectsUnit.pas' {frmDeleteAN8Objects},
  Threads in '..\GL Visir\TGLVisir\Threads.pas';

{$R *.res}

begin
//  MemChk;

//  LoadLibrary('OPENGL32');
//  LoadLibrary('ATIOGLXX');
  Application.Initialize;
  Application.HelpFile := 'OPENZONE.HLP';
  Application.CreateForm(TfrmMain, frmMain);
  Application.CreateForm(TfrmSplash, frmSplash);
  Application.CreateForm(TfrmTextureChooser, frmTextureChooser);
  Application.CreateForm(TfrmEditPolygonProperties, frmEditPolygonProperties);
  Application.CreateForm(TfrmXWFExportOptions, frmXWFExportOptions);
  Application.CreateForm(TfrmFlipTexCoords, frmFlipTexCoords);
  Application.CreateForm(TfrmMultiplyTexCoords, frmMultiplyTexCoords);
  Application.CreateForm(TfrmSelectHotSpot, frmSelectHotSpot);
  Application.CreateForm(TDataModule1, DataModule1);
  Application.CreateForm(TfrmDeleteAN8Objects, frmDeleteAN8Objects);
  frmMain.SplashShowTime := GetTickCount;
  frmMain.SetClearTypeSettings;
  frmSplash.Show;
  Application.ProcessMessages;
  Application.CreateForm(TfrmNewZone, frmNewZone);
  Application.CreateForm(TfrmScriptLog, frmScriptLog);
  Application.CreateForm(TfrmStatus, frmStatus);
  Application.CreateForm(TfrmWaterProperties, frmWaterProperties);
  Application.CreateForm(TfrmEditTextureList, frmEditTextureList);
  Application.CreateForm(TfrmTexPicker, frmTexPicker);
  Application.CreateForm(TfrmTranslate, frmTranslate);
  Application.CreateForm(TfrmMountainize, frmMountainize);
  Application.CreateForm(TfrmConvertBMP, frmConvertBMP);
  Application.CreateForm(TfrmExtendGround, frmExtendGround);
  Application.CreateForm(TfrmGenerateMap, frmGenerateMap);
  Application.CreateForm(TfrmZoneProperties, frmZoneProperties);
  Application.CreateForm(TfrmPreferences, frmPreferences);
  Application.CreateForm(TfrmShiftZone, frmShiftZone);
  Application.CreateForm(TfrmAddZoneExit, frmAddZoneExit);
  Application.Run;
end.
