object frmStatus: TfrmStatus
  Left = 1444
  Top = 1314
  BorderStyle = bsNone
  Caption = 'frmStatus'
  ClientHeight = 65
  ClientWidth = 468
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  FormStyle = fsStayOnTop
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 120
  TextHeight = 16
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 468
    Height = 65
    Align = alClient
    TabOrder = 0
    DesignSize = (
      468
      65)
    object lblStatus: TClearTypeLabel
      Left = 8
      Top = 39
      Width = 453
      Height = 20
      Anchors = [akLeft, akTop, akRight]
      AutoSize = False
      Caption = 'lblStatus'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      ClearType = DataModule1.ClearTypeText
    end
    object pbStatus: TProgressBar
      Left = 8
      Top = 8
      Width = 453
      Height = 20
      Anchors = [akLeft, akTop, akRight]
      Min = 0
      Max = 100
      TabOrder = 0
    end
  end
end
