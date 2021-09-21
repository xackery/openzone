object frmMountainize: TfrmMountainize
  Left = 506
  Top = 200
  Width = 261
  Height = 237
  Caption = 'Mountainize Edges'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  Scaled = False
  PixelsPerInch = 120
  TextHeight = 16
  object Label1: TClearTypeLabel
    Left = 8
    Top = 88
    Width = 119
    Height = 16
    Caption = 'Mountainize amount:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Arial'
    Font.Style = []
    ParentFont = False
    ClearType = DataModule1.ClearTypeText
  end
  object cbNorth: TClearTypeCheckBox
    Left = 88
    Top = 8
    Width = 73
    Height = 17
    Caption = 'North'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Arial'
    Font.Style = []
    ParentFont = False
    TabOrder = 0
    ClearType = DataModule1.ClearTypeText
  end
  object cbSouth: TClearTypeCheckBox
    Left = 88
    Top = 56
    Width = 73
    Height = 17
    Caption = 'South'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Arial'
    Font.Style = []
    ParentFont = False
    TabOrder = 1
    ClearType = DataModule1.ClearTypeText
  end
  object cbEast: TClearTypeCheckBox
    Left = 152
    Top = 32
    Width = 73
    Height = 17
    Caption = 'East'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Arial'
    Font.Style = []
    ParentFont = False
    TabOrder = 2
    ClearType = DataModule1.ClearTypeText
  end
  object cbWest: TClearTypeCheckBox
    Left = 24
    Top = 32
    Width = 73
    Height = 17
    Caption = 'West'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Arial'
    Font.Style = []
    ParentFont = False
    TabOrder = 3
    ClearType = DataModule1.ClearTypeText
  end
  object btnOk: TIAEverButton
    Left = 24
    Top = 168
    Width = 88
    Height = 32
    Caption = 'OK'
    Default = True
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Arial'
    Font.Style = []
    ModalResult = 1
    ParentFont = False
    TabOrder = 4
    ButtonAngle = 0
    ButtonWidth = 88
    ButtonHeight = 32
    CaptionLeft = 45
    CaptionAngle = 0
    ButtonDepth = 3
    CaptionHAlign = haNone
    MainBitmap.Data = {
      F6060000424DF606000000000000360000002800000018000000180000000100
      180000000000C006000011170000111700000000000000000000B2B2B2B2B2B2
      B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2A7AEA683A2826D9A6B5D955A5D95
      5A6D9A6B83A282A7AEA6B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B4
      B4B4B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B295A8944F914C24891D199E0A
      1AAC071CB7051CB7051AAC07199E0A258C1D55935295A894B2B2B2B2B2B2B2B2
      B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2AEB0AE6397601E8D151B
      B4051ECD001FD2001FD2001FD2001FD2001FD2001FD2001ECD001BB4051E8D15
      639760AEB0AEB2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2AAAFAA468F
      4219A2091ECE001FD2001EC8011ECC011FD2001FD2001FD2001FD2001FD2001F
      D2001FD2001ECE001AA508468F42AAAFAAB2B2B2B2B2B2B2B2B2B2B2B2B2B2B2
      AEB0AE468F421BAE071ED1001FD2001ECA011D94111B990D1ECE001FD2001FD2
      001FD2001FD2001FD2001FD2001FD2001ED1001BAE07468F42AEB0AEB2B2B2B2
      B2B2B2B2B2B2B2B26397601AA5081ED1001FD2001EC8011B920E6AC17161BE69
      1C9E0C1ECF001FD2001FD2001FD2001FD2001FD2001FD2001FD2001ED10019A2
      09639760B2B2B2B2B2B2B2B2B295A8941E8D151ECE001FD2001ECC011C930F73
      C178BBFFCDB1FFC665C16E1C9C0D1ECC011FD2001FD2001FD2001FD2001FD200
      1FD2001FD2001ECE001E8D1595A894B2B2B2B2B2B25593521BB4051FD2001ECA
      011B930F7EC180CDFDD9C5FFD4BBFFCDAFFDC365C16E1B930E1ECA011FD2001F
      D2001FD2001FD2001FD2001FD2001FD2001BB4054F914CB2B2B2A7AEA6258C1D
      1ECD001ECD001C990E82BE81E1FFE9D9FFE3CFFFDCC4FFD4BBFFCDB1FFC661BE
      691B970D1ECF001FD2001FD2001FD2001FD2001FD2001FD2001ECD0024891DA7
      AEA683A282199E0A1ECD001C901091C18EF6FFF8EBFFF1E3FFEAD9FFE3CFFFDC
      C5FFD4BBFFCDB1FFC666C16F1C9C0D1EC9011FD2001FD2001FD2001FD2001FD2
      001FD200199E0A83A2826D9A6B1AAC071CB20642953CEDF5EDFDFFFDF6FFF8ED
      FFF2BCE6C1B3E6BACFFFDCC5FFD4BBFFCDB1FDC566C16F1B970D1ECA011FD200
      1FD2001FD2001FD2001FD2001AAC076D9A6B5D955A1CB7051FD2001BA40860A6
      5BF4F8F3FDFFFDCDE6CD2F93272D8E27B3E6BACFFFDCC4FFD4BCFFCEB3FFC762
      BE6A1B930E1ECC011FD2001FD2001FD2001FD2001CB7055D955A5D955A1CB705
      1FD2001ECF001BA50858A153C5DEC32E94241DC4021CBB042A8E23B0E4B7CFFF
      DCC7FFD6BDFFCEB3FFC766C16F1B900F1ECC011FD2001FD2001FD2001CB7055D
      955A6D9A6B1AAC071FD2001FD2001ED0001BA90720891720C20624D30624D305
      21BA092C8F25B3E6BAD0FFDDC7FFD6BDFFCEB1FDC566C16F1B930E1ECA011FD2
      001FD2001AAC076D9A6B83A282199E0A1FD2001FD2002DD41050DB3869D75775
      E36172E25F69E15561DF4B4BC4392F8E29B5E6BBD1FFDDC7FFD5BDFFCEB3FFC7
      62BE6A1B930E1ECC011FD200199E0A83A282A7AEA624891D3FD32681E570ABEE
      9FA5ED989BEB8C91E98187E7767CE56A72E25F67E0524BC4392F8F28B2E4B8D1
      FFDDC7FFD6BDFFCEB3FFC754B1591A9E0A1ECD00258C1DA7AEA6B2B2B2559352
      85C97DC0F2B8B6F0ACACEEA1A2EC9598EA8A8FE87F84E6737AE4676EE25B64E0
      4F49C2372E8F28B5E6BBD1FFDDC7FFD694DFA026961D1DC6021BB405559352B2
      B2B2B2B2B295A8943B9336C2F0BABFF2B6B4F0AAAAEE9EA1EC9396EA888CE87C
      82E67176E4646CE15863DF4D49C3362F8E29B5E6BBAAE3B225921C1DC6021ECE
      001E8A1695A894B2B2B2B2B2B2B2B2B263976067B261C5F3BDBDF1B3B2EFA7A8
      ED9C9FEB9194E9858AE77A7FE56D74E3616AE15661DF4B47C3352A8C222C9423
      33CD1A21D1031AA508639760B2B2B2B2B2B2B2B2B2B2B2B2AEB0AE4B904778C0
      70C1F2B8BBF1B1B0EFA5A6ED9A9CEB8E92E98387E7767CE56A72E35F69E0545F
      DE4848C63444CE2E40D82626B013468F42AEB0AEB2B2B2B2B2B2B2B2B2B2B2B2
      B2B2B2AAAFAA488F4460B458BCEFB4B8F1AEAEEFA3A4ED989AEA8C8FE87F84E6
      737AE46870E25D66E0515DDE4650D8392CA61E478F43AAAFAAB2B2B2B2B2B2B2
      B2B2B2B2B2B2B2B2B2B2B2B2B2B2AEB0AE639760338E2E83C97CADEAA4ACEEA1
      A2EC9597EA888CE87C82E67178E46669DC5649BD39288F20639760AEB0AEB2B2
      B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B295A89456
      9353348F2E53AA4B67BB5E73C8686BC75F56B84A3FA6352C8B2656935295A894
      B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2
      B2B2B2B2B2B2B2B2B2B2A7AEA683A2826D9A6B5D955A5D955A6D9A6B83A282A7
      AEA6B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B7B7B7}
    Transparent = True
    BitmapLeft = 6
    BitmapHAlign = haNone
    BitmapVAlign = vaCenter
    UserRGNAUTO = True
    RotationPointX = 0
    RotationPointY = 0
    Rotated = False
    CaptionFixed = False
    GradientFixed = False
    GradientBitmapLine = 0
    Caption3dKind = ckSimple
    RadiusRatio = 0.5
    ArcAngle = 2.0943951023932
    ShowFocusRGN = False
    ClearType = DataModule1.ClearTypeText
  end
  object btnCancel: TIAEverButton
    Left = 152
    Top = 168
    Width = 88
    Height = 32
    Cancel = True
    Caption = 'Cancel'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Arial'
    Font.Style = []
    ModalResult = 2
    ParentFont = False
    TabOrder = 5
    ButtonAngle = 0
    ButtonWidth = 88
    ButtonHeight = 32
    CaptionLeft = 40
    CaptionAngle = 0
    ButtonDepth = 3
    CaptionHAlign = haNone
    MainBitmap.Data = {
      F6060000424DF606000000000000360000002800000018000000180000000100
      180000000000C006000011170000111700000000000000000000B2B2B2B2B2B2
      B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2A6A7AE8184A26B6E9B595E95595E
      956B6E9B8184A2A6A7AEB2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B4
      B4B4B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B29495A84B51921C248F090FB0
      070AC60507D60507D6070AC6090FB01C23935156949495A8B2B2B2B2B2B2B2B2
      B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2AEAEB0606497141B9505
      08D10001F70000FF0000FF0000FF0000FF0000FF0000FF0001F70508D1141B95
      606497AEAEB0B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2AAAAAF4248
      91080DB70000FA0000FB0000FF0000FF0000FF0000FF0000FF0000FF0000FF00
      00FF0000FB0000FA080CBB424891AAAAAFB2B2B2B2B2B2B2B2B2B2B2B2B2B2B2
      AEAEB0424891070BC80000FD0102F00A0FAD0203ED0000FF0000FF0000FF0000
      FF0000FF0000FF0305DF090FAF0101F40000FD070BC8424891AEAEB0B2B2B2B2
      B2B2B2B2B2B2B2B2606497080CBB0000FD0000FA1016A3686DBA1B22960204E8
      0000FF0000FF0000FF0000FF0102F11A21935B60BA0F16A00000FA0000FD080D
      B7606497B2B2B2B2B2B2B2B2B29495A8141B950000FA0102F31016A39093D1D2
      D2FFA0A2DF161D980204E70000FF0000FF0203ED19209A9597E3B6B6FF7A7DD1
      10179A0102F30000FA141B959495A8B2B2B2B2B2B25156940508D10000FB090F
      AF7175BADBDBFFD8D8FFD5D5FFA1A4DE1F26940305DF0203ED1A219D9395DDBF
      BFFFBCBCFFB9B9FF5E63BA090EB00000FF0508D14B5192B2B2B2A6A7AE1C2393
      0001F70000FF0305E11D2494ADAFDFDFDFFFDBDBFFD8D8FFA6A8DF1C23941B21
      9A9EA0DFC9C9FFC5C5FFC2C2FF9396E019209A0203ED0000FF0001F71C248FA6
      A7AE8184A2090FB00000FF0000FF0000FF0102F11D2494A9ABDBE2E2FFDFDFFF
      DCDCFFAEB0E3ACADE3D2D2FFCFCFFFCBCBFF9C9EDF1A219A0102F10000FF0000
      FF0000FF090FB08184A26B6E9B070AC60000FF0000FF0000FF0000FF0305DF21
      2894B3B5DFE5E5FFE2E2FFDFDFFFDBDBFFD8D8FFD5D5FFA3A5DF1D249E0203ED
      0000FF0000FF0000FF0000FF070AC66B6E9B595E950507D60000FF0000FF0000
      FF0000FF0000FF0305DF1E2590BCBEE3E8E8FFE5E5FFE2E2FFDFDFFFB0B2E31B
      229A0203ED0000FF0000FF0000FF0000FF0000FF0507D6595E95595E950507D6
      0000FF0000FF0000FF0000FF0000FF0203ED1D249AC2C4E3EFEFFFECECFFE8E8
      FFE5E5FFB7B9E31D25900305E10000FF0000FF0000FF0000FF0000FF0507D659
      5E956B6E9B070AC60000FF0000FF0000FF0000FF0203ED21289EC2C4DFF8F8FF
      F5F5FFF1F1FFEEEEFFEBEBFFE8E8FFB2B4DF20279A0203ED0000FF0000FF0000
      FF0000FF070AC66B6E9B8184A2090FB00000FF0000FF1010FF2A2BF11E259AC5
      C7DFFEFEFFFDFDFFFBFBFFC7C9E3C4C6E3F1F1FFEEEEFFEBEBFFB4B6DF1C239A
      0102F10000FF0000FF0000FF090FB08184A2A6A7AE1C248F2626F77070FF7E80
      E12F3694C4C6DFFFFFFFFFFFFFFFFFFFC5C7DF2A319A29309ABFC1DFF5F5FFF1
      F1FFEFEFFFBFC0E31D24970204E70000FF0001F71C2393A6A7AEB2B2B2515694
      7D80D1B4B4FB4F54B07F84BAFFFFFFFFFFFFFFFFFFC5C7DF33399E5051ED4748
      ED2E349EC1C3DFF8F8FFF5F5FFF1F1FF787DBA090EB20000FF0508D1515694B2
      B2B2B2B2B29495A8353C95BABAFAABACF63B419FAAAED1FFFFFFCCCEE331389A
      6364ED6464FF5858FF4042E1262E94C2C5DFFBFBFFA6A9D11118A30101F60000
      FA151C919495A8B2B2B2B2B2B2B2B2B26064976065B7BDBDFDA8A9F5363D9C7F
      84BA343B9A797AF17A7AFF6D6DFF6161FF5656FF4546F1262D947F84BA1C22A3
      1B1BFA0303FD080CBB606497B2B2B2B2B2B2B2B2B2B2B2B2AEAEB0464C917074
      C8B8B8FDA1A2F14C51B08789ED8E8EFF8383FF7676FF6A6AFF5F5FFF5454FF3E
      40E52328B23030F62626FD1316C8424891AEAEB0B2B2B2B2B2B2B2B2B2B2B2B2
      B2B2B2AAAAAF434991585CBBB3B4FAAEAEFFA3A3FF9898FF8C8CFF7F7FFF7373
      FF6868FF5D5DFF5151FF4646FF393AFA1D22B7424991AAAAAFB2B2B2B2B2B2B2
      B2B2B2B2B2B2B2B2B2B2B2B2B2B2AEAEB06064972D35917B7ED1A3A4F7A1A1FF
      9595FF8888FF7C7CFF7171FF6666FF5657F7383CD1202795606497AEAEB0B2B2
      B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B29495A853
      58942D34934B50B05D62C6676AD65F62D64A4EC6343AB0262D8F5257949495A8
      B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2
      B2B2B2B2B2B2B2B2B2B2A6A7AE8184A26B6E9B595E95595E956B6E9B8184A2A6
      A7AEB2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B7B7B7}
    Transparent = True
    BitmapLeft = 6
    BitmapHAlign = haNone
    BitmapVAlign = vaCenter
    UserRGNAUTO = True
    RotationPointX = 0
    RotationPointY = 0
    Rotated = False
    CaptionFixed = False
    GradientFixed = False
    GradientBitmapLine = 0
    Caption3dKind = ckSimple
    RadiusRatio = 0.5
    ArcAngle = 2.0943951023932
    ShowFocusRGN = False
    ClearType = DataModule1.ClearTypeText
  end
  object cbGrow: TClearTypeCheckBox
    Left = 8
    Top = 136
    Width = 209
    Height = 17
    Caption = 'Grow zone outward'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Arial'
    Font.Style = []
    ParentFont = False
    TabOrder = 6
    ClearType = DataModule1.ClearTypeText
  end
  object edtAmount: TClearTypeEdit
    Left = 144
    Top = 88
    Width = 81
    Height = 24
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Arial'
    Font.Style = []
    ParentFont = False
    TabOrder = 7
    ClearType = DataModule1.ClearTypeText
  end
end
