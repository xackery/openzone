object frmTexPicker: TfrmTexPicker
  Left = 541
  Top = 350
  AutoScroll = False
  Caption = 'Ground Editor'
  ClientHeight = 715
  ClientWidth = 959
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  Scaled = False
  OnActivate = FormActivate
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnDeactivate = FormDeactivate
  OnMouseDown = FormMouseDown
  OnMouseMove = FormMouseMove
  OnMouseUp = FormMouseUp
  OnPaint = FormPaint
  OnResize = FormResize
  OnShow = FormShow
  PixelsPerInch = 120
  TextHeight = 16
  object Splitter1: TSplitter
    Left = 212
    Top = 0
    Width = 8
    Height = 649
    Cursor = crHSplit
    Beveled = True
    ResizeStyle = rsUpdate
    OnMoved = Splitter1Moved
  end
  object pnlLeft: TPanel
    Left = 0
    Top = 0
    Width = 212
    Height = 649
    Align = alLeft
    BevelOuter = bvNone
    TabOrder = 0
    object grpCurrentTexture: TClearTypeGroupBox
      Left = 0
      Top = 0
      Width = 212
      Height = 113
      Align = alTop
      Caption = 'Current texture'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      TabOrder = 0
      ClearType = DataModule1.ClearTypeText
      object imgTexture: TImage
        Left = 24
        Top = 20
        Width = 64
        Height = 64
        Stretch = True
      end
      object btnRotateLeft: TSpeedButton
        Left = 112
        Top = 32
        Width = 40
        Height = 40
        Hint = 'Rotate left'
        Glyph.Data = {
          360C0000424D360C000000000000360000002800000020000000200000000100
          180000000000000C000011170000111700000000000000000000B2B2B2B2B2B2
          B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2
          B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2
          B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B6B6B6B2B2B2B2B2B2
          B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2
          B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2
          B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2
          B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2
          B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2
          B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2
          B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2
          B2B2B2B2B2B2B2B2B2B2B0A8A8B0A8A8B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2
          B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2
          B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2AC95
          95A15C5C972D2D941A1A941A1A941A1A941A1A993636A46F6FB0A8A8B2B2B2B2
          B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2
          B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2AE9F9F9F5353941A
          1A951C1C941D1D921C1C8F1C1C8D1B1B8D1A1A8E1A1A921A1A952323A26666B2
          B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2
          B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2AA8C8C9523239A1E1E9E22
          229E22229C22229A2121982020961F1F941E1E921D1D8F1C1C8F1B1B931A1A99
          3636B0A8A8B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2
          B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2A67979941A1AA32323AA2828A927
          27A92E2EB44D4DBF6969BD6868B86060A333339A2121982020961F1F941D1D94
          1B1B972D2DAE9F9FB2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2
          B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2AE9F9F941A1AA72424B42C2CB12B2BBD50
          50CA7373C26262B74747BC5555C46D6DD29393C57676A632329E22229C22229A
          2121951C1C993636B0A8A8B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2
          B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2A36666961B1BB42A2AC55151B53E
          3E9D1F1F941A1A941A1A941A1A981C1CA12121B34040C77373AD3737A42525A2
          2424A02323961C1CA15C5CB2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2
          B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B29F5353961B1BA72424941A
          1A9F5353AA8C8CAA8C8CAA8C8CA46F6F972D2D951B1BA72525BE5353AD2C2CAA
          2828A92727A42424941A1AAE9F9FB2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2
          B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B29F5353952323AC95
          95B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2A26666941A1AA92828B93C3CB1
          2B2BAF2A2AAD29299D1F1F9D4949B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2
          B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B0A8A8B2B2
          B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2A67979961B1BB83232BA
          2E2EB82D2DB62C2CAD2828952323B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2
          B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2
          B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B29B4040A52323C2
          3232C03131BE3030BC2F2F961B1BAA8C8CB2B2B2B2B2B2B2B2B2B2B2B2B2B2B2
          B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2
          B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2A88282971B1BCB
          3636C83535C63434C433339F1F1FA67979B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2
          B2B2B2B0A8A8AA8C8CAC9595B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2
          B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2AA8C8C941A1AD3
          3939D13838CF3737CD3636A12020A36666B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2
          B2B2B2B2B2B2941A1A941A1A941A1A972D2D9B4040A15C5CA36666AA8C8CAC95
          95B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2AA8C8C941A1AE0
          5454DD4848D93D3DD73B3BA42121A36666B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2
          B2B2B2B2B2B2972D2DD17070DD7F7FD77575C05656BA4D4DAA36369F2828941A
          1A941A1A972D2D9B4040A46F6FB2B2B2B2B2B2B2B2B2B2B2B29F5353AD3232E5
          6666E56363E45D5DE04747A62222A88282B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2
          B2B2B2B2B2B29B4040C15C5CEF9D9DEE9B9BEE9898ED9696ED9292EC8F8FEC8C
          8CD56E6E992020993636B0A8A8B2B2B2B2B2B2B2B2B2AC9595941A1AD25C5CE7
          6F6FE66B6BE66969E56464941A1AAC9595B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2
          B2B2B2B2B2B2A36666B64E4EF0A4A4EFA2A2EF9F9FEF9D9DEE9B9BEE9898E78D
          8DA43030993636B2B2B2B2B2B2B2B2B2B2B2B2AA8C8C952323B94444E97A7AE8
          7777E87474E77272CC5151972D2DB2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2
          B2B2B2B2B2B2A46F6FAB3E3EF1ADADF1AAAAF0A7A7F0A4A4EFA1A1EF9F9FEF9D
          9DC15B5B941A1A993636A36666A366669B4040941A1AC45757EA8383EA8181EA
          7E7EE97C7CE97A7AA72A2AA26666B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2
          B2B2B2B2B2B2AA8C8C992323F3B5B5F2B2B2F2AFAFF1ADADF0AAAAF0A7A7F0A4
          A4EFA1A1E38E8EC76464B04343AA3A3AC15858E18383EC8F8FEB8B8BEB8989EB
          8686EA8585CF6060952323B0A8A8B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2
          B2B2B2B2B2B2AE9F9F941A1AF4BDBDF4BBBBF3B8B8F3B5B5F2B2B2F2AFAFF1AC
          ACF0A9A9F0A7A7F0A4A4EFA1A1EF9F9FEE9C9CEE9A9AEE9797ED9494ED9191ED
          9696E68686992020A46F6FB2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2
          B2B2B2B2B2B2B2B2B2941A1ADD9999F5C1C1F4BFBFF4BDBDF3BABAF3B7B7F3B4
          B4F2B1B1F2AFAFF1ACACF0A9A9F0A7A7EFA3A3EFA0A0EF9E9EEF9F9FF1A9A9E9
          9595A430309F5353B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2
          B2B2B2B2B2B2B2B2B2993636D79494F5C1C1DA8989F6C6C6F5C4C4F4BFBFF4BC
          BCF3BABAF3B7B7F3B4B4F2B1B1F2AFAFF1ACACF1ACACF2B6B6F3BABADF8C8CA5
          32329D4949B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2
          B2B2B2B2B2B2B2B2B29B4040BF6B6BAF4040941A1ABC5D5DF0BFBFF7CFCFF7CF
          CFF7CCCCF6C7C7F5C3C3F6C7C7F6C8C8F7CECEF7CECEEDAFAFC26161941A1AA2
          6666B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2
          B2B2B2B2B2B2B2B2B2A36666941A1A993636AC95959B40409A2525BF6868DC96
          96F5C6C6F8D3D3F8D2D2F7CFCFEFBABAD58787B755559923239B4040AC9595B2
          B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2
          B2B2B2B2B2B2B2B2B2A46F6F9F5353B0A8A8B2B2B2B2B2B2AA8C8C9D49499523
          23941A1A941A1A941A1A941A1A941A1A972D2DA15C5CAC9595B2B2B2B2B2B2B2
          B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2
          B2B2B2B2B2B2B2B2B2B0A8A8B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2
          B2AE9F9FAA8C8CAA8C8CAA8C8CB2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2
          B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2
          B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2
          B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2
          B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2
          B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2
          B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2
          B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2
          B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2
          B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2
          B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2BBBBBB}
        ParentShowHint = False
        ShowHint = True
        OnClick = btnRotateLeftClick
      end
      object btnRotateRight: TSpeedButton
        Left = 160
        Top = 32
        Width = 40
        Height = 40
        Hint = 'Rotate right'
        Glyph.Data = {
          360C0000424D360C000000000000360000002800000020000000200000000100
          180000000000000C000011170000111700000000000000000000B2B2B2B2B2B2
          B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2
          B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2
          B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2
          B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2
          B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2
          B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2
          B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2
          B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2
          B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2
          B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2
          B2B2B2B2B0A8A8B0A8A8B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2
          B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2
          B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2AE9F9FA46F6F9936
          36941A1A941A1A941A1A941A1A972D2DA15C5CAC9595B2B2B2B2B2B2B2B2B2B2
          B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2
          B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2A26666952323921A1A8E1A
          1A8D1A1A8E1B1B901C1C931C1C941D1D951C1C941A1A9F5353AE9F9FB2B2B2B2
          B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2
          B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B0A8A8993636931A1A901B1B901C1C921D
          1D941E1E961F1F9820209A21219C22229E22229F22229A1E1E952323AA8C8CB2
          B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2
          B2B2B2B2B2B2B2B2B2B2B2B2AC9595972D2D941B1B941D1D961F1F9820209A21
          21A43333B96060BD6868BF6969B54D4DAA2E2EA92727AA2828A32323941A1AA6
          7979B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2
          B2B2B2B2B2B2B2B2B2B0A8A8993636961C1C9A21219C22229E2222A63232C576
          76D29393C46D6DBC5555B74747C36262CA7373BD5050B22B2BB42C2CA7242494
          1A1AAC9595B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2
          B2B2B2B2B2B2B2B2B2A15C5C971C1CA02323A22424A52525AD3737C77373B341
          41A12121981C1C941A1A941A1A941A1A9D1F1FB63E3EC55151B42A2A961B1BA4
          6F6FB2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2
          B2B2B2B2B2B2AE9F9F941A1AA52424A92727AA2828AD2C2CBE5353A82525951B
          1B972D2DA46F6FAA8C8CAA8C8CAA8C8C9F5353941A1AA72424961B1B9F5353B2
          B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2
          B2B2B2B2B2B29F53539D1F1FAD2929B02A2AB22B2BBA3C3CA92828941A1AA266
          66B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2AC95959523239F5353B2B2B2B2
          B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2
          B2B2B2B2B2B2952323AE2828B62C2CB82D2DBA2F2FB83232961B1BA67979B2B2
          B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2AE9F9FB2B2B2B2B2B2B2
          B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2
          B2B2B2AA8C8C961B1BBC2F2FBE3030C03131C23232A523239D4949B2B2B2B2B2
          B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2
          B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2
          B2B2B2A679799F2020C43333C73434C93535CB3636971B1BA88282B2B2B2B2B2
          B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2
          B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2
          B2B2B2A36666A22121CD3636CF3737D13838D43939941A1AAC9595B2B2B2B2B2
          B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2
          B2B2B2B2B2B2B2B2AC9595A88282B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2
          B2B2B2A36666A42222D73B3BDA3E3EDE4949E15454941A1AAA8C8CB2B2B2B2B2
          B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2AC9595AA8C8CA36666A15C5C9B
          4040972D2D941A1A941A1A941A1AB2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2
          B2B2B2A88282A62323E04747E45D5DE56464E66767AD32329F5353B2B2B2B2B2
          B2B2B2B2B2B2B2A679799B4040972D2D941A1A941A1A9F2828AA3737BA4E4EC0
          5656D77676DD7F7FD27171993636B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2
          B2B2B2AC9595941A1AE56565E66969E76D6DE76F6FD25C5C941A1AAC9595B2B2
          B2B2B2B2B2B2B2B0A8A89936369E2727D66F6FEC8D8DEC9090ED9494ED9696EE
          9999EE9C9CEF9D9DC15D5D9B4040B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2
          B2B2B2B2B2B2972D2DCC5252E77272E87575E87878E97A7AB94545952323AA8C
          8CB2B2B2B2B2B2B2B2B2B0A8A8972D2DA43030E88E8EEE9898EE9C9CEF9D9DEF
          A0A0EFA2A2F0A5A5B64E4EA36666B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2
          B2B2B2B2B2B2A26666A72A2AE97A7AE97D7DEA7F7FEA8181EA8484C55757941A
          1A9B4040A36666A36666993636941A1AC15B5BEF9D9DEF9F9FEFA2A2F0A5A5F0
          A8A8F1ABABF1ADADAB3F3FA46F6FB2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2
          B2B2B2B2B2B2B0A8A8952323CF6161EA8686EB8787EB8989EC8C8CEC8F8FE183
          83C15959AA3A3AB04343C76464E38F8FEFA2A2F0A4A4F0A7A7F1ABABF1ADADF2
          B0B0F2B3B3F3B5B5992323AA8C8CB2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2
          B2B2B2B2B2B2B2B2B2A46F6F992020E68787ED9797ED9292ED9595EE9898EE9A
          9AEF9D9DEF9F9FEFA2A2F0A4A4F0A7A7F1AAAAF1ADADF2AFAFF2B3B3F3B5B5F3
          B9B9F4BBBBF4BDBD941A1AAE9F9FB2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2
          B2B2B2B2B2B2B2B2B2B2B2B29F5353A43030E99595F1AAAAEFA0A0EF9F9FEFA1
          A1F0A4A4F0A7A7F1AAAAF1ADADF2AFAFF2B2B2F3B5B5F3B8B8F4BBBBF4BDBDF5
          BFBFF5C2C2DD9999941A1AB2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2
          B2B2B2B2B2B2B2B2B2B2B2B2B0A8A89D4949A53232DF8D8DF4BABAF2B6B6F1AD
          ADF1ADADF2AFAFF2B2B2F3B5B5F3B8B8F4BBBBF4BDBDF4BFBFF5C5C5F6C6C6DA
          8A8AF5C2C2D79494993636B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2
          B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2A26666941A1AC26262EDAFAFF7CE
          CEF7CECEF6C8C8F6C8C8F5C3C3F6C7C7F7CCCCF7D0D0F7CFCFF0C0C0BC5E5E94
          1A1AAF4141BF6B6B9B4040B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2
          B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2AC95959B4040992323B755
          55D58787EFBABAF7D0D0F8D2D2F8D3D3F6C7C7DC9696BF68689A25259B4040AC
          9595972D2D941A1AA36666B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2
          B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2AC9595A15C
          5C972D2D941A1A941A1A941A1A941A1A941A1A9523239D4949AA8C8CB2B2B2B2
          B2B2B0A8A89F5353A46F6FB2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2
          B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2
          B2B2B2B2B2B2B2AA8C8CAA8C8CAA8C8CAE9F9FB2B2B2B2B2B2B2B2B2B2B2B2B2
          B2B2B2B2B2B2B2B2B0A8A8B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2
          B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2
          B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2
          B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2
          B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2
          B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2
          B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B6B6B6B2B2B2
          B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2
          B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2
          B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2BBBBBB}
        ParentShowHint = False
        ShowHint = True
        OnClick = btnRotateRightClick
      end
      object lblCurrentTex: TClearTypeLabel
        Left = 8
        Top = 88
        Width = 77
        Height = 16
        Caption = 'lblCurrentTex'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
        ClearType = DataModule1.ClearTypeText
      end
    end
    object GroupBox2: TClearTypeGroupBox
      Left = 0
      Top = 113
      Width = 212
      Height = 536
      Align = alClient
      Caption = 'Texture list'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      TabOrder = 1
      ClearType = DataModule1.ClearTypeText
      object pcGroundEditor: TPageControl
        Left = 2
        Top = 18
        Width = 208
        Height = 516
        ActivePage = tbNames
        Align = alClient
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        MultiLine = True
        ParentFont = False
        TabIndex = 0
        TabOrder = 0
        OnChange = pcGroundEditorChange
        object tbNames: TTabSheet
          Caption = 'Names'
          object lbTextures: TClearTypeListBox
            Left = 0
            Top = 0
            Width = 200
            Height = 398
            Style = lbOwnerDrawFixed
            Align = alClient
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -13
            Font.Name = 'Arial'
            Font.Style = []
            ItemHeight = 16
            ParentFont = False
            TabOrder = 0
            OnClick = lbTexturesClick
            OnDblClick = lbTexturesDblClick
            ClearType = DataModule1.ClearTypeText
          end
          object Panel5: TPanel
            Left = 0
            Top = 398
            Width = 200
            Height = 45
            Align = alBottom
            BevelOuter = bvLowered
            TabOrder = 1
            object Label16: TClearTypeLabel
              Left = 4
              Top = 8
              Width = 374
              Height = 33
              AutoSize = False
              Caption = 
                'Hint: Repeatedly clicking on a'#13#10'name in the list rotates it for ' +
                'you.'
              Font.Charset = DEFAULT_CHARSET
              Font.Color = clWindowText
              Font.Height = -13
              Font.Name = 'Arial'
              Font.Style = []
              ParentFont = False
              ClearType = DataModule1.ClearTypeText
            end
          end
        end
        object tbIcons: TTabSheet
          Caption = 'Icons'
          ImageIndex = 1
          object dgTextures: TDrawGrid
            Left = 0
            Top = 0
            Width = 200
            Height = 398
            Align = alClient
            DefaultRowHeight = 64
            DefaultDrawing = False
            FixedCols = 0
            FixedRows = 0
            GridLineWidth = 0
            Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goThumbTracking]
            ScrollBars = ssVertical
            TabOrder = 0
            OnClick = dgTexturesClick
            OnDblClick = dgTexturesDblClick
            OnDrawCell = dgTexturesDrawCell
          end
          object Panel6: TPanel
            Left = 0
            Top = 398
            Width = 200
            Height = 45
            Align = alBottom
            BevelOuter = bvLowered
            TabOrder = 1
            object Label17: TClearTypeLabel
              Left = 4
              Top = 8
              Width = 354
              Height = 33
              AutoSize = False
              Caption = 'Hint: Repeatedly clicking on a'#13#10'texture icon rotates it for you.'
              Font.Charset = DEFAULT_CHARSET
              Font.Color = clWindowText
              Font.Height = -13
              Font.Name = 'Arial'
              Font.Style = []
              ParentFont = False
              ClearType = DataModule1.ClearTypeText
            end
          end
        end
        object tbBounds: TTabSheet
          Caption = 'Bounds'
          ImageIndex = 2
          object Panel2: TPanel
            Left = 0
            Top = 0
            Width = 200
            Height = 73
            Align = alTop
            BevelOuter = bvNone
            TabOrder = 0
            object ToolBar1: TToolBar
              Left = 0
              Top = 0
              Width = 200
              Height = 29
              AutoSize = True
              ButtonHeight = 30
              ButtonWidth = 31
              Caption = 'ToolBar1'
              Flat = True
              Images = ilBoundsEditor
              ParentShowHint = False
              ShowHint = True
              TabOrder = 0
              object tbBoundsSelect: TToolButton
                Left = 0
                Top = 0
                Hint = 'Select boundary'
                Caption = 'tbBoundsSelect'
                Down = True
                Grouped = True
                ImageIndex = 0
                Style = tbsCheck
              end
              object tbBoundsDraw: TToolButton
                Left = 31
                Top = 0
                Hint = 'Draw new boundary'
                Caption = 'tbBoundsDraw'
                Grouped = True
                ImageIndex = 1
                Style = tbsCheck
              end
              object ToolButton1: TToolButton
                Left = 62
                Top = 0
                Width = 8
                Caption = 'ToolButton1'
                ImageIndex = 1
                Style = tbsSeparator
              end
              object tbBoundsSnapToGrid: TToolButton
                Left = 70
                Top = 0
                Hint = 'Snap to grid'
                AllowAllUp = True
                Caption = 'tbBoundsSnapToGrid'
                ImageIndex = 2
                Style = tbsCheck
              end
              object tbBoundsSnapToEndpoints: TToolButton
                Left = 101
                Top = 0
                Hint = 'Snap to endpoints'
                AllowAllUp = True
                Caption = 'tbBoundsSnapToEndpoints'
                ImageIndex = 3
                Style = tbsCheck
              end
            end
            object ToolBar2: TToolBar
              Left = 0
              Top = 32
              Width = 200
              Height = 29
              AutoSize = True
              ButtonHeight = 30
              ButtonWidth = 31
              Caption = 'ToolBar2'
              Flat = True
              Images = ilBoundsEditor
              ParentShowHint = False
              ShowHint = True
              TabOrder = 1
              object tbBoundsUndo: TToolButton
                Left = 0
                Top = 0
                Hint = 'Undo'
                Caption = 'tbBoundsUndo'
                ImageIndex = 4
                OnClick = tbBoundsUndoClick
              end
              object tbBoundsRedo: TToolButton
                Left = 31
                Top = 0
                Hint = 'Redo'
                Caption = 'tbBoundsRedo'
                ImageIndex = 5
                OnClick = tbBoundsRedoClick
              end
              object ToolButton5: TToolButton
                Left = 62
                Top = 0
                Width = 8
                Caption = 'ToolButton5'
                ImageIndex = 3
                Style = tbsSeparator
              end
              object tbBoundsDelete: TToolButton
                Left = 70
                Top = 0
                Hint = 'Delete boundary'
                Caption = 'tbBoundsDelete'
                ImageIndex = 6
                OnClick = tbBoundsDeleteClick
              end
            end
          end
          object GroupBox1: TClearTypeGroupBox
            Left = 8
            Top = 72
            Width = 185
            Height = 169
            Caption = 'Heights'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -13
            Font.Name = 'Arial'
            Font.Style = []
            ParentFont = False
            TabOrder = 1
            ClearType = DataModule1.ClearTypeText
            object Label11: TClearTypeLabel
              Left = 16
              Top = 32
              Width = 22
              Height = 16
              Caption = 'Top'
              Font.Charset = DEFAULT_CHARSET
              Font.Color = clWindowText
              Font.Height = -13
              Font.Name = 'Arial'
              Font.Style = []
              ParentFont = False
              ClearType = DataModule1.ClearTypeText
            end
            object Label12: TClearTypeLabel
              Left = 16
              Top = 72
              Width = 41
              Height = 16
              Caption = 'Bottom'
              Font.Charset = DEFAULT_CHARSET
              Font.Color = clWindowText
              Font.Height = -13
              Font.Name = 'Arial'
              Font.Style = []
              ParentFont = False
              ClearType = DataModule1.ClearTypeText
            end
            object edtTop: TClearTypeEdit
              Left = 96
              Top = 32
              Width = 73
              Height = 24
              TabOrder = 0
              Text = '0'
              ClearType = DataModule1.ClearTypeText
            end
            object edtBottom: TClearTypeEdit
              Left = 96
              Top = 72
              Width = 73
              Height = 24
              TabOrder = 1
              Text = '0'
              ClearType = DataModule1.ClearTypeText
            end
            object cbInfinite: TClearTypeCheckBox
              Left = 16
              Top = 112
              Width = 97
              Height = 17
              Caption = 'Infinite'
              Font.Charset = DEFAULT_CHARSET
              Font.Color = clWindowText
              Font.Height = -13
              Font.Name = 'Arial'
              Font.Style = []
              ParentFont = False
              TabOrder = 2
              ClearType = DataModule1.ClearTypeText
            end
          end
        end
        object tbMeshes: TTabSheet
          Caption = 'Meshes'
          ImageIndex = 3
          object Splitter2: TSplitter
            Left = 0
            Top = 233
            Width = 200
            Height = 10
            Cursor = crVSplit
            Align = alBottom
            Beveled = True
            ResizeStyle = rsUpdate
            OnMoved = Splitter2Moved
          end
          object ToolBar3: TToolBar
            Left = 0
            Top = 0
            Width = 200
            Height = 33
            ButtonHeight = 30
            ButtonWidth = 31
            Caption = 'ToolBar3'
            Flat = True
            Images = ilBoundsEditor
            TabOrder = 0
            object tbMeshesSelect: TToolButton
              Left = 0
              Top = 0
              Hint = 'Select mesh object'
              Caption = 'tbMeshesSelect'
              Down = True
              Grouped = True
              ImageIndex = 0
              ParentShowHint = False
              ShowHint = True
              Style = tbsCheck
            end
            object tbMeshPlace: TToolButton
              Left = 31
              Top = 0
              Hint = 'Place new mesh object'
              Caption = 'tbMeshPlace'
              Grouped = True
              ImageIndex = 7
              ParentShowHint = False
              ShowHint = True
              Style = tbsCheck
            end
            object ToolButton2: TToolButton
              Left = 62
              Top = 0
              Width = 8
              Caption = 'ToolButton2'
              ImageIndex = 2
              Style = tbsSeparator
            end
            object tbMeshDelete: TToolButton
              Left = 70
              Top = 0
              Hint = 'Delete mesh object'
              Caption = 'tbMeshDelete'
              ImageIndex = 6
              ParentShowHint = False
              ShowHint = True
              OnClick = tbMeshDeleteClick
            end
            object ToolButton3: TToolButton
              Left = 101
              Top = 0
              Width = 8
              Caption = 'ToolButton3'
              ImageIndex = 7
              Style = tbsSeparator
            end
            object tbMeshVarySize: TToolButton
              Left = 109
              Top = 0
              Hint = 'Slightly vary the size of selected mesh objects'
              Caption = 'tbMeshVarySize'
              ImageIndex = 8
              ParentShowHint = False
              ShowHint = True
              OnClick = tbMeshVarySizeClick
            end
            object tbMeshVaryAngle: TToolButton
              Left = 140
              Top = 0
              Hint = 'Slightly vary the angle of selected mesh objects'
              Caption = 'tbMeshVaryAngle'
              ImageIndex = 9
              ParentShowHint = False
              ShowHint = True
              OnClick = tbMeshVaryAngleClick
            end
          end
          object Panel1: TPanel
            Left = 0
            Top = 33
            Width = 200
            Height = 200
            Align = alClient
            BevelOuter = bvNone
            TabOrder = 1
            object Panel3: TPanel
              Left = 0
              Top = 0
              Width = 200
              Height = 65
              Align = alTop
              BevelOuter = bvNone
              TabOrder = 0
              object Label2: TClearTypeLabel
                Left = 0
                Top = 49
                Width = 51
                Height = 16
                Align = alBottom
                Caption = 'Mesh list'
                Font.Charset = DEFAULT_CHARSET
                Font.Color = clWindowText
                Font.Height = -13
                Font.Name = 'Arial'
                Font.Style = []
                ParentFont = False
                ClearType = DataModule1.ClearTypeText
              end
              object lblSelectedMesh: TClearTypeLabel
                Left = 0
                Top = 4
                Width = 94
                Height = 16
                Caption = 'lblSelectedMesh'
                Font.Charset = ANSI_CHARSET
                Font.Color = clBlue
                Font.Height = -13
                Font.Name = 'Arial'
                Font.Style = [fsBold]
                ParentFont = False
                ClearType = DataModule1.ClearTypeText
              end
              object cbVaryAngle: TClearTypeCheckBox
                Left = 0
                Top = 24
                Width = 89
                Height = 17
                Caption = 'Vary angle'
                Font.Charset = DEFAULT_CHARSET
                Font.Color = clWindowText
                Font.Height = -13
                Font.Name = 'Arial'
                Font.Style = []
                ParentFont = False
                TabOrder = 0
                ClearType = DataModule1.ClearTypeText
              end
              object cbVarySize: TClearTypeCheckBox
                Left = 92
                Top = 24
                Width = 81
                Height = 17
                Caption = 'Vary size'
                Font.Charset = DEFAULT_CHARSET
                Font.Color = clWindowText
                Font.Height = -13
                Font.Name = 'Arial'
                Font.Style = []
                ParentFont = False
                TabOrder = 1
                ClearType = DataModule1.ClearTypeText
              end
            end
            object tvMeshes: TTreeView
              Left = 0
              Top = 65
              Width = 200
              Height = 135
              Align = alClient
              Indent = 19
              TabOrder = 1
              OnChange = tvMeshesChange
              OnCustomDraw = tvMeshesCustomDraw
              OnCustomDrawItem = tvMeshesCustomDrawItem
            end
          end
          object glvMesh: TGLVisir
            Left = 0
            Top = 243
            Width = 200
            Height = 200
            Align = alBottom
            Constraints.MinHeight = 200
            Constraints.MinWidth = 200
            ParentShowHint = False
            ShowHint = True
            TabOrder = 2
            BackgroundColor = clSilver
            PlaneColor = clWhite
            AxisNamesColor = clYellow
            Caption = '3D Model'
            AxisXName = 'X'
            AxisYName = 'Y'
            AxisZName = 'Z'
            ShowCaption = False
            ShowAnimation = False
            ShowMovement = False
            ShowDiapasones = False
            ShowTicks = False
          end
        end
        object tbZonePlanes: TTabSheet
          Caption = 'Zonelines'
          ImageIndex = 4
          object Label4: TClearTypeLabel
            Left = 80
            Top = 68
            Width = 29
            Height = 16
            Caption = 'ZMin'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -13
            Font.Name = 'Arial'
            Font.Style = []
            ParentFont = False
            ClearType = DataModule1.ClearTypeText
          end
          object Label5: TClearTypeLabel
            Left = 80
            Top = 96
            Width = 32
            Height = 16
            Caption = 'ZMax'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -13
            Font.Name = 'Arial'
            Font.Style = []
            ParentFont = False
            ClearType = DataModule1.ClearTypeText
          end
          object Label6: TClearTypeLabel
            Left = 0
            Top = 128
            Width = 112
            Height = 16
            Caption = 'Destination zone ID'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -13
            Font.Name = 'Arial'
            Font.Style = []
            ParentFont = False
            ClearType = DataModule1.ClearTypeText
          end
          object Label7: TClearTypeLabel
            Left = 0
            Top = 160
            Width = 9
            Height = 16
            Caption = 'X'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -13
            Font.Name = 'Arial'
            Font.Style = []
            ParentFont = False
            ClearType = DataModule1.ClearTypeText
          end
          object Label8: TClearTypeLabel
            Left = 0
            Top = 188
            Width = 9
            Height = 16
            Caption = 'Y'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -13
            Font.Name = 'Arial'
            Font.Style = []
            ParentFont = False
            ClearType = DataModule1.ClearTypeText
          end
          object Label9: TClearTypeLabel
            Left = 0
            Top = 216
            Width = 8
            Height = 16
            Caption = 'Z'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -13
            Font.Name = 'Arial'
            Font.Style = []
            ParentFont = False
            ClearType = DataModule1.ClearTypeText
          end
          object Label10: TClearTypeLabel
            Left = 0
            Top = 244
            Width = 48
            Height = 16
            Caption = 'Heading'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -13
            Font.Name = 'Arial'
            Font.Style = []
            ParentFont = False
            ClearType = DataModule1.ClearTypeText
          end
          object Bevel1: TBevel
            Left = 0
            Top = 124
            Width = 169
            Height = 13
            Shape = bsTopLine
          end
          object ToolBar4: TToolBar
            Left = 0
            Top = 0
            Width = 200
            Height = 29
            AutoSize = True
            ButtonHeight = 30
            ButtonWidth = 31
            Caption = 'ToolBar4'
            Flat = True
            Images = ilBoundsEditor
            TabOrder = 0
            object tbZonePlanesSelect: TToolButton
              Left = 0
              Top = 0
              Hint = 'Select zoneline'
              Caption = 'tbZonePlanesSelect'
              Down = True
              Grouped = True
              ImageIndex = 0
              ParentShowHint = False
              ShowHint = True
              Style = tbsCheck
            end
            object tbZonePlanesDraw: TToolButton
              Left = 31
              Top = 0
              Hint = 'Draw new zoneline'
              Caption = 'tbZonePlanesDraw'
              Grouped = True
              ImageIndex = 1
              ParentShowHint = False
              ShowHint = True
              Style = tbsCheck
            end
            object ToolButton7: TToolButton
              Left = 62
              Top = 0
              Width = 8
              Caption = 'ToolButton7'
              ImageIndex = 2
              Style = tbsSeparator
            end
            object tbZonePlanesSnapToGrid: TToolButton
              Left = 70
              Top = 0
              Hint = 'Snap to grid'
              AllowAllUp = True
              Caption = 'tbZonePlanesSnapToGrid'
              ImageIndex = 2
              ParentShowHint = False
              ShowHint = True
              Style = tbsCheck
            end
            object tbZonePlanesSnapToEndpoints: TToolButton
              Left = 101
              Top = 0
              Hint = 'Snap to endpoints'
              AllowAllUp = True
              Caption = 'tbZonePlanesSnapToEndpoints'
              ImageIndex = 3
              ParentShowHint = False
              ShowHint = True
              Style = tbsCheck
            end
          end
          object ToolBar5: TToolBar
            Left = 0
            Top = 32
            Width = 200
            Height = 29
            AutoSize = True
            ButtonHeight = 30
            ButtonWidth = 31
            Caption = 'ToolBar5'
            Flat = True
            Images = ilBoundsEditor
            TabOrder = 1
            object tbZonePlanesUndo: TToolButton
              Left = 0
              Top = 0
              Hint = 'Undo'
              Caption = 'tbZonePlanesUndo'
              ImageIndex = 4
              ParentShowHint = False
              ShowHint = True
              OnClick = tbZonePlanesUndoClick
            end
            object tbZonePlanesRedo: TToolButton
              Left = 31
              Top = 0
              Hint = 'Redo'
              Caption = 'tbZonePlanesRedo'
              ImageIndex = 5
              ParentShowHint = False
              ShowHint = True
              OnClick = tbZonePlanesRedoClick
            end
            object ToolButton12: TToolButton
              Left = 62
              Top = 0
              Width = 8
              Caption = 'ToolButton12'
              ImageIndex = 2
              Style = tbsSeparator
            end
            object tbZonePlanesDelete: TToolButton
              Left = 70
              Top = 0
              Hint = 'Delete zoneline'
              Caption = 'tbZonePlanesDelete'
              ImageIndex = 6
              ParentShowHint = False
              ShowHint = True
              OnClick = tbZonePlanesDeleteClick
            end
          end
          object cbZonePlaneInfiniteZ: TClearTypeCheckBox
            Left = 0
            Top = 68
            Width = 73
            Height = 17
            Caption = 'Infinite Z'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -13
            Font.Name = 'Arial'
            Font.Style = []
            ParentFont = False
            TabOrder = 2
            OnClick = cbZonePlaneInfiniteZClick
            ClearType = DataModule1.ClearTypeText
          end
          object edtZonePlaneMinZ: TClearTypeEdit
            Left = 120
            Top = 68
            Width = 49
            Height = 24
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -13
            Font.Name = 'Arial'
            Font.Style = []
            ParentFont = False
            TabOrder = 3
            Text = '0'
            OnChange = edtZonePlaneMinZChange
            ClearType = DataModule1.ClearTypeText
          end
          object edtZonePlaneMaxZ: TClearTypeEdit
            Left = 120
            Top = 96
            Width = 49
            Height = 24
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -13
            Font.Name = 'Arial'
            Font.Style = []
            ParentFont = False
            TabOrder = 4
            Text = '0'
            OnChange = edtZonePlaneMaxZChange
            ClearType = DataModule1.ClearTypeText
          end
          object edtZonePlaneZoneID: TClearTypeEdit
            Left = 128
            Top = 128
            Width = 41
            Height = 24
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -13
            Font.Name = 'Arial'
            Font.Style = []
            ParentFont = False
            TabOrder = 5
            Text = '0'
            OnChange = edtZonePlaneZoneIDChange
            ClearType = DataModule1.ClearTypeText
          end
          object edtZonePlaneDestX: TClearTypeEdit
            Left = 24
            Top = 160
            Width = 73
            Height = 24
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -13
            Font.Name = 'Arial'
            Font.Style = []
            ParentFont = False
            TabOrder = 6
            Text = '0'
            OnChange = edtZonePlaneDestXChange
            ClearType = DataModule1.ClearTypeText
          end
          object edtZonePlaneDestY: TClearTypeEdit
            Left = 24
            Top = 188
            Width = 73
            Height = 24
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -13
            Font.Name = 'Arial'
            Font.Style = []
            ParentFont = False
            TabOrder = 7
            Text = '0'
            OnChange = edtZonePlaneDestYChange
            ClearType = DataModule1.ClearTypeText
          end
          object edtZonePlaneDestZ: TClearTypeEdit
            Left = 24
            Top = 216
            Width = 73
            Height = 24
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -13
            Font.Name = 'Arial'
            Font.Style = []
            ParentFont = False
            TabOrder = 8
            Text = '0'
            OnChange = edtZonePlaneDestZChange
            ClearType = DataModule1.ClearTypeText
          end
          object edtZonePlaneDestHeading: TClearTypeEdit
            Left = 56
            Top = 244
            Width = 41
            Height = 24
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -13
            Font.Name = 'Arial'
            Font.Style = []
            ParentFont = False
            TabOrder = 9
            Text = '0'
            OnChange = edtZonePlaneDestHeadingChange
            ClearType = DataModule1.ClearTypeText
          end
          object cbIgnoreZonePlaneDestX: TClearTypeCheckBox
            Left = 104
            Top = 160
            Width = 65
            Height = 17
            Caption = 'Ignore'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -13
            Font.Name = 'Arial'
            Font.Style = []
            ParentFont = False
            TabOrder = 10
            OnClick = cbIgnoreZonePlaneDestXClick
            ClearType = DataModule1.ClearTypeText
          end
          object cbIgnoreZonePlaneDestY: TClearTypeCheckBox
            Left = 104
            Top = 188
            Width = 65
            Height = 17
            Caption = 'Ignore'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -13
            Font.Name = 'Arial'
            Font.Style = []
            ParentFont = False
            TabOrder = 11
            OnClick = cbIgnoreZonePlaneDestYClick
            ClearType = DataModule1.ClearTypeText
          end
          object cbIgnoreZonePlaneDestZ: TClearTypeCheckBox
            Left = 104
            Top = 216
            Width = 65
            Height = 17
            Caption = 'Ignore'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -13
            Font.Name = 'Arial'
            Font.Style = []
            ParentFont = False
            TabOrder = 12
            OnClick = cbIgnoreZonePlaneDestZClick
            ClearType = DataModule1.ClearTypeText
          end
          object cbIgnoreZonePlaneDestHeading: TClearTypeCheckBox
            Left = 104
            Top = 244
            Width = 65
            Height = 17
            Caption = 'Ignore'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -13
            Font.Name = 'Arial'
            Font.Style = []
            ParentFont = False
            TabOrder = 13
            OnClick = cbIgnoreZonePlaneDestHeadingClick
            ClearType = DataModule1.ClearTypeText
          end
        end
        object tbSounds: TTabSheet
          Caption = 'Sounds'
          ImageIndex = 5
          object ToolBar6: TToolBar
            Left = 0
            Top = 0
            Width = 200
            Height = 29
            AutoSize = True
            ButtonHeight = 30
            ButtonWidth = 31
            Caption = 'ToolBar6'
            Flat = True
            Images = ilBoundsEditor
            TabOrder = 0
            object tbSoundsSelect: TToolButton
              Left = 0
              Top = 0
              Hint = 'Select sound'
              Caption = 'tbSoundsSelect'
              Down = True
              Grouped = True
              ImageIndex = 0
              ParentShowHint = False
              ShowHint = True
              Style = tbsCheck
            end
            object tbSoundsDraw: TToolButton
              Left = 31
              Top = 0
              Hint = 'Place new sound'
              Caption = 'tbSoundsDraw'
              Grouped = True
              ImageIndex = 12
              ParentShowHint = False
              ShowHint = True
              Style = tbsCheck
            end
            object ToolButton8: TToolButton
              Left = 62
              Top = 0
              Width = 8
              Caption = 'ToolButton8'
              ImageIndex = 2
              Style = tbsSeparator
            end
            object tbSoundsSnapToGrid: TToolButton
              Left = 70
              Top = 0
              Hint = 'Snap to grid'
              AllowAllUp = True
              Caption = 'tbSoundsSnapToGrid'
              ImageIndex = 2
              ParentShowHint = False
              ShowHint = True
            end
          end
          object ToolBar7: TToolBar
            Left = 0
            Top = 32
            Width = 200
            Height = 29
            AutoSize = True
            ButtonHeight = 30
            ButtonWidth = 31
            Caption = 'ToolBar7'
            Flat = True
            Images = ilBoundsEditor
            TabOrder = 1
            object tbSoundsUndo: TToolButton
              Left = 0
              Top = 0
              Hint = 'Undo'
              AllowAllUp = True
              Caption = 'tbSoundsUndo'
              ImageIndex = 4
              ParentShowHint = False
              ShowHint = True
              OnClick = tbSoundsUndoClick
            end
            object tbSoundsRedo: TToolButton
              Left = 31
              Top = 0
              Hint = 'Redo'
              AllowAllUp = True
              Caption = 'tbSoundsRedo'
              ImageIndex = 5
              ParentShowHint = False
              ShowHint = True
              OnClick = tbSoundsRedoClick
            end
            object ToolButton14: TToolButton
              Left = 62
              Top = 0
              Width = 8
              Caption = 'ToolButton14'
              ImageIndex = 2
              Style = tbsSeparator
            end
            object tbSoundsDelete: TToolButton
              Left = 70
              Top = 0
              Hint = 'Delete placed sound'
              AllowAllUp = True
              Caption = 'tbSoundsDelete'
              ImageIndex = 6
              ParentShowHint = False
              ShowHint = True
              OnClick = tbSoundsDeleteClick
            end
            object ToolButton6: TToolButton
              Left = 101
              Top = 0
              Width = 8
              Caption = 'ToolButton6'
              ImageIndex = 7
              Style = tbsSeparator
            end
            object tbPlaySound: TToolButton
              Left = 109
              Top = 0
              Hint = 'Play the placed sound'
              Caption = 'tbPlaySound'
              ImageIndex = 10
              ParentShowHint = False
              ShowHint = True
              OnClick = tbPlaySoundClick
            end
          end
          object Panel4: TPanel
            Left = 0
            Top = 137
            Width = 200
            Height = 306
            Align = alClient
            BevelOuter = bvNone
            TabOrder = 2
            object lbSounds: TClearTypeListBox
              Left = 0
              Top = 0
              Width = 200
              Height = 274
              Style = lbOwnerDrawFixed
              Align = alClient
              Font.Charset = DEFAULT_CHARSET
              Font.Color = clWindowText
              Font.Height = -13
              Font.Name = 'Arial'
              Font.Style = []
              ItemHeight = 16
              ParentFont = False
              TabOrder = 0
              OnClick = lbSoundsClick
              ClearType = DataModule1.ClearTypeText
            end
            object ToolBar8: TToolBar
              Left = 0
              Top = 283
              Width = 200
              Height = 29
              Align = alBottom
              AutoSize = True
              ButtonHeight = 30
              ButtonWidth = 31
              Caption = 'ToolBar8'
              Flat = True
              Images = ilBoundsEditor
              TabOrder = 1
              object tbSoundsPlay: TToolButton
                Left = 0
                Top = 0
                Hint = 'Play the highlighted sound from the list'
                Caption = 'tbSoundsPlay'
                ImageIndex = 10
                ParentShowHint = False
                ShowHint = True
                OnClick = tbSoundsPlayClick
              end
              object ToolButton4: TToolButton
                Left = 31
                Top = 0
                Width = 8
                Caption = 'ToolButton4'
                ImageIndex = 12
                Style = tbsSeparator
              end
              object tbSoundsStop: TToolButton
                Left = 39
                Top = 0
                Hint = 'Stop playback'
                Caption = 'tbSoundsStop'
                ImageIndex = 11
                ParentShowHint = False
                ShowHint = True
                OnClick = tbSoundsStopClick
              end
            end
          end
          object pnlSoundInfo: TPanel
            Left = 0
            Top = 58
            Width = 200
            Height = 73
            Align = alTop
            BevelOuter = bvNone
            TabOrder = 3
            OnResize = pnlSoundInfoResize
            object cbAreaSound: TClearTypeCheckBox
              Left = 16
              Top = 52
              Width = 97
              Height = 17
              Caption = 'Area sound'
              Font.Charset = DEFAULT_CHARSET
              Font.Color = clWindowText
              Font.Height = -13
              Font.Name = 'Arial'
              Font.Style = []
              ParentFont = False
              TabOrder = 0
              OnClick = cbAreaSoundClick
              ClearType = DataModule1.ClearTypeText
            end
            object rbDaySound: TClearTypeRadioButton
              Left = 0
              Top = 4
              Width = 113
              Height = 17
              Caption = 'Day sound'
              Checked = True
              Font.Charset = ANSI_CHARSET
              Font.Color = clBlue
              Font.Height = -13
              Font.Name = 'Arial'
              Font.Style = [fsBold]
              ParentFont = False
              TabOrder = 1
              TabStop = True
              ClearType = DataModule1.ClearTypeText
            end
            object rbNightSound: TClearTypeRadioButton
              Left = 0
              Top = 28
              Width = 113
              Height = 17
              Caption = 'Night sound'
              Font.Charset = ANSI_CHARSET
              Font.Color = clBlue
              Font.Height = -13
              Font.Name = 'Arial'
              Font.Style = [fsBold]
              ParentFont = False
              TabOrder = 2
              ClearType = DataModule1.ClearTypeText
            end
          end
        end
        object tbClipElevation: TTabSheet
          Caption = 'Clip at Elevation'
          ImageIndex = 6
          DesignSize = (
            200
            443)
          object Label13: TClearTypeLabel
            Left = 8
            Top = 8
            Width = 53
            Height = 16
            Caption = 'Elevation'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -13
            Font.Name = 'Arial'
            Font.Style = []
            ParentFont = False
            ClearType = DataModule1.ClearTypeText
          end
          object Label14: TClearTypeLabel
            Left = 0
            Top = 88
            Width = 78
            Height = 16
            Caption = 'Upper texture'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -13
            Font.Name = 'Arial'
            Font.Style = []
            ParentFont = False
            ClearType = DataModule1.ClearTypeText
          end
          object Label15: TClearTypeLabel
            Left = 0
            Top = 192
            Width = 78
            Height = 16
            Caption = 'Lower texture'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -13
            Font.Name = 'Arial'
            Font.Style = []
            ParentFont = False
            ClearType = DataModule1.ClearTypeText
          end
          object Bevel2: TBevel
            Left = 0
            Top = 80
            Width = 193
            Height = 9
            Anchors = [akLeft, akTop, akRight]
            Shape = bsTopLine
          end
          object Bevel3: TBevel
            Left = 0
            Top = 288
            Width = 201
            Height = 9
            Anchors = [akLeft, akTop, akRight]
            Shape = bsTopLine
          end
          object tbElevation: TTrackBar
            Left = 0
            Top = 40
            Width = 193
            Height = 32
            Anchors = [akLeft, akTop, akRight]
            Max = 40
            Orientation = trHorizontal
            Frequency = 1
            Position = 0
            SelEnd = 0
            SelStart = 0
            TabOrder = 0
            TickMarks = tmBottomRight
            TickStyle = tsAuto
            OnChange = tbElevationChange
          end
          object edtElevation: TClearTypeEdit
            Left = 80
            Top = 8
            Width = 113
            Height = 24
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -13
            Font.Name = 'Arial'
            Font.Style = []
            ParentFont = False
            TabOrder = 1
            Text = '0'
            OnChange = edtElevationChange
            ClearType = DataModule1.ClearTypeText
          end
          object btnSplitPolys: TIAEverButton
            Left = 8
            Top = 304
            Width = 185
            Height = 32
            Anchors = [akLeft, akTop, akRight]
            Caption = 'Split'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -13
            Font.Name = 'Arial'
            Font.Style = []
            ParentFont = False
            TabOrder = 2
            OnClick = btnSplitPolysClick
            ButtonAngle = 0
            ButtonWidth = 185
            ButtonHeight = 32
            CaptionAngle = 0
            ButtonDepth = 3
            MainBitmap.Data = {
              F6060000424DF606000000000000360000002800000018000000180000000100
              180000000000C006000011170000111700000000000000000000B2B2B2B2B2B2
              B2B2B2B2B2B2B1B0B09F8787906464997777AFACACB2B2B2B2B2B2B2B2B2B2B2
              B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2
              B2B2B2B2B2B2B2B2B2B2B2B2B2B29671717E2304B770119B490C792A26AAA1A1
              B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2
              B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B27B2A1EE6AE0DB37237C7
              914BCF921B752424ADA7A7B2B2B2B2B2B2B2B2B2B2B2B2A59494956F6F9D8181
              B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2A89B9B7D24
              0BE2A500711B1B7C302BCF9C50B267048D5D5DB2B2B2B2B2B2B1B0B08D5B5B84
              2900B36A0F95410C7F3C3CAEAAAAB2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2
              B2B2B2ADA6A67D2612EFBA0A761C0D9C80807D281CE8B1117F2913ACA4A4B2B2
              B2A18B8B822908E6AB00A05112C791479B490A9C8080B2B2B2B2B2B2B2B2B2B2
              B2B2B2B2B2B2B2B2B2B2B2B2B2B27F3835CD932EBF7803792B287C3434B86E00
              AB5C00916666B2B2B27F3833D5960A8F3600864A4A87300EBB74088C5959B2B2
              B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B29F8686792014E7BC4FCE
              8E0E9E4A00C98400D69A077C3434A696968C3B1AE0AC2D6E1414782823CA8500
              943E009B7C7CB2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B1B1
              B19A7A7A7B261BB6701DE4A905DBA20FAF9D7D6E18188B57577D4A3FE5CD75B7
              7012D6980FB56B027A2D2AAEA8A8B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2
              B2B2B2B2B2B2B2B2B2B2B2B2A99E9E8E5D5D7D36366C0E0EA592927E4A4A680A
              0A7B6D6D844937A55B24842C147F3936ACA5A5B2B2B2B2B2B2B2B2B2B2B2B2B2
              B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2916666
              905C5C9890907138387B6F6F711E1E997777A89A9AB2B2B2B2B2B2B2B2B2B2B2
              B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2
              B2B2B2B2B2A697977B2C2CA6A4A4868282744B4B8D5B5BB2B2B2B2B2B2B2B2B2
              B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2
              B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B27A2E2E9C89895A5A5A6E2323A49191B2
              B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2
              B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B27B33339A84849A9A
              9A732525A49191B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2
              B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2A49191
              6E25258A8989A7A7A78153538A5555B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2
              B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2
              B2B2B2B2B28A55557551517F7F7FA79C9C928585732020B2B2B2B2B2B2B2B2B2
              B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2
              B2B2B2B2B2B2B2B2B2B2B2B2B2701E1E7D75757965659260609B9A9A732929A0
              8888B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2
              B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2A18A8A6E2C2C7E7E7E7037377B2D
              2DA3A1A1845A5A8A5656B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2
              B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2884F4F765555
              7D77776B1616752424A59696928686792C2CB1AFAFB2B2B2B2B2B2B2B2B2B2B2
              B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2AF
              ADAD7422227D7676785D5D7E3A3A9166669668689A9A9A732222ADA6A6B2B2B2
              B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2
              B2B2B2B2B2B2B2ABA3A37023237F7F7F703232977474A492927E3333A4A3A370
              1F1FA99D9DB2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2
              B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2A89B9B6A17177D76766F1B1BACA5A5B0AF
              AF792A2AA49494711E1EABA1A1B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2
              B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2AFABAB762828713939
              834646B2B2B2B2B2B28C5B5B741C1C8D5C5CB2B2B2B2B2B2B2B2B2B2B2B2B2B2
              B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2
              B2B2A390907A2F2F9E8383B2B2B2B2B2B2A89C9C9C7E7EB1AFAFB2B2B2B2B2B2
              B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2
              B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2
              B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2}
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
          object btnBrowseUpper: TIAEverButton
            Left = 0
            Top = 144
            Width = 100
            Height = 32
            Caption = 'Choose...'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -13
            Font.Name = 'Arial'
            Font.Style = []
            ParentFont = False
            TabOrder = 3
            OnClick = btnBrowseUpperClick
            ButtonAngle = 0
            ButtonWidth = 100
            ButtonHeight = 32
            CaptionLeft = 35
            CaptionAngle = 0
            ButtonDepth = 3
            CaptionHAlign = haNone
            MainBitmap.Data = {
              F6060000424DF606000000000000360000002800000018000000180000000100
              180000000000C006000000000000000000000000000000000000B2B2B2B2B2B2
              B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2
              B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2ABAFB25187BC3A7CBF869EB6B2
              B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2
              B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2ADB0B24B84BE1B7E
              E2268BF01470CC8AA0B6B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2
              B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2
              4983BD197CE03298FE369AFF4298EE407FBEB2B2B2B2B2B2B2B2B2B2B2B2B2B2
              B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2
              B2B2ADB0B24983BD197DE23298FE369AFF5EADFC2B81D75C8BBBB2B2B2B2B2B2
              B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2
              B2B2B2B2B2B2B2ADB0B24B84BE197CE03298FE379BFF5DADFC2B81D74B84BDAD
              B0B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2A7A7A7
              B2B2B2B2B2B2B2B2B2B2B2B2B2B2B24983BD197CE03298FE369AFF5DADFC2C82
              D84983BDADB0B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B29D9D9D76767659
              59595151514F4F4F4E4E4E5A5A5A797979A3A3A35287BC2983DD3C9DFE369AFF
              5EADFC2B81D74983BDB2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2ADADAD7B7B
              7B5959596868686D6D6D6D6D6D6969696161615454544C4C4C5353532F5B8773
              96B88AAAC964B0FC2B81D74B84BDADB0B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2
              AEAEAE6868686969698181818D8D86BEBFA0E2E4B4EAEBBAE6E7B8C7C8AB8484
              804E4E4E4C4C4C6E6E6E83A5C74E98E24983BDADB0B2B2B2B2B2B2B2B2B2B2B2
              B2B2B2B2B2B0B0B0747474747474939393B6B79EF8FABBFDFFBDFDFFBEFEFFC0
              FEFFC1FEFFC2FBFCC5BDBEA95B5B5B4C4C4C2D5985588ABCB2B2B2B2B2B2B2B2
              B2B2B2B2B2B2B2B2B2B2B2B2B29595956363639E9E9EC3C4A9FBFCBDFDFFBFFE
              FFC0FEFFC2FEFFC3FEFFC5FEFFC6FEFFC8FCFDCAC7C7B44F4F4F4F4F4F9E9E9E
              B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2696969929292B7B7AAF9FA
              BFFEFFC1FEFFC3FEFFC4FEFFC6FEFFC8FEFFC9FEFFCAFEFFCBFEFFCDFCFDCE90
              908A4C4C4C737373B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2A4A4A4666666
              AFAFAFE0E0BAFEFFC4FEFFC5FEFFC7FEFFC8FEFFCAFEFFCBFEFFCCFEFFCEFEFF
              D0FEFFD1FEFFD3D5D5BB565656535353AFAFAFB2B2B2B2B2B2B2B2B2B2B2B2B2
              B2B2989898777777B4B4B4F6F7C5FEFFC8FEFFC9FEFFCBFEFFCCFEFFCDFEFFCF
              FEFFD1FEFFD2FEFFD3FEFFD4FEFFD6F0F1D0666664505050A0A0A0B2B2B2B2B2
              B2B2B2B2B2B2B2B2B2B28A8A8A858585BBBBBBFEFFCAFEFFCBFEFFCDFEFFCFFE
              FFD0FEFFD2FEFFD3FEFFD4FEFFD5FEFFD7FEFFD8FEFFDAF1F2D46D6D6B5E5E5E
              9B9B9BB2B2B2B2B2B2B2B2B2B2B2B2B2B2B2989898868686C4C4C4FEFFCEFEFF
              D0FEFFD1FEFFD3FEFFD4FEFFD5FEFFD6FEFFD8FEFFD9FEFFDBFEFFDDFEFFDEF0
              F0D57272705A5A5A9E9E9EB2B2B2B2B2B2B2B2B2B2B2B2B2B2B29C9C9C707070
              CDCDCDF0F1CFFEFFD3FEFFD4FEFFD6FEFFD7FEFFD9FEFFDBFEFFDCFEFFDDFEFF
              DFFEFFE0FFFFE1D6D6C26F6F6E525252A8A8A8B2B2B2B2B2B2B2B2B2B2B2B2B2
              B2B2AEAEAE646464CBCBCBDDDECDFEFFD7FEFFD8FEFFDAFEFFDBFEFFDDFEFFDE
              FEFFE0FFFFE1FFFFE3FFFFE4FFFFE6A9A9A16F6F6F646464B2B2B2B2B2B2B2B2
              B2B2B2B2B2B2B2B2B2B2B2B2B2858585919191D9D9D8F2F2D7FEFFDCFEFFDEFE
              FFDFFFFFE0FFFFE2FFFFE4FFFFE5FFFFE7FFFFE8DDDDCD8484845F5F5F8B8B8B
              B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2AAAAAA606060C3C3C3DCDC
              D7F3F3DBFFFFE2FFFFE3FFFFE5FFFFE6FFFFE8FFFFE9FEFEE9E9E9DAA5A5A37E
              7E7E676767AFAFAFB2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2
              9E9E9E5E5E5EC5C5C5DEDEDCE5E5D8F9F9E4FFFFE8FFFFE9FFFFEBF7F7E6D3D3
              CBAFAFAE939393616161A3A3A3B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2
              B2B2B2B2B2B2B2B2B2B2B29E9E9E5E5E5E9F9F9FD8D8D8D8D8D8D7D7D4D3D3D0
              CECECBBFBFBFAFAFAF828282606060A7A7A7B2B2B2B2B2B2B2B2B2B2B2B2B2B2
              B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2A8A8A87C7C7C67676783
              83839C9C9CA7A7A7949494777777616161808080ACACACB2B2B2B2B2B2B2B2B2
              B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2
              B2B2B2B2ACACAC949494858585858585858585969696ADADADB2B2B2B2B2B2B2
              B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2}
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
          object btnBrowseLower: TIAEverButton
            Left = 0
            Top = 248
            Width = 100
            Height = 32
            Caption = 'Choose...'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -13
            Font.Name = 'Arial'
            Font.Style = []
            ParentFont = False
            TabOrder = 4
            OnClick = btnBrowseLowerClick
            ButtonAngle = 0
            ButtonWidth = 100
            ButtonHeight = 32
            CaptionLeft = 35
            CaptionAngle = 0
            ButtonDepth = 3
            CaptionHAlign = haNone
            MainBitmap.Data = {
              F6060000424DF606000000000000360000002800000018000000180000000100
              180000000000C006000000000000000000000000000000000000B2B2B2B2B2B2
              B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2
              B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2ABAFB25187BC3A7CBF869EB6B2
              B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2
              B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2ADB0B24B84BE1B7E
              E2268BF01470CC8AA0B6B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2
              B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2
              4983BD197CE03298FE369AFF4298EE407FBEB2B2B2B2B2B2B2B2B2B2B2B2B2B2
              B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2
              B2B2ADB0B24983BD197DE23298FE369AFF5EADFC2B81D75C8BBBB2B2B2B2B2B2
              B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2
              B2B2B2B2B2B2B2ADB0B24B84BE197CE03298FE379BFF5DADFC2B81D74B84BDAD
              B0B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2A7A7A7
              B2B2B2B2B2B2B2B2B2B2B2B2B2B2B24983BD197CE03298FE369AFF5DADFC2C82
              D84983BDADB0B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B29D9D9D76767659
              59595151514F4F4F4E4E4E5A5A5A797979A3A3A35287BC2983DD3C9DFE369AFF
              5EADFC2B81D74983BDB2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2ADADAD7B7B
              7B5959596868686D6D6D6D6D6D6969696161615454544C4C4C5353532F5B8773
              96B88AAAC964B0FC2B81D74B84BDADB0B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2
              AEAEAE6868686969698181818D8D86BEBFA0E2E4B4EAEBBAE6E7B8C7C8AB8484
              804E4E4E4C4C4C6E6E6E83A5C74E98E24983BDADB0B2B2B2B2B2B2B2B2B2B2B2
              B2B2B2B2B2B0B0B0747474747474939393B6B79EF8FABBFDFFBDFDFFBEFEFFC0
              FEFFC1FEFFC2FBFCC5BDBEA95B5B5B4C4C4C2D5985588ABCB2B2B2B2B2B2B2B2
              B2B2B2B2B2B2B2B2B2B2B2B2B29595956363639E9E9EC3C4A9FBFCBDFDFFBFFE
              FFC0FEFFC2FEFFC3FEFFC5FEFFC6FEFFC8FCFDCAC7C7B44F4F4F4F4F4F9E9E9E
              B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2696969929292B7B7AAF9FA
              BFFEFFC1FEFFC3FEFFC4FEFFC6FEFFC8FEFFC9FEFFCAFEFFCBFEFFCDFCFDCE90
              908A4C4C4C737373B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2A4A4A4666666
              AFAFAFE0E0BAFEFFC4FEFFC5FEFFC7FEFFC8FEFFCAFEFFCBFEFFCCFEFFCEFEFF
              D0FEFFD1FEFFD3D5D5BB565656535353AFAFAFB2B2B2B2B2B2B2B2B2B2B2B2B2
              B2B2989898777777B4B4B4F6F7C5FEFFC8FEFFC9FEFFCBFEFFCCFEFFCDFEFFCF
              FEFFD1FEFFD2FEFFD3FEFFD4FEFFD6F0F1D0666664505050A0A0A0B2B2B2B2B2
              B2B2B2B2B2B2B2B2B2B28A8A8A858585BBBBBBFEFFCAFEFFCBFEFFCDFEFFCFFE
              FFD0FEFFD2FEFFD3FEFFD4FEFFD5FEFFD7FEFFD8FEFFDAF1F2D46D6D6B5E5E5E
              9B9B9BB2B2B2B2B2B2B2B2B2B2B2B2B2B2B2989898868686C4C4C4FEFFCEFEFF
              D0FEFFD1FEFFD3FEFFD4FEFFD5FEFFD6FEFFD8FEFFD9FEFFDBFEFFDDFEFFDEF0
              F0D57272705A5A5A9E9E9EB2B2B2B2B2B2B2B2B2B2B2B2B2B2B29C9C9C707070
              CDCDCDF0F1CFFEFFD3FEFFD4FEFFD6FEFFD7FEFFD9FEFFDBFEFFDCFEFFDDFEFF
              DFFEFFE0FFFFE1D6D6C26F6F6E525252A8A8A8B2B2B2B2B2B2B2B2B2B2B2B2B2
              B2B2AEAEAE646464CBCBCBDDDECDFEFFD7FEFFD8FEFFDAFEFFDBFEFFDDFEFFDE
              FEFFE0FFFFE1FFFFE3FFFFE4FFFFE6A9A9A16F6F6F646464B2B2B2B2B2B2B2B2
              B2B2B2B2B2B2B2B2B2B2B2B2B2858585919191D9D9D8F2F2D7FEFFDCFEFFDEFE
              FFDFFFFFE0FFFFE2FFFFE4FFFFE5FFFFE7FFFFE8DDDDCD8484845F5F5F8B8B8B
              B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2AAAAAA606060C3C3C3DCDC
              D7F3F3DBFFFFE2FFFFE3FFFFE5FFFFE6FFFFE8FFFFE9FEFEE9E9E9DAA5A5A37E
              7E7E676767AFAFAFB2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2
              9E9E9E5E5E5EC5C5C5DEDEDCE5E5D8F9F9E4FFFFE8FFFFE9FFFFEBF7F7E6D3D3
              CBAFAFAE939393616161A3A3A3B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2
              B2B2B2B2B2B2B2B2B2B2B29E9E9E5E5E5E9F9F9FD8D8D8D8D8D8D7D7D4D3D3D0
              CECECBBFBFBFAFAFAF828282606060A7A7A7B2B2B2B2B2B2B2B2B2B2B2B2B2B2
              B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2A8A8A87C7C7C67676783
              83839C9C9CA7A7A7949494777777616161808080ACACACB2B2B2B2B2B2B2B2B2
              B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2
              B2B2B2B2ACACAC949494858585858585858585969696ADADADB2B2B2B2B2B2B2
              B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2}
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
          object cbUpperTexture: TComboBox
            Left = 0
            Top = 112
            Width = 193
            Height = 24
            Style = csDropDownList
            Anchors = [akLeft, akTop, akRight]
            DropDownCount = 24
            ItemHeight = 0
            TabOrder = 5
          end
          object cbLowerTexture: TComboBox
            Left = 0
            Top = 216
            Width = 193
            Height = 24
            Style = csDropDownList
            Anchors = [akLeft, akTop, akRight]
            DropDownCount = 24
            ItemHeight = 0
            TabOrder = 6
          end
        end
      end
    end
  end
  object pnlBottom: TPanel
    Left = 0
    Top = 649
    Width = 959
    Height = 66
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    DesignSize = (
      959
      66)
    object Label3: TClearTypeLabel
      Left = 651
      Top = 4
      Width = 293
      Height = 16
      Anchors = [akTop, akRight]
      Caption = 'When in paint mode, right-click on a block to hide it.'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      ClearType = DataModule1.ClearTypeText
    end
    object btnOk: TIAEverButton
      Left = 669
      Top = 32
      Width = 88
      Height = 32
      Anchors = [akTop, akRight]
      Caption = 'OK'
      Default = True
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Arial'
      Font.Style = []
      ModalResult = 1
      ParentFont = False
      TabOrder = 0
      OnClick = btnOkClick
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
      Left = 765
      Top = 32
      Width = 88
      Height = 32
      Anchors = [akTop, akRight]
      Cancel = True
      Caption = 'Cancel'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Arial'
      Font.Style = []
      ModalResult = 2
      ParentFont = False
      TabOrder = 1
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
    object gbShading: TClearTypeGroupBox
      Left = 440
      Top = 0
      Width = 201
      Height = 65
      Caption = 'Shading'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      TabOrder = 2
      ClearType = DataModule1.ClearTypeText
      object rbNoShading: TClearTypeRadioButton
        Left = 8
        Top = 16
        Width = 57
        Height = 17
        Caption = 'None'
        Checked = True
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
        TabOrder = 0
        TabStop = True
        OnClick = rbNoShadingClick
        ClearType = DataModule1.ClearTypeText
      end
      object rbShadeBySlope: TClearTypeRadioButton
        Left = 96
        Top = 16
        Width = 81
        Height = 17
        Caption = 'By slope'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
        TabOrder = 1
        OnClick = rbShadeBySlopeClick
        ClearType = DataModule1.ClearTypeText
      end
      object rbShadeByElevation: TClearTypeRadioButton
        Left = 96
        Top = 40
        Width = 97
        Height = 17
        Caption = 'By elevation'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
        TabOrder = 2
        OnClick = rbShadeByElevationClick
        ClearType = DataModule1.ClearTypeText
      end
    end
    object btnHelp: TIAEverButton
      Left = 862
      Top = 32
      Width = 88
      Height = 32
      Anchors = [akTop, akRight]
      Caption = '&Help'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      TabOrder = 3
      OnClick = btnHelpClick
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
        B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2ACAEA69AA2828F9A6B86955A8695
        5A8F9A6B9AA282ACAEA6B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B4
        B4B4B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2A3A89480914C718A2080A114
        8CB01595BC1795BC178CB01580A114738D20839352A3A894B2B2B2B2B2B2B2B2
        B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B0B0AE8A9760728E1992
        B916A8D419ADDA1AADDA1AADDA1AADDA1AADDA1AADDA1AA8D41992B916728E19
        8A9760B0B0AEB2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2AEAFAA7D8F
        4383A614AAD619ADDA1AADDA1AAAD7199DC61887AA1597BF17ABD819ADDA1AAD
        DA1AADDA1AAAD61986A9157D8F43AEAFAAB2B2B2B2B2B2B2B2B2B2B2B2B2B2B2
        B0B0AE7D8F438DB216ACD919ADDA1AADDA1AADDA1A94BB16809834AABD6F879F
        3D83A615ADDA1AADDA1AADDA1AADDA1AACD9198DB2167D8F43B0B0AEB2B2B2B2
        B2B2B2B2B2B2B2B28A976086A915ACD919ADDA1AADDA1AADDA1AADDA1A7C9C13
        CEDBA5E9F5C4DDEDAB759021A6D119ADDA1AADDA1AADDA1AADDA1AACD91983A6
        148A9760B2B2B2B2B2B2B2B2B2A3A894728E19AAD619ADDA1AADDA1AADDA1AAD
        DA1AADDA1A82A414B2C280EDF7CECFDE9D75911BA9D519ADDA1AADDA1AADDA1A
        ADDA1AADDA1AAAD619728E19A3A894B2B2B2B2B2B283935292B916ADDA1AADDA
        1AADDA1AADDA1AADDA1AADDA1A9FC91875911A748C29708A1D92B916ABD819AD
        DA1AADDA1AADDA1AADDA1AADDA1AADDA1A92B91680914CB2B2B2ACAEA6738D20
        A8D419ADDA1AADDA1AADDA1AADDA1AADDA1AADDA1A82A4146E8919637D116782
        116D8912A3CE18ADDA1AADDA1AADDA1AADDA1AADDA1AADDA1AA8D419718A20AC
        AEA69AA28280A114ADDA1AADDA1AADDA1AADDA1AADDA1AADDA1AADDA1A82A514
        C4CFA3F7FBE9F1F8D891A65098C017ADDA1AADDA1AADDA1AADDA1AADDA1AADDA
        1AADDA1A80A1149AA2828F9A6B8CB015ADDA1AADDA1AADDA1AADDA1AADDA1AAD
        DA1AADDA1A8DB216A5B476FAFDF3F4FAE2BFCD92799717A8D419ADDA1AADDA1A
        ADDA1AADDA1AADDA1AADDA1A8CB0158F9A6B86955A95BC17ADDA1AADDA1AADDA
        1AADDA1AADDA1AADDA1AADDA1AA4CF18789227E5EAD6F7FBEAF1F8DAAFC07B71
        8F12A5D119ADDA1AADDA1AADDA1AADDA1AADDA1A95BC1786955A86955A95BC17
        ADDA1AADDA1AADDA1AADDA1AADDA1AADDA1AABD719ADDA1A9BC41779922AD1DA
        B5F5FBE5F0F8D7C0CF8E78951DA4CF18ADDA1AADDA1AADDA1AADDA1A95BC1786
        955A8F9A6B8CB015ADDA1AADDA1AADDA1AADDA1AADDA1AA7D31A799813A8D21C
        AEDA1EA0C91B77931CB9C68FF2F9DFEDF6D0B6C7817C9C15ADDA1AADDA1AADDA
        1AADDA1A8CB0158F9A6B9AA28280A114ADDA1AADDA1AB2DC28BFE24CC8E6668B
        A73183984385A12BC4E35CC0E352B4D8407E9B24CDD8ACF1F9DAE7F2C57B9231
        A2CC18ADDA1AADDA1AADDA1A80A1149AA282ACAEA6718A20B4DA3BD0EA7EE0F1
        A9DEF0A3A9BF6093A55AFBFBF998A96191AD3AB9D757B5D54E809C24BECA99F5
        FBE4F0F8D69BAD619FC818ADDA1AADDA1AA8D419738D20ACAEA6B2B2B2839352
        B9CA82E8F5BFE4F3B5C6D98A788F30EDF0E4FFFFFFF4F6EFABB97F7F9538889D
        46ADBB82F5F7EEF8FCEDECF3D47D9433A6D119ADDA1AADDA1A92B916839352B2
        B2B2B2B2B2A3A8947D9337E5F1C1E7F4BDBBCD817B9136F0F2E8FFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFEFFFEF8FAF1A1B16B87A720AEDA1EADDA1AAAD6
        19708B19A3A894B2B2B2B2B2B2B2B2B28A97609FB364E9F5C4E6F4BBB3C6767B
        9232BEC99CECEFE3FFFFFFFFFFFFFFFFFFFDFDFDE6EAD990A25486A426B7DD3E
        B5DE32ADD91C86A9158A9760B2B2B2B2B2B2B2B2B2B2B2B2B0B0AE7E9047AFC2
        75E7F4BFE5F4B9D4E59E9EB4597F9636869B4493A55B94A65C859B417D962CA4
        C243BEE04FBCE147B8DF3C91B4217D8F43B0B0AEB2B2B2B2B2B2B2B2B2B2B2B2
        B2B2B2AEAFAA7D8F44A0B55DE4F1BBE4F3B7E1F2ADDCEEA0CADF86B9D06EB8D1
        68C1DB6BC9E66CC7E663C3E459BCDF4C8AA9277D8F44AEAFAAB2B2B2B2B2B2B2
        B2B2B2B2B2B2B2B2B2B2B2B2B2B2B0B0AE8A9760788E2FB8CA80DDECACE0F1AA
        DDEFA0D8EE94D5EC8AD1EA7FCDE975C4E166A3C0447690248A9760B0B0AEB2B2
        B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2A3A89484
        935379902F95AB4FA8BD64B5CB6FB2CA68A1BA528EA83A748C28839353A3A894
        B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2
        B2B2B2B2B2B2B2B2B2B2ACAEA69AA2828F9A6B86955A86955A8F9A6B9AA282AC
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
    object gbPaintOn: TClearTypeGroupBox
      Left = 0
      Top = 0
      Width = 113
      Height = 65
      Caption = 'Paint on'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      TabOrder = 4
      ClearType = DataModule1.ClearTypeText
      object rbLand: TClearTypeRadioButton
        Left = 8
        Top = 16
        Width = 73
        Height = 17
        Caption = 'Land'
        Checked = True
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
        TabOrder = 0
        TabStop = True
        ClearType = DataModule1.ClearTypeText
      end
      object rbWater: TClearTypeRadioButton
        Left = 8
        Top = 40
        Width = 97
        Height = 17
        Caption = 'Underwater'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
        TabOrder = 1
        ClearType = DataModule1.ClearTypeText
      end
    end
    object gbShow: TClearTypeGroupBox
      Left = 120
      Top = 0
      Width = 313
      Height = 65
      Caption = 'Show'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      TabOrder = 5
      ClearType = DataModule1.ClearTypeText
      object cbTopographic: TClearTypeCheckBox
        Left = 8
        Top = 40
        Width = 113
        Height = 17
        Caption = 'Topographic'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
        TabOrder = 0
        OnClick = cbTopographicClick
        ClearType = DataModule1.ClearTypeText
      end
      object cbShowBounds: TClearTypeCheckBox
        Left = 8
        Top = 16
        Width = 113
        Height = 17
        Caption = 'Zone bounds'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
        TabOrder = 1
        OnClick = cbShowBoundsClick
        ClearType = DataModule1.ClearTypeText
      end
      object cbShowHiddenGridElements: TClearTypeCheckBox
        Left = 160
        Top = 16
        Width = 121
        Height = 17
        Caption = 'Hidden blocks'
        Checked = True
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
        State = cbChecked
        TabOrder = 2
        OnClick = cbShowHiddenGridElementsClick
        ClearType = DataModule1.ClearTypeText
      end
      object cbShowOverlay: TClearTypeCheckBox
        Left = 160
        Top = 40
        Width = 105
        Height = 17
        Caption = 'Other polys'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
        TabOrder = 3
        OnClick = cbShowOverlayClick
        ClearType = DataModule1.ClearTypeText
      end
    end
  end
  object sbHorizontal: TScrollBar
    Left = 224
    Top = 362
    Width = 145
    Height = 20
    PageSize = 0
    TabOrder = 2
    OnChange = sbHorizontalChange
  end
  object sbVertical: TScrollBar
    Left = 446
    Top = 120
    Width = 20
    Height = 161
    Kind = sbVertical
    PageSize = 0
    TabOrder = 3
    OnChange = sbVerticalChange
  end
  object ilBoundsEditor: TImageList
    Height = 24
    Width = 24
    Left = 296
    Top = 400
    Bitmap = {
      494C01010D000E00040018001800FFFFFFFFFF10FFFFFFFFFFFFFFFF424D3600
      0000000000003600000028000000600000006000000001002000000000000090
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000FFFF0000FFFF0000FF
      FF0000FFFF0000FFFF0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000FFFF0000FFFF0000FFFF0000FFFF0000FFFF0000FF
      FF0000FFFF0000FFFF0000FFFF0000FFFF0000FFFF0000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000FFFF0000FFFF0000FFFF0000FFFF0000FFFF0000000000000000000000
      0000000000000000000000FFFF0000FFFF0000FFFF0000FFFF0000FFFF000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000000000000000FF
      FF0000FFFF0000FFFF0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000FFFF0000FFFF0000FF
      FF00000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000FFFF0000FF
      FF00000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000000000000000FF
      FF0000FFFF000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000FFFF0000FF
      FF00000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000000000000000FF
      FF0000FFFF000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000FFFF0000FFFF000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000FFFF0000FFFF0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000FFFF0000FFFF000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000FFFF0000FFFF0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000FFFF0000FFFF000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000FFFF0000FFFF0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000FFFF0000FFFF00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000FFFF0000FFFF00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000FFFF0000FFFF00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000FFFF0000FFFF00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000FFFF0000FFFF00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000FFFF0000FFFF00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000FFFF0000FFFF00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000FFFF0000FFFF00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000FFFF0000FFFF00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000FFFF0000FFFF00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000FFFF0000FFFF000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000FFFF0000FFFF0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000FFFF0000FFFF000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000FFFF0000FFFF0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000FFFF0000FFFF000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000FFFF0000FFFF0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000FFFF0000FF
      FF00000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000000000000000FF
      FF0000FFFF000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000FFFF0000FF
      FF00000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000000000000000FF
      FF0000FFFF000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000000000000000FF
      FF0000FFFF0000FFFF0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000FFFF0000FFFF0000FF
      FF00000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000FFFF0000FFFF0000FFFF0000FFFF0000FFFF0000000000000000000000
      0000000000000000000000FFFF0000FFFF0000FFFF0000FFFF0000FFFF000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000FFFF0000FFFF0000FFFF0000FFFF0000FFFF0000FF
      FF0000FFFF0000FFFF0000FFFF0000FFFF0000FFFF0000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000FFFF0000FFFF0000FF
      FF0000FFFF0000FFFF0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000B0B1B0009CA79A00A9AD
      A800000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000096A4940029761E003579
      2B006A8E6500AAAEAA0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000829A7E003E93320071D1
      6200479F3A002B7420006B8F6600A6ACA5000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000829A7E003A922D0070DC
      600070DB600065CD55003D98300031792700698E6400A7ACA600000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00009494A40060608C0060608C0060608C0060608C0060608C0060608C006060
      8C0060608C0060608C0060608C0060608C0060608C0060608C0060608C009494
      A400000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000829A7E003591280064D8
      520064D8520064D8520064D852005CCD4A0040A431002F7725006A8E6500AAAE
      AA00000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000060608C003C3CA9004B4BBF004C4CBF004D4DBF004E4EBF004F4FC0004F4F
      C0005151C0005151C0005252C0005252C1005353C1005454C1004343AA006060
      8C00000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000829A7E003090220056D5
      440056D5440056D5440056D5440056D5440056D5440053D04100339825002976
      1E006B8F6600A6ACA50000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000060608C004242BD005353DA005454DA005656DA005656DA005858DB005858
      DB005B5BDC005B5BDC005D5DDC005D5DDC005F5FDC006060DD004C4CBF006060
      8C00000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000829A7E002C8F1D004AD2
      35004AD235004AD235004AD235004AD235004AD235004AD2350049D2350043C6
      30002E971E002E792300698E6400A7ACA6000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000060608C003838BB004545D7004747D7004848D7004949D7004B4BD8004B4B
      D8004D4DD8004D4DD8004F4FD9005050D9005151D9005252DA004141BD006060
      8C00000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000829A7E00278E18003DCF
      27003DCF27003DCF27003DCF27003DCF27003DCF27003DCF27003DCF27003DCF
      27003DCF270038C32400299819002B7221006A8E6500AAAEAA00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000060608C002F2FB9003939D4003A3AD4003B3BD4003C3CD5003D3DD5003D3D
      D5004040D5004040D5004242D6004242D6004444D6004545D6003737BA006060
      8C00000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000829A7E00238D130032CC
      1B0032CC1B0032CC1B0032CC1B0032CC1B0032CC1B0032CC1B0032CC1B0032CC
      1B0032CC1B0032CC1B0032CC1B002FC119002497140026751B006B8F6600A6AC
      A500000000000000000000000000000000000000000000000000000000000000
      000060608C002525B7002D2DD2002E2ED2002F2FD2003030D2003232D3003232
      D3003434D3003434D3003535D3003636D3003737D3003838D4002E2EB8006060
      8C00000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000829A7E00228C130031CC
      1A0031CC1A0031CC1A0031CC1A0031CC1A0031CC1A0031CC1A0031CC1A0031CC
      1A0031CC1A0031CC1A0031CC1A0031CC1A0030CB19002FC41900239213002C76
      2100698E6400A7ACA60000000000000000000000000000000000000000000000
      000060608C001B1BB4002020CE002121CF002222CF002222CF002424D0002424
      D0002727D0002727D0002929D1002929D1002B2BD1002C2CD1002525B6006060
      8C00000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000829A7E00228C130031CC
      1A0031CC1A0031CC1A0031CC1A0031CC1A0031CC1A0031CC1A0031CC1A0031CC
      1A0031CC1A0031CC1A0031CC1A0031CC1A0031CC1A0031CC1A0031CC1A002EBF
      1800249614002F772500A0A99E00000000000000000000000000000000000000
      000060608C002525B7003939D4003A3AD4003939D4003434D3002525D0001C1C
      CE001C1CCE001C1CCE001D1DCE001D1DCE001E1ECE001F1FCE001B1BB4006060
      8C00000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000FF000000FF000000FF0000000000
      0000FF000000FF000000FF000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000829A7E00228C130036CD
      200039CE230035CD1F0031CC1A0031CC1A0031CC1A0031CC1A0031CC1A0031CC
      1A0031CC1A0031CC1A0031CC1A0031CC1A0031CC1A0031CC1A0031CC1A002EC2
      18002493130032762800A1AAA000000000000000000000000000000000000000
      000060608C006262C4007A7AE2007676E1007171E0006D6DDF006969DE006060
      DC004D4DD8003A3AD4002D2DD1002222CF001C1CCE001C1CCE001818B4006060
      8C00000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000FF000000FF000000000000000000
      000000000000FF000000FF000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000829A7E003B922E0079DD
      690075DC65006DDB5D005FD74D004AD2360038CD220032CC1B0031CC1A0031CC
      1A0031CC1A0031CC1A0031CC1A0031CC1A0031CC1A002EC11800259814002D7A
      2100698E6400A7ACA60000000000000000000000000000000000000000000000
      000060608C006A6AC5008585E4008181E4007D7DE3007878E2007474E1006F6F
      E0006B6BDF006767DE006464DD005B5BDB005454DA005050D9003939BB006060
      8C00000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000FF00000000000000FF000000FF00
      0000FF00000000000000FF000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000829A7E004595390088E1
      7B0083E074007CDE6D0076DD660070DB600068DA580059D646004AD2360040CF
      2B0038CD230036CD200035CC1E0030C11A002494130027771B006B8F6600A6AC
      A500000000000000000000000000000000000000000000000000000000000000
      000060608C007373C7008F8FE6008B8BE6008787E5008383E4007F7FE3007B7B
      E2007676E1007272E0006E6EDF006969DF006565DE006262DD004A4ABE006060
      8C00000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000829A7E0049963D0093E4
      87008DE27F0086E1790080DF71007ADE6B0074DC64006DDB5D0067D9560061D8
      4F005AD648004CC53900309721002C7322006A8E6500AAAEAA00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000060608C007B7BC9009C9CE9009797E8009393E7008E8EE6008A8AE5008686
      E4008282E4007E7EE3007979E2007575E1007070E0006C6CDF005252C0006060
      8C00000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000829A7E004C9741009DE7
      920096E58B0090E3840089E27C0084E076007DDE6F0077DD670070DB610062D1
      51003C9F2E002F772400698E6400A7ACA6000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000060608C008383CB00A6A6EC00A2A2EB009E9EEA009A9AE9009595E8009090
      E7008C8CE6008888E5008484E4008080E3007C7CE2007777E2005B5BC2006060
      8C00000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000829A7E0050984500A6E9
      9C00A0E795009AE68F0093E487008DE3800087E1790079D76B00449D37002C77
      22006B8F6600A6ACA50000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000060608C008C8CCD00B1B1EE00ADADED00A8A8EC00A4A4EB00A1A1EB009C9C
      E9009898E8009393E7008F8FE6008A8AE5008686E4008383E4006363C4006060
      8C00000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000829A7E0054994A00B0EB
      A800AAEAA100A4E89A009EE792008BD7800052A04600317528006A8E6500AAAE
      AA00000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000060608C007272B4009090CE008D8DCD008A8ACC008686CC008383CB007F7F
      CA007D7DCA007A7AC9007777C8007373C7007070C7006C6CC6005353AE006060
      8C00000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000829A7E00579A4E00BAEE
      B200B3ECAB009BDC91005AA84F00387B2F00698E6400A7ACA600000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00009A9AA70060608C0060608C0060608C0060608C0060608C0060608C006060
      8C0060608C0060608C0060608C0060608C0060608C0060608C0060608C009A9A
      A700000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000829A7E005B9A5200B8E5
      B10068AA5E00307925006B8F6600A6ACA5000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000097A59500347C2A003177
      27006A8E6500AAAEAA0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000B1B1B1009DA79B00A7AC
      A600000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000ACACA900A0A094009393800088886C008E8E
      7600A0A09400ACACA90000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000AFAFAE009C9C8D007C7C58007E7E530098987100A7A781009C9C67009898
      5D0085854D00747442007A7A540096968400ABABA70000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000ACAC
      A800797953008A8A5E00C9C9AA00E8E8CF00EDEDD500EDEDD500DEDEB200D0D0
      9000D0D09000CBCB8B00B3B376008F8F540070703E0085856800ACACA8000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000008080
      5F00B1B18A00EBEBD000EBEBD000EBEBCF00EBEBCF00EBEBCF00DDDDAF00D0D0
      9000D0D09000D0D09000D0D09000D0D09000CCCC8C00A0A0640070703C008D8D
      7600B0B0AF000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000B0AC
      AC00A46C6C00AE9E9E0000000000000000000000000000000000B1B0B000A46E
      6E00AC9696000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000007878
      4E00DEDEBE00E8E8CA00E8E8CA00E8E8C900E8E8C900E8E8C900DFDFB300D0D0
      9000C4C88700CACC8B00D0D09000D0D09000D0D09000D0D09000C6C686008E8E
      540089896D000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000AEA1A1009E4C
      4C00921C1C009D484800B1AEAE00000000000000000000000000A8838300941A
      1A00972B2B00A77E7E0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000007676
      4500DFDFBC00E6E6C400E6E6C400E6E6C400E6E6C300E6E6C300DADCB00080A4
      5A005FAF4C004D9E34009FB46D00BFC68400BBC38100D1D19100D1D19100C9C9
      8A0070703B000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000AFA3
      A300000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000B1B0B000A8838300982A2A008F1A
      1A008B1A1A00901A1A00A98686000000000000000000B0A9A9009A3A3A008A19
      19008F1C1C00911A1A009F535300AFA4A4000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000B0AAAA0000000000000000000000000000000000000000007C7C
      4B00E2E2BD00E4E4BF00E4E4BF00E4E4BE00E4E4BE00E4E4BE00BCC9990075C1
      690065C7570032AF20002E971B0088A55E0057853C00A8B87500CED09000D0D0
      920077773F00B0B0AE0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000009A39
      3900A2666600AFA6A60000000000000000000000000000000000000000000000
      000000000000000000000000000000000000AA8B8B00951E1E00971F1F00971F
      1F00961F1F00961E1E009C4444000000000000000000A9888800951D1D00961F
      1F00961F1F00971F1F00961C1C009D4949000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000B1AFAF00AA8A
      8A009A393900A880800000000000000000000000000000000000A4A49B008383
      5200E3E3BA00E3E3BA00E3E3BA00E3E3BA00E3E3BA00E3E3BA00ACC18B008AD5
      7F00559542007A9D5400328620006A924A0070BD650096D28D0071A15900C7CC
      8D007D7D4500A3A39A0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000AEA1A100961C
      1C009C1F1F00982F2F00A8838300B1B0B0000000000000000000000000000000
      000000000000000000000000000000000000B0ABAB009B3939009E212100A124
      2400A1242400A124240098272700AFA3A30000000000A15E5E009D212100A124
      2400A1242400A12424009A1E1E00A98686000000000000000000000000000000
      00000000000000000000000000000000000000000000AEA1A1009E505000981E
      1E009B1F1F00A05A5A00000000000000000000000000000000009F9F93009292
      6100E1E1B600E1E1B600E2E2B600E1E1B600E2E2B600E2E2B600B7C58C007BC4
      700052B0440080A45900CACE8F0092AB67003FBB2D0071C7640083CE780078A3
      59008A8A5100A2A2980000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000A67A7A00A021
      2100AE292900AB2727009D1F1F009B3F3F00AB93930000000000000000000000
      00000000000000000000000000000000000000000000A77A7A009F202000AE29
      2900AE292900AE2929009D1F1F00AA8C8C00000000009C3D3D00AA272700AE29
      2900AE292900AB2828009A383800B0ACAC000000000000000000000000000000
      0000000000000000000000000000AFA7A700A367670099232300A5242400AD29
      2900AB27270099373700B1ADAD0000000000000000000000000093937F009C9C
      6B00E1E1B400E1E1B300E1E1B300E0E0B200E0E0B200E1E1B200D1D59E00609F
      4F0057C24700508F3600D7D79A00C3CB8C0081A25B0056913B00428A34005FB2
      4F007B84480093937F0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000009F555500AE28
      2800B92E2E00B92E2E00B82D2D00AF2828009E252500A0575700AEA2A200B1B0
      B0000000000000000000000000000000000000000000AE9E9E009D1F1F00B92E
      2E00B92E2E00B92E2E00A6242400A8838300000000009A2E2E00B42B2B00B92E
      2E00B92E2E00AD272700A05A5A00000000000000000000000000000000000000
      000000000000B0AAAA00A88080009A333300A6242400B62C2C00B92E2E00B92E
      2E00B92E2E009C232300AEA0A0000000000000000000000000008B8B7200AFAF
      7F00E2E2B400E2E2B300E2E2B200E1E1B000DEDEAB00DBDBA200D2D5980082A3
      5E003D8D2A00317B2000D2D49700D9D99D00DADA9D00DADA9E006F9F4F0045B7
      34007586480091917B0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000B0AAAA0098313100C232
      3200C4333300C4333300C4333300C4333300C3323200AF2727009A2B2B00A36B
      6B000000000000000000000000000000000000000000AEA1A100A0202000C433
      3300C4333300C4333300AC262600A9848400000000009A2E2E00BE2F2F00C433
      3300C4333300B2292900A15C5C00000000000000000000000000000000000000
      0000AD9999009B3E3E00A3232300BB2E2E00C4333300C4333300C4333300C433
      3300C4333300A9252500A880800000000000000000000000000083836500BBBB
      8B00E3E3B400E3E3B300E2E2B200DFDFAB00DCDCA300DBDBA100DBDBA000DBDB
      9F0086A460004A7D3300C7CE9000DBDB9F00D9DA9E00D0D49700287819002A92
      1900869356008383650000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000AC979700A2232300D44C
      4C00D6595900D5535300D2404000D0373700D0373700B72B2B009E2424009C45
      4500AB91910000000000000000000000000000000000A77C7C00AB252500D037
      3700D0373700D0373700A8252500AA8E8E00000000009E454500C6333300D037
      3700D0373700C833330099353500B0ACAC00000000000000000000000000B0AC
      AC00A1606000982D2D00AA252500C8343400D0373700D0383800D13D3D00D13F
      3F00D13D3D00BD2F2F009F54540000000000000000000000000077775000CECE
      9D00E4E4B300E3E3B200E1E1AC00E0E0A900DFDFA800DFDFA700DFDFA600DEDE
      A50068964B001F7D11008CA86500DADBA00079A256006D9A4E00719B5100709C
      5000BDBF850080805F0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000A5747400C05A5A00EB98
      9800EA959500E9919100E8888800E5747400E15D5D00A82E2E00A36A6A00B1AF
      AF0000000000000000000000000000000000AD9D9D009A2F2F00D13D3D00DD44
      4400DC424200D93C3C009F2C2C00AFA7A70000000000A1606000C1303000DD48
      4800DF545400DF515100AB2C2C00A4707000B1AFAF0000000000000000000000
      000000000000AD99990097292900D0434300DF525200E0575700E0575700E055
      5500DF525200D54848009D323200B1AFAF0000000000B1B1B10073734A00DCDC
      AB00E4E4B200E3E3AF00E2E2AE00E2E2AD00E2E2AD00E2E2AC00E2E2AB00E1E1
      AA0095B16F002B9E1A006C9E4D005B8E400036A924003A9D2A00C3CC8F00DCDD
      A200C9C98F0074744C0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000009F525200D9858500EFA4
      A400EFA0A000EE9B9B00ED979700ED929200EC8E8E00E27D7D00A1343400A573
      7300B0ACAC0000000000B0ACAC00A46C6C009B313100CA4C4C00E4616100E45C
      5C00E3575700CB4040009E4848000000000000000000AB929200AF353500EC8D
      8D00EC8E8E00EC8A8A00E27E7E00AD3939009E505000AA8C8C00000000000000
      0000AB9393009B3E3E00BE464600E66B6B00E6696900E5656500E4616100E45E
      5E00E45B5B00E3575700A42B2B00AC97970000000000ADADAA0070704300E6E6
      B500E6E6B400E5E5B300E5E5B200E5E5B100E4E4B000E4E4AF00E4E4AF00E4E4
      AE00D5DAA2004B99370053BF4300519E42007BD56E007FCE73005D8C4400DFE0
      A500DEDEA40070704500B0B0AE00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000B0A9A9009A2E2E00B85353009E2A
      2A00E08A8A00F0A3A300EF9E9E00EE999900ED959500ED909000E5838300B848
      4800A12D2D00A33F3F00A12C2C00B8424200DD666600E66C6C00E6686800E564
      6400E25D5D00A62D2D00AB9191000000000000000000B1AEAE009D484800D87B
      7B00ED969600ED929200EC8F8F00EB8A8A00CC636300AA363600A1353500A133
      3300AA343400CF5E5E00E8767600E7737300E7707000E66C6C00E6696900E465
      6500AE313100AB2D2D00A6292900A67A7A0000000000ABABA7007B7B4C00E8E8
      B800E8E8B700E7E7B700E7E7B600E7E7B500E6E6B400E6E6B300E6E6B300E5E5
      B200E5E5B100C4CF960062A44C006DC060007DC7710076A55D00A0B77700D7DB
      A000E3E3AA006D6D3E00ACACA900000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000AC9797009B404000A5727200A880
      8000A93F3F00EBA0A000F0A6A600EFA1A100EF9D9D00EE989800ED949400EC8F
      8F00E7848400E57F7F00E67D7D00E97D7D00E8787800E7737300E76F6F00E56A
      6A00BE414100A05A5A00B1B0B000000000000000000000000000AC999900A436
      3600E6919100EE9A9A00ED979700ED949400EC909000EB8B8B00E6848400E680
      8000E9828200EA818100E97D7D00E97A7A00E8787800E8747400E7717100C74B
      4B009C424200AA8E8E009F555500A160600000000000A7A7A10079794500EAEA
      BA00E9E9BA00E9E9B900E9E9B900E8E8B800E5E5B600E2E2B200DFDFAF00E0E0
      AF00E4E4B200E7E7B300DBDEA500B4C58700B3C48600DCDFA600E4E4AB00E4E4
      AB00E4E4AB007C7C4900AAAAA500000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000B1ADAD00B1AFAF00000000000000
      0000A2626200AF434300EEA8A800F1A9A900F0A4A400EFA0A000EE9B9B00ED96
      9600ED919100EC8D8D00EB898900EA848400EA808000E97B7B00E8777700C54B
      4B009B3C3C00B1AEAE000000000000000000000000000000000000000000A77B
      7B00AA3E3E00E8949400EF9E9E00EE9B9B00EE989800ED959500ED929200EC8E
      8E00EC8B8B00EB898900EB858500EA828200EA7F7F00E97C7C00CB5858009E3B
      3B00AEA1A1000000000000000000B1AEAE0000000000A6A69F007C7C4A00EAEA
      BE00E9E9BD00E7E7BC00E4E4BA00E1E1B800E0E0B600DFDFB400DFDFB300DEDE
      B100DDDDAF00E0E0B000E0E0A800E5E5AC00E5E5AD00E5E5AD00E5E5AD00E5E5
      AE00E5E5AE0079794200A7A7A100000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000B1B0B000A46E6E00A53A3A00D9858500F0A9A900F0A7A700F0A3A300EF9E
      9E00EE999900ED949400EC909000EC8B8B00EB878700E2797900B64040009E4C
      4C00AFA7A7000000000000000000000000000000000000000000000000000000
      0000A77D7D00A1313100DB828200EFA2A200EFA0A000EE9C9C00EE9A9A00ED96
      9600ED939300EC909000EC8C8C00EB898900E37E7E00BA4949009B404000AFA3
      A30000000000000000000000000000000000000000000000000087876B009090
      6100D5D5B000E4E4C100E4E4BF00E3E3BE00E2E2BB00E1E1B900E1E1B700E0E0
      B600DFDFB400DFDFB300D3D39800D3D39500DEDEA400E5E5AD00E7E7AF00E7E7
      AF00E7E7B00092925C00A0A09500000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000AB9292009F494900AC454500CD747400E3949400E99A
      9A00E9979700E8939300E68C8C00DA797900BC4E4E00A0383800A4707000B0AC
      AC00000000000000000000000000000000000000000000000000000000000000
      0000B1B1B100AC9797009E454500AC434300CF737300E5939300E9989800E995
      9500E8929200E48A8A00D8777700B84A4A00A0383800A46E6E00B0ACAC000000
      0000000000000000000000000000000000000000000000000000B1B1B0009090
      7A0075754700A4A47700DCDCB800E6E6C300E4E4C100E4E4BF00E3E3BD00E2E2
      BB00E1E1B900E1E1B700D4D49900D0D09000D0D09000D3D39400DDDDA100E5E5
      AD00E8E8B10093935D0098988700000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000B1B0B000AC969600A26363009D454500A345
      45009F2B2B00A1333300A04040009E515100A77D7D00AFA7A700000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000B1AFAF00AB8F8F00A26464009C3E3E00A23D3D009F2A
      2A00A13636009D3D3D009F545400A77E7E00B0AAAA0000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000AAAAA5008484660071714000A6A67800DBDBB800E6E6C400E6E6C300E5E5
      C100E4E4BF00E3E3BD00D4D49B00D0D09000D0D09000D0D09000D0D09000D1D1
      9100D2D295007F7F4900A1A19700000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000AEAEAB008B8B72007070420093936300BFBF9800E2E2
      C000E6E6C500E5E5C200D5D59B00D0D09000D0D09000D0D09000CECE8E00B6B6
      78007D7D460088886D00B1B1B000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000AEAEAB009A9A8A007D7D5A007878
      4A0095956800B7B78D00BABA7E00B0B07300A6A66A00909055007A7A46007979
      52009B9B8C00B1B1B00000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000ACAC
      A8009999890089896E007E7E5D007E7E5D0089896E0096968400A8A8A2000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000FFFFFF00FFFFFF00FFFFFF0000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000FFFFFF00FFFFFF00FFFFFF0000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000FFFFFF00FFFFFF00FFFFFF000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000FFFFFF00FFFFFF00FFFFFF000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000FF000000FF000000FF00000000000000000000000000000000000000
      0000000000000000000000000000000000000000FF000000FF000000FF000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000FFFFFF00FFFFFF00FFFFFF00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000FF000000FF000000FF00000000000000000000000000000000000000
      0000000000000000000000000000000000000000FF000000FF000000FF000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000FFFFFF00FFFFFF00FFFFFF00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000FFFF0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000FF000000FF000000FF00000000000000000000000000000000000000
      0000000000000000000000000000000000000000FF000000FF000000FF000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000FFFFFF0000000000000000000000000000000000FFFF
      FF00FFFFFF00FFFFFF0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000FFFF0000FFFF0000FFFF000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000FFFFFF00FFFFFF00000000000000000000000000FFFF
      FF00FFFFFF00FFFFFF0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000FFFF0000FFFF0000FFFF0000FFFF0000FFFF00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000000000FF000000
      FF000000FF000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000FFFFFF00FFFFFF00FFFFFF0000000000FFFFFF00FFFF
      FF00FFFFFF000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000FFFF0000FFFF0000FFFF0000FFFF0000FF
      FF00000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000000000FF000000
      FF000000FF00FF00000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000FFFF0000FFFF0000FF
      FF0000FFFF0000FFFF0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000000000FF000000
      FF000000FF000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000000000FF000000
      FF000000FF0000000000FF000000FF000000FF00000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF0000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000000000000000FF
      FF0000FFFF0000FFFF0000FFFF0000FFFF000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000000000FF000000
      FF000000FF000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000FF000000FF000000FF00
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000FFFF0000FFFF0000FFFF0000FFFF0000FFFF00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000000000FF000000
      FF000000FF000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000FF000000FF000000FF000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000FFFF0000FFFF0000FFFF0000FFFF0000FF
      FF00000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000FF000000FF000000FF000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000FFFF0000FFFF0000FF
      FF00000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000FF000000FF00
      0000FF0000000000000000000000000000000000000000000000000000000000
      00000000000000000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000000000000000FF
      FF00000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000FF000000FF000000FF00000000000000000000000000000000000000
      0000000000000000000000000000000000000000FF000000FF000000FF000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000FF000000FF000000FF00000000000000000000000000000000000000
      0000000000000000000000000000000000000000FF000000FF000000FF000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000FF000000FF000000FF00000000000000000000000000000000000000
      0000000000000000000000000000000000000000FF000000FF000000FF000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000FFFFFF00FFFFFF00FFFFFF0000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000FFFFFF00FFFFFF000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000FFFFFF00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000424D3E000000000000003E000000
      2800000060000000600000000100010000000000800400000000000000000000
      000000000000000000000000FFFFFF00FFFFFF000000000000000000FF83FF00
      0000000000000000FC007F000000000000000000F07C1F000000000000000000
      E3FF8F000000000000000000CFFFE7000000000000000000CFFFE70000000000
      000000009FFFF30000000000000000009FFFF30000000000000000009FFFF300
      00000000000000003FFFF90000000000000000003FC7F9000000000000000000
      3FC7F90000000000000000003FC7F90000000000000000003FFFF90000000000
      000000009FFFF30000000000000000009FFFF30000000000000000009FFFF300
      0000000000000000CFFFE7000000000000000000CFFFE7000000000000000000
      E3FF8F000000000000000000F07C1F000000000000000000FC007F0000000000
      00000000FF83FF000000000000000000FFFFFFFFFFFFFFFFFFFFFFFFFC007FFF
      FFFF8FFFFFFFFFFFFFEFFFFFFFFF83FFFFFFFFFFFFC7FFFFEFFF80FFFFFFFFFF
      FF83FFFFEFFF803FFFF0000FFF01FFFFD7FF800FFFF0000FFFEFFFFFD7FF8003
      FFF0000FFFEFFFFFABFF8000FFF0000FFFEFFFFFBBFF80003FF0000FFFEFFFFF
      6DFF80000FF0000FFFEFFFFF7DFF800003F0000FFFEFFFFEEEFF800001F0000F
      FFEFFFFE10FF800001F0000FFFEFFFFD297F800003F0000FFFEFFFFD457F8000
      0FF0000FFFEFFFFBEFBF80003FF0000FFFEFFFFBFFBF8000FFF0000FFFEFFFF7
      EFDF8003FFF0000FFF01FFF7FFDF800FFFF0000FFF83FFEFEFEF803FFFF0000F
      FFC7FFEFFFEF80FFFFFFFFFFFFEFFFDFEFF783FFFFFFFFFFFC007FC000078FFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFE03FFFFFFFFFFFFFFFF
      FFFFF0007FFFFFFFFFFFFFFFFFFFE0001FFFFFFFFFFFFFFFFFFFE00007FF83FF
      FFFFE3C7FFFFE00007FE7CFFFFFFC1C3FFFFE00007F9EF3FEFFF0180FFFBE000
      03F7EFDFE3FF0180FFC3C00003F7EFDFC0FF0080FF83C00003EFEFEFC07F8080
      FE01C00003EFEFEFC00F8081F801C00003DFEFF7800F8081F001C00003DFFFF7
      80078080E001C00003D03817800F00807800800003DFFFF78004018030008000
      01DFEFF7000001800000800001EFEFEF000001C00000800001EFEFEF300003E0
      0006800001F7EFDFF00007F0000FC00001F7EFDFFC000FF0001FC00001F9EF3F
      FE003FFC007FF00001FE7CFFFFFFFFFFFFFFFC0001FF83FFFFFFFFFFFFFFFF00
      03FFFFFFFFFFFFFFFFFFFFE01FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFC7FFF
      FFFFFFFFFFFFFFFFFFF83FFFFFFFFBFFBFFFFFFFFFF83FFFFFFFFFFFFFFFFFFF
      FFF07FFFFFFFFBFFBFFFFFFFFFF07FFFFFFFF1FF1FFFFFFFFBE0FFC7FFFFA0AA
      0BFFFFFFF9E0FFC3FFFFF1FF1FFFFFFFF8C1FFC0FFFFFBFFBFFFFFFFF841FFF8
      3FFFFFFFFFC7FFFFF803FFFE0FFFFBFFBFC3FFFFF8001FFF83FFFFC7FFC47FFF
      F8003FFFE0FFFBC7BFFF8FFFF8007FFFF83FFFC7FFFFF1FFF800FFFFFE0FFBFF
      BFFFFE3FF801FFFFFF81FFFFFFFFFFC7F803FFFFFFE1FBFFBFFFFFFFF807FFFF
      FFF1F1FF1FFFFFFFF80FFFFFFFFFA0AA0BFFFFFFF81FFFFFFFFFF1FF1FFFFFFF
      F83FFFFFFFFFFBFFBFFFFFFFF87FFFFFFFFFFFFFFFFFFFFFF8FFFFFFFFFFFBFF
      BFFFFFFFF9FFFFFFFFFFFFFFFFFFFFFF00000000000000000000000000000000
      000000000000}
  end
end
