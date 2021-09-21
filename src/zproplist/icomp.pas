Unit IComp;

Interface

Type
  IPersistent = interface(IUnknown)
  ['{44E1E164-C189-11D2-94E7-00002161286F}']
  end;

  IComponent = interface(IPersistent)
  ['{44E1E162-C189-11D2-94E7-00002161286F}']
    procedure DestroyComponents; stdcall;
    function Name: WideString; stdcall;
  end;

  IControl = interface(IComponent)
  ['{44E1E166-C189-11D2-94E7-00002161286F}']
    procedure BringtoFront; stdcall;
    procedure Hide; stdcall;
    procedure Refresh; stdcall;
    procedure Repaint; stdcall;
    procedure Show; stdcall;
    procedure Update; stdcall;
  end;

  IWinControl = interface(IControl)
  ['{44E1E168-C189-11D2-94E7-00002161286F}']
    procedure SetFocus; stdcall;
    function CanFocus: WordBool; stdcall;
    procedure DisableAlign; stdcall;
    procedure EnableAlign; stdcall;
    procedure ReAlign; stdcall;
    procedure ScaleBy(M: Integer; D: Integer); stdcall;
    procedure ScrollBy(DeltaX: Integer; DeltaY: Integer); stdcall;
  end;

  ITabSheet = interface(IWinControl)
  ['{28DB4C03-0DC0-11D3-9585-00002161286F}']
    procedure TabShow; stdcall;
    procedure TabHide; stdcall;
  end;

  IPageControl = interface(IWinControl)
  ['{28DB4C01-0DC0-11D3-9585-00002161286F}']
    function PageCount: Integer; stdcall;
    function Pages(Index: Integer): ITabSheet; stdcall;
    function ActivePage: ITabSheet; stdcall;
    function SetActivePage(const PageName: WideString): WordBool; stdcall;
  end;

  IppReport = interface(IComponent)
  ['{AD55E2ED-2497-11D3-95A7-00002161286F}']
    procedure Print; stdcall;
    procedure Printtodevices; stdcall;
    procedure Reset; stdcall;
    procedure Resetdevices; stdcall;
    procedure ShowDesigner; stdcall;
  end;

  IppDesigner = interface(IComponent)
  ['{FEE2BEC5-8C45-11D3-9644-00002161286F}']
    procedure ShowModal; stdcall;
  end;

  IDSHDataset = interface(IComponent)
  ['{C2378190-9289-11D3-964C-00002161286F}']
    procedure Close; stdcall;
    procedure Open; stdcall;
    function BOF: WordBool; stdcall;
    function EOF: WordBool; stdcall;
    function Recordcount: Integer; stdcall;
    function RecNO: Integer; stdcall;
  end;

  IDSHField = interface(IComponent)
  ['{15961270-9297-11D3-964C-00002161286F}']
    function Get_asVariant(out Value: OleVariant): HResult; stdcall;
    function Set_asVariant(Value: OleVariant): HResult; stdcall;
  end;

  IDSHStrings = interface(IPersistent)
  ['{BB233353-96AD-11D3-9651-00002161286F}']
    function Count: Integer; stdcall;
    procedure Clear; stdcall;
    function Add(Item: OleVariant): Integer; stdcall;
    function Items(Index: Integer; out Value: OleVariant): HResult; stdcall;
    function Get_Text(out Value: OleVariant): HResult; stdcall;
    function Set_Text(Value: OleVariant): HResult; stdcall;
    procedure Change(Index: Integer; Value: OleVariant); stdcall;
    procedure Delete(Index: Integer); stdcall;
    procedure Replace(Oldvalue: OleVariant; Newvalue: OleVariant); stdcall;
  end;

  IBasicAction = interface(IComponent)
  ['{2BBFA456-A62E-11D3-9665-00002161286F}']
    function Execute: WordBool; stdcall;
  end;

Implementation

End.
