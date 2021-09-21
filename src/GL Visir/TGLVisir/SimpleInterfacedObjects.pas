unit SimpleInterfacedObjects;

interface

Type
  TSimpleInterfacedObject = class(TObject, IInterface)
  protected
    FRefCount: Integer;
    function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    class function NewInstance: TObject; override;
    property RefCount: Integer read FRefCount;
  end;

implementation

procedure TSimpleInterfacedObject.AfterConstruction;
begin
// Release the constructor's implicit refcount
//  InterlockedDecrement(FRefCount);
end;

procedure TSimpleInterfacedObject.BeforeDestruction;
begin
//  if RefCount <> 0 then
//    Error(reInvalidPtr);
end;

// Set an implicit refcount so that refcounting
// during construction won't destroy the object.
class function TSimpleInterfacedObject.NewInstance: TObject;
begin
  Result := inherited NewInstance;
//  TSimpleInterfacedObject(Result).FRefCount := 1;
end;

function TSimpleInterfacedObject.QueryInterface(const IID: TGUID; out Obj): HResult;
begin
  if GetInterface(IID, Obj) then
    Result := 0
  else
    Result := E_NOINTERFACE;
end;

function TSimpleInterfacedObject._AddRef: Integer;
begin
//  Result := InterlockedIncrement(FRefCount);
end;

function TSimpleInterfacedObject._Release: Integer;
begin
//  Result := InterlockedDecrement(FRefCount);
//  if Result = 0 then
//    Destroy;
end;

end.
 
