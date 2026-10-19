{ Independent Delphi-mode compiler witness for indexed/default property lookup.
  Nyx's source tests inspect this same unit without executing any getter. }
unit indexed.properties.fixture;
{$mode delphi}{$H+}
interface

type
  { A real element keeps member completion distinct from the container type. }
  TIndexValue = class
    Value: LongInt;
  end;
  { One generic result flows through both named and default indexed access. }
  TIndexBag<T> = class
  private
    FItem: T;
    function GetItem(AIndex: LongInt): T;
    procedure PutItem(AIndex: LongInt; AValue: T);
  public
    constructor Create(AItem: T);
    property Items[AIndex: LongInt]: T read GetItem write PutItem; default;
    property Current: T read FItem;
    property WriteOnly[AIndex: LongInt]: T write PutItem;
  end;
  { Absence of a new default declaration leaves the parent's default available. }
  TIndexChild = class(TIndexBag<TIndexValue>);
  { A same-named declaration tests whether default lookup follows normal hiding. }
  TIndexHidden = class(TIndexChild)
  private
    function GetNumber(AIndex: LongInt): LongInt;
  public
    property Items[AIndex: LongInt]: LongInt read GetNumber;
  end;
  { A new explicitly default name replaces the inherited default contract. }
  TIndexAlternate = class(TIndexChild)
  private
    function GetOther(const AKey: String): TIndexValue;
  public
    property Other[const AKey: String]: TIndexValue read GetOther; default;
  end;
  { Record default properties have value receivers, without class ancestry. }
  TIndexRecord = record
    Item: TIndexValue;
    function GetItem(AIndex: LongInt): TIndexValue;
    property Items[AIndex: LongInt]: TIndexValue read GetItem; default;
  end;

function IndexGetterCalls: LongInt;
{ Generic templates may call this exported bridge without naming private data. }
procedure RecordIndexCall;
function IndexedPropertyProof: LongInt;

implementation

var
  GCalls: LongInt;

constructor TIndexBag<T>.Create(AItem: T);
begin
  inherited Create;
  FItem := AItem;
end;

function TIndexBag<T>.GetItem(AIndex: LongInt): T;
begin
  RecordIndexCall;
  Result := FItem;
end;

procedure TIndexBag<T>.PutItem(AIndex: LongInt; AValue: T);
begin
  RecordIndexCall;
  FItem := AValue;
end;

function TIndexHidden.GetNumber(AIndex: LongInt): LongInt;
begin
  Inc(GCalls);
  Result := 7;
end;

function TIndexAlternate.GetOther(const AKey: String): TIndexValue;
begin
  Result := Current;
  Inc(GCalls);
end;

function TIndexRecord.GetItem(AIndex: LongInt): TIndexValue;
begin
  Inc(GCalls);
  Result := Item;
end;

function IndexGetterCalls: LongInt;
begin
  Result := GCalls;
end;

procedure RecordIndexCall;
begin
  Inc(GCalls);
end;

function IndexedPropertyProof: LongInt;
var
  LValue: TIndexValue;
  LChild: TIndexChild;
  LHidden: TIndexHidden;
  LAlternate: TIndexAlternate;
  LRecord: TIndexRecord;
begin
  LValue := TIndexValue.Create;
  LChild := TIndexChild.Create(LValue);
  LHidden := TIndexHidden.Create(LValue);
  LAlternate := TIndexAlternate.Create(LValue);
  try
    LValue.Value := 7;
    LRecord.Item := LValue;
    Result := LChild[0].Value + LChild.Items[1].Value + LHidden[0].Value +
      LHidden.Items[0] + LAlternate['key'].Value + LRecord[0].Value;
  finally
    LAlternate.Free;
    LHidden.Free;
    LChild.Free;
    LValue.Free;
  end;
end;

end.
