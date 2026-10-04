{ nyx
  Copyright (c) 2020 mr-highball

  Permission is hereby granted, free of charge, to any person obtaining a copy
  of this software and associated documentation files (the "Software"), to deal
  in the Software without restriction, including without limitation the rights
  to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
  copies of the Software, and to permit persons to whom the Software is
  furnished to do so, subject to the following conditions:

  The above copyright notice and this permission notice shall be included in all
  copies or substantial portions of the Software.

  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
  SOFTWARE.
}

unit nyx.collections;

{$mode delphi}{$H+}
{$codepage utf8}

interface

uses
  SysUtils,
  nyx.text,
  nyx.state,
  nyx.contract;

const
  NyxMaximumCollectionFields = 64;
  NyxMaximumCollectionItems = 16384;
  NyxMaximumCollectionBytes = 8 * 1024 * 1024;
  NyxMaximumCollectionEdits = 256;
  NyxMaximumCollectionSubscriptions = 1024;

type
  ENyxCollection = class(ENyxState);
  { A committed edit can still report failed observers. Validators reject before
    publication; this exception explicitly distinguishes the committed phase. }
  ENyxCollectionNotification = class(ENyxCollection);

  { Open names are data, with exact case-sensitive Unicode identity. Default
    records are invalid. Collection/item identities and each field family are
    distinct Pascal types, so a Boolean field cannot enter a text operation. }
  TNyxCollectionRef = record
  private
    FName: TNyxText;
    FInitialized: Boolean;
    function GetName: TNyxText;
  public
    property Name: TNyxText read GetName;
  end;

  { Item identity is scoped to its collection, never to a visible row index.
    A reference from another collection is rejected even when IDs happen to match.
    Cloned independent stores can deliberately retain the same logical scope. }
  TNyxItemRef = record
  private
    FCollection: TNyxCollectionRef;
    FID: TNyxText;
    FInitialized: Boolean;
    function GetCollection: TNyxCollectionRef;
    function GetID: TNyxText;
  public
    { An undefined reference is an explicit optional focus/anchor, never an
      item with an empty ID. Checked identity readers retain their admission. }
    property Defined: Boolean read FInitialized;
    property Collection: TNyxCollectionRef read GetCollection;
    property ID: TNyxText read GetID;
  end;

  TNyxTextFieldRef = record
  private
    FName: TNyxText;
    FInitialized: Boolean;
    function GetName: TNyxText;
  public
    property Name: TNyxText read GetName;
  end;
  TNyxBooleanFieldRef = record
  private
    FName: TNyxText;
    FInitialized: Boolean;
    function GetName: TNyxText;
  public
    property Name: TNyxText read GetName;
  end;
  TNyxIntegerFieldRef = record
  private
    FName: TNyxText;
    FInitialized: Boolean;
    function GetName: TNyxText;
  public
    property Name: TNyxText read GetName;
  end;
  TNyxNumberFieldRef = record
  private
    FName: TNyxText;
    FInitialized: Boolean;
    function GetName: TNyxText;
  public
    property Name: TNyxText read GetName;
  end;

  { Immutable field definition, copied by readers. Defaults materialize missing
    fields on insert/replace; updates change only explicitly supplied fields.
    Domains retain the existing scalar kind/choice/range rules. }
  TNyxCollectionField = record
  private
    FName: TNyxText;
    FDefault: TNyxStateValue;
    FDomain: TNyxValueDomain;
    FDefaultBytes: Integer;
    function GetDefault: TNyxStateValue;
    function GetDomain: TNyxValueDomain;
    function GetKind: TNyxStateKind;
  public
    property Name: TNyxText read FName;
    property Kind: TNyxStateKind read GetKind;
    property DefaultValue: TNyxStateValue read GetDefault;
    property Domain: TNyxValueDomain read GetDomain;
  end;

  { Immutable fluent schema. Each call returns an independently owned definition;
    changing a builder never changes an earlier schema or live collection.
    Field order is deliberate and duplicate names are refused across families.
    The normal authoring API uses the typed methods, never behavioral strings. }
  TNyxCollectionSchema = record
  private
    FFields: array of TNyxCollectionField;
    FInitialized: Boolean;
    FMetadataBytes: Integer;
    function AddField(const AName: TNyxText; const ADefault: TNyxStateValue;
      const ADomain: TNyxValueDomain): TNyxCollectionSchema;
    function GetCount: Integer;
    function GetDataBytes: Integer;
    function IndexOf(const AName: TNyxText): Integer;
  public
    function Text(const AField: TNyxTextFieldRef;
      const ADefault: TNyxText): TNyxCollectionSchema; overload;
    function Text(const AField: TNyxTextFieldRef;
      const ADefault: TNyxText; const ADomain: TNyxTextDomain): TNyxCollectionSchema; overload;
    function Boolean(const AField: TNyxBooleanFieldRef;
      const ADefault: Boolean): TNyxCollectionSchema; overload;
    function Boolean(const AField: TNyxBooleanFieldRef;
      const ADefault: Boolean; const ADomain: TNyxBooleanDomain): TNyxCollectionSchema; overload;
    function Integer(const AField: TNyxIntegerFieldRef;
      const ADefault: Integer): TNyxCollectionSchema; overload;
    function Integer(const AField: TNyxIntegerFieldRef;
      const ADefault: Integer; const ADomain: TNyxIntegerDomain): TNyxCollectionSchema; overload;
    function Number(const AField: TNyxNumberFieldRef;
      const ADefault: Double): TNyxCollectionSchema; overload;
    function Number(const AField: TNyxNumberFieldRef;
      const ADefault: Double; const ADomain: TNyxNumberDomain): TNyxCollectionSchema; overload;
    function FieldAt(AIndex: Integer): TNyxCollectionField;
    { Explicit descriptor/codec boundary. Normal authoring uses the four typed
      field families above; tagged defaults and defined domains must agree
      exactly. NyxNoDomain omits a constraint while retaining the default's type. }
    function Field(const AName: TNyxText; const ADefault: TNyxStateValue;
      const ADomain: TNyxValueDomain): TNyxCollectionSchema;
    function Copy: TNyxCollectionSchema;
    function SameSchema(const AOther: TNyxCollectionSchema): Boolean;
    procedure Validate;
    property Count: Integer read GetCount;
    { Logical UTF-8 metadata/default/domain payload, included in store budgets. }
    property DataBytes: Integer read GetDataBytes;
  end;

  { Immutable partial/full item value. WithValue returns a new value, preserving
    prior rows and caller arrays under native and pas2js record semantics.
    Reads require the exact scalar family. FieldAt-style access is an explicit
    tagged descriptor boundary, useful for inspectors and future persistence.
    Unknown fields/types are refused when the item enters its collection schema. }
  TNyxCollectionItem = record
  private
    FRef: TNyxItemRef;
    FNames: array of TNyxText;
    FValues: array of TNyxStateValue;
    FValueBytes: array of Integer;
    FPayloadBytes: Integer;
    function Put(const AName: TNyxText; const AValue: TNyxStateValue): TNyxCollectionItem;
    function ReadValue(const AName: TNyxText; AKind: TNyxStateKind): TNyxStateValue;
    function IndexOf(const AName: TNyxText): Integer;
    function GetCount: Integer;
    function GetRef: TNyxItemRef;
  public
    function WithValue(const AField: TNyxTextFieldRef;
      const AValue: TNyxText): TNyxCollectionItem; overload;
    function GetValue(const AField: TNyxTextFieldRef): TNyxText; overload;
    function WithValue(const AField: TNyxBooleanFieldRef;
      const AValue: Boolean): TNyxCollectionItem; overload;
    function GetValue(const AField: TNyxBooleanFieldRef): Boolean; overload;
    function WithValue(const AField: TNyxIntegerFieldRef;
      const AValue: Integer): TNyxCollectionItem; overload;
    function GetValue(const AField: TNyxIntegerFieldRef): Integer; overload;
    function WithValue(const AField: TNyxNumberFieldRef;
      const AValue: Double): TNyxCollectionItem; overload;
    function GetValue(const AField: TNyxNumberFieldRef): Double; overload;
    function Has(const AField: TNyxTextFieldRef): Boolean; overload;
    function Has(const AField: TNyxBooleanFieldRef): Boolean; overload;
    function Has(const AField: TNyxIntegerFieldRef): Boolean; overload;
    function Has(const AField: TNyxNumberFieldRef): Boolean; overload;
    function FieldName(AIndex: Integer): TNyxText;
    function FieldValue(AIndex: Integer): TNyxStateValue;
    function Copy: TNyxCollectionItem;
    function SameItem(const AOther: TNyxCollectionItem): Boolean;
    property Ref: TNyxItemRef read GetRef;
    property Count: Integer read GetCount;
  end;

  { Final indexes use the sequence after removing the moved item. Insert accepts
    0..Count; Move accepts 0..Count-1. Unknown removals/moves/updates reject, rather
    than silently acting on a stale visible index. Batch entries execute in order
    on one detached proposal, with one final publication/revision. }
  TNyxCollectionEditKind = (nceInsert, nceRemove, nceMove, nceUpdate, nceReplace);
  TNyxCollectionEdit = record
  private
    FKind: TNyxCollectionEditKind;
    FItem: TNyxCollectionItem;
    FRef: TNyxItemRef;
    FIndex: Integer;
    FInitialized: Boolean;
  public
    property Kind: TNyxCollectionEditKind read FKind;
  end;

  INyxCollectionSnapshot = interface;
  INyxCollectionChanges = interface;
  INyxCollection = interface;

  { Managed read-only owned snapshot. Retain this interface across later edits or
    store disposal. The implementation shares private immutable rows, never a
    caller-mutable array. Snapshot lookup is indexed; rows keep stable identities.
    DataBytes measures logical UTF-8/scalar payload, excluding index/record overhead. }
  INyxCollectionSnapshot = interface
    ['{361963FA-2B3D-4438-B23F-93D50C7F11EE}']
    function GetKey: TNyxCollectionRef;
    function GetSchema: TNyxCollectionSchema;
    function GetCount: Integer;
    function GetRevision: Integer;
    function GetDataBytes: Integer;
    function ItemAt(AIndex: Integer): TNyxCollectionItem;
    function Item(const ARef: TNyxItemRef): TNyxCollectionItem;
    function IndexOf(const ARef: TNyxItemRef): Integer;
    function Has(const ARef: TNyxItemRef): Boolean;
    property Key: TNyxCollectionRef read GetKey;
    property Schema: TNyxCollectionSchema read GetSchema;
    property Count: Integer read GetCount;
    property Revision: Integer read GetRevision;
    property DataBytes: Integer read GetDataBytes;
  end;

  { Ordered operation log with retained before/after datasets. Positions and row
    values describe each step of the batch, allowing incremental consumers to
    replay it. Pure/no-op updates are omitted. A batch returning to its exact
    baseline does not notify or advance revision. Accessors refuse absent rows. }
  INyxCollectionChanges = interface
    ['{A0D04F31-BCB8-4B38-A08C-0433B18AAD20}']
    function GetCount: Integer;
    function GetBefore: INyxCollectionSnapshot;
    function GetAfter: INyxCollectionSnapshot;
    function Kind(AIndex: Integer): TNyxCollectionEditKind;
    function ItemRef(AIndex: Integer): TNyxItemRef;
    function BeforeIndex(AIndex: Integer): Integer;
    function AfterIndex(AIndex: Integer): Integer;
    function BeforeItem(AIndex: Integer): TNyxCollectionItem;
    function AfterItem(AIndex: Integer): TNyxCollectionItem;
    property Count: Integer read GetCount;
    property Before: INyxCollectionSnapshot read GetBefore;
    property After: INyxCollectionSnapshot read GetAfter;
  end;

  TNyxCollectionValidator = procedure(const ACandidate: INyxCollectionSnapshot;
    const AChanges: INyxCollectionChanges) of object;
  TNyxCollectionObserver = procedure(const ACollection: INyxCollection;
    const AChanges: INyxCollectionChanges) of object;

  { Caller retains/disconnects this managed token before freeing its callback
    receiver. Releasing the last interface disconnects it. Tokens borrow the
    store; they do not keep it alive or form a reference cycle. Store disposal
    detaches outstanding tokens. Callbacks may disconnect/release tokens;
    mutation/new subscriptions during callbacks are refused. }
  INyxCollectionSubscription = interface
    ['{DB85AA78-96DA-4D5C-9D33-65156C59B973}']
    function GetConnected: Boolean;
    procedure Disconnect;
    property Connected: Boolean read GetConnected;
  end;

  { Managed mutation contract, independent of renderer types or implementation.
    All data/subscriptions are confined to the owning UI thread. A scheduler
    marshals worker results before calling Apply. Validators see a full immutable
    proposal while Snapshot still returns the accepted baseline. Every observer
    sees the complete committed dataset; failures do not prevent other observers.
    Snapshot and Clone own data without UI/control backreferences or listeners. }
  INyxCollection = interface
    ['{DE4EC1E9-8C3F-4B86-A2B2-7C7B809F86F5}']
    function Snapshot: INyxCollectionSnapshot;
    function Append(const AItem: TNyxCollectionItem): INyxCollection;
    function Insert(AIndex: Integer; const AItem: TNyxCollectionItem): INyxCollection;
    function Remove(const ARef: TNyxItemRef): INyxCollection;
    function Move(const ARef: TNyxItemRef; AIndex: Integer): INyxCollection;
    function Update(const AItem: TNyxCollectionItem): INyxCollection;
    function Replace(const AItem: TNyxCollectionItem): INyxCollection;
    procedure Apply(const AEdits: array of TNyxCollectionEdit; AExpectedRevision: Integer = -1);
    { Assign requires the same key/schema, admits all rows independently and
      reports remove/insert steps. Clone preserves revision and has no listeners. }
    procedure Assign(const ASource: INyxCollectionSnapshot; AExpectedRevision: Integer = -1);
    function Clone: INyxCollection;
    function Subscribe(AObserver: TNyxCollectionObserver;
      AValidator: TNyxCollectionValidator = nil): INyxCollectionSubscription;
  end;

{ Typed identity/value/schema factories. Names require 1..128 valid Unicode
  scalars without controls, matching Nyx's portable state-reference convention. }
function NyxCollection(const AName: TNyxText): TNyxCollectionRef;
function NyxItem(const ACollection: TNyxCollectionRef; const AID: TNyxText): TNyxItemRef;
function NyxTextField(const AName: TNyxText): TNyxTextFieldRef;
function NyxBooleanField(const AName: TNyxText): TNyxBooleanFieldRef;
function NyxIntegerField(const AName: TNyxText): TNyxIntegerFieldRef;
function NyxNumberField(const AName: TNyxText): TNyxNumberFieldRef;
function NyxCollectionSchema: TNyxCollectionSchema;
function NyxCollectionItem(const ARef: TNyxItemRef): TNyxCollectionItem;
function NewNyxCollection(const AKey: TNyxCollectionRef;
  const ASchema: TNyxCollectionSchema): INyxCollection; overload;
{ Whole admitted defaults start at revision zero without synthetic edits or
  subscribers. Rows are independently normalized; duplicate identity, scope,
  schema and payload failures release the complete unpublished implementation. }
function NewNyxCollection(const AKey: TNyxCollectionRef;
  const ASchema: TNyxCollectionSchema;
  const AItems: array of TNyxCollectionItem): INyxCollection; overload;
{ Admit a foreign read-only snapshot without trusting its reported bytes/index.
  Seeded stores start at revision zero. Runtime Clone preserves its revision. }
function NewNyxCollection(const ADefaults: INyxCollectionSnapshot): INyxCollection; overload;
function NyxInsert(AIndex: Integer; const AItem: TNyxCollectionItem): TNyxCollectionEdit;
function NyxRemove(const ARef: TNyxItemRef): TNyxCollectionEdit;
function NyxMove(const ARef: TNyxItemRef; AIndex: Integer): TNyxCollectionEdit;
function NyxUpdate(const AItem: TNyxCollectionItem): TNyxCollectionEdit;
function NyxReplace(const AItem: TNyxCollectionItem): TNyxCollectionEdit;

implementation

uses
  nyx.data;

{$I nyx.collections.implementation.inc}

end.
