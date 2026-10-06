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

program nyx_control_contracts;

{$mode delphi}{$H+}
{$codepage utf8}

uses
  Classes,
  SysUtils,
  StrUtils,
  nyx.text,
  nyx.types,
  nyx.model,
  nyx.catalog;

type
  { The public families carry scalar/property meaning. The finite kind map is
    reviewed here; the emitted Pascal is checked in and compiled normally.
    This maintenance tool never runs inside Studio or a consumer application. }
  TControlFamily = (cfCaption, cfContainer, cfText, cfBoolean, cfInteger,
    cfImage, cfChoice, cfReference, cfSplit);
const
  CFamilyNames: array[TControlFamily] of String = ('CaptionControl', 'LayoutControl',
    'TextInput', 'BooleanInput', 'IntegerInput', 'ImageControl', 'ChoiceControl',
    'ReferenceControl', 'SplitControl');
  CKindFamilies: array[TNyxKind] of TControlFamily = (
    cfContainer, cfContainer, cfContainer, cfContainer, cfContainer, cfContainer,
    cfContainer, cfContainer, cfContainer, cfContainer, cfContainer,
    cfCaption, cfCaption, cfCaption, cfCaption, cfText, cfText, cfBoolean,
    cfBoolean, cfBoolean, cfChoice, cfInteger, cfInteger, cfText, cfText, cfText,
    cfCaption, cfCaption, cfCaption, cfImage, cfImage, cfInteger, cfCaption,
    cfCaption, cfCaption, cfCaption, cfCaption, cfText, cfContainer, cfReference, cfSplit,
    cfContainer, cfContainer, cfText, cfContainer, cfContainer, cfContainer,
    cfContainer, cfInteger, cfChoice, cfInteger, cfContainer, cfContainer,
    cfInteger, cfContainer, cfContainer, cfContainer, cfContainer, cfContainer,
    cfContainer, cfContainer, cfContainer, cfContainer, cfContainer, cfContainer,
    cfContainer, cfContainer, cfContainer, cfInteger, cfContainer, cfContainer,
    cfContainer, cfContainer, cfContainer, cfContainer, cfContainer, cfContainer
  );
var
  GTypes: TStringList;
  GFactories: TStringList;
  GImplementation: TStringList;
  GFacadeTypes: TStringList;
  GFacadeImplementation: TStringList;

function Stem(const AName: TNyxText): String;
var
  LIndex: Integer;
  LUpper: Boolean;
  LChar: Char;
begin
  Result := '';
  LUpper := True;
  for LIndex := 1 to Length(AName) do
  begin
    LChar := AName[LIndex];

    if LChar in ['-', '/'] then
    begin
      LUpper := True;
    end
    else
    begin

      if LUpper then
      begin
        LChar := UpCase(LChar);
      end;
      Result := Result + LChar;
      LUpper := False;
    end;
  end;
end;

function KindGUID(const AName: TNyxText): String;
var
  LHash: QWord;
  LIndex: Integer;
begin
  { Stable name-based GUIDs do not change when a future kind is inserted into
    the enum. Multiplication fits in QWord before truncation to 32 bits. }
  LHash := 2166136261;
  for LIndex := 1 to Length(AName) do
  begin
    LHash := ((LHash xor Ord(AName[LIndex])) * 16777619) and $FFFFFFFF;
  end;
  Result := '{737A7921-4621-4C6F-8C01-0200' + IntToHex(LHash, 8) + '}';
end;

procedure EmitControls;
var
  LKind: TNyxKind;
  LStem: String;
  LFamily: String;
  LInterface: String;
  LClass: String;
  LSeen: TStringList;
  LCatalog: TNyxCatalog;
  LRecipe: TNyxNode;

  procedure EmitParts(ANode: TNyxNode; const APrefix: String;
    ADeclarations, AImplementations: Boolean);
  var
    LIndex: Integer;
    LPart: TNyxNode;
    LPath: String;
    LName: String;
    LPartType: String;
  begin
    for LIndex := 0 to ANode.Count - 1 do
    begin
      LPart := ANode.Children[LIndex];
      LPath := String(LPart.Prop(NyxAttributeName(atPart)));

      if LPath = '' then
      begin
        Continue;
      end;

      if APrefix <> '' then
      begin
        LPath := APrefix + '/' + LPath;
      end;
      LName := Stem(LPath);
      LPartType := Stem(LPart.Kind);

      if not EndsText(LPartType, LName) then
      begin
        LName := LName + LPartType;
      end;

      if SameText(LName, 'Label') then
      begin
        LName := 'CaptionLabel';
      end;

      if ADeclarations then
      begin
        GTypes.Add('    { Retains the named ' + LPath + ' part; missing/retyped parts raise a contract error. }');
        GTypes.Add('    function Get' + LName + ': INyx' + LPartType + ';');
        GTypes.Add('    property ' + LName + ': INyx' + LPartType + ' read Get' + LName + ';');
      end;

      if AImplementations then
      begin
        GImplementation.Add('function ' + LClass + '.Get' + LName + ': INyx' + LPartType + ';');
        GImplementation.Add('begin');
        GImplementation.Add('  Result := Part(NyxPart(''' + LPath + ''')) as INyx' + LPartType + ';');
        GImplementation.Add('end;');
        GImplementation.Add('');
      end;
      EmitParts(LPart, LPath, ADeclarations, AImplementations);
    end;
  end;
begin
  LSeen := TStringList.Create;
  LCatalog := TNyxCatalog.Create;
  LRecipe := nil;
  try
    { Named compound accessors may return a later interface in the catalog. }
    for LKind := Low(TNyxKind) to High(TNyxKind) do
    begin
      GTypes.Add('  INyx' + Stem(NyxKindName(LKind)) + ' = interface;');
    end;
    GTypes.Add('');
    for LKind := Low(TNyxKind) to High(TNyxKind) do
    begin
      LStem := Stem(NyxKindName(LKind));
      LInterface := 'INyx' + LStem;
      LClass := 'TNyx' + LStem;
      LFamily := CFamilyNames[CKindFamilies[LKind]];
      FreeAndNil(LRecipe);

      if (LKind >= nkLabeledButton) and (LKind <> nkSlotOverride) then
      begin
        LRecipe := LCatalog.NewNode(LKind, 'contract-recipe');
      end;

      if LSeen.IndexOf(KindGUID(NyxKindName(LKind))) >= 0 then
      begin
        raise Exception.Create('Control interface GUID collision');
      end;
      LSeen.Add(KindGUID(NyxKindName(LKind)));
      GTypes.Add('  { Specialized ' + String(NyxKindName(LKind)) +
        ' contract; inherited properties retain the ' + LFamily + ' family.');
      GTypes.Add('    WithText returns this exact interface for further fluent composition. }');
      GTypes.Add('  ' + LInterface + ' = interface(INyx' + LFamily + ')');
      GTypes.Add('    [''' + KindGUID(NyxKindName(LKind)) + ''']');
      GTypes.Add('    function WithText(const AText: TNyxText): ' + LInterface + ';');

      if LRecipe <> nil then
      begin
        EmitParts(LRecipe, '', True, False);
      end;
      GTypes.Add('  end;');
      GTypes.Add('');
      GTypes.Add('  { Default managed ' + String(NyxKindName(LKind)) +
        ' implementation. Subclass for behavior or implement the interface');
      GTypes.Add('    independently; adapters require only the retained portable Node contract. }');
      GTypes.Add('  ' + LClass + ' = class(TNyx' + LFamily + ', ' + LInterface + ')');
      GTypes.Add('  public');
      GTypes.Add('    constructor Create(const AID: TNyxText = '''';');
      GTypes.Add('      AConstruction: TNyxConstruction = ncoDefault); reintroduce;');
      GTypes.Add('    function WithText(const AText: TNyxText): ' + LInterface + ';');

      if LRecipe <> nil then
      begin
        EmitParts(LRecipe, '', True, False);
      end;
      GTypes.Add('  end;');
      GTypes.Add('');
      GFactories.Add('{ Own a managed ' + String(NyxKindName(LKind)) +
        '. Default compounds include independently owned recipe parts;');
      GFactories.Add('  ncoDescriptor constructs an empty exact descriptor for source reconstruction. }');
      GFactories.Add('function NewNyx' + LStem + '(const AID: TNyxText = '''';');
      GFactories.Add('  AConstruction: TNyxConstruction = ncoDefault): ' + LInterface + ';');
      GFactories.Add('');
      GImplementation.Add('constructor ' + LClass + '.Create(const AID: TNyxText;');
      GImplementation.Add('  AConstruction: TNyxConstruction);');
      GImplementation.Add('begin');
      GImplementation.Add('  inherited Create(nk' + LStem + ', AID, AConstruction);');
      GImplementation.Add('end;');
      GImplementation.Add('');
      GImplementation.Add('function ' + LClass + '.WithText(const AText: TNyxText): ' + LInterface + ';');
      GImplementation.Add('begin');
      GImplementation.Add('  Text := AText;');
      GImplementation.Add('  Result := Self as ' + LInterface + ';');
      GImplementation.Add('end;');
      GImplementation.Add('');
      GImplementation.Add('function NewNyx' + LStem + '(const AID: TNyxText;');
      GImplementation.Add('  AConstruction: TNyxConstruction): ' + LInterface + ';');
      GImplementation.Add('begin');
      GImplementation.Add('  Result := ' + LClass + '.Create(AID, AConstruction);');
      GImplementation.Add('end;');
      GImplementation.Add('');

      if LRecipe <> nil then
      begin
        EmitParts(LRecipe, '', False, True);
      end;
    end;
    GImplementation.Add('function NewNyxBuiltinControl(AKind: TNyxKind; const AID: TNyxText;');
    GImplementation.Add('  AConstruction: TNyxConstruction): INyxControl;');
    GImplementation.Add('begin');
    GImplementation.Add('  case AKind of');
    for LKind := Low(TNyxKind) to High(TNyxKind) do
    begin
      LStem := Stem(NyxKindName(LKind));
      GImplementation.Add('    nk' + LStem + ':');
      GImplementation.Add('    begin');
      GImplementation.Add('      Result := NewNyx' + LStem + '(AID, AConstruction);');
      GImplementation.Add('    end;');
    end;
    GImplementation.Add('  end;');
    GImplementation.Add('end;');
    GImplementation.Add('');
    GImplementation.Add('function RetainNyxControl(ANode: TNyxNode): INyxControl;');
    GImplementation.Add('var');
    GImplementation.Add('  LKind: TNyxKind;');
    GImplementation.Add('  LReference: INyxNode;');
    GImplementation.Add('begin');
    GImplementation.Add('');
    GImplementation.Add('  if ANode = nil then');
    GImplementation.Add('  begin');
    GImplementation.Add('    raise ENyxModel.Create(''A retained descriptor is required'');');
    GImplementation.Add('  end;');
    GImplementation.Add('');
    GImplementation.Add('  LReference := ANode.ComponentReference;');
    GImplementation.Add('');
    GImplementation.Add('  if (LReference <> nil) and Supports(LReference, INyxControl, Result) then');
    GImplementation.Add('  begin');
    GImplementation.Add('    Exit;');
    GImplementation.Add('  end;');
    GImplementation.Add('');
    GImplementation.Add('  if not TryNyxKind(ANode.Kind, LKind) then');
    GImplementation.Add('  begin');
    GImplementation.Add('    Exit(TNyxControl.CreateFromNode(ANode));');
    GImplementation.Add('  end;');
    GImplementation.Add('  case LKind of');
    for LKind := Low(TNyxKind) to High(TNyxKind) do
    begin
      LStem := Stem(NyxKindName(LKind));
      GImplementation.Add('    nk' + LStem + ':');
      GImplementation.Add('    begin');
      GImplementation.Add('      Result := TNyx' + LStem + '.CreateFromNode(ANode);');
      GImplementation.Add('    end;');
    end;
    GImplementation.Add('  end;');
    GImplementation.Add('end;');
  finally
    LRecipe.Free;
    LCatalog.Free;
    LSeen.Free;
  end;
end;

function Arguments(const ASignature: String): String;
var
  LStart: Integer;
  LFinish: Integer;
  LParameters: String;
  LGroups: TStringList;
  LIndex: Integer;
  LNames: String;
begin
  Result := '';
  LStart := Pos('(', ASignature);
  LFinish := LastDelimiter(')', ASignature);

  if LStart = 0 then
  begin
    Exit;
  end;
  LParameters := Copy(ASignature, LStart + 1, LFinish - LStart - 1);
  LGroups := TStringList.Create;
  try
    LGroups.StrictDelimiter := True;
    LGroups.Delimiter := ';';
    LGroups.DelimitedText := LParameters;
    for LIndex := 0 to LGroups.Count - 1 do
    begin
      LNames := Trim(Copy(LGroups[LIndex], 1, Pos(':', LGroups[LIndex]) - 1));

      if Pos('const ', LNames) = 1 then
      begin
        Delete(LNames, 1, 6);
      end;

      if Pos('out ', LNames) = 1 then
      begin
        Delete(LNames, 1, 4);
      end;

      if Pos('var ', LNames) = 1 then
      begin
        Delete(LNames, 1, 4);
      end;

      if Result <> '' then
      begin
        Result := Result + ', ';
      end;
      Result := Result + LNames;
    end;
  finally
    LGroups.Free;
  end;
end;

procedure EmitFacade(const AFile, ASourceClass, AFacade, AMember, AGUID: String);
var
  LLines: TStringList;
  LMethods: TStringList;
  LIndex: Integer;
  LSignature: String;
  LHeader: String;
  LName: String;
  LStart: Integer;
  LFinish: Integer;
  LInClass: Boolean;
  LPublic: Boolean;
  LFunction: Boolean;
  LFluent: Boolean;
  LCall: String;
begin
  LLines := TStringList.Create;
  LMethods := TStringList.Create;
  try
    LLines.LoadFromFile(AFile);
    LInClass := False;
    LPublic := False;
    LIndex := 0;
    while LIndex < LLines.Count do
    begin
      LSignature := Trim(LLines[LIndex]);

      if LSignature = ASourceClass + ' = class' then
      begin
        LInClass := True;
      end
      else if LInClass and (LSignature = 'public') then
      begin
        LPublic := True;
      end
      else if LInClass and (LSignature = 'end;') then
      begin
        Break;
      end
      else if LPublic and ((Pos('function ', LSignature) = 1) or
        (Pos('procedure ', LSignature) = 1)) then
      begin
        while (Pos(';', LSignature) = 0) or
          ((Pos('(', LSignature) > 0) and (Pos(')', LSignature) = 0)) do
        begin
          Inc(LIndex);
          LSignature := LSignature + ' ' + Trim(LLines[LIndex]);
        end;
        { Methods taking raw stores/returning owned raw clones remain at the
          explicit descriptor boundary, rather than leaking borrowed stores. }

        if (Pos('Assign(', LSignature) = 0) and (Pos('Overlay(', LSignature) = 0) and
          (Pos('Clone:', LSignature) = 0) then
        begin
          LMethods.Add(LSignature);
        end;
      end;
      Inc(LIndex);
    end;

    if LMethods.Count = 0 then
    begin
      raise Exception.Create('No public facade methods: ' + ASourceClass);
    end;
    GFacadeTypes.Add('  { Managed ' + AMember + ' authoring retains its control. Returned values are');
    GFacadeTypes.Add('    independent snapshots; release this interface normally, never Free it. }');
    GFacadeTypes.Add('  I' + AFacade + ' = interface(IInterface)');
    GFacadeTypes.Add('    [''' + AGUID + ''']');
    for LIndex := 0 to LMethods.Count - 1 do
    begin
      LSignature := StringReplace(LMethods[LIndex], ASourceClass, 'I' + AFacade, [rfReplaceAll]);

      if Pos('function Done:', LSignature) = 1 then
      begin
        LSignature := 'function Done: INyxControl;';
      end;
      GFacadeTypes.Add('    ' + LSignature);
    end;
    GFacadeTypes.Add('  end;');
    GFacadeTypes.Add('');
    GFacadeTypes.Add('  { Fresh, acyclic facade: it owns the control interface; the control does');
    GFacadeTypes.Add('    not cache this facade. Fluent chains return the same managed object. }');
    GFacadeTypes.Add('  T' + AFacade + ' = class(TInterfacedObject, I' + AFacade + ')');
    GFacadeTypes.Add('  private');
    GFacadeTypes.Add('    FOwner: INyxControl;');

    if AMember = 'Configure' then
    begin
      GFacadeTypes.Add('    FPlatform: TNyxPlatform;');
      GFacadeTypes.Add('    FViewport: TNyxViewportCondition;');
      GFacadeTypes.Add('    FPresentation: TNyxPresentationRef;');
      GFacadeTypes.Add('    function ConfigurationScope: TNyxNodeConfig;');
    end;
    GFacadeTypes.Add('  public');
    GFacadeTypes.Add('    constructor Create(const AOwner: INyxControl);');
    for LIndex := 0 to LMethods.Count - 1 do
    begin
      LSignature := StringReplace(LMethods[LIndex], ASourceClass, 'I' + AFacade, [rfReplaceAll]);

      if Pos('function Done:', LSignature) = 1 then
      begin
        LSignature := 'function Done: INyxControl;';
      end;
      GFacadeTypes.Add('    ' + LSignature);
    end;
    GFacadeTypes.Add('  end;');
    GFacadeTypes.Add('');
    GFacadeImplementation.Add('constructor T' + AFacade + '.Create(const AOwner: INyxControl);');
    GFacadeImplementation.Add('begin');
    GFacadeImplementation.Add('  inherited Create;');
    GFacadeImplementation.Add('');
    GFacadeImplementation.Add('  if AOwner = nil then');
    GFacadeImplementation.Add('  begin');
    GFacadeImplementation.Add('    raise ENyxModel.Create(''A managed facade requires its control'');');
    GFacadeImplementation.Add('  end;');
    GFacadeImplementation.Add('  FOwner := AOwner;');
    GFacadeImplementation.Add('end;');
    GFacadeImplementation.Add('');

    if AMember = 'Configure' then
    begin
      GFacadeImplementation.Add('function TNyxConfiguration.ConfigurationScope: TNyxNodeConfig;');
      GFacadeImplementation.Add('begin');
      GFacadeImplementation.Add('  Result := FOwner.Node.Configure.ForPlatform(FPlatform);');
      GFacadeImplementation.Add('');
      GFacadeImplementation.Add('  if FPresentation.Defined then');
      GFacadeImplementation.Add('  begin');
      GFacadeImplementation.Add('    Result := Result.WhenPresentation(FPresentation);');
      GFacadeImplementation.Add('  end');
      GFacadeImplementation.Add('  else');
      GFacadeImplementation.Add('  begin');
      GFacadeImplementation.Add('    Result := Result.WhenViewport(FViewport);');
      GFacadeImplementation.Add('  end;');
      GFacadeImplementation.Add('end;');
      GFacadeImplementation.Add('');
    end;
    for LIndex := 0 to LMethods.Count - 1 do
    begin
      LSignature := LMethods[LIndex];
      LFunction := Pos('function ', LSignature) = 1;

      if LFunction then
      begin
        LStart := 10;
      end
      else
      begin
        LStart := 11;
      end;
      LFinish := Pos('(', LSignature);

      if LFinish = 0 then
      begin
        LFinish := Pos(':', LSignature);
      end;

      if LFinish = 0 then
      begin
        LFinish := Pos(';', LSignature);
      end;
      LName := Copy(LSignature, LStart, LFinish - LStart);
      LFluent := Pos(': ' + ASourceClass + ';', LSignature) > 0;
      LHeader := StringReplace(LSignature, ASourceClass, 'I' + AFacade, [rfReplaceAll]);

      if LName = 'Done' then
      begin
        LHeader := 'function Done: INyxControl;';
      end;
      Insert('T' + AFacade + '.', LHeader, LStart);
      LFinish := LastDelimiter(')', LHeader);

      if LFinish = 0 then
      begin
        LFinish := 1;
      end;
      LFinish := Pos(';', Copy(LHeader, LFinish, MaxInt)) + LFinish - 1;
      LHeader := Copy(LHeader, 1, LFinish);
      GFacadeImplementation.Add(LHeader);

      if (AMember = 'Configure') and ((LName = 'ForPlatform') or
        (LName = 'WhenViewport') or (LName = 'WhenPresentation')) then
      begin
        GFacadeImplementation.Add('var');
        GFacadeImplementation.Add('  LFacade: TNyxConfiguration;');
      end;
      GFacadeImplementation.Add('begin');
      LCall := 'FOwner.Node.' + AMember + '.' + LName;

      if AMember = 'Configure' then
      begin
        LCall := 'ConfigurationScope.' + LName;
      end;

      if Arguments(LSignature) <> '' then
      begin
        LCall := LCall + '(' + Arguments(LSignature) + ')';
      end;

      if (AMember = 'Configure') and (LName = 'ForPlatform') then
      begin
        GFacadeImplementation.Add('  LFacade := TNyxConfiguration.Create(FOwner);');
        GFacadeImplementation.Add('  LFacade.FPlatform := APlatform;');
        GFacadeImplementation.Add('  LFacade.FViewport := FViewport;');
        GFacadeImplementation.Add('  LFacade.FPresentation := FPresentation;');
        GFacadeImplementation.Add('  Result := LFacade;');
      end
      else if (AMember = 'Configure') and (LName = 'WhenViewport') then
      begin
        GFacadeImplementation.Add('  LFacade := TNyxConfiguration.Create(FOwner);');
        GFacadeImplementation.Add('  LFacade.FPlatform := FPlatform;');

        if Pos('AWidth:', LSignature) > 0 then
        begin
          GFacadeImplementation.Add('  LFacade.FViewport := TNyxViewportCondition.FromWidth(AWidth);');
        end
        else
        begin
          GFacadeImplementation.Add('  LFacade.FViewport := ACondition;');
        end;
        GFacadeImplementation.Add('  Result := LFacade;');
      end
      else if (AMember = 'Configure') and (LName = 'WhenPresentation') then
      begin
        GFacadeImplementation.Add('  ConfigurationScope.WhenPresentation(AReference);');
        GFacadeImplementation.Add('  LFacade := TNyxConfiguration.Create(FOwner);');
        GFacadeImplementation.Add('  LFacade.FPlatform := FPlatform;');
        GFacadeImplementation.Add('  LFacade.FPresentation := AReference;');
        GFacadeImplementation.Add('  Result := LFacade;');
      end
      else if LName = 'Done' then
      begin
        GFacadeImplementation.Add('  Result := FOwner;');
      end
      else if LFluent then
      begin
        GFacadeImplementation.Add('  ' + LCall + ';');
        GFacadeImplementation.Add('  Result := Self as I' + AFacade + ';');
      end
      else if LFunction then
      begin
        GFacadeImplementation.Add('  Result := ' + LCall + ';');
      end
      else
      begin
        GFacadeImplementation.Add('  ' + LCall + ';');
      end;
      GFacadeImplementation.Add('end;');
      GFacadeImplementation.Add('');
    end;
  finally
    LMethods.Free;
    LLines.Free;
  end;
end;

procedure SaveInclude(ALines: TStringList; const APath: String);
var
  LLicense: TStringList;
  LSource: String;
  LFinish: Integer;
begin
  LLicense := TStringList.Create;
  try
    LLicense.LoadFromFile('src/nyx.model.pas');
    LSource := LLicense.Text;
    LFinish := Pos('unit nyx.model;', LSource);
    LSource := Copy(LSource, 1, LFinish - 1);
    LSource := StringReplace(LSource, #13#10, #10, [rfReplaceAll]);
  finally
    LLicense.Free;
  end;
  { Includes contain owned Pascal only. Their source is this maintained tool
    and the existing typed descriptor declarations, not external templates. }
  ALines.Insert(0, '{ Maintained by tools/nyx_control_contracts.lpr; edit the contract source, then regenerate. }');
  ALines.Insert(0, LSource);
  ALines.LineBreak := #10;
  ALines.SaveToFile(APath);
end;

begin
  GTypes := TStringList.Create;
  GFactories := TStringList.Create;
  GImplementation := TStringList.Create;
  GFacadeTypes := TStringList.Create;
  GFacadeImplementation := TStringList.Create;
  try
    EmitControls;
    EmitFacade('src/nyx.model.pas', 'TNyxNodeConfig', 'NyxConfiguration', 'Configure',
      '{737A7921-4621-4C6F-8C01-030000000001}');
    EmitFacade('src/nyx.model.pas', 'TNyxNodeBindings', 'NyxBindings', 'Binds',
      '{737A7921-4621-4C6F-8C01-030000000002}');
    EmitFacade('src/nyx.contract.pas', 'TNyxContract', 'NyxControlContract', 'Contract',
      '{737A7921-4621-4C6F-8C01-030000000003}');
    EmitFacade('src/nyx.data.pas', 'TNyxExtensions', 'NyxControlExtensions', 'Extensions',
      '{737A7921-4621-4C6F-8C01-030000000004}');
    SaveInclude(GTypes, 'src/nyx.controls.types.inc');
    SaveInclude(GFactories, 'src/nyx.controls.factories.inc');
    SaveInclude(GImplementation, 'src/nyx.controls.implementation.inc');
    SaveInclude(GFacadeTypes, 'src/nyx.controls.facades.inc');
    SaveInclude(GFacadeImplementation, 'src/nyx.controls.facades.implementation.inc');
    WriteLn('PASS maintained contracts for ', Ord(High(TNyxKind)) + 1,
      ' kinds and four managed authoring facades');
  finally
    GFacadeImplementation.Free;
    GFacadeTypes.Free;
    GImplementation.Free;
    GFactories.Free;
    GTypes.Free;
  end;
end.
