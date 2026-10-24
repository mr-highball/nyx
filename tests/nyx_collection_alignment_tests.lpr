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
program nyx_collection_alignment_tests;

{$mode delphi}{$H+}{$codepage utf8}

uses SysUtils, nyx.text, nyx.data, nyx.json, nyx.state, nyx.collections,
  nyx.collections.arrangement, nyx.collections.alignment
  {$ifdef PAS2JS}, Web{$endif};

const
  CField: TNyxText = 'Path🌙 é';
  CExact: TNyxText = 'C:\服务\🌙' + #0 + #9 + 'é' + #13#10 + 'tail';

var
  GChecks: Integer;

procedure Check(AValue: Boolean; const AMessage: TNyxText);
begin

  if not AValue then
  begin
    raise Exception.Create(AMessage);
  end;
  Inc(GChecks);
end;

{ Use the real strict reader with a retained receiving value. Every refused
  wire must leave that receiving policy unchanged, not partially reset it. }
procedure RefusePolicy(const AJSON: TNyxText);
var
  LPolicy: TNyxColumnAlignmentPolicy;
  LBefore: TNyxText;
  LRefused: Boolean;
begin
  LPolicy := NyxColumnAlignment.Horizontal(nchaEnd).Vertical(ncvaBottom);
  LBefore := LPolicy.ToData.ToJSON;
  LRefused := False;
  try
    LPolicy := TNyxColumnAlignmentPolicy.FromData(TNyxDataValue.ParseJSON(AJSON));
  except
    on EArgumentException do
    begin
      LRefused := True;
    end;
    on ENyxJSON do
    begin
      LRefused := True;
    end;
  end;
  Check(LRefused and (LPolicy.ToData.ToJSON = LBefore),
    'invalid complete alignment wire refuses atomically: ' + AJSON);
end;

{ A preference must also retain its original full typed field on failed
  persistence admission; names alone never authorize a changed family. }
procedure RefusePreference(const AData: TNyxDataValue);
var
  LPreference: TNyxColumnAlignmentPreference;
  LBefore: TNyxText;
  LRefused: Boolean;
begin
  LPreference := NyxColumnAlignmentPreference(NyxColumnIdentity(NyxTextField(CField)),
    NyxColumnAlignment.Horizontal(nchaStart));
  LBefore := LPreference.ToData.ToJSON;
  LRefused := False;
  try
    LPreference := TNyxColumnAlignmentPreference.FromData(AData);
  except
    on EArgumentException do
    begin
      LRefused := True;
    end;
    on ENyxJSON do
    begin
      LRefused := True;
    end;
  end;
  Check(LRefused and (LPreference.ToData.ToJSON = LBefore),
    'invalid field alignment preference refuses without replacing its original field');
end;

procedure Run;
var
  LPolicy: TNyxColumnAlignmentPolicy;
  LOriginal: TNyxColumnAlignmentPolicy;
  LOverride: TNyxColumnAlignmentPolicy;
  LCopy: TNyxColumnAlignmentPolicy;
  LHorizontal: TNyxColumnHorizontalAlignment;
  LVertical: TNyxColumnVerticalAlignment;
  LPreference: TNyxColumnAlignmentPreference;
  LRetained: TNyxColumnAlignmentPreference;
  LData: TNyxDataValue;
  LWire: TNyxText;
  LBefore: TNyxText;
  LRefused: Boolean;
  LOrdinal: Integer;
  LField: TNyxColumnIdentity;
  LSameNameNumber: TNyxColumnIdentity;
begin
  LPolicy := NyxColumnAlignment;
  Check(not LPolicy.Defined and not LPolicy.HasHorizontal and not LPolicy.HasVertical and
    (LPolicy.ToData.Kind = ndNull), 'absent alignment retains legacy host behavior on both axes');
  LCopy := TNyxColumnAlignmentPolicy.FromData(NyxNull);
  Check(not LCopy.Defined and (LCopy.Copy.ToData.ToJSON = 'null'),
    'null roundtrip and copy preserve complete absence');
  LRefused := False;
  try
    LHorizontal := LPolicy.HorizontalAlignment;
  except
    on ENyxCollectionAlignment do
    begin
      LRefused := True;
    end;
  end;
  Check(LRefused, 'an absent horizontal axis never manufactures Start');
  LRefused := False;
  try
    LVertical := LPolicy.VerticalAlignment;
  except
    on ENyxCollectionAlignment do
    begin
      LRefused := True;
    end;
  end;
  Check(LRefused, 'an absent vertical axis never manufactures Top');

  for LHorizontal := Low(TNyxColumnHorizontalAlignment) to High(TNyxColumnHorizontalAlignment) do
  begin
    Check(ReadNyxColumnHorizontalAlignment(NyxColumnHorizontalAlignmentName(LHorizontal)) = LHorizontal,
      'horizontal persistence names are complete exact closed choices');
    for LVertical := Low(TNyxColumnVerticalAlignment) to High(TNyxColumnVerticalAlignment) do
    begin
      LPolicy := NyxColumnAlignment.Horizontal(LHorizontal).Vertical(LVertical);
      LWire := LPolicy.ToData.ToJSON;
      LCopy := TNyxColumnAlignmentPolicy.FromData(TNyxDataValue.ParseJSON(LWire));
      Check(LCopy.Defined and LCopy.HasHorizontal and LCopy.HasVertical and
        (LCopy.HorizontalAlignment = LHorizontal) and (LCopy.VerticalAlignment = LVertical) and
        (LCopy.ToData.ToJSON = LWire), 'every horizontal vertical pair roundtrips exactly');
    end;
  end;
  for LVertical := Low(TNyxColumnVerticalAlignment) to High(TNyxColumnVerticalAlignment) do
  begin
    Check(ReadNyxColumnVerticalAlignment(NyxColumnVerticalAlignmentName(LVertical)) = LVertical,
      'vertical persistence names are complete exact closed choices');
  end;
  Check((ResolveNyxColumnHorizontalAlignment(nchaStart, False) = ncpaLeft) and
    (ResolveNyxColumnHorizontalAlignment(nchaEnd, False) = ncpaRight),
    'logical Start End resolve against explicit left to right direction');
  Check((ResolveNyxColumnHorizontalAlignment(nchaStart, True) = ncpaRight) and
    (ResolveNyxColumnHorizontalAlignment(nchaEnd, True) = ncpaLeft),
    'logical Start End mirror exactly against explicit right to left direction');
  Check((ResolveNyxColumnHorizontalAlignment(nchaCenter, False) = ncpaCenter) and
    (ResolveNyxColumnHorizontalAlignment(nchaCenter, True) = ncpaCenter),
    'Center is independent of reading direction');

  LOriginal := NyxColumnAlignment.Horizontal(nchaEnd).Vertical(ncvaMiddle);
  LBefore := LOriginal.ToData.ToJSON;
  LOverride := NyxColumnAlignment.Vertical(ncvaTop);
  LPolicy := LOriginal.Overlay(LOverride);
  Check((LPolicy.HorizontalAlignment = nchaEnd) and (LPolicy.VerticalAlignment = ncvaTop) and
    not LOverride.HasHorizontal, 'vertical runtime override leaves authored horizontal choice intact');
  LOverride := LOverride.UseDefaultVertical;
  LPolicy := LOriginal.Overlay(LOverride);
  Check(not LOverride.Defined and (LPolicy.ToData.ToJSON = LBefore),
    'clearing a runtime axis restores the complete authored choice');
  LOverride := NyxColumnAlignment.Horizontal(nchaStart);
  LPolicy := LOriginal.Overlay(LOverride);
  Check((LPolicy.HorizontalAlignment = nchaStart) and (LPolicy.VerticalAlignment = ncvaMiddle),
    'horizontal runtime override leaves authored vertical choice intact');
  LPolicy := LPolicy.UseDefaultHorizontal;
  Check(not LPolicy.HasHorizontal and LPolicy.HasVertical and
    (LPolicy.VerticalAlignment = ncvaMiddle), 'horizontal reset preserves independent vertical presence');
  LPolicy := LPolicy.UseDefaultVertical;
  Check(not LPolicy.Defined and (LOriginal.ToData.ToJSON = LBefore),
    'independent resets and derived values preserve their original immutable policy');
  LOriginal := LOriginal.Copy;
  LOriginal := LOriginal.Overlay(LOriginal);
  Check(LOriginal.ToData.ToJSON = LBefore, 'same destination Copy and Overlay preserve every entered axis');

  RefusePolicy('{}');
  RefusePolicy('{"version":2,"horizontal":"start","vertical":null}');
  RefusePolicy('{"version":1.0,"horizontal":"start","vertical":null}');
  RefusePolicy('{"version":1,"horizontal":"start"}');
  RefusePolicy('{"version":1,"horizontal":"start","vertical":null,"unknown":false}');
  RefusePolicy('{"version":1,"horizontal":null,"vertical":null}');
  RefusePolicy('{"version":1,"horizontal":"left","vertical":null}');
  RefusePolicy('{"version":1,"horizontal":"Start","vertical":null}');
  RefusePolicy('{"version":1,"horizontal":false,"vertical":"top"}');
  RefusePolicy('{"version":1,"horizontal":"start","vertical":"center"}');
  RefusePolicy('{"version":1,"horizontal":"start","vertical":2}');
  RefusePolicy('[]');

  LPolicy := LOriginal;
  LOrdinal := 99;
  LRefused := False;
  try
    LPolicy := LPolicy.Horizontal(TNyxColumnHorizontalAlignment(LOrdinal));
  except
    on ENyxCollectionAlignment do
    begin
      LRefused := True;
    end;
  end;
  Check(LRefused and (LPolicy.ToData.ToJSON = LBefore),
    'invalid horizontal ordinal refuses before replacing its receiving policy');
  LRefused := False;
  try
    LPolicy := LPolicy.Vertical(TNyxColumnVerticalAlignment(LOrdinal));
  except
    on ENyxCollectionAlignment do
    begin
      LRefused := True;
    end;
  end;
  Check(LRefused and (LPolicy.ToData.ToJSON = LBefore),
    'invalid vertical ordinal refuses before replacing its receiving policy');

  LField := NyxColumnIdentity(NyxTextField(CField));
  LSameNameNumber := NyxColumnIdentity(NyxNumberField(CField));
  LPreference := NyxColumnAlignmentPreference(LField, LOriginal);
  LWire := LPreference.ToData.ToJSON;
  LRetained := TNyxColumnAlignmentPreference.FromData(TNyxDataValue.ParseJSON(LWire));
  LRetained.ValidateFor(LField);
  Check(LRetained.Field.Same(LField) and (LRetained.Field.Name = CField) and
    (LRetained.ToData.ToJSON = LWire), 'supplementary and combining field identity roundtrips exactly');
  LRefused := False;
  try
    LRetained.ValidateFor(LSameNameNumber);
  except
    on ENyxCollectionAlignment do
    begin
      LRefused := True;
    end;
  end;
  Check(LRefused and (LRetained.ToData.ToJSON = LWire),
    'same name changed scalar family cannot redirect an alignment preference');
  LRefused := False;
  try
    LRetained.ValidateFor(NyxColumnIdentity(NyxTextField('Other')));
  except
    on ENyxCollectionAlignment do
    begin
      LRefused := True;
    end;
  end;
  Check(LRefused, 'a missing destination field cannot acquire a retained alignment preference');
  RefusePreference(NyxObject([NyxField('version', NyxData(1)),
    NyxField('field', LField.ToData), NyxField('alignment', NyxNull)]));
  RefusePreference(NyxObject([NyxField('version', NyxData(1)),
    NyxField('field', NyxNull), NyxField('alignment', LOriginal.ToData)]));
  RefusePreference(TNyxDataValue.ParseJSON('{"version":1,"field":{"version":1,' +
    '"name":"Path","kind":"object"},"alignment":{"version":1,' +
    '"horizontal":"end","vertical":null}}'));
  RefusePreference(NyxObject([NyxField('version', NyxData(1)),
    NyxField('field', LField.ToData)]));
  LPreference := LPreference.Copy;
  LPreference := NyxColumnAlignmentPreference(LSameNameNumber,
    NyxColumnAlignment.Horizontal(nchaStart));
  Check(LRetained.Field.Same(LField) and (LRetained.ToData.ToJSON = LWire),
    'retained copied preference survives replacement of the producing value and field');
  LPreference := TNyxColumnAlignmentPreference.FromData(NyxNull);
  Check(not LPreference.Defined and (LPreference.Copy.ToData.ToJSON = 'null'),
    'absent preference has an exact null roundtrip without a field coordinate');

  { Exact accepted text is carried by the existing data contract, independently
    of presentation. Both field and axis records remain valid after these
    detached packets are replaced; they own neither packet nor a data owner. }
  LData := NyxObject([NyxField('raw', NyxData(CExact)),
    NyxField('alignment', LRetained.ToData)]);
  LCopy := TNyxColumnAlignmentPolicy.FromData(LData.Field('alignment').Field('alignment'));
  Check((LData.Field('raw').AsText = CExact) and
    (LCopy.ToData.ToJSON = LOriginal.ToData.ToJSON),
    'alignment persistence preserves exact accompanying Unicode NUL tab and CRLF RAW text');
  LData := NyxNull;
  LField := Default(TNyxColumnIdentity);
  LOriginal := NyxColumnAlignment;
  Check((LRetained.Field.Name = CField) and (LRetained.Alignment.HorizontalAlignment = nchaEnd) and
    (LRetained.Alignment.VerticalAlignment = ncvaMiddle),
    'retained alignment records need no source packet field or policy owner');
end;

begin
  try
    Run;
    {$ifdef PAS2JS}
    document.body.setAttribute('data-column-alignment', 'passed');
    document.body.setAttribute('data-column-alignment-checks', IntToStr(GChecks));
    {$else}
    WriteLn('PASS ', GChecks, ' typed column alignment value checks; no target paint claim');
    {$endif}
  except
    on E: Exception do
    begin
      {$ifdef PAS2JS}
      document.body.setAttribute('data-column-alignment', 'failed');
      document.body.setAttribute('data-column-alignment-checks', IntToStr(GChecks));
      document.body.setAttribute('data-column-alignment-error', E.Message);
      {$else}
      WriteLn('FAIL ', E.Message);
      DumpExceptionBackTrace(Output);
      ExitCode := 1;
      {$endif}
    end;
  end;
end.
