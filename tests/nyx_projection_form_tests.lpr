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
program nyx_projection_form_tests;

{$mode delphi}{$H+}{$codepage utf8}

uses
  SysUtils, nyx.text, nyx.types, nyx.data, nyx.model, nyx.controls, nyx.state,
  nyx.composition, nyx.projection.refresh
  {$ifdef PAS2JS}, Web{$endif};

const
  CContext = 'test.form.baseline';
  CPolicy = 'test.form.policy';
  COwner = 'test.form.owner';

var
  GChecks: Integer;

procedure Check(AValue: Boolean; const AReason: TNyxText);
begin

  if not AValue then
  begin
    raise ENyxModel.Create('Form projection: ' + AReason);
  end;
  Inc(GChecks);
end;

function ContextFor(const AID: TNyxText): TNyxProjectionFormContext;
begin
  Result := TNyxProjectionFormContext.ForForm(AID, AID,
    [NyxExtension(CContext), NyxExtension(CPolicy)]);
end;

function Fixture: TNyxDocument;
var
  LPage: INyxPage;
  LForm: INyxCard;
  LNested: INyxColumn;
  LInput: INyxInput;
begin
  Result := TNyxDocument.Create;
  try
    LPage := NewNyxPage('forms');
    Result.AddPage(LPage);
    LForm := NewNyxCard('form-a');
    LForm.Node.SetProp(CContext, 'accepted-a').SetProp(COwner, 'owner-a');
    LInput := NewNyxInput('entry-a');
    LInput.Configure.Value('7').Done;
    LForm.Add(LInput);
    LNested := NewNyxColumn('nested-a');
    LNested.Node.SetProp(CPolicy, 'policy-a');
    LNested.Add(NewNyxInput('empty-a'));
    LForm.Add(LNested);
    LPage.Add(LForm);
    LForm := NewNyxCard('form-b');
    LForm.Node.SetProp(CContext, 'accepted-b').SetProp(COwner, 'owner-b');
    LInput := NewNyxInput('entry-b');
    LInput.Configure.Value('Independent default').Done;
    LForm.Add(LInput);
    LPage.Add(LForm);
    LPage.Add(NewNyxLabel('outside').WithText('Outside the forms'));
  except
    Result.Free;
    raise;
  end;
end;

procedure Run;
var
  LDocument: TNyxDocument;
  LExisting: TNyxNode;
  LCandidate: TNyxNode;
  LBaseline: TNyxNode;
  LBefore: TNyxNode;
  LForms: TNyxProjectionFormContexts;
  LInvalidForms: TNyxProjectionFormContexts;
  LRestores: TNyxProjectionValueRestores;
  LInvalidRestores: TNyxProjectionValueRestores;
  LContext: TNyxProjectionFormContext;
  LCopy: TNyxProjectionFormContext;
  LRefused: Boolean;
  LMetadata: TNyxText;
begin
  LDocument := Fixture;
  LExisting := nil;
  LCandidate := nil;
  LBaseline := nil;
  LBefore := nil;
  try
    LExisting := RealizeNyxView(LDocument, LDocument.Pages[0]);
    LCandidate := LExisting.Clone;
    LBaseline := LExisting.Clone;
    SetLength(LForms, 2);
    LForms[0] := ContextFor('form-a');
    LForms[1] := ContextFor('form-b');
    Check(CanRefreshNyxProjection(LExisting, LCandidate), 'strict unchanged shape remains compatible');
    Check(CanRefreshNyxFormProjection(LExisting, LCandidate, LForms),
      'explicit unchanged form contexts retain ordinary admission');
    Check(PrepareNyxProjectionFormRestores(LBaseline, LCandidate, LForms, LRestores) and
      (Length(LRestores) = 0), 'unchanged contexts produce no draft resets');

    LExisting.Find('entry-a').Configure.Value('Unfinished / 🌙').Done;
    LExisting.Find('empty-a').Configure.Value('-').Done;
    LExisting.Find('entry-b').Configure.Value('Keep this draft / 🌙').Done;
    LMetadata := TNyxText('new context / 🌙') + #0 + ' / exact';
    LCandidate.Find('form-a').SetProp(CContext, LMetadata);
    LCandidate.Find('nested-a').SetProp(CPolicy, 'new-policy-a');
    Check(not CanRefreshNyxProjection(LExisting, LCandidate),
      'default API still rejects changing extension metadata');
    Check(CanRefreshNyxFormProjection(LExisting, LCandidate, LForms),
      'explicit exact form scope admits its named existing metadata');
    Check(PrepareNyxProjectionFormRestores(LBaseline, LCandidate, LForms, LRestores) and
      (Length(LRestores) = 2), 'changed form prepares its complete input reset group');
    Check((LRestores[0].RuntimeID = 'entry-a') and (LRestores[1].RuntimeID = 'empty-a') and
      (LRestores[1].DesignID = 'empty-a'), 'reset identities are exact and exclude unrelated form');
    Check(RefreshNyxFormProjectionProperties(LExisting, LCandidate, LBaseline, LForms),
      'complete context and accepted-value group publishes');
    Check(LExisting.Find('form-a').Prop(CContext) = LMetadata,
      'context metadata retains supplementary Unicode and embedded NUL');
    Check(LExisting.Find('nested-a').Prop(CPolicy) = 'new-policy-a',
      'named descendant metadata follows the same form publication');
    Check(LExisting.Find('entry-a').Prop('value') = '7',
      'unchanged accepted default replaces old-context unfinished text');
    Check(LExisting.Find('empty-a').Props.IndexOfName('value') < 0,
      'absent accepted value removes an old-context runtime property');
    Check(LExisting.Find('entry-b').Prop('value') = TNyxText('Keep this draft / 🌙'),
      'unchanged neighboring form retains independent runtime draft');
    Check(LBaseline.Find('form-a').Prop(CContext) = 'accepted-a',
      'publication does not mutate the independent previous baseline');

    LBaseline.Free;
    LBaseline := LCandidate.Clone;
    LExisting.Find('entry-a').Configure.Value('A newer current-context draft').Done;
    LCandidate.Find('outside').Configure.Text('An unrelated caption changed').Done;
    Check(RefreshNyxFormProjectionProperties(LExisting, LCandidate, LBaseline, LForms) and
      (LExisting.Find('entry-a').Prop('value') = 'A newer current-context draft'),
      'unrelated authored change preserves draft after context is current');

    LBefore := LCandidate.Clone;
    LCandidate.Find('form-a').SetProp(CContext, 'a refused candidate');
    LCandidate.Find('form-a').SetProp(COwner, 'a different owner');
    Check(not RefreshNyxFormProjectionProperties(LExisting, LCandidate, LBaseline, LForms),
      'undeclared semantic owner change refuses the whole group');
    Check((LExisting.Find('form-a').Prop(CContext) = LMetadata) and
      (LExisting.Find('entry-a').Prop('value') = 'A newer current-context draft'),
      'refusal cannot partially publish earlier context or input changes');
    LCandidate.Free;
    LCandidate := LBefore;
    LBefore := nil;

    LCandidate.Find('outside').SetProp(CContext, 'outside the declared owner');
    Check(not CanRefreshNyxFormProjection(LExisting, LCandidate, LForms),
      'same named property outside its explicit scope remains strict');
    LCandidate.Find('outside').Props.Delete(LCandidate.Find('outside').Props.IndexOfName(CContext));
    LCandidate.Find('form-a').Props.Delete(LCandidate.Find('form-a').Props.IndexOfName(CContext));
    Check(not CanRefreshNyxFormProjection(LExisting, LCandidate, LForms) and
      not PrepareNyxProjectionFormRestores(LBaseline, LCandidate, LForms, LRestores) and
      (Length(LRestores) = 0), 'metadata removal refuses before returning a partial reset group');
    LCandidate.Find('form-a').SetProp(CContext, LMetadata);

    SetLength(LInvalidForms, 2);
    LInvalidForms[0] := LForms[0];
    LInvalidForms[1] := LForms[0];
    Check(not CanRefreshNyxFormProjection(LExisting, LCandidate, LInvalidForms),
      'duplicate context scopes refuse');
    LInvalidForms[1] := ContextFor('nested-a');
    Check(not CanRefreshNyxFormProjection(LExisting, LCandidate, LInvalidForms),
      'nested context scopes refuse');
    LInvalidForms[0] := LInvalidForms[1];
    LInvalidForms[1] := LForms[0];
    Check(not CanRefreshNyxFormProjection(LExisting, LCandidate, LInvalidForms),
      'reversed nested context scopes also refuse');
    SetLength(LInvalidForms, 1);
    LInvalidForms[0] := TNyxProjectionFormContext.ForForm('form-a', 'foreign-instance',
      [NyxExtension(CContext)]);
    Check(not CanRefreshNyxFormProjection(LExisting, LCandidate, LInvalidForms),
      'matching local ID cannot borrow a different design identity');
    LInvalidForms[0] := Default(TNyxProjectionFormContext);
    Check(not CanRefreshNyxFormProjection(LExisting, LCandidate, LInvalidForms),
      'uninitialized context is not a permissive policy');

    LCandidate.Find('form-a').SetProp(CContext, 'must not publish');
    SetLength(LInvalidRestores, 1);
    LInvalidRestores[0] := TNyxProjectionValueRestore.ForField('missing-field', 'missing-field');
    Check(not RefreshNyxFormProjectionProperties(LExisting, LCandidate, LBaseline,
      LForms, LInvalidRestores) and (LExisting.Find('form-a').Prop(CContext) = LMetadata),
      'invalid explicit restore refuses before any context publication');

    LContext := ContextFor('form-a');
    LCopy := LContext;
    LContext := TNyxProjectionFormContext.ForForm('form-b', 'form-b',
      [NyxExtension(COwner), NyxExtension('test.other.context')]);
    Check((LCopy.RuntimeID = 'form-a') and LCopy.Permits(CContext) and
      not LCopy.Permits(COwner) and not LContext.Permits(CContext) and
      (LContext.RuntimeID = 'form-b'),
      'reconstruction retains independent immutable policy copies');
    LRefused := False;
    try
      LContext := TNyxProjectionFormContext.ForForm('form-a', 'form-a', [NyxExtension('width')]);
    except
      on ENyxModel do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused, 'built-in layout attributes cannot be relabelled context metadata');
    LRefused := False;
    try
      LContext := TNyxProjectionFormContext.ForForm('form-a', 'form-a',
        [NyxExtension(CContext), NyxExtension(CContext)]);
    except
      on ENyxModel do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused, 'duplicate context keys reject construction');
    LRefused := False;
    try
      LContext := TNyxProjectionFormContext.ForForm('', 'form-a', [NyxExtension(CContext)]);
    except
      on ENyxModel do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused, 'empty form identities reject construction');

    LExisting.Find('entry-a').Binds.Value(NyxTextState('accepted-field')).Done;
    LBaseline.Find('entry-a').Binds.Value(NyxTextState('accepted-field')).Done;
    LCandidate.Find('entry-a').Binds.Value(NyxTextState('accepted-field')).Done;
    Check(not CanRefreshNyxFormProjection(LExisting, LCandidate, LForms) and
      CanRefreshNyxBoundFormProjection(LExisting, LCandidate, LForms),
      'live descriptors require the separate exact-binding admission boundary');
    LCandidate.Find('entry-a').Binds.Value(NyxTextState('another-field')).Done;
    Check(not CanRefreshNyxBoundFormProjection(LExisting, LCandidate, LForms),
      'form context cannot authorize a changed live binding');
    LCandidate.Find('entry-a').Binds.Value(NyxTextState('accepted-field')).Done;
    LCandidate.Find('entry-a').Configure.Value('Current projected store value').Done;
    Check(RefreshNyxBoundFormProjectionProperties(LExisting, LCandidate, LBaseline, LForms) and
      (LExisting.Find('entry-a').Prop('value') = 'Current projected store value'),
      'exact-binding helper consumes the already projected accepted candidate');
  finally
    LBefore.Free;
    LBaseline.Free;
    LCandidate.Free;
    LExisting.Free;
    LDocument.Free;
  end;
end;

begin
  try
    Run;
    WriteLn('PASS ', GChecks, ' form projection checks');
    {$ifdef PAS2JS}
    document.body.textContent := 'PASS ' + IntToStr(GChecks) + ' form projection checks';
    document.body.setAttribute('data-projection-forms', 'passed');
    document.body.setAttribute('data-projection-forms-checks', IntToStr(GChecks));
    {$endif}
  except
    on LException: Exception do
    begin
      WriteLn('FAIL ', LException.Message);
      {$ifdef PAS2JS}
      document.body.textContent := 'FAIL ' + LException.Message;
      document.body.setAttribute('data-projection-forms', 'failed');
      document.body.setAttribute('data-projection-forms-error', LException.Message);
      {$else}
      ExitCode := 1;
      {$endif}
    end;
  end;
end.
