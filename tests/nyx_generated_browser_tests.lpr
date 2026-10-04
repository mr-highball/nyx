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

program nyx_generated_browser_tests;

{$mode delphi}{$H+}
{$codepage utf8}

uses
  SysUtils,
  Web,
  nyx.text,
  nyx.data,
  nyx.types,
  nyx.state,
  nyx.binding,
  nyx.behavior,
  nyx.model,
  nyx.codec,
  nyx.composition,
  nyx.test.core,
  nyx.test.identity,
  nyx.test.source,
  nyx.test.controls,
  nyx.test.source.managed,
  nyx.managed.view,
  nyx.test.source.structural,
  nyx.structural.view,
  nyx.legacy.controls,
  nyx.edited.view,
  nyx.generated.view;

var
  LExpected: TNyxDocument;
  LActual: TNyxDocument;
  LRuntime: TNyxNode;
  LIndex: Integer;
  LCount: Integer;
  LStore: TNyxState;
  LLive: TNyxLiveBindings;
  LDispatch: TNyxDispatch;
  LSource: TNyxText;

procedure Check(ACondition: Boolean; const AReason: TNyxText);
begin

  if not ACondition then
  begin
    raise ENyxModel.Create('Compiled browser source: ' + AReason);
  end;
  Inc(LCount);
end;

begin
  LExpected := nil;
  LActual := nil;
  try
    try
      { Execute the source emitted by the native generator with pas2js too.
        This catches parser, record, numeric literal and Unicode differences
        that same-target encode/decode fixtures cannot establish. }
      LExpected := CreateNyxPersistenceFixture;
      LActual := BuildNyxDocument;
      Check(TNyxCodec.Encode(LExpected) = TNyxCodec.Encode(LActual),
        'complete reconstructed design differs');
      Check(LActual.Extensions.Value(NyxExtension('studio.assets')).Field('tokens').Item(2).
        AsDecimal.Text = '9007199254740993', 'opaque integer precision differs');
      Check(LActual.Extensions.Value(NyxExtension('studio.assets')).Field('tokens').Item(3).
        Field('ratio').AsDecimal.Text = '1.234567890123456789', 'opaque decimal precision differs');
      Check(LActual.Find('project-name').Extensions.Value(NyxExtension('app.validation')).
        Field('message').AsText = TNyxText('Café / 🌙 / 漢字') + NyxScalarText(0) +
        TNyxText('''asset'''), 'node extension Unicode/NUL differs');
      for LIndex := 0 to LExpected.State.Count - 1 do
      begin
        Check(LExpected.State.Value(LExpected.State.Key(LIndex)).SameValue(
          LActual.State.Value(LExpected.State.Key(LIndex))),
          'typed default differs');
      end;
      LRuntime := RealizeNyxView(LActual, LActual.Find('welcome-instance'));
      try
        Check((LRuntime.Part('title').Prop('text') = 'My welcome / 🌙 漢字') and
          (LRuntime.Find('welcome-instance/fixture-added-action') <> nil),
          'reusable customization differs');
        Check((LRuntime.Extensions.Value(NyxExtension('app.presentation')).AsText = 'instance') and
          (LRuntime.Part('title').Extensions.Value(NyxExtension('app.presentation')).AsText =
          'custom title'), 'realized instance/part extensions differ');
      finally
        LRuntime.Free;
      end;
      LRuntime := RealizeNyxView(LActual, LActual.Find(NyxIdentityInstanceID));
      try
        Check((LRuntime.Part('nested').SourceID = NyxIdentityDefinitionID) and
          (LRuntime.Part('nested').Part('action').Prop('text') = 'Save this instance / 🌙'),
          'nested Unicode identity differs');
      finally
        LRuntime.Free;
      end;
      LRuntime := RealizeNyxView(LActual, LActual.Pages[0]);
      LStore := nil;
      LLive := nil;
      try
        LStore := LActual.State.Clone;
        LLive := TNyxLiveBindings.Create(LRuntime, LStore);
        LLive.Activate;
        LDispatch := LLive.Edit(LRuntime.Find('project-name'), 'Reconstructed / 🌙');
        Check((LDispatch.Info.Trigger = ntChange) and
          LDispatch.Info.IsNamed(NyxEvent('project/name/changed/🌙')),
          'compiled semantic event reference differs');
        Check(LDispatch.Info.HasValue and (LDispatch.Info.ValueKind = nskText) and
          (LDispatch.Info.Value.AsText = TNyxText('Reconstructed / 🌙')) and
          (LActual.State.GetValue(NyxTextState('empty')) = ''),
          'compiled event payload/default ownership differs');
        LDispatch := LLive.Dispatch(LRuntime.Find('fixture-ratio-commit'), ntClick);
        Check(LDispatch.Info.HasValue and (LDispatch.Info.ValueKind = nskNumber) and
          (LDispatch.Info.Value.AsNumber = 0.125) and
          (LDispatch.Info.ValueID = 'fixture-ratio-input') and
          (LDispatch.Info.TargetID = 'fixture-ratio-commit'),
          'compiled named-field event differs');
        LDispatch := LLive.Dispatch(LRuntime.Find('fixture-rating').Part(NyxPart('star-5')), ntClick);
        Check((LDispatch.Info.ValueKind = nskInteger) and (LDispatch.Info.Value.AsInteger = 5),
          'compiled selection changed its integer contract');
      finally
        LLive.Free;
        LStore.Free;
        LRuntime.Free;
      end;
      LActual.Free;
      LActual := nil;
      LExpected.Free;
      LExpected := nil;
      LExpected := CreateNyxEditedFixture(LSource);
      LActual := nyx.edited.view.BuildNyxDocument;
      Check(TNyxCodec.Encode(LExpected) = TNyxCodec.Encode(LActual),
        'edited companion source changed accepted design');
      Check(nyx.edited.view.AppCaption = TNyxText('A crafted application / 🌙 漢字'),
        'preserved handwritten helper changed Unicode meaning');
      Check(LActual.State.GetValue(NyxTextState('empty')) = TNyxText('Authored reply / 🌙'),
        'source-edited default changed meaning in compiled browser');
      Check(LActual.Find('eyebrow').Bindings[0].StateName = 'code-caption',
        'source-created binding changed meaning in compiled browser');
      Check(LActual.Find('fixture-ratio').Contract.Snapshot.Field('value').Field('max').AsNumber = 2,
        'source-edited domain changed meaning in compiled browser');
      Check(LActual.Extensions.Value(NyxExtension('studio.assets')).Field('tokens').Item(3).
        Field('ratio').AsDecimal.Text = '9.876543210987654321',
        'source-edited extension lost exact decimal spelling in compiled browser');
      LRuntime := RealizeNyxView(LActual, LActual.Pages[0]);
      LStore := nil;
      LLive := nil;
      try
        LStore := LActual.State.Clone;
        LLive := TNyxLiveBindings.Create(LRuntime, LStore);
        LLive.Activate;
        Check((LRuntime.Find('eyebrow').Prop('text') = TNyxText('Crafted status / 🌙')) and
          (LRuntime.Find('project-name').Prop('value') = TNyxText('Authored reply / 🌙')),
          'compiled edited defaults/bindings reach independent runtime controls');
      finally
        LLive.Free;
        LStore.Free;
        LRuntime.Free;
      end;
      LActual.Free;
      LActual := nil;
      LExpected.Free;
      LExpected := nil;
      LExpected := CreateNyxStructuralSourceFixture(LSource);
      LActual := nyx.structural.view.BuildNyxDocument;
      Check(TNyxCodec.Encode(LExpected) = TNyxCodec.Encode(LActual),
        'compiled structural source changed complete design meaning');
      Check((LActual.Find('obsolete') = nil) and (LActual.Find('intro').Parent.ID = 'code-notes'),
        'compiled structural source retained obsolete ownership');
      Check((LActual.Find('code-reply').Prop('hint') = 'Keep a thoughtful reply / 🌙') and
        (LActual.State.GetValue(NyxTextState('code/reply')) = 'From crafted source / 🌙'),
        'compiled source-created control lost its visual edit or typed state');
      LActual.Free;
      LActual := nil;
      LExpected.Free;
      LExpected := nil;
      LExpected := CreateNyxManagedSourceFixture(LSource);
      LActual := nyx.managed.view.BuildNyxDocument;
      Check(TNyxCodec.Encode(LExpected) = TNyxCodec.Encode(LActual),
        'compiled managed names/comments/expressions lost visual edits');
      Check(LActual.State.GetValue(NyxTextState('journal/reply')) = 'Ready to write / 🌙',
        'compiled deliberate state-name migration lost exact Unicode');
      LActual.Free;
      LActual := nil;
      LExpected.Free;
      LExpected := nil;
      LExpected := CreateNyxLegacyControlFixture(LSource);
      LActual := nyx.legacy.controls.BuildNyxDocument;
      Check(TNyxCodec.Encode(LExpected) = TNyxCodec.Encode(LActual),
        'legacy recovery changed its regenerated specialized design');
      Check(nyx.legacy.controls.LegacyNote = TNyxText('A retained legacy helper / 🌙'),
        'legacy handwritten helper changed its compiled meaning');
      document.body.textContent := 'PASS ' + IntToStr(LCount) +
        ' compiled browser reconstruction checks';
      document.body.setAttribute('data-generated-tests', 'passed');
    finally
      LActual.Free;
      LExpected.Free;
    end;
  except
    on LException: Exception do
    begin
      document.body.textContent := 'FAIL ' + LException.Message;
      document.body.setAttribute('data-generated-tests', 'failed');
    end;
  end;
end.
