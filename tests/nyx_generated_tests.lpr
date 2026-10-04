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

program nyx_generated_tests;

{$mode delphi}{$H+}
{$codepage utf8}

uses
  nyx.text,
  SysUtils,
  nyx.types,
  nyx.state,
  nyx.binding,
  nyx.behavior,
  nyx.model,
  nyx.codec,
  nyx.schema,
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
  LStore: TNyxState;
  LLive: TNyxLiveBindings;
  LDispatch: TNyxDispatch;
  LSource: TNyxText;
begin
  { Execute the generated entry point instead of merely checking its syntax.
    Equality proves the emitted program reconstructs the complete design contract. }
  LExpected := CreateNyxPersistenceFixture;
  LActual := BuildNyxDocument;
  try

    if TNyxCodec.Encode(LExpected) <> TNyxCodec.Encode(LActual) then
    begin
      raise Exception.Create('Generated Pascal changed design meaning');
    end;
    ValidateNyxDocumentProperties(LActual);
    LRuntime := RealizeNyxView(LActual, LActual.Find('welcome-instance'));
    try

      if (LRuntime.Part('title').Prop('text') <> TNyxText('My welcome / 🌙 漢字')) or
        (LRuntime.Find('welcome-instance/fixture-added-action') = nil) then
      begin
        raise ENyxModel.Create('Compiled instance customization changed view meaning');
      end;
    finally
      LRuntime.Free;
    end;
    WriteLn('PASS compiled Pascal reconstructs the design');
    LRuntime := RealizeNyxView(LActual, LActual.Find(NyxIdentityInstanceID));
    try

      if (LRuntime.Part('nested').SourceID <> NyxIdentityDefinitionID) or
        (LRuntime.Part('nested').Part('action').Prop('text') <>
        TNyxText('Save this instance / 🌙')) then
      begin
        raise ENyxModel.Create('Compiled long Unicode identity lost its nested view meaning');
      end;
    finally
      LRuntime.Free;
    end;
    WriteLn('PASS compiled long Unicode/separator identities realize nested customized views');
    LRuntime := RealizeNyxView(LActual, LActual.Pages[0]);
    LStore := nil;
    LLive := nil;
    try
      LStore := LActual.State.Clone;
      LLive := TNyxLiveBindings.Create(LRuntime, LStore);
      LLive.Activate;
      LDispatch := LLive.Edit(LRuntime.Find('project-name'), 'Reconstructed / 🌙');

      if (LDispatch.Info.Trigger <> ntChange) or
        not LDispatch.Info.IsNamed(NyxEvent('project/name/changed/🌙')) or
        not LDispatch.Info.HasValue or (LDispatch.Info.ValueKind <> nskText) or
        (LDispatch.Info.Value.AsText <> TNyxText('Reconstructed / 🌙')) or
        (LActual.State.GetValue(NyxTextState('empty')) <> '') then
      begin
        raise ENyxModel.Create('Compiled event contract changed its name/value/ownership');
      end;
      LDispatch := LLive.Dispatch(LRuntime.Find('fixture-ratio-commit'), ntClick);

      if not LDispatch.Info.HasValue or (LDispatch.Info.ValueKind <> nskNumber) or
        (LDispatch.Info.Value.AsNumber <> 0.125) or
        (LDispatch.Info.ValueID <> 'fixture-ratio-input') or
        (LDispatch.Info.TargetID <> 'fixture-ratio-commit') then
      begin
        raise ENyxModel.Create('Compiled named-field event changed its payload contract');
      end;
      LDispatch := LLive.Dispatch(LRuntime.Find('fixture-rating').Part(NyxPart('star-5')), ntClick);

      if (LDispatch.Info.ValueKind <> nskInteger) or (LDispatch.Info.Value.AsInteger <> 5) then
      begin
        raise ENyxModel.Create('Compiled selection changed its integer contract');
      end;
    finally
      LLive.Free;
      LStore.Free;
      LRuntime.Free;
    end;
    WriteLn('PASS compiled Unicode event contract preserves typed payload/default ownership');
  finally
    LActual.Free;
    LExpected.Free;
  end;
  LExpected := CreateNyxStructuralSourceFixture(LSource);
  LActual := nyx.structural.view.BuildNyxDocument;
  try

    if (TNyxCodec.Encode(LExpected) <> TNyxCodec.Encode(LActual)) or
      (LActual.Find('obsolete') <> nil) or
      (LActual.Find('intro').Parent.ID <> 'code-notes') then
    begin
      raise ENyxModel.Create('Compiled structural source changed creation, omission or ownership');
    end;
    WriteLn('PASS compiled structural source preserves typed creation, reuse and ownership');
  finally
    LActual.Free;
    LExpected.Free;
  end;
  LExpected := CreateNyxManagedSourceFixture(LSource);
  LActual := nyx.managed.view.BuildNyxDocument;
  try

    if TNyxCodec.Encode(LExpected) <> TNyxCodec.Encode(LActual) then
    begin
      raise ENyxModel.Create('Crafted/reconciled source changed compiled design meaning');
    end;
    WriteLn('PASS compiled managed names, comments and expressions reconstruct their visual edits');
  finally
    LActual.Free;
    LExpected.Free;
  end;
  LExpected := CreateNyxLegacyControlFixture(LSource);
  LActual := nyx.legacy.controls.BuildNyxDocument;
  try

    if (TNyxCodec.Encode(LExpected) <> TNyxCodec.Encode(LActual)) or
      (nyx.legacy.controls.LegacyNote <> TNyxText('A retained legacy helper / 🌙')) then
    begin
      raise ENyxModel.Create('Legacy recovery lost its regenerated contract or handwritten helper');
    end;
    WriteLn('PASS compiled legacy companion recovers into specialized interfaces and retains its helper');
  finally
    LActual.Free;
    LExpected.Free;
  end;
  LExpected := CreateNyxEditedFixture(LSource);
  LActual := nyx.edited.view.BuildNyxDocument;
  try

    if (TNyxCodec.Encode(LExpected) <> TNyxCodec.Encode(LActual)) or
      (nyx.edited.view.AppCaption <> TNyxText('A crafted application / 🌙 漢字')) then
    begin
      raise ENyxModel.Create('Edited companion source or handwritten helper changed meaning');
    end;
    WriteLn('PASS edited Pascal reconstructs accepted design and executes handwritten helper');
    LRuntime := RealizeNyxView(LActual, LActual.Pages[0]);
    LStore := nil;
    LLive := nil;
    try
      LStore := LActual.State.Clone;
      LLive := TNyxLiveBindings.Create(LRuntime, LStore);
      LLive.Activate;

      if (LRuntime.Find('eyebrow').Prop('text') <> TNyxText('Crafted status / 🌙')) or
        (LRuntime.Find('project-name').Prop('value') <> TNyxText('Authored reply / 🌙')) then
      begin
        raise ENyxModel.Create('Compiled edited defaults/bindings changed runtime meaning');
      end;
      WriteLn('PASS edited state/default bindings activate the compiled native runtime');
    finally
      LLive.Free;
      LStore.Free;
      LRuntime.Free;
    end;
  finally
    LActual.Free;
    LExpected.Free;
  end;
end.
