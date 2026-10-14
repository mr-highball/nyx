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

program nyx_expression_local_source_tests;

{$mode delphi}{$H+}{$codepage utf8}

uses
  SysUtils, nyx.text, nyx.data, nyx.model, nyx.codec, nyx.codegen, nyx.source,
  nyx.state, nyx.expressions, nyx.binding.types, nyx.studio.projects,
  nyx.studio.session, nyx.generated.view
  {$ifdef PAS2JS}, Web{$else}, Classes{$endif};

var
  GChecks: Integer;

procedure Check(ACondition: Boolean; const AReason: TNyxText);
begin

  if not ACondition then
  begin
    raise Exception.Create('Expression local source: ' + AReason);
  end;
  Inc(GChecks);
end;

function ReplaceOnce(const ASource, AExpected, AReplacement: TNyxText): TNyxText;
var
  LPosition: Integer;
begin
  LPosition := Pos(AExpected, ASource);
  Check(LPosition > 0, 'exact source anchor exists');
  Result := Copy(ASource, 1, LPosition - 1) + AReplacement +
    Copy(ASource, LPosition + Length(AExpected), MaxInt);
end;

procedure Verify;
var
  LDocument: TNyxDocument;
  LParsed: TNyxDocument;
  LWorkspace: TNyxSourceWorkspace;
  LSession: TNyxStudioSession;
  LSource: TNyxText;
  LBefore: TNyxText;
  LAfter: TNyxText;
  LInvalid: TNyxText;
  LPair: TNyxProjectPair;
  LSpec: TNyxBindingSpec;
  LCaption: INyxTextExpression;
  LTotal: INyxNumberExpression;
  LQuantity: INyxIntegerExpression;
  LReady: INyxBooleanExpression;
  LRefused: Boolean;
  LIndex: Integer;
  {$ifndef PAS2JS}
  LDirectory: TNyxText;
  LStream: TFileStream;
  {$endif}
begin
  LDocument := BuildNyxDocument;
  LParsed := nil;
  LWorkspace := nil;
  LSession := nil;
  try
    LQuantity := NyxIntegerExpression(NyxIntegerState('quantity'));
    LTotal := NyxNumberExpression(NyxNumberState('price')).Times(LQuantity.AsNumber)
      .Plus(NyxNumberExpression(NyxNumberState('tax')));
    LCaption := NyxTextExpression(TNyxText('Total 🌙: ')).Append(LTotal.AsText);
    LReady := NyxBooleanExpression(NyxBooleanState('ready'))
      .AndAlso(LQuantity.GreaterThan(NyxIntegerExpression(0)));
    LDocument.Find('order-total').SetBinding(TNyxBindingSpec.Computed(bpText, LCaption));
    LDocument.Find('order-submit').SetBinding(TNyxBindingSpec.Computed(bpEnabled, LReady));
    LSource := TNyxCodegen.Generate(LDocument);
    LSource := ReplaceOnce(LSource, 'var' + #10,
      'var' + #10 +
      '  LQuantity: INyxIntegerExpression;' + #10 +
      '  LTotal: INyxNumberExpression;' + #10 +
      '  LCaption: INyxTextExpression;' + #10 +
      '  LReady: INyxBooleanExpression;' + #10);
    LSource := ReplaceOnce(LSource, '    LHomePage :=',
      '    // Deliberate local expressions keep application intent readable.' + #10 +
      '    LQuantity := NyxIntegerExpression(LQuantityIntegerState);' + #10 +
      '    LTotal := NyxNumberExpression(LPriceNumberState)' + #10 +
      '      .Times(LQuantity.AsNumber)' + #10 +
      '      .Plus(NyxNumberExpression(LTaxNumberState));' + #10 +
      '    LCaption := NyxTextExpression(''Total 🌙: '').Append(LTotal.AsText);' + #10 +
      '    LReady := NyxBooleanExpression(LReadyBooleanState)' + #10 +
      '      .AndAlso(LQuantity.GreaterThan(NyxIntegerExpression(0)));' + #10 + #10 +
      '    LHomePage :=');
    LSource := ReplaceOnce(LSource, TNyxCodegen.Expression(LCaption, '        '), 'LCaption');
    LSource := ReplaceOnce(LSource, TNyxCodegen.Expression(LReady, '        '), 'LReady');
    LBefore := TNyxCodec.Encode(LDocument);
    LParsed := TNyxSourceWorkspace.PrepareDraft(LSource, LWorkspace);
    Check(TNyxCodec.Encode(LParsed) = LBefore, 'specialized locals reconstruct exact compiled expression meaning');
    Check(LWorkspace.Render(LParsed) = LSource, 'no-op source retains handcrafted local declarations and comments');
    FreeAndNil(LParsed);
    FreeAndNil(LWorkspace);
    LPair := NyxProjectPair(LBefore, LSource);
    LSession := TNyxStudioSession.Create(LPair);
    LSession.Select('order-submit');
    LSession.SetProperty('text', 'Place order');
    Check((Pos('LTotal: INyxNumberExpression;', LSession.Source) > 0) and
      (Pos('.Times(LQuantity.AsNumber)', LSession.Source) > 0) and
      (Pos('Deliberate local expressions', LSession.Source) > 0),
      'unrelated design edit preserves handcrafted local expressions');
    LAfter := EncodeNyxProject(LSession.ProjectSnapshot);
    LSession.Undo;
    Check(EncodeNyxProject(LSession.ProjectSnapshot) = EncodeNyxProject(LPair),
      'paired Undo restores exact handwritten source and design');
    LSession.Redo;
    Check(EncodeNyxProject(LSession.ProjectSnapshot) = LAfter,
      'paired Redo restores exact edited source and design');
    LSession.RenameState('quantity', 'units');
    Check((LSession.Document.State.Has('units')) and
      (Pos('LQuantity := NyxIntegerExpression(LQuantityIntegerState)', LSession.Source) > 0),
      'typed state rename retains deliberate expression locals');
    LSession.Undo;
    Check(EncodeNyxProject(LSession.ProjectSnapshot) = LAfter,
      'state rename Undo restores exact handcrafted pair');
    LSession.Select('order-total');
    LSession.SetBinding(TNyxBindingSpec.Computed(bpText,
      NyxTextExpression('Changed total: ').Append(LTotal.AsText)));
    Check(LSession.Document.Find('order-total').FindBinding(bpText, LSpec) and
      ((LSpec.Expression as INyxTextExpression).Evaluate(
        CaptureNyxExpressionInputs(LSession.Document.State)) = 'Changed total: 40'),
      'a changed binding reconciles without corrupting local expression meaning');
    LSession.Undo;
    Check(EncodeNyxProject(LSession.ProjectSnapshot) = LAfter,
      'binding edit Undo restores original exact expression-local pair');
    for LIndex := 0 to 4 do
    begin
      case LIndex of
        0:
          begin
            LInvalid := ReplaceOnce(LSource, 'LTotal: INyxNumberExpression;',
              'LTotal: INyxIntegerExpression;');
          end;
        1:
          begin
            LInvalid := ReplaceOnce(LSource,
              'LQuantity := NyxIntegerExpression(LQuantityIntegerState);', '');
          end;
        2:
          begin
            LInvalid := ReplaceOnce(LSource, 'LReady: INyxBooleanExpression;',
              'LReady: INyxExpression;');
          end;
        3:
          begin
            LInvalid := ReplaceOnce(LSource, 'LReady: INyxBooleanExpression;',
              'lquantity: INyxBooleanExpression;');
          end;
        4:
          begin
            LInvalid := ReplaceOnce(LSource, '    LHomePage :=',
              '    LQuantity := NyxIntegerExpression(9);' + #10 + '    LHomePage :=');
          end;
      end;
      LRefused := False;
      try
        LParsed := TNyxSourceWorkspace.PrepareDraft(LInvalid, LWorkspace);
      except
        on ENyxSource do
        begin
          LRefused := True;
        end;
      end;
      Check(LRefused and (EncodeNyxProject(LSession.ProjectSnapshot) = LAfter),
        'invalid type/order/duplicate/reassignment refuses independently of accepted pair');
      FreeAndNil(LParsed);
      FreeAndNil(LWorkspace);
    end;
    Check(LSession.Document.Find('order-total').FindBinding(bpText, LSpec) and
      ((LSpec.Expression as INyxTextExpression).Evaluate(
        CaptureNyxExpressionInputs(LSession.Document.State)) = TNyxText('Total 🌙: 40')),
      'edited local caption keeps exact Unicode and arithmetic');
    {$ifndef PAS2JS}

    if ParamCount = 1 then
    begin
      LDirectory := IncludeTrailingPathDelimiter(ParamStr(1));
      ForceDirectories(LDirectory);
      LStream := TFileStream.Create(LDirectory + 'nyx.generated.view.pas', fmCreate);
      try
        LStream.WriteBuffer(LSource[1], Length(LSource));
      finally
        LStream.Free;
      end;
      LStream := TFileStream.Create(LDirectory + 'project.nyx', fmCreate);
      try
        LBefore := EncodeNyxProject(LPair);
        LStream.WriteBuffer(LBefore[1], Length(LBefore));
      finally
        LStream.Free;
      end;
    end;
    {$endif}
  finally
    LSession.Free;
    LWorkspace.Free;
    LParsed.Free;
    LDocument.Free;
  end;
end;

begin
  try
    Verify;
    {$ifdef PAS2JS}
    document.body.setAttribute('data-expression-locals', 'passed');
    document.body.textContent := 'PASS ' + IntToStr(GChecks) + ' expression local/source checks';
    {$else}
    WriteLn('PASS ', GChecks, ' expression local/source checks');
    {$endif}
  except
    on LException: Exception do
    begin
      {$ifdef PAS2JS}
      document.body.setAttribute('data-expression-locals', 'failed');
      document.body.setAttribute('data-expression-error', LException.Message);
      {$else}
      WriteLn('FAIL ', LException.Message);
      DumpExceptionBackTrace(Output);
      ExitCode := 1;
      {$endif}
    end;
  end;
end.
