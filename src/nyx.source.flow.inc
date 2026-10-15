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
{ Closed saved-graph grammar constructs inert declarations and immutable values.
  Parsing a project never resolves creator code or invokes a Logic operation. }

function TConfigurationReader.FlowConstructor(const AName: TNyxText): TValue;
var
  LArgs: TValues;
  LMethod: TNyxText;
  LGraph: INyxFlowBuilder;
  LConditional: INyxFlowConditions;
  LOperation: INyxLogicOperation;
  LDescriptor: TNyxLogicDescriptor;
  LLayout: TNyxJSONLayout;
  LIndent: Integer;
  LBuilt: Boolean;

  procedure Count(AMinimum, AMaximum: Integer);
  begin

    if (Length(LArgs) < AMinimum) or (Length(LArgs) > AMaximum) then
    begin
      Fail('Wrong typed Logic argument count');
    end;
  end;

  procedure Kind(AIndex: Integer; AKind: TValueKind);
  begin

    if (AIndex >= Length(LArgs)) or (LArgs[AIndex].Kind <> AKind) then
    begin
      Fail('Wrong Logic/graph argument family');
    end;
  end;

begin
  Result := Default(TValue);
  LArgs := nil;
  LBuilt := False;

  if AName = 'tnyxflowlimits' then
  begin
    Expect('.');
    Expect('Defaults');
    Result.Kind := vkFlowLimits;
    Result.FlowLimits := TNyxFlowLimits.Defaults;
  end
  else if AName = 'tnyxdatavalue' then
  begin
    Expect('.');
    Expect('ParseJSON');
    LArgs := Arguments(1);
    Count(1, 1);
    Kind(0, vkText);
    Result.Kind := vkData;
    Result.Data := TNyxDataValue.ParseJSON(LArgs[0].Text);
    Exit;
  end
  else
  begin

    if At('(') then
    begin
      LArgs := Arguments(5);
    end;

    if AName = 'nyxfileopenoptions' then
    begin
      Count(0, 0);
      Result.Kind := vkFileOpenOptions;
      Result.FileOpenOptions := NyxFileOpenOptions;
    end
    else if AName = 'nyxfilesaveoptions' then
    begin
      Count(1, 1);
      Kind(0, vkText);
      Result.Kind := vkFileSaveOptions;
      Result.FileSaveOptions := NyxFileSaveOptions(LArgs[0].Text);
    end
    else if AName = 'nyxmediatype' then
    begin
      Count(1, 1);
      Result.Kind := vkMediaType;

      if LArgs[0].Kind = vkFileMediaType then
      begin
        Result.Text := NyxMediaType(TNyxFileMediaType(LArgs[0].Ordinal)).Name;
      end
      else
      begin
        Kind(0, vkText);
        Result.Text := NyxMediaType(LArgs[0].Text).Name;
      end;
    end
    else if AName = 'nyxopenfileport' then
    begin
      Count(1, 1);
      Kind(0, vkOpenFilePort);
      Result.Kind := vkFlowPort;
      Result.Text := NyxOpenFilePort(TNyxOpenFilePort(LArgs[0].Ordinal)).Name;
    end
    else if AName = 'nyxfileport' then
    begin
      Count(1, 1);
      Kind(0, vkFilePort);
      Result.Kind := vkFlowPort;
      Result.Text := NyxFilePort(TNyxFilePort(LArgs[0].Ordinal)).Name;
    end
    else if AName = 'nyxhttprequest' then
    begin
      Count(1, 1);
      Kind(0, vkResourceURL);
      Result.Kind := vkHTTPRequest;
      Result.HTTPRequest := NyxHTTPRequest(NyxResourceURL(LArgs[0].Text));
    end
    else if AName = 'nyxhttpmethod' then
    begin
      Count(1, 1);
      Result.Kind := vkHTTPMethodRef;

      if LArgs[0].Kind = vkHTTPMethod then
      begin
        Result.Text := NyxHTTPMethod(TNyxHTTPMethod(LArgs[0].Ordinal)).Name;
      end
      else
      begin
        Kind(0, vkText);
        Result.Text := NyxHTTPMethod(LArgs[0].Text).Name;
      end;
    end
    else if AName = 'nyxhttpport' then
    begin
      Count(1, 1);
      Kind(0, vkHTTPPort);
      Result.Kind := vkFlowPort;
      Result.Text := NyxHTTPPort(TNyxHTTPPort(LArgs[0].Ordinal)).Name;
    end
    else if (AName = 'nyxflowref') or (AName = 'nyxflowstep') or (AName = 'nyxflowport') or
      (AName = 'nyxlogickind') then
    begin
      Count(1, 1);
      Kind(0, vkText);
      Result.Text := LArgs[0].Text;

      if AName = 'nyxflowref' then
      begin
        Result.Kind := vkFlowRef;
        NyxFlowRef(Result.Text);
      end
      else if AName = 'nyxflowstep' then
      begin
        Result.Kind := vkFlowStep;
        NyxFlowStep(Result.Text);
      end
      else if AName = 'nyxflowport' then
      begin
        Result.Kind := vkFlowPort;
        NyxFlowPort(Result.Text);
      end
      else
      begin
        Result.Kind := vkLogicKind;
        NyxLogicKind(Result.Text);
      end;
    end
    else if AName = 'nyxlogicport' then
    begin
      Count(1, 1);
      Kind(0, vkLogicPort);
      Result.Kind := vkFlowPort;
      Result.Text := NyxLogicPort(TNyxLogicPort(LArgs[0].Ordinal)).Name;
    end
    else if AName = 'nyxflowendpoint' then
    begin
      Count(2, 2);
      Kind(0, vkFlowStep);
      Kind(1, vkFlowPort);
      Result.Kind := vkFlowEndpoint;
      Result.FlowEndpoint := NyxFlowEndpoint(NyxFlowStep(LArgs[0].Text), NyxFlowPort(LArgs[1].Text));
    end
    else if AName = 'nyxflowconditions' then
    begin
      Count(1, 1);
      Kind(0, vkFlowGraph);
      Result.Kind := vkFlowGraph;
      LConditional := NyxFlowConditions(NyxFlowSignatureGraph(
        NyxFlowDefinitionFromData(LArgs[0].Data)));
    end
    else if AName = 'newnyxflowgraph' then
    begin
      Count(0, 1);
      Result.Kind := vkFlowGraph;

      if Length(LArgs) = 1 then
      begin
        Kind(0, vkFlowLimits);
        LGraph := NewNyxFlowGraph(LArgs[0].FlowLimits);
      end
      else
      begin
        LGraph := NewNyxFlowGraph;
      end;
    end
    else if AName = 'nyxlogicdescriptor' then
    begin
      Count(4, 5);
      Kind(0, vkLogicKind);
      Kind(1, vkText);
      Kind(2, vkText);
      Kind(3, vkLogicGroup);
      Result.Kind := vkLogicDescriptor;
      Result.LogicDescriptor := NyxLogicDescriptor(NyxLogicKind(LArgs[0].Text),
        LArgs[1].Text, LArgs[2].Text, TNyxLogicGroup(LArgs[3].Ordinal));

      if Length(LArgs) = 5 then
      begin
        Kind(4, vkLogicAffinity);
        Result.LogicDescriptor := NyxLogicDescriptor(NyxLogicKind(LArgs[0].Text),
          LArgs[1].Text, LArgs[2].Text, TNyxLogicGroup(LArgs[3].Ordinal),
          TNyxLogicAffinity(LArgs[4].Ordinal));
      end;
    end
    else
    begin
      Result.Kind := vkLogicOperation;

      if AName = 'newnyxutf8encode' then
      begin
        Count(0, 0);
        LOperation := NewNyxUTF8Encode;
      end
      else if AName = 'newnyxopenfile' then
      begin
        Count(1, 1);
        Kind(0, vkFileOpenOptions);
        LOperation := NewNyxOpenFile(LArgs[0].FileOpenOptions);
      end
      else if AName = 'newnyxsavefile' then
      begin
        Count(1, 1);
        Kind(0, vkFileSaveOptions);
        LOperation := NewNyxSaveFile(LArgs[0].FileSaveOptions);
      end
      else if AName = 'newnyxutf8decode' then
      begin
        Count(0, 0);
        LOperation := NewNyxUTF8Decode;
      end
      else if AName = 'newnyxhttpcall' then
      begin
        Count(1, 1);
        Kind(0, vkHTTPRequest);
        LOperation := NewNyxHTTPCall(LArgs[0].HTTPRequest);
      end
      else if AName = 'newnyxjsonparse' then
      begin
        Count(0, 0);
        LOperation := NewNyxJSONParse;
      end
      else if AName = 'newnyxjsonformat' then
      begin
        Count(0, 2);
        LLayout := njlIndented;
        LIndent := 2;

        if Length(LArgs) > 0 then
        begin
          Kind(0, vkJSONLayout);
          LLayout := TNyxJSONLayout(LArgs[0].Ordinal);
        end;

        if Length(LArgs) > 1 then
        begin
          Kind(1, vkInteger);
          LIndent := StrToInt(LArgs[1].Text);
        end;
        LOperation := NewNyxJSONFormat(LLayout, LIndent);
      end
      else if AName = 'newnyxjsonselect' then
      begin
        Count(1, 1);
        Kind(0, vkResourcePath);
        LOperation := NewNyxJSONSelect(LArgs[0].ResourcePath);
      end
      else if AName = 'newnyxjsonfilter' then
      begin
        Count(3, 3);
        Kind(0, vkResourcePath);
        Kind(1, vkJSONComparison);
        Kind(2, vkData);
        LOperation := NewNyxJSONFilter(LArgs[0].ResourcePath,
          TNyxJSONComparison(LArgs[1].Ordinal), LArgs[2].Data);
      end
      else if AName = 'newnyxjsonsort' then
      begin
        Count(2, 3);
        Kind(0, vkResourcePath);
        Kind(1, vkJSONKeyKind);

        if Length(LArgs) = 3 then
        begin
          Kind(2, vkJSONDirection);
          LOperation := NewNyxJSONSort(LArgs[0].ResourcePath,
            TNyxJSONKeyKind(LArgs[1].Ordinal), TNyxJSONDirection(LArgs[2].Ordinal));
        end
        else
        begin
          LOperation := NewNyxJSONSort(LArgs[0].ResourcePath, TNyxJSONKeyKind(LArgs[1].Ordinal));
        end;
      end
      else if AName = 'newnyxjsonmap' then
      begin
        Count(1, 1);
        Kind(0, vkResourcePath);
        LOperation := NewNyxJSONMap(LArgs[0].ResourcePath);
      end
      else if AName = 'newnyxjsonslice' then
      begin
        Count(2, 2);
        Kind(0, vkInteger);
        Kind(1, vkInteger);
        LOperation := NewNyxJSONSlice(StrToInt(LArgs[0].Text), StrToInt(LArgs[1].Text));
      end
      else if (AName = 'newnyxjsonset') or (AName = 'newnyxjsonremove') then
      begin
        Count(1, 2);
        Kind(0, vkResourcePath);

        if AName = 'newnyxjsonset' then
        begin
          LOperation := NewNyxJSONSet(LArgs[0].ResourcePath);

          if Length(LArgs) = 2 then
          begin
            Kind(1, vkJSONSetMode);
            LOperation := NewNyxJSONSet(LArgs[0].ResourcePath, TNyxJSONSetMode(LArgs[1].Ordinal));
          end;
        end
        else
        begin
          LOperation := NewNyxJSONRemove(LArgs[0].ResourcePath);

          if Length(LArgs) = 2 then
          begin
            Kind(1, vkJSONMissing);
            LOperation := NewNyxJSONRemove(LArgs[0].ResourcePath, TNyxJSONMissing(LArgs[1].Ordinal));
          end;
        end;
      end
      else if AName = 'newnyxjsonmerge' then
      begin
        Count(0, 1);
        LOperation := NewNyxJSONMerge;

        if Length(LArgs) = 1 then
        begin
          Kind(0, vkJSONMergePolicy);
          LOperation := NewNyxJSONMerge(TNyxJSONMergePolicy(LArgs[0].Ordinal));
        end;
      end
      else if AName = 'newnyxtextjoin' then
      begin
        Count(0, 1);

        if Length(LArgs) = 1 then
        begin
          Kind(0, vkText);
          LOperation := NewNyxTextJoin(LArgs[0].Text);
        end
        else
        begin
          LOperation := NewNyxTextJoin;
        end;
      end
      else if AName = 'newnyxlogicdeclaration' then
      begin
        Count(2, 2);
        Kind(0, vkLogicDescriptor);
        Kind(1, vkData);
        LOperation := NewNyxLogicDeclaration(LArgs[0].LogicDescriptor, LArgs[1].Data);
      end
      else
      begin
        Fail('Unsupported Logic constructor');
      end;
      Result.Data := NyxObject([NyxField('descriptor', LOperation.Descriptor.ToData),
        NyxField('configuration', LOperation.Configuration)]);
    end;
  end;
  while At('.') do
  begin
    Expect('.');
    LMethod := LowerCase(FTokens[FCursor].Text);
    Inc(FCursor);
    LArgs := nil;

    if At('(') then
    begin
      LArgs := Arguments(3);
    end;
    case Result.Kind of
      vkFileOpenOptions:
        begin
          Count(1, 1);

          if LMethod = 'maximumbytes' then
          begin
            Kind(0, vkInteger);
            Result.FileOpenOptions := Result.FileOpenOptions.MaximumBytes(StrToInt(LArgs[0].Text));
          end
          else
          begin
            Kind(0, vkText);

            if LMethod = 'title' then
            begin
              Result.FileOpenOptions := Result.FileOpenOptions.Title(LArgs[0].Text);
            end
            else if LMethod = 'allow' then
            begin
              Result.FileOpenOptions := Result.FileOpenOptions.Allow(LArgs[0].Text);
            end
            else
            begin
              Fail('Unsupported typed file-open option');
            end;
          end;
        end;
      vkFileSaveOptions:
        begin
          Count(1, 1);

          if LMethod = 'mode' then
          begin
            Kind(0, vkFileSaveMode);
            Result.FileSaveOptions := Result.FileSaveOptions.Mode(TNyxFileSaveMode(LArgs[0].Ordinal));
          end
          else if LMethod = 'downloadactivation' then
          begin
            Kind(0, vkDownloadActivation);
            Result.FileSaveOptions := Result.FileSaveOptions.DownloadActivation(
              TNyxDownloadActivation(LArgs[0].Ordinal));
          end
          else if LMethod = 'mediatype' then
          begin
            Kind(0, vkMediaType);
            Result.FileSaveOptions := Result.FileSaveOptions.MediaType(NyxMediaType(LArgs[0].Text));
          end
          else if LMethod = 'maximumbytes' then
          begin
            Kind(0, vkInteger);
            Result.FileSaveOptions := Result.FileSaveOptions.MaximumBytes(StrToInt(LArgs[0].Text));
          end
          else if LMethod = 'alltypes' then
          begin
            Kind(0, vkBoolean);
            Result.FileSaveOptions := Result.FileSaveOptions.AllTypes(LArgs[0].Ordinal <> 0);
          end
          else
          begin
            Kind(0, vkText);

            if LMethod = 'suggest' then
            begin
              Result.FileSaveOptions := Result.FileSaveOptions.Suggest(LArgs[0].Text);
            end
            else if LMethod = 'title' then
            begin
              Result.FileSaveOptions := Result.FileSaveOptions.Title(LArgs[0].Text);
            end
            else if LMethod = 'allow' then
            begin
              Result.FileSaveOptions := Result.FileSaveOptions.Allow(LArgs[0].Text);
            end
            else
            begin
              Fail('Unsupported typed file-save option');
            end;
          end;
        end;
      vkHTTPRequest:
        begin

          if LMethod = 'method' then
          begin
            Count(1, 1);

            if LArgs[0].Kind = vkHTTPMethod then
            begin
              Result.HTTPRequest := Result.HTTPRequest.Method(TNyxHTTPMethod(LArgs[0].Ordinal));
            end
            else
            begin
              Kind(0, vkHTTPMethodRef);
              Result.HTTPRequest := Result.HTTPRequest.Method(NyxHTTPMethod(LArgs[0].Text));
            end;
          end
          else if (LMethod = 'header') or (LMethod = 'appendheader') then
          begin
            Count(2, 2);
            Kind(0, vkText);
            Kind(1, vkText);
            if LMethod = 'header' then
            begin
              Result.HTTPRequest := Result.HTTPRequest.Header(LArgs[0].Text, LArgs[1].Text);
            end
            else
            begin
              Result.HTTPRequest := Result.HTTPRequest.AppendHeader(LArgs[0].Text, LArgs[1].Text);
            end;
          end
          else if LMethod = 'withbody' then
          begin
            Count(1, 1);
            Kind(0, vkBytes);
            Result.HTTPRequest := Result.HTTPRequest.WithBody(LArgs[0].Bytes);
          end
          else
          begin
            Count(1, 1);
            Kind(0, vkInteger);

            if LMethod = 'wholerequest' then
            begin
              Result.HTTPRequest := Result.HTTPRequest.WholeRequest(StrToInt(LArgs[0].Text));
            end
            else if LMethod = 'maximumbytes' then
            begin
              Result.HTTPRequest := Result.HTTPRequest.MaximumBytes(StrToInt(LArgs[0].Text));
            end
            else
            begin
              Fail('Unsupported typed HTTP request method');
            end;
          end;
        end;
      vkFlowGraph:
        begin

          if LBuilt then
          begin
            Fail('An immutable graph has no builder methods');
          end;

          if LConditional <> nil then
          begin

            if LMethod = 'when' then
            begin
              Count(2, 3);
              Kind(0, vkFlowStep);
              Kind(1, vkFlowEndpoint);

              if Length(LArgs) = 3 then
              begin
                Kind(2, vkBoolean);
                LConditional.When(NyxFlowStep(LArgs[0].Text), LArgs[1].FlowEndpoint,
                  LArgs[2].Ordinal <> 0);
              end
              else
              begin
                LConditional.When(NyxFlowStep(LArgs[0].Text), LArgs[1].FlowEndpoint);
              end;
            end
            else if LMethod = 'always' then
            begin
              Count(1, 1);
              Kind(0, vkFlowStep);
              LConditional.Always(NyxFlowStep(LArgs[0].Text));
            end
            else if LMethod = 'build' then
            begin
              Count(0, 0);
              Result.Data := LConditional.Build.ToData;
              LBuilt := True;
            end
            else
            begin
              Fail('Unsupported typed conditional-graph method');
            end;
          end
          else if LMethod = 'withstep' then
          begin
            Count(2, 2);
            Kind(0, vkFlowStep);
            Kind(1, vkLogicOperation);
            LDescriptor := TNyxLogicDescriptor.FromData(LArgs[1].Data.Field('descriptor'));
            LGraph.WithStep(NyxFlowStep(LArgs[0].Text),
              NewNyxLogicDeclaration(LDescriptor, LArgs[1].Data.Field('configuration')));
          end
          else if LMethod = 'connect' then
          begin
            Count(2, 2);
            Kind(0, vkFlowEndpoint);
            Kind(1, vkFlowEndpoint);
            LGraph.Connect(LArgs[0].FlowEndpoint, LArgs[1].FlowEndpoint);
          end
          else if LMethod = 'build' then
          begin
            Count(0, 0);
            Result.Data := LGraph.Build.ToData;
            LBuilt := True;
          end
          else
            Fail('Unsupported typed graph method');
        end;
      vkFlowLimits:
        begin
          Count(1, 1);
          Kind(0, vkInteger);

          if LMethod = 'steps' then
          begin
            Result.FlowLimits := Result.FlowLimits.Steps(StrToInt(LArgs[0].Text));
          end
          else if LMethod = 'wires' then
          begin
            Result.FlowLimits := Result.FlowLimits.Wires(StrToInt(LArgs[0].Text));
          end
          else if LMethod = 'valuebytes' then
          begin
            Result.FlowLimits := Result.FlowLimits.ValueBytes(StrToInt(LArgs[0].Text));
          end
          else if LMethod = 'runbytes' then
          begin
            Result.FlowLimits := Result.FlowLimits.RunBytes(StrToInt(LArgs[0].Text));
          end
          else
            Fail('Unsupported graph budget method');
        end;
      vkLogicDescriptor:
        begin

          if LMethod = 'labelled' then
          begin
            Count(1, 1);
            Kind(0, vkText);
            Result.LogicDescriptor := Result.LogicDescriptor.Labelled(LArgs[0].Text);
          end
          else
          begin
            Count(3, 3);
            Kind(0, vkFlowPort);
            Kind(1, vkFlowType);
            Kind(2, vkText);

            if LMethod = 'input' then
            begin
              Result.LogicDescriptor := Result.LogicDescriptor.Input(NyxFlowPort(LArgs[0].Text),
                TNyxFlowType(LArgs[1].Ordinal), LArgs[2].Text);
            end
            else if LMethod = 'output' then
            begin
              Result.LogicDescriptor := Result.LogicDescriptor.Output(NyxFlowPort(LArgs[0].Text),
                TNyxFlowType(LArgs[1].Ordinal), LArgs[2].Text);
            end
            else
              Fail('Unsupported Logic descriptor method');
          end;
        end;
    else
      Fail('Typed Logic value has no fluent methods');
    end;
  end;

  if (Result.Kind = vkFlowGraph) and not LBuilt then
  begin
    Fail('Saved graph requires its immutable Build result');
  end;
end;

procedure TConfigurationReader.FlowDefaults;
var
  LArgs: TValues;
  LReference: TNyxFlowRef;
begin
  Inc(FCursor, 3);
  repeat
    Expect('.');
    Expect('Define');
    LArgs := Arguments(2);

    if (Length(LArgs) <> 2) or (LArgs[0].Kind <> vkFlowRef) or
      (LArgs[1].Kind <> vkFlowGraph) then
    begin
      Fail('Flows.Define requires a distinct graph reference and immutable typed graph');
    end;
    LReference := NyxFlowRef(LArgs[0].Text);

    if FApply then
    begin

      if FDocument.Flows.Contains(LReference) then
      begin
        Fail('Define each saved graph once in the managed builder');
      end;
      FDocument.Flows.Define(LReference, NyxFlowDefinitionFromData(LArgs[1].Data));
    end;
  until not At('.');
  Expect(';');
end;
