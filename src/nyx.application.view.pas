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
unit nyx.application.view;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses nyx.text, nyx.types, nyx.model, nyx.root.types, nyx.resources,
  nyx.application.state, nyx.collections.bindings;

type
  { Copied initial application-view intent, independent of target widgets.
    Default shows ordinary pages/navigation. Focused selects one exact authored
    control while retaining its source/dependency context and hiding automatic
    page navigation. It stores no document, runtime or target reference.
    Configure before mounting; focus names resolve in that document and
    missing/empty selections refuse.
    Resolve/OwnerRoot borrow only for the call. CreateRuntime returns caller-owned
    independent stores; InitialCollections retains no runtime/document owner. }
  TNyxApplicationView = record
  private
    FControl: TNyxControlRef;
    function GetIsFocused: Boolean;
  public
    class function Focused(const AControl: TNyxControlRef): TNyxApplicationView; static;
    function Resolve(ADocument: TNyxDocument): TNyxNode;
    function OwnerRoot(ADocument: TNyxDocument): TNyxRootRef;
    function CreateRuntime(ADocument: TNyxDocument; APlatform: TNyxPlatform;
      const ALocale, AFallback: TNyxLocaleRef): TNyxApplicationState;
    { A descendant focus recomposes only its binding scope against the same
      runtime registry. Its containing hidden prototype remains validated. }
    function InitialCollections(ADocument: TNyxDocument;
      ARuntime: TNyxApplicationState): INyxCollectionBindings;
    property IsFocused: Boolean read GetIsFocused;
    property Control: TNyxControlRef read FControl;
  end;

implementation

uses nyx.composition;

class function TNyxApplicationView.Focused(const AControl: TNyxControlRef): TNyxApplicationView;
begin

  if AControl.ID = '' then
  begin
    raise ENyxModel.Create('An application focus requires an exact control reference');
  end;
  Result := Default(TNyxApplicationView);
  Result.FControl := AControl;
end;

function TNyxApplicationView.GetIsFocused: Boolean;
begin
  Result := FControl.ID <> '';
end;

function TNyxApplicationView.Resolve(ADocument: TNyxDocument): TNyxNode;
begin

  if ADocument = nil then
  begin
    raise ENyxModel.Create('An application view requires its source document');
  end;

  if IsFocused then
  begin
    Result := ADocument.Find(FControl.ID);
  end
  else
  begin

    if ADocument.Count = 0 then
    begin
      raise ENyxModel.Create('An application needs at least one page');
    end;
    Result := ADocument.Pages[0];
  end;

  if (Result = nil) or Result.IsRealized then
  begin
    raise ENyxModel.Create('Application focus must identify an existing authored control');
  end;
end;

function TNyxApplicationView.OwnerRoot(ADocument: TNyxDocument): TNyxRootRef;
var
  LRoot: TNyxNode;
  LIndex: Integer;
begin
  LRoot := Resolve(ADocument);
  while LRoot.Parent <> nil do
  begin
    LRoot := LRoot.Parent;
  end;
  for LIndex := 0 to ADocument.Count - 1 do
  begin

    if ADocument.Pages[LIndex] = LRoot then
    begin
      Exit(NyxPageRoot(LRoot.ID));
    end;
  end;
  for LIndex := 0 to ADocument.ComponentCount - 1 do
  begin

    if ADocument.Components[LIndex] = LRoot then
    begin
      Exit(NyxReusableRoot(LRoot.ID));
    end;
  end;
  raise ENyxModel.Create('Application focus must belong to an authored source root');
end;

function TNyxApplicationView.CreateRuntime(ADocument: TNyxDocument;
  APlatform: TNyxPlatform; const ALocale, AFallback: TNyxLocaleRef): TNyxApplicationState;
begin
  Resolve(ADocument);

  if IsFocused then
  begin
    Result := TNyxApplicationState.CreateForRoot(ADocument, OwnerRoot(ADocument),
      APlatform, ALocale, AFallback);
  end
  else
  begin
    Result := TNyxApplicationState.Create(ADocument, APlatform, ALocale, AFallback);
  end;
end;

function TNyxApplicationView.InitialCollections(ADocument: TNyxDocument;
  ARuntime: TNyxApplicationState): INyxCollectionBindings;
var
  LSource: TNyxNode;
  LRealized: TNyxNode;
  LOwner: TNyxRootRef;
begin

  if ARuntime = nil then
  begin
    raise ENyxModel.Create('Application bindings require their runtime owner');
  end;
  LSource := Resolve(ADocument);
  LOwner := OwnerRoot(ADocument);
  Result := ARuntime.RootCollections(LOwner);

  if LSource.ID <> LOwner.Name then
  begin
    LRealized := RealizeNyxView(ADocument, LSource);
    try
      Result := Result.Recompose(LRealized);
    finally
      LRealized.Free;
    end;
  end;
end;

end.
