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

unit nyx.sample;

{$mode delphi}{$H+}
{$codepage utf8}

interface

uses
  nyx.text,
  nyx.types,
  nyx.model;

function CreateNyxSample: TNyxDocument;

implementation

function CreateNyxSample: TNyxDocument;
var
  LPage: TNyxNode;
  LCard: TNyxNode;
  LRow: TNyxNode;
  LDefinition: TNyxNode;
begin
  Result := TNyxDocument.Create;
  try
    { A starter is ordinary project content. Branch names and build milestones
      belong in development records, never in the editor's default document. }
    Result.Title := 'Untitled project';
    LDefinition := TNyxNode.Create(nkCard, 'welcome-card')
      .Configure.Padding(24).Gap(12).Done;
    Result.AddComponent(LDefinition);
    LDefinition.Add(TNyxNode.Create(nkHeading, 'welcome-title')
      .Configure.PartName(NyxPart('title'))
      .Text('Make something wonderful.').Done);
    LDefinition.Add(TNyxNode.Create(nkLabel, 'welcome-description')
      .Configure.PartName(NyxPart('description'))
      .Text('Create a page, arrange components, and make it your own.').Done);
    LPage := TNyxNode.Create(nkPage, 'home')
      .Configure.Padding(32).Gap(20).Done;
    Result.AddPage(LPage);
    LPage.Add(TNyxNode.Create(nkBadge, 'eyebrow').Configure.Text('WELCOME').Done);
    LPage.Add(TNyxNode.Create(nkComponent, 'welcome-instance')
      .Configure.Component(NyxComponent('welcome-card')).Done);
    LCard := TNyxNode.Create(nkCard, 'form').Configure.Padding(24).Gap(14).Done;
    LPage.Add(LCard);
    LCard.Add(TNyxNode.Create(nkHeading, 'form-title').Configure.Text('Your next idea').Done);
    LCard.Add(TNyxNode.Create(nkInput, 'project-name').Configure.Text('Project name')
      .Placeholder('A brilliant little application').Done);
    LCard.Add(TNyxNode.Create(nkMemo, 'project-description')
      .Configure.Text('Description').Placeholder('What will you build?').Done);
    LRow := TNyxNode.Create(nkRow, 'form-options').Configure.Layout(nlRow)
      .Gap(16).Done;
    LCard.Add(LRow);
    LRow.Add(TNyxNode.Create(nkCheckbox, 'email-updates').Configure.Text('Email updates')
      .Value(True).Done);
    LRow.Add(TNyxNode.Create(nkCheckbox, 'remember-preferences').Configure.Text('Remember preferences')
      .Value(True).Done);
    LCard.Add(TNyxNode.Create(nkButton, 'create-project').Configure.Text('Create project')
      .Variant(nvPrimary).Done);
  except
    Result.Free;
    raise;
  end;
end;

end.
