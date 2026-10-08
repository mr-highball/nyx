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
unit nyx.test.image.lifecycle;

{$mode delphi}{$H+}
{$codepage utf8}

interface

uses
  {$ifdef PAS2JS}JS;{$else}SysUtils;{$endif}

procedure ImageLifecycleShared;
{$ifdef PAS2JS}
function ImageLifecycleControls: JSValue; async;
{$else}
procedure ImageLifecycleControls;
{$endif}

implementation

uses
  {$ifdef PAS2JS}SysUtils, Web, nyx.render.browser,{$else}
  Classes, Interfaces, Forms, Controls, Graphics, ExtCtrls, nyx.render.lcl,{$endif}
  Math, nyx.text, nyx.types, nyx.images, nyx.image.lifecycle, nyx.image.events,
  nyx.events, nyx.scheduler, nyx.behavior, nyx.callbacks, nyx.schema,
  nyx.model, nyx.codec, nyx.codegen, nyx.source, nyx.data,
  nyx.generated.view, nyx.image.fixtures, nyx.studio.session, nyx.studio.projects;

type
  TImageAction = (iaObserve, iaReplace, iaClear, iaRetire);
  TImageObservation = record
    Tag: Integer;
    Info: TNyxEventInfo;
  end;
  TImageObserver = class(TNyxEventCallback)
  private
    FTag: Integer;
  public
    constructor Create(ATag: Integer);
    procedure Invoke(const AEvent: TNyxEventInfo; const AExecution: INyxExecution); override;
  end;
  TImageChild = class(TInterfacedObject, INyxWork)
  private
    FRequest: TNyxImageRequestID;
  public
    constructor Create(ARequest: TNyxImageRequestID);
    procedure Execute(const AExecution: INyxExecution);
  end;

var
  GChecks: Integer;
  GLog: array of TImageObservation;
  GChildren: array of TNyxImageRequestID;
  GAction: TImageAction;
  GPost: Boolean;
  GFailReady: Boolean;
  {$ifdef PAS2JS}GView: TNyxBrowserRenderer;{$else}GView: TNyxLCLRenderer;{$endif}

procedure Check(ACondition: Boolean; const AReason: TNyxText);
begin

  if not ACondition then
  begin
    raise Exception.Create('Image lifecycle: ' + AReason);
  end;
  Inc(GChecks);
end;

constructor TImageChild.Create(ARequest: TNyxImageRequestID);
begin
  inherited Create;
  FRequest := ARequest;
end;

procedure TImageChild.Execute(const AExecution: INyxExecution);
begin
  SetLength(GChildren, Length(GChildren) + 1);
  GChildren[High(GChildren)] := FRequest;
end;

constructor TImageObserver.Create(ATag: Integer);
begin
  inherited Create;
  FTag := ATag;
end;

procedure TImageObserver.Invoke(const AEvent: TNyxEventInfo;
  const AExecution: INyxExecution);
var
  LAction: TImageAction;
  {$ifdef PAS2JS}LRetired: TNyxBrowserRenderer;{$else}LRetired: TNyxLCLRenderer;{$endif}
begin
  Check(AEvent.HasImage and AEvent.Image.Defined, 'actual callback owns its typed image snapshot');
  Check(not NyxEventResponse(AExecution).CanConsume, 'image observation cannot cancel a host default');
  SetLength(GLog, Length(GLog) + 1);
  GLog[High(GLog)].Tag := FTag;
  GLog[High(GLog)].Info := AEvent.Copy;

  if (FTag = 1) and (AEvent.Trigger = ntImageLoading) then
  begin

    if GPost then
    begin
      GView.Events.Scheduler.PostUI(TImageChild.Create(AEvent.Image.Request), AExecution);
    end;
    LAction := GAction;
    GAction := iaObserve;
    case LAction of
      iaObserve:
        begin
          { This registration only records its immutable observation. }
        end;
      iaReplace:
        begin
          GView.Root.Find('hero-image').Configure.Source(NyxEmbeddedImage(nimJPEG, ImageJPEG)).Done;
          GView.Sync;
        end;
      iaClear:
        begin
          GView.Root.Find('hero-image').Configure.Source(NyxNoImage).Done;
          GView.Sync;
        end;
      iaRetire:
        begin
          LRetired := GView;
          GView := nil;
          LRetired.Free;
        end;
    end;
  end;

  if GFailReady and (FTag = 1) and (AEvent.Trigger = ntImageReady) then
  begin
    raise Exception.Create('Deliberate image callback failure');
  end;
end;

procedure ImageLifecycleShared;
var
  LPeer: INyxImageLifecycle;
  LScope: INyxCancellationScope;
  LOldScope: INyxCancellationScope;
  LSnapshot: TNyxImageSnapshot;
  LOwned: TNyxImageSnapshot;
  LFirst, LNext: TNyxImageRequestID;
  LDocument: TNyxDocument;
  LSession: TNyxStudioSession;
  LMetadata: TNyxEventSchemas;
  LTrigger: TNyxTrigger;
  LIndex: Integer;
  LLine: Integer;
  LCount: Integer;
  LBefore, LAfter: TNyxText;
  LHandler: TNyxHandlerRef;
  LParsed: TNyxDocument;
  LWorkspace: TNyxSourceWorkspace;
  {$ifndef PAS2JS}LFile: TFileStream; LBytes: UTF8String;{$endif}
begin
  LPeer := NewNyxImageLifecycle;
  Check(not LPeer.Current.Defined, 'new image request peer is absent');
  LFirst := LPeer.Start(NyxEmbeddedImage(nimPNG, ImagePNG));
  Check(LPeer.Take(LSnapshot, LOldScope) and (LSnapshot.Phase = nipLoading),
    'loading owns an independent cancellation generation');
  Check(LPeer.Ready(LFirst, 100, 50), 'current request admits positive host dimensions');
  Check(LPeer.Take(LOwned, LScope) and (LOwned.Width = 100) and (LOwned.Height = 50),
    'ready snapshot retains dimensions and source');
  Check(not LPeer.Fail(LFirst, nifDecode, 'late failure'), 'duplicate terminal completion refuses');
  LNext := LPeer.Start(NyxNoImage);
  Check(LOldScope.Cancelled and LScope.Cancelled and (LNext <> LFirst),
    'replacement revokes every old phase before new delivery');
  Check(not LPeer.Ready(LFirst, 1, 1), 'stale completion cannot change clear');
  Check(LPeer.Take(LSnapshot, LScope) and (LSnapshot.Phase = nipCleared),
    'NoImage produces a separate cleared generation');
  Check((LOwned.Source.Kind = nisEmbedded) and (LOwned.Phase = nipReady),
    'retained snapshot remains exact after replacement');
  LPeer.Close;
  Check(LScope.Cancelled and not LPeer.Take(LSnapshot, LScope), 'closed peer revokes queued delivery');
  LPeer := nil;
  LScope := nil;
  LOldScope := nil;

  LDocument := BuildNyxDocument;
  LSession := TNyxStudioSession.Create;
  LParsed := nil;
  LWorkspace := nil;
  try
    LSession.Load(TNyxCodec.Encode(LDocument));
    LSession.Select('hero-image');
    LMetadata := NyxEventsMetadata(LSession.Document.Find('hero-image'), LSession.Document);
    LCount := 0;
    for LIndex := 0 to High(LMetadata) do
    begin

      if LMetadata[LIndex].Trigger in [ntImageLoading, ntImageReady, ntImageError, ntImageCleared] then
      begin
        Check((nctxImage in LMetadata[LIndex].Contexts) and
          (LMetadata[LIndex].Browser = ncAvailable) and (LMetadata[LIndex].Native = ncAvailable),
          'Studio metadata describes the typed both-target image producer');
        Inc(LCount);
      end;
    end;
    Check(LCount = 4, 'image metadata has every implemented lifecycle phase');
    for LTrigger := ntImageLoading to ntImageCleared do
    begin
      LBefore := EncodeNyxProject(LSession.ProjectSnapshot);
      LHandler := LSession.AddCallback(LTrigger, LLine);
      Check((LLine > 0) and (LSession.CallbackLine(LHandler) > 0),
        'ordinary authoring adds a handler and source navigation');
      Check(Pos(NyxTriggerTitle(LTrigger), LSession.Source) > 0,
        'generated specialized source uses the fluent image event method');
      LAfter := EncodeNyxProject(LSession.ProjectSnapshot);
      LSession.Undo;
      Check(EncodeNyxProject(LSession.ProjectSnapshot) = LBefore, 'one Undo restores exact event/source pair');
      LSession.Redo;
      Check(EncodeNyxProject(LSession.ProjectSnapshot) = LAfter, 'one Redo restores exact image handler pair');
    end;
    LHandler := LSession.AddCallback(ntImageReady, LLine);
    Check((LLine > 0) and (LSession.CallbackLine(LHandler) > 0),
      'second authored ready registration has its own handler');
    LSession.SetCallbackPolicy(ntImageReady, neUIQueue);
    Check(Pos('nyx.image.lifecycle', LSession.Source) > 0,
      'crafted companion imports its typed callback context and phase enums');
    LParsed := TNyxSourceWorkspace.PrepareDraft(LSession.Source, LWorkspace);
    Check(TNyxCodec.Encode(LParsed) = TNyxCodec.Encode(LSession.Document),
      'managed source round trip retains typed registrations and policy');
    {$ifndef PAS2JS}

    if ParamCount >= 4 then
    begin
      Check(Pos('unit nyx.generated.view;', LSession.Source) > 0,
        'emitted companion has the expected owned namespace');
      LBytes := StringReplace(LSession.Source, 'unit nyx.generated.view;',
        'unit nyx.generated.image.lifecycle;', []);
      LFile := TFileStream.Create(ParamStr(4), fmCreate);
      try
        LFile.WriteBuffer(Pointer(LBytes)^, Length(LBytes));
      finally
        LFile.Free;
      end;
    end;
    {$endif}
  finally
    LWorkspace.Free;
    LParsed.Free;
    LSession.Free;
    LDocument.Free;
  end;
end;

function Count(ARequest: TNyxImageRequestID; APhase: TNyxImagePhase): Integer;
var
  LIndex: Integer;
begin
  Result := 0;
  for LIndex := 0 to High(GLog) do
  begin

    if (GLog[LIndex].Info.Image.Request = ARequest) and
      (GLog[LIndex].Info.Image.Phase = APhase) then
    begin
      Inc(Result);
    end;
  end;
end;

function Latest: TNyxImageRequestID;
begin
  Check(Length(GLog) > 0, 'request has observable callbacks');
  Result := GLog[High(GLog)].Info.Image.Request;
end;

{$ifdef PAS2JS}
function Pause: JSValue; async;
var
  LPending: TJSPromise;

  procedure Start(AResolve, AReject: TJSPromiseResolver);

    procedure Resume;
    begin
      AResolve(Undefined);
    end;

  begin
    window.setTimeout(@Resume, 10);
  end;

begin
  LPending := TJSPromise.new(@Start);
  Result := await(TJSPromise.resolve(LPending));
end;

function Pump: JSValue; async;
var
  LIndex: Integer;
begin
  Result := Undefined;
  for LIndex := 1 to 12 do
  begin
    await(Pause);
  end;
end;

function AwaitPhase(APhase: TNyxImagePhase): JSValue; async;
var
  LDeadline: Double;
  LIndex: Integer;
  LTrace: String;
begin
  Result := Undefined;
  document.body.setAttribute('data-image-await-phase', IntToStr(Ord(APhase)));
  LDeadline := TJSDate.now + 5000;
  repeat
    await(Pause);

    if (Length(GLog) > 0) and (GLog[High(GLog)].Info.Image.Phase = APhase) then
    begin
      await(Pump);
      Exit;
    end;
  until TJSDate.now >= LDeadline;
  document.body.setAttribute('data-image-await-log-count', IntToStr(Length(GLog)));
  LTrace := '';
  for LIndex := Max(0, Length(GLog) - 12) to High(GLog) do
  begin
    LTrace := LTrace + IntToStr(GLog[LIndex].Tag) + ':' +
      IntToStr(GLog[LIndex].Info.Image.Request) + ':' +
      IntToStr(Ord(GLog[LIndex].Info.Image.Phase)) + '|';
  end;
  document.body.setAttribute('data-image-await-trace', LTrace);

  if Length(GLog) > 0 then
  begin
    document.body.setAttribute('data-image-await-last-phase',
      IntToStr(Ord(GLog[High(GLog)].Info.Image.Phase)));
  end;
  raise Exception.Create('Image lifecycle phase ' + IntToStr(Ord(APhase)) +
    ' exceeded five-second deadline after ' + IntToStr(Length(GLog)) + ' observations');
end;
{$else}
procedure Pump;
var
  LDeadline: QWord;
begin
  LDeadline := GetTickCount64 + 100;
  repeat
    Application.ProcessMessages;
    CheckSynchronize(0);
    Sleep(1);
  until GetTickCount64 >= LDeadline;
end;

procedure AwaitPhase(APhase: TNyxImagePhase);
begin
  Pump;
  Check((Length(GLog) > 0) and (GLog[High(GLog)].Info.Image.Phase = APhase),
    'native request completes at the UI delivery turn');
end;
{$endif}

{$ifdef PAS2JS}
function ImageLifecycleControls: JSValue; async;
{$else}
procedure ImageLifecycleControls;
{$endif}
var
  LDocument: TNyxDocument;
  LEvents: INyxEvents;
  LReady: INyxEventSubscription;
  LSibling: INyxEventSubscription;
  LSub: INyxEventSubscription;
  LTrigger: TNyxTrigger;
  LInitial, LRequest, LDiscarded: TNyxImageRequestID;
  LCount: Integer;
  LOwned: TNyxEventInfo;
  {$ifndef PAS2JS}LBytes: TNyxImageBytes; LRefused: Boolean;{$endif}
  {$ifdef PAS2JS}LHost: TJSHTMLElement; LFace: TJSHTMLImageElement;
  {$else}LHost: TForm; LFace: TImage;{$endif}
begin
  {$ifdef PAS2JS}Result := Undefined;{$endif}
  GLog := nil;
  GChildren := nil;
  GAction := iaObserve;
  GPost := False;
  GFailReady := True;
  LDocument := BuildNyxDocument;
  {$ifdef PAS2JS}
  LHost := TJSHTMLElement(document.createElement('main'));
  LHost.style.setProperty('width', '380px');
  document.body.appendChild(LHost);
  GView := TNyxBrowserRenderer.Create;
  {$else}
  LHost := TForm.CreateNew(nil);
  LHost.SetBounds(30, 30, 420, 620);
  LHost.Show;
  GView := TNyxLCLRenderer.Create;
  {$endif}
  LEvents := GView.Events;
  try
    for LTrigger := ntImageLoading to ntImageCleared do
    begin
      LSub := LEvents.On(NyxControlEvents('hero-image'), LTrigger).Subscribe(TImageObserver.Create(1));

      if LTrigger = ntImageReady then
      begin
        LReady := LSub;
      end;
      LSub := LEvents.On(NyxControlEvents('hero-image'), LTrigger).Subscribe(TImageObserver.Create(2));

      if LTrigger = ntImageReady then
      begin
        LSibling := LSub;
      end;
    end;
    GView.Render(LDocument, LDocument.Pages[0], LHost, False);
    Check(Length(GLog) = 0, 'candidate construction and publication cannot invoke image callbacks');
    {$ifdef PAS2JS}await(AwaitPhase(nipReady));{$else}AwaitPhase(nipReady);{$endif}
    LInitial := Latest;
    Check((Count(LInitial, nipLoading) = 2) and (Count(LInitial, nipReady) = 2),
      'actual initial image delivers both phases and ordered siblings');
    Check((GLog[0].Tag = 1) and (GLog[1].Tag = 2) and (GLog[2].Tag = 1) and (GLog[3].Tag = 2),
      'phase and registration ordering is retained');
    Check((LReady.LastExecution.Status = nesFailed) and
      (LReady.LastExecution.Failure = 'Deliberate image callback failure') and
      (LSibling.LastExecution.Status = nesSucceeded), 'failure stays per registration and siblings continue');
    GFailReady := False;
    LOwned := GLog[High(GLog)].Info.Copy;
    Check((LOwned.Image.Width = 100) and (LOwned.Image.Height = 50), 'actual decoded dimensions are typed');
    {$ifdef PAS2JS}
    LFace := TJSHTMLImageElement(GView.ElementFor('hero-image'));
    {$else}
    LFace := TImage(GView.ControlFor('hero-image'));
    {$endif}
    LCount := Length(GLog);
    GView.Sync;
    {$ifdef PAS2JS}await(Pump);{$else}Pump;{$endif}
    Check(Length(GLog) = LCount, 'unchanged source has no duplicate lifecycle');

    GView.Root.Find('hero-image').Configure.Source(NyxEmbeddedImage(nimJPEG, ImageJPEG)).Done;
    GView.Sync;
    {$ifdef PAS2JS}await(AwaitPhase(nipReady));{$else}AwaitPhase(nipReady);{$endif}
    LRequest := Latest;
    Check((LRequest <> LInitial) and (Count(LRequest, nipLoading) = 2) and
      (Count(LRequest, nipReady) = 2), 'source replacement has its own complete lifecycle');
    Check(LOwned.Image.Source.ToWire = NyxEmbeddedImage(nimPNG, ImagePNG).ToWire,
      'retained initial snapshot is independent of later source');
    {$ifdef PAS2JS}Check(GView.ElementFor('hero-image') = LFace, 'browser replacement retains the physical face');
    {$else}Check(GView.ControlFor('hero-image') = LFace, 'native replacement retains the physical face');{$endif}

    LDiscarded := LRequest + 1;
    GView.Root.Find('hero-image').Configure.Source(NyxEmbeddedImage(nimPNG, ImagePNG)).Done;
    GView.Sync;
    GView.Root.Find('hero-image').Configure.Source(NyxEmbeddedImage(nimJPEG, ImageJPEG)).Done;
    GView.Sync;
    {$ifdef PAS2JS}await(AwaitPhase(nipReady));{$else}AwaitPhase(nipReady);{$endif}
    LRequest := Latest;
    Check((Count(LDiscarded, nipLoading) = 0) and (Count(LDiscarded, nipReady) = 0) and
      (Count(LRequest, nipReady) = 2), 'rapid replacement suppresses stale queued phases');

    GView.Root.Find('hero-image').Configure.Source(NyxImage(NyxImageLocation('./missing-image-lifecycle.png'))).Done;
    GView.Sync;
    {$ifdef PAS2JS}await(AwaitPhase(nipFailed));{$else}AwaitPhase(nipFailed);{$endif}
    LRequest := Latest;
    Check((Count(LRequest, nipLoading) = 2) and (Count(LRequest, nipFailed) = 2) and
      (GLog[High(GLog)].Info.Image.Failure <> nifNone) and
      (GLog[High(GLog)].Info.Image.Message <> ''), 'host failure owns typed failure and diagnostic');

    GView.Root.Find('hero-image').Configure.Source(NyxEmbeddedImage(nimPNG, ImagePNG)).Done;
    GView.Sync;
    {$ifdef PAS2JS}await(AwaitPhase(nipReady));{$else}AwaitPhase(nipReady);{$endif}
    Check(Count(Latest, nipReady) = 2, 'valid replacement recovers after failed location');

    {$ifndef PAS2JS}
    { Keep the same explicitly unchecked damaged-pixel case as the maintained
      media probe. Its public error observation must survive Sync's exception;
      the independent native decoder still preserves the previously drawn face. }
    LBytes := NyxEmbeddedImage(nimPNG, ImagePNG).Bytes;
    Check((Length(LBytes) > 64) and (LBytes[58] = Ord('I')) and
      (LBytes[59] = Ord('D')) and (LBytes[60] = Ord('A')) and
      (LBytes[61] = Ord('T')), 'unchecked decode fixture identifies its IDAT chunk');
    LBytes[64] := $07;
    GView.Root.Find('hero-image').Configure.Source(NyxEmbeddedImageBytes(nimPNG,
      LBytes, NyxImageValidation.ContainerChecksums(False))).Done;
    LRefused := False;
    try
      GView.Sync;
    except
      on Exception do
      begin
        LRefused := True;
      end;
    end;
    AwaitPhase(nipFailed);
    Check(LRefused and (GLog[High(GLog)].Info.Image.Failure = nifDecode) and
      not GLog[High(GLog)].Info.Image.Source.Validation.ChecksumsRequired and
      (LFace.Picture.Width = 100) and (LFace.Picture.Height = 50),
      'native decoder failure preserves old pixels and reports the exact request policy');
    GView.Root.Find('hero-image').Configure.Source(NyxEmbeddedImage(nimPNG, ImagePNG)).Done;
    GView.Sync;
    AwaitPhase(nipReady);
    Check(Count(Latest, nipReady) = 2, 'restoring the retained native picture starts a fresh successful request');
    LBytes := nil;
    {$endif}

    LEvents.OnImageReady(NyxControlEvents('hero-image')).Policy(neUIQueue);
    GPost := True;
    GAction := iaReplace;
    LDiscarded := Latest + 1;
    { Supersede one pending request before pumping; the current PNG request's
      first Loading registration then replaces itself with JPEG reentrantly. }
    GView.Root.Find('hero-image').Configure.Source(NyxEmbeddedImage(nimJPEG, ImageJPEG)).Done;
    GView.Sync;
    GView.Root.Find('hero-image').Configure.Source(NyxEmbeddedImage(nimPNG, ImagePNG)).Done;
    GView.Sync;
    LDiscarded := LDiscarded + 1;
    {$ifdef PAS2JS}await(AwaitPhase(nipReady));{$else}AwaitPhase(nipReady);{$endif}
    Check((Count(LDiscarded, nipLoading) = 1) and (Count(LDiscarded, nipReady) = 0),
      'reentrant source replacement cancels not-yet-started sequential siblings and ready');
    Check((Length(GChildren) = 1) and (GChildren[0] = Latest),
      'PostUI inherits request cancellation even before Submit returns');

    GAction := iaClear;
    LDiscarded := Latest + 1;
    GView.Root.Find('hero-image').Configure.Source(NyxEmbeddedImage(nimPNG, ImagePNG)).Done;
    GView.Sync;
    {$ifdef PAS2JS}await(AwaitPhase(nipCleared));{$else}AwaitPhase(nipCleared);{$endif}
    Check((Count(LDiscarded, nipLoading) = 1) and (Count(LDiscarded, nipReady) = 0) and
      (Count(Latest, nipCleared) = 2), 'reentrant clear cancels stale siblings and announces a new clear');
    {$ifdef PAS2JS}Check(not LFace.hasAttribute('src'), 'clear removes src without fetching the page');
    {$else}Check(LFace.Picture.Width = 0, 'clear empties the native accepted picture');{$endif}

    GAction := iaRetire;
    LDiscarded := Latest + 1;
    LCount := Length(GChildren);
    GView.Root.Find('hero-image').Configure.Source(NyxEmbeddedImage(nimPNG, ImagePNG)).Done;
    GView.Sync;
    {$ifdef PAS2JS}await(Pump);{$else}Pump;{$endif}
    Check((GView = nil) and (Count(LDiscarded, nipLoading) = 1) and
      (Count(LDiscarded, nipReady) = 0) and (Length(GChildren) = LCount),
      'retiring inside callback revokes siblings, host completion and child UI work');
    {$ifdef PAS2JS}
    LCount := Length(GLog);
    LFace.dispatchEvent(TJSEvent.new('load'));
    LFace.dispatchEvent(TJSEvent.new('error'));
    await(Pump);
    Check(Length(GLog) = LCount, 'retired browser face has no remaining image event sink');
    {$endif}
    Check(LOwned.Image.Defined and (LOwned.Image.Width = 100),
      'snapshot remains owned after the physical view is destroyed');
  finally

    if GView <> nil then
    begin
      GView.Free;
      GView := nil;
    end;
    LEvents.Close;
    LEvents := nil;
    LReady := nil;
    LSibling := nil;
    LSub := nil;
    {$ifdef PAS2JS}LHost.remove; await(Pump);{$else}LHost.Free; Pump;{$endif}
    LDocument.Free;
    GLog := nil;
    GChildren := nil;
  end;
  WriteLn('PASS / image lifecycle / ', GChecks, ' checks');
  {$ifdef PAS2JS}document.body.setAttribute('data-image-lifecycle-checks', IntToStr(GChecks));{$endif}
end;

end.
