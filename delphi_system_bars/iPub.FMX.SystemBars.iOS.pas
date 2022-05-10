{************************************************************************}
{                                                                        }
{                          iPub.FMX.SystemBars                           }
{                                                                        }
{ Copyright (c) 2021-2022 iPub                                           }
{ https://github.com/viniciusfbb/fmx_tutorials                           }
{                                                                        }
{ Use of this source code is governed by a MIT license that can be found }
{ at https://opensource.org/licenses/MIT                                 }
{                                                                        }
{************************************************************************}
unit iPub.FMX.SystemBars.iOS;

interface

{$SCOPEDENUMS ON}
{$IFDEF iOS}

implementation

uses
  { Delphi }
  System.Classes,
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Math,
  System.Math.Vectors,
  System.Messaging,
  System.Rtti,
  System.TypInfo,
  iOSapi.Helpers,
  iOSapi.UIKit,
  iOSapi.Foundation,
  FMX.Forms,
  FMX.Platform,

  { iPub }
  iPub.FMX.SystemBars;

type
  { TipSystemBarsServiceiOS }

  TipSystemBarsServiceiOS = class(TInterfacedObject, TipFormSystemBars.IFMXWindowSystemBarsService, IFMXWindowSystemStatusBarService)
  private
    FAfterCreateFormHandleMessageId: Integer;
    FFormActivateMessageId: Integer;
    FDefaultStatusBarService: IFMXWindowSystemStatusBarService;
    FGestureBarChecked: Boolean;
    FGestureBarOffset: Single;
    FOrientationChangedMessageId: Integer;
    FRegisteredBarsService: Boolean;
    FVirtualKeyboardBounds: TRect;
    FVirtualKeyboardMessageId: Integer;
    procedure AfterCreateFormHandle(const ASender: TObject; const AMessage: TMessage);
    procedure CheckInsetsChanges(const AForm: TCommonCustomForm);
    procedure FormActivate(const ASender: TObject; const AMessage: TMessage);
    function GetGestureBarOffset(const AForm: TCommonCustomForm): Single;
    function GetStatusBarOffset(const AForm: TCommonCustomForm): Single;
    procedure OrientationChanged(const ASender: TObject; const AMessage: TMessage);
    function RemoveKeyboardOverlappedBars(const AInsets: TRectF): TRectF;
    procedure VirtualKeyboardChangeHandler(const ASender: TObject; const AMessage: TMessage);
  public
    constructor Create;
    destructor Destroy; override;
    { IFMXWindowSystemBarsService }
    function GetInsets(const AForm: TCommonCustomForm): TRectF;
    function GetTappableInsets(const AForm: TCommonCustomForm): TRectF;
    procedure SetNavigationBarBackgroundColor(const AForm: TCommonCustomForm; const AColor: TAlphaColor);
    { IFMXWindowSystemStatusBarService / IFMXWindowSystemBarsService }
    procedure IFMXWindowSystemStatusBarService.SetBackgroundColor = SetStatusBarBackgroundColor;
    procedure SetStatusBarBackgroundColor(const AForm: TCommonCustomForm; const AColor: TAlphaColor);
    procedure SetVisibility(const AForm: TCommonCustomForm; const AMode: TFormSystemStatusBar.TVisibilityMode);
  end;

var
  FSystemBarsServiceiOS: TipSystemBarsServiceiOS;

{ TipSystemBarsServiceiOS }

procedure TipSystemBarsServiceiOS.AfterCreateFormHandle(const ASender: TObject;
  const AMessage: TMessage);
begin
  if (ASender is TCommonCustomForm) and (TFmxFormState.Recreating in TCommonCustomForm(ASender).FormState) then
    CheckInsetsChanges(TCommonCustomForm(ASender));
end;

procedure TipSystemBarsServiceiOS.CheckInsetsChanges(
  const AForm: TCommonCustomForm);
var
  LNewInsets: TRectF;
  LNewTappableInsets: TRectF;
  LFormSystemBars: TipFormSystemBars;
begin
  if Assigned(AForm) and AForm.Active then
  begin
    LFormSystemBars := AForm.SystemBars;
    if Assigned(LFormSystemBars) then
    begin
      LNewInsets := GetInsets(AForm);
      LNewTappableInsets := GetTappableInsets(AForm);
      if (not LNewInsets.EqualsTo(LFormSystemBars.Insets, TEpsilon.Position)) or
        (not LNewTappableInsets.EqualsTo(LFormSystemBars.TappableInsets, TEpsilon.Position)) then
      begin
        TMessageManager.DefaultManager.SendMessage(AForm, TipFormSystemBars.TInsetsChangeMessage.Create(LNewInsets, LNewTappableInsets));
      end;
    end;
  end;
end;

constructor TipSystemBarsServiceiOS.Create;
begin
  inherited;
  if TPlatformServices.Current.SupportsPlatformService(IFMXWindowSystemStatusBarService, FDefaultStatusBarService) then
  begin
    TPlatformServices.Current.RemovePlatformService(IFMXWindowSystemStatusBarService);
    TPlatformServices.Current.AddPlatformService(IFMXWindowSystemStatusBarService, Self);
  end
  else
    raise TipFormSystemBars.Exception.Create('Cannot possible to find the service IFMXWindowSystemStatusBarService');
  if not TPlatformServices.Current.SupportsPlatformService(TipFormSystemBars.IFMXWindowSystemBarsService) then
  begin
    TPlatformServices.Current.AddPlatformService(TipFormSystemBars.IFMXWindowSystemBarsService, Self);
    FRegisteredBarsService := True;
  end;
  FAfterCreateFormHandleMessageId := TMessageManager.DefaultManager.SubscribeToMessage(TAfterCreateFormHandle, AfterCreateFormHandle);
  FFormActivateMessageId := TMessageManager.DefaultManager.SubscribeToMessage(TFormActivateMessage, FormActivate);
  FOrientationChangedMessageId := TMessageManager.DefaultManager.SubscribeToMessage(TOrientationChangedMessage, OrientationChanged);
  FVirtualKeyboardMessageId := TMessageManager.DefaultManager.SubscribeToMessage(TVKStateChangeMessage, VirtualKeyboardChangeHandler);
end;

destructor TipSystemBarsServiceiOS.Destroy;
begin
  TMessageManager.DefaultManager.Unsubscribe(TVKStateChangeMessage, FVirtualKeyboardMessageId);
  TMessageManager.DefaultManager.Unsubscribe(TOrientationChangedMessage, FOrientationChangedMessageId);
  TMessageManager.DefaultManager.Unsubscribe(TFormActivateMessage, FFormActivateMessageId);
  TMessageManager.DefaultManager.Unsubscribe(TAfterCreateFormHandle, FAfterCreateFormHandleMessageId);
  if FRegisteredBarsService then
    TPlatformServices.Current.RemovePlatformService(TipFormSystemBars.IFMXWindowSystemBarsService);
  if Assigned(FDefaultStatusBarService) then
  begin
    TPlatformServices.Current.RemovePlatformService(IFMXWindowSystemStatusBarService);
    TPlatformServices.Current.AddPlatformService(IFMXWindowSystemStatusBarService, FDefaultStatusBarService);
  end;
  inherited;
end;

procedure TipSystemBarsServiceiOS.FormActivate(const ASender: TObject;
  const AMessage: TMessage);
begin
  if ASender is TCommonCustomForm then
    CheckInsetsChanges(TCommonCustomForm(ASender));
end;

function TipSystemBarsServiceiOS.GetGestureBarOffset(
  const AForm: TCommonCustomForm): Single;

  procedure CalcGestureBarOffset;
  begin
    TThread.Synchronize(nil,
      procedure()
      var
        LSharedApplication: UIApplication;
        LWindows: NSArray;
        LWindow: UIWindow;
        LViewController: UIViewController;
        LView: UIView;
      begin
        if not FSystemBarsServiceiOS.FGestureBarChecked then
        begin
          LSharedApplication := TiOSHelper.SharedApplication;
          if Assigned(LSharedApplication) then
          begin
            LWindows := LSharedApplication.windows;
            if Assigned(LWindows) and (LWindows.count > 0) then
            begin
              LWindow := TUIWindow.Wrap(LWindows.objectAtIndex(0));
              if Assigned(LWindow) then
              begin
                LViewController := LWindow.rootViewController;
                if Assigned(LViewController) then
                begin
                  LView := LViewController.view;
                  if Assigned(LView) then
                  begin
                    FSystemBarsServiceiOS.FGestureBarOffset := (LView.bounds.origin.y + LView.bounds.size.height) -
                      (LWindow.safeAreaLayoutGuide.layoutFrame.origin.y + LWindow.safeAreaLayoutGuide.layoutFrame.size.height);
                    FSystemBarsServiceiOS.FGestureBarChecked := True;
                  end;
                end;
              end;
            end;
          end;
        end;
      end);
  end;

  function HasFormGestureBar(const AForm: TCommonCustomForm): Boolean;
  begin
    Result := (AForm <> nil) and (AForm.BorderStyle <> TFmxFormBorderStyle.None);
  end;

begin
  Result := 0;
  if HasFormGestureBar(AForm) then
  begin
    if not FGestureBarChecked then
      CalcGestureBarOffset;
    Result := FGestureBarOffset;
  end;
end;

function TipSystemBarsServiceiOS.GetInsets(const AForm: TCommonCustomForm): TRectF;
begin
  Result := GetTappableInsets(AForm);
  Result.Bottom := Max(Result.Bottom, GetGestureBarOffset(AForm));
  Result := RemoveKeyboardOverlappedBars(Result);
end;

function TipSystemBarsServiceiOS.GetStatusBarOffset(const AForm: TCommonCustomForm): Single;

  function HasFormStatusBarOffset(const AForm: TCommonCustomForm): Boolean;
  begin
    Result := (AForm <> nil) and (AForm.BorderStyle <> TFmxFormBorderStyle.None) and
      (AForm.SystemStatusBar.Visibility = TFormSystemStatusBar.TVisibilityMode.VisibleAndOverlap);
  end;

  function GetStatusBarOffsetUsingRtti: Single;
  const
    CLASS_NOT_FOUND = 'Cannot possible to find the class FMX.Platform.iOS.TCocoaTouchWindowManager';
    PROPERTY_NOT_FOUND = 'Cannot possible to find the property "StatusBarOffset: Single" in class FMX.Platform.iOS.TCocoaTouchWindowManager';
  var
    LRttiContext: TRttiContext;
    LRttiType: TRttiType;
    LRttiProperty: TRttiProperty;
    LCocoaTouchWindowManager: TObject;
  begin
    {$IF CompilerVersion > 35} // Delphi 11 Alexandria
      {$MESSAGE WARN 'Check in file FMX.Platform.iOS.pas if the class TCocoaTouchWindowManager have already the property "StatusBarOffset: Single" and adjust the IFDEF'}
    {$ENDIF}
    LCocoaTouchWindowManager := TObject(FDefaultStatusBarService);
    Assert(LCocoaTouchWindowManager.ClassName = 'TCocoaTouchWindowManager', CLASS_NOT_FOUND);
    LRttiContext := TRttiContext.Create;
    try
      LRttiType := LRttiContext.GetType(LCocoaTouchWindowManager.ClassType);
      if not (LRttiType is TRttiInstanceType) then
        raise TipFormSystemBars.Exception.Create(CLASS_NOT_FOUND);
      LRttiProperty := LRttiType.GetProperty('StatusBarOffset');
      if (not Assigned(LRttiProperty)) or (LRttiProperty.PropertyType.Handle <> TypeInfo(Single)) then
        raise TipFormSystemBars.Exception.Create(PROPERTY_NOT_FOUND);
      Result := LRttiProperty.GetValue(LCocoaTouchWindowManager).AsExtended;
    finally
      LRttiContext.Free;
    end;
  end;

begin
  if HasFormStatusBarOffset(AForm) and Assigned(FDefaultStatusBarService) then
    Result := GetStatusBarOffsetUsingRtti
  else
    Result := 0;
end;

function TipSystemBarsServiceiOS.GetTappableInsets(const AForm: TCommonCustomForm): TRectF;
begin
  Result := TRectF.Create(0, GetStatusBarOffset(AForm), 0, 0);
  Result := RemoveKeyboardOverlappedBars(Result);
end;

procedure TipSystemBarsServiceiOS.OrientationChanged(const ASender: TObject;
  const AMessage: TMessage);
begin
  if Assigned(Screen) then
    CheckInsetsChanges(Screen.ActiveForm);
end;

function TipSystemBarsServiceiOS.RemoveKeyboardOverlappedBars(
  const AInsets: TRectF): TRectF;
var
  LScreenSize: TSizeF;
begin
  Result := AInsets;
  if (not FVirtualKeyboardBounds.IsEmpty) and (AInsets <> TRectF.Empty) then
  begin
    LScreenSize := Screen.Size;
    // Check if virtual keyboard is in bottom
    if SameValue(LScreenSize.Height, FVirtualKeyboardBounds.Bottom, TEpsilon.Position) and (FVirtualKeyboardBounds.Left = 0) and
      SameValue(FVirtualKeyboardBounds.Right, LScreenSize.Width, TEpsilon.Position) then
    begin
      // Removing bottom system bars
      Result.Bottom := Max(Result.Bottom - FVirtualKeyboardBounds.Height, 0);
    end;
  end;
end;

procedure TipSystemBarsServiceiOS.SetNavigationBarBackgroundColor(
  const AForm: TCommonCustomForm; const AColor: TAlphaColor);
begin
end;

procedure TipSystemBarsServiceiOS.SetStatusBarBackgroundColor(
  const AForm: TCommonCustomForm; const AColor: TAlphaColor);
begin
  FDefaultStatusBarService.SetBackgroundColor(AForm, AColor);
end;

procedure TipSystemBarsServiceiOS.SetVisibility(const AForm: TCommonCustomForm;
  const AMode: TFormSystemStatusBar.TVisibilityMode);
var
  LWindowService: IFMXWindowService;
begin
  if Assigned(AForm) then
  begin
    FDefaultStatusBarService.SetVisibility(AForm, AMode);
    if AForm.Active and TPlatformServices.Current.SupportsPlatformService(IFMXWindowService, LWindowService) then
      LWindowService.ShowWindow(AForm); // Force update
    TMessageManager.DefaultManager.SendMessage(AForm,
      TipFormSystemBars.TInsetsChangeMessage.Create(GetInsets(AForm), GetTappableInsets(AForm)));
  end;
end;

procedure TipSystemBarsServiceiOS.VirtualKeyboardChangeHandler(
  const ASender: TObject; const AMessage: TMessage);
begin
  if AMessage is TVKStateChangeMessage then
  begin
    FVirtualKeyboardBounds := TVKStateChangeMessage(AMessage).KeyboardBounds;
    if Assigned(Screen) then
      CheckInsetsChanges(Screen.ActiveForm);
  end;
end;

initialization
  FSystemBarsServiceiOS := TipSystemBarsServiceiOS.Create;
finalization
  FreeAndNil(FSystemBarsServiceiOS);
{$ELSE}
implementation
{$ENDIF}
end.
