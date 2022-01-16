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
unit iPub.FMX.SystemBars;

interface

{$SCOPEDENUMS ON}

uses
  { Delphi }
  System.SysUtils,
  System.Classes,
  System.Messaging,
  System.Types,
  System.UITypes,
  System.Generics.Collections,
  FMX.Forms;

type
  { TipFormSystemBars }

  TipFormSystemBars = class(TPersistent)
  public type
    TVisibilityMode = TFormSystemStatusBar.TVisibilityMode;
  public const
    DefaultNavigationBarBackgroundColor = TAlphaColorRec.Null;
    DefaultStatusBarBackgroundColor = TFormSystemStatusBar.DefaultBackgroundColor;
    DefaultVisibility = TFormSystemStatusBar.DefaultVisibility;
  {$REGION 'internal'}
  public type
    Exception = class(System.SysUtils.Exception);

    /// <summary>Called from a form as Sender always when the form's insets has changed</summary>
    TInsetsChangeMessage = class(TMessage)
    private
      FInsets: TRectF;
      FTappableInsets: TRectF;
    public
      constructor Create(const AInsets, ATappableInsets: TRectF);
      property Insets: TRectF read FInsets;
      property TappableInsets: TRectF read FTappableInsets;
    end;

    /// <summary>Service for working with native system navigation bar</summary>
    IFMXWindowSystemBarsService = interface
    ['{124BEBCA-0F61-4A94-92E4-CA279E1BE2E3}']
      /// <summary>Sizes of all current system bars</summary>
      function GetInsets(const AForm: TCommonCustomForm): TRectF;
      /// <summary>Sizes of all current system bars without gesture bar</summary>
      function GetTappableInsets(const AForm: TCommonCustomForm): TRectF;
      /// <summary>Sets background color of system status bar</summary>
      procedure SetStatusBarBackgroundColor(const AForm: TCommonCustomForm; const AColor: TAlphaColor);
      /// <summary>Sets background color of system navigation bar</summary>
      procedure SetNavigationBarBackgroundColor(const AForm: TCommonCustomForm; const AColor: TAlphaColor);
      /// <summary>Sets how system bars will be shown. See TipFormSystemBars.TVisibilityMode</summary>
      procedure SetVisibility(const AForm: TCommonCustomForm; const AMode: TipFormSystemBars.TVisibilityMode);
    end;
  strict private
    [Weak] FForm: TCommonCustomForm;
    FFormInsetsChangeMessageId: Integer;
    FInsets: TRectF;
    FNavigationBarBackgroundColor: TAlphaColor;
    FOnInsetsChange: TNotifyEvent;
    FTappableInsets: TRectF;
    procedure FormInsetsChange(const ASender: TObject; const AMessage: TMessage);
    function GetStatusBarBackgroundColor: TAlphaColor;
    function GetVisibility: TVisibilityMode;
    procedure SetNavigationBarBackgroundColor(const AValue: TAlphaColor);
    procedure SetStatusBarBackgroundColor(const AValue: TAlphaColor);
    procedure SetVisibility(const AValue: TVisibilityMode);
  protected
    procedure AssignTo(ADest: TPersistent); override;
  {$ENDREGION}
  public
    constructor Create(const AForm: TCommonCustomForm);
    destructor Destroy; override;
    /// <summary>Distances in which the system bars are overlapping the sides of the form (top, left, right and bottom)</summary>
    property Insets: TRectF read FInsets;
    /// <summary>Distances in which the system bars, without gesture bar, are overlapping the sides of the form (top, left, right and bottom)</summary>
    property TappableInsets: TRectF read FTappableInsets;
    /// <summary>When system bars overlapping distances change, like navigation bar going to right when the phone go to landscape</summary>
    property OnInsetsChange: TNotifyEvent read FOnInsetsChange write FOnInsetsChange;
  published
    /// <summary>Background color of system navigation bar</summary>
    property NavigationBarBackgroundColor: TAlphaColor read FNavigationBarBackgroundColor write SetNavigationBarBackgroundColor default DefaultNavigationBarBackgroundColor;
    /// <summary>Background color of system status bar</summary>
    property StatusBarBackgroundColor: TAlphaColor read GetStatusBarBackgroundColor write SetStatusBarBackgroundColor default DefaultStatusBarBackgroundColor;
    /// <summary>Different modes of showing system bars</summary>
    property Visibility: TVisibilityMode read GetVisibility write SetVisibility default DefaultVisibility;
  end;

  { TipFormHelper }

  TipFormHelper = class helper for TCommonCustomForm
  {$REGION 'internal'}
  // The correct solution would be to change the FMX source to insert the SystemBars
  // property in TCommonCustomForm. But to avoid patches on Embarcadero's source, we
  // made this helper for the forms
  strict private
    class var
      FAfterCreateFormHandleMessageId: Integer;
      FDictionary: TObjectDictionary<TCommonCustomForm, TipFormSystemBars>;
      FFormReleasedMessageId: Integer;
    class procedure AfterCreateFormHandle(const ASender: TObject; const AMessage: TMessage); static;
    class constructor Create;
    class destructor Destroy;
    class procedure FormReleased(const ASender: TObject; const AMessage: TMessage); static;
    class function GetFormSystemBars(AForm: TCommonCustomForm): TipFormSystemBars; static;
  strict private
    function GetOnSystemBarsInsetsChange: TNotifyEvent;
    function GetSystemBars: TipFormSystemBars;
    procedure SetOnSystemBarsInsetsChange(const AValue: TNotifyEvent);
    procedure SetSystemBars(const AValue: TipFormSystemBars);
  {$ENDREGION}
  public
    /// <summary>Settings of system bars on mobile platforms</summary>
    property SystemBars: TipFormSystemBars read GetSystemBars write SetSystemBars;
    /// <summary>When system bars overlapping distances change, like navigation bar going to right when the phone go to landscape</summary>
    property OnSystemBarsInsetsChange: TNotifyEvent read GetOnSystemBarsInsetsChange write SetOnSystemBarsInsetsChange;
  end;

implementation

uses
  { Delphi }
  FMX.Platform,

  { iPub }
  iPub.FMX.SystemBars.Android,
  iPub.FMX.SystemBars.iOS;

{ TipFormSystemBars }

procedure TipFormSystemBars.AssignTo(ADest: TPersistent);
var
  LDestSystemBars: TipFormSystemBars;
begin
  if ADest is TipFormSystemBars then
  begin
    LDestSystemBars := TipFormSystemBars(ADest);
    LDestSystemBars.NavigationBarBackgroundColor := NavigationBarBackgroundColor;
    LDestSystemBars.StatusBarBackgroundColor := StatusBarBackgroundColor;
    LDestSystemBars.Visibility := Visibility;
  end
  else
    inherited;
end;

constructor TipFormSystemBars.Create(const AForm: TCommonCustomForm);
begin
  FForm := AForm;
  FNavigationBarBackgroundColor := DefaultNavigationBarBackgroundColor;
  FFormInsetsChangeMessageId := TMessageManager.DefaultManager.SubscribeToMessage(TInsetsChangeMessage, FormInsetsChange);
end;

destructor TipFormSystemBars.Destroy;
begin
  TMessageManager.DefaultManager.Unsubscribe(TInsetsChangeMessage, FFormInsetsChangeMessageId);
  inherited;
end;

procedure TipFormSystemBars.FormInsetsChange(const ASender: TObject;
  const AMessage: TMessage);
begin
  if ASender = FForm then
  begin
    FInsets := TInsetsChangeMessage(AMessage).Insets;
    FTappableInsets := TInsetsChangeMessage(AMessage).TappableInsets;
    if Assigned(FOnInsetsChange) then
      FOnInsetsChange(FForm);
  end;
end;

function TipFormSystemBars.GetStatusBarBackgroundColor: TAlphaColor;
begin
  Result := FForm.SystemStatusBar.BackgroundColor;
end;

function TipFormSystemBars.GetVisibility: TVisibilityMode;
begin
  Result := FForm.SystemStatusBar.Visibility;
end;

procedure TipFormSystemBars.SetNavigationBarBackgroundColor(
  const AValue: TAlphaColor);
var
  LService: IFMXWindowSystemBarsService;
begin
  if FNavigationBarBackgroundColor <> AValue then
  begin
    FNavigationBarBackgroundColor := AValue;
    if TPlatformServices.Current.SupportsPlatformService(IFMXWindowSystemBarsService, LService) then
      LService.SetNavigationBarBackgroundColor(FForm, FNavigationBarBackgroundColor);
  end;
end;

procedure TipFormSystemBars.SetStatusBarBackgroundColor(
  const AValue: TAlphaColor);
begin
  FForm.SystemStatusBar.BackgroundColor := AValue;
end;

procedure TipFormSystemBars.SetVisibility(const AValue: TVisibilityMode);
begin
  FForm.SystemStatusBar.Visibility := AValue;
end;

{ TipFormSystemBars.TInsetsChangeMessage }

constructor TipFormSystemBars.TInsetsChangeMessage.Create(const AInsets,
  ATappableInsets: TRectF);
begin
  inherited Create;
  FInsets := AInsets;
  FTappableInsets := ATappableInsets;
end;

{ TipFormHelper }

class procedure TipFormHelper.AfterCreateFormHandle(const ASender: TObject;
  const AMessage: TMessage);
begin
  // To approach a simulation of the creation of the system bars property in TCommonCustomForm.Create,
  // because the TipSystemBars need to subscribe to the TInsetsChangeMessage as soon as possible
  if ASender is TCommonCustomForm then
    TCommonCustomForm(ASender).SystemBars;
end;

class constructor TipFormHelper.Create;
begin
  FDictionary := TObjectDictionary<TCommonCustomForm, TipFormSystemBars>.Create([doOwnsValues]);
  FAfterCreateFormHandleMessageId := TMessageManager.DefaultManager.SubscribeToMessage(TAfterCreateFormHandle, AfterCreateFormHandle);
  FFormReleasedMessageId := TMessageManager.DefaultManager.SubscribeToMessage(TFormReleasedMessage, FormReleased);
end;

class destructor TipFormHelper.Destroy;
begin
  TMessageManager.DefaultManager.Unsubscribe(TFormReleasedMessage, FFormReleasedMessageId);
  TMessageManager.DefaultManager.Unsubscribe(TAfterCreateFormHandle, FAfterCreateFormHandleMessageId);
  FreeAndNil(FDictionary);
end;

class procedure TipFormHelper.FormReleased(const ASender: TObject;
  const AMessage: TMessage);
begin
  if ASender is TCommonCustomForm then
    FDictionary.Remove(TCommonCustomForm(ASender));
end;

class function TipFormHelper.GetFormSystemBars(
  AForm: TCommonCustomForm): TipFormSystemBars;
begin
  if not Assigned(AForm) then
    Exit(nil);
  if not FDictionary.TryGetValue(AForm, Result) then
  begin
    if (csDestroying in AForm.ComponentState) or (TFmxFormState.Released in AForm.FormState) then
      Exit(nil);
    Result := TipFormSystemBars.Create(AForm);
    FDictionary.Add(AForm, Result);
  end;
end;

function TipFormHelper.GetOnSystemBarsInsetsChange: TNotifyEvent;
begin
  Result := SystemBars.OnInsetsChange;
end;

function TipFormHelper.GetSystemBars: TipFormSystemBars;
begin
  Result := GetFormSystemBars(Self);
end;

procedure TipFormHelper.SetOnSystemBarsInsetsChange(const AValue: TNotifyEvent);
begin
  SystemBars.OnInsetsChange := AValue;
end;

procedure TipFormHelper.SetSystemBars(const AValue: TipFormSystemBars);
begin
  SystemBars.Assign(AValue);
end;

end.
